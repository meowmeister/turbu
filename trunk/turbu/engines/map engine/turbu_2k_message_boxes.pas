{*****************************************************************************
* The contents of this file are used with permission, subject to
* the Mozilla Public License Version 1.1 (the "License"); you may
* not use this file except in compliance with the License. You may
* obtain a copy of the License at
* http://www.mozilla.org/MPL/MPL-1.1.html
*
* Software distributed under the License is distributed on an
* "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
* implied. See the License for the specific language governing
* rights and limitations under the License.
*
*****************************************************************************
*
* This file was created by Mason Wheeler.  He can be reached for support at
* www.turbu-rpg.com.
*****************************************************************************}

unit turbu_2k_message_boxes;

interface
uses
   Types,
   sdl_sprite, sdl_canvas,
   turbu_defs, turbu_2k_frames, turbu_2k_menu_basis;

type
   TMessageBox = class(TGameMenuBox)
   private
      FPortrait: TSprite;
      FNextArrow: TTiledAreaSprite;
      FRightPortrait: boolean;

      procedure setRightside(const value: boolean);
   private //drawing section
      FTextRate: single;
      FRemainder: single;
      FImmediate: boolean;

      procedure DrawNextChar;
      procedure SetTextRate(value: integer);
   protected
      procedure NewLine; override;
      procedure DrawSpecialChar(const line: string); override;
      procedure InsertText(const text: string);
      procedure DrawText; override;
      procedure PostDrawText; override;
      procedure ResetText; override;
      procedure doButton(const input: TButtonCode); override;
      procedure DoSetPosition(const Value: TMboxLocation); override;
   public
      constructor Create(parent: TMenuSpriteEngine; coords: TRect; main: TMenuEngine; owner: TMenuPage); override;
      procedure moveTo(coords: TRect); override;
      procedure setPortrait(const filename: string; const index: byte);
      function DoneWriting: boolean;

      property portrait: TSprite read FPortrait;
      property rightside: boolean read FRightPortrait write setRightside;
   end;

   TInputBox = class(TCustomMessageBox)
   private
      FAcceptCancel: boolean;
      FOnValidate: TValidateEvent;
      FTextDrawn: boolean;
      function Validate(const text: string): boolean;
   protected
      procedure PrepareText; virtual; abstract;
      procedure DrawText; virtual; abstract;
      procedure DoDraw; override;
      procedure BasicDrawText;
   public
      property OnValidate: TValidateEvent read FOnValidate write FOnValidate;
      property canCancel: boolean  read FAcceptCancel write FAcceptCancel;
   end;

   TChoiceBox = class(TInputBox)
   private
      FChoices: TArray<string>;
   protected
      procedure PrepareText; override;
      procedure DrawText; override;
      procedure parseText(const input: string); override;
   public
      procedure button(const input: TButtonCode); override;
      procedure SetChoices(const choices: TArray<string>);
   end;

   TValueInputBox = class(TInputBox)
   private
      FInputResult: array of byte;
      procedure changeInputResult(digit, value: byte);
      function getInputResult(digit: byte): byte;
      function computeInputResult: integer;
   protected
      procedure PrepareText; override;
      procedure DrawText; override;
   public
      procedure button(const input: TButtonCode); override;
      procedure setupInput(const digits: byte);
      procedure placeCursor(position: smallint); override;
      property inputResult[x: byte]: byte read getInputResult write changeInputResult;
   end;

   TPromptBox = class(TInputBox)
   public
      procedure button(const input: TButtonCode); override;
   end;

implementation
uses
   SysUtils, Math,
   sdl_13, sg_defs, SDL_ImageManager,
   commons, timing, turbu_text_utils, turbu_2k_environment;

{ TMessageBox }

constructor TMessageBox.Create(parent: TMenuSpriteEngine; coords: TRect; main: TMenuEngine; owner: TMenuPage);
begin
   //these lines go before the inherited constructor because they're
   //needed (the first, at least) by the virtual MoveTo function that gets
   //called from the inherited constructor
   FNextArrow := TSystemTile.Create(parent, parent.SystemGraphic.rects[srArrowD], ORIGIN, 1);
   FNextArrow.ImageName := parent.SystemGraphic.filename;
   FPortrait := TSprite.Create(parent);
   FPortrait.SetSpecialRender;
   FPortrait.Visible := false;
   inherited Create(parent, coords, main, owner);
   if parent.SystemGraphic.translucent then
   begin
      SDL_SetTextureBlendMode(FBackground.Image.surface, [sdlbBlend]);
      FBackground.Alpha := 200;
   end;

   SetTextRate(1);
   SetPosition(mb_top);

end;

procedure TMessageBox.DrawText;
begin
   if FTextCounter = 0 then
      FPortrait.Draw;
   DrawNextChar;
   if FTextCounter >= FParsedText.Count then
      DrawingDone
   else DrawingInProgress;
end;

procedure TMessageBox.PostDrawText;
begin
   if FTextCounter >= FParsedText.Count then
      FNextArrow.Draw;
end;

procedure TMessageBox.NewLine;
const
   TOP_MARGIN = 3;
   LINE_HEIGHT = 16;
begin
   if FPortrait.Visible and not FRightPortrait then
      FTextPosX := 65
   else FTextPosX := 3;
   inc(FTextLine);
   FTextPosY := (LINE_HEIGHT * FTextLine) + TOP_MARGIN;
end;

procedure TMessageBox.moveTo(coords: TRect);
begin
   inherited moveTo(coords);

   SetX(FNextArrow, 152 + trunc(FCorners[topLeft].X));
   SetY(FNextArrow, trunc(FCorners[bottomLeft].Y));
   FPortrait.Y := 8;
   setRightside(FRightPortrait);
end;

procedure TMessageBox.SetTextRate(value: integer);
begin
   FTextRate := value * 0.01;
end;

procedure TMessageBox.InsertText(const text: string);
var
   i: integer;
begin
   for i := 1 to length(text) do
      FParsedText.Insert(FTextCounter + i, text[i]);
end;

procedure TMessageBox.DrawSpecialChar(const line: string);
const HALF_CHAR = 3;
begin
   if line[1] = '$' then
      inherited DrawSpecialChar(line)
   else begin
      assert(line[1] = '\');
      try
         case line[2] of
            '$': InsertText(IntToStr(GEnvironment.money));
            '!':; //TODO: implement this
            '.': FRemainder := FRemainder - 0.25;  //quarter-second delay
            '|': FRemainder := FRemainder - 1;     //full-second delay
            '>': FImmediate := true;
            '<': FImmediate := false;
            '^': endMessage;
            '_': FTextPosX := FTextPosX + HALF_CHAR;
            'E': Abort; //TODO: implement error reporting
            'e': Abort; //TODO: implement error reporting
            'C': FTextColor := clamp(GetIntegerValue(FParsedText[FTextCounter]), 1, 20);
            'S': SetTextRate(clamp(GetIntegerValue(FParsedText[FTextCounter]), 1, 20));
            'N': InsertText(GetHeroName(GetIntegerValue(FParsedText[FTextCounter])));
            'V': InsertText(IntToStr(GEnvironment.Ints[GetIntegerValue(FParsedText[FTextCounter])]));
            'T': Abort; //TODO: implement string array in Environment
            'F': Abort; //TODO: implement float array in Environment
            'O': Abort; //TODO: implement vocab display
         end;
      except
         on EAbort do ;
      end;
   end;
end;

procedure TMessageBox.DrawNextChar;
begin
   if FTextCounter >= FParsedText.Count then
      Exit;

   //to prevent deadlocking on the synchronized part of ResetText
   if TMonitor.TryEnter(self) then
      try
         FRemainder := FRemainder + (TRpgTimestamp.FrameLength / 1000);
         while (FTextCounter < FParsedText.Count) and (FImmediate or (FRemainder > FTextRate)) do
         begin
            DrawChar(FParsedText[FTextCounter]);
            inc(FTextCounter);
            if not FImmediate then
               FRemainder := FRemainder - FTextRate;
         end;
      finally
         TMonitor.Exit(self);
      end;
end;

function TMessageBox.DoneWriting: boolean;
begin
   result := FTextCounter >= FParsedText.Count;
end;

procedure TMessageBox.DoSetPosition(const Value: TMboxLocation);
begin
   SetY(FNextArrow, ((ord(value) + 1) * 80) - FNextArrow.Height);
end;

procedure TMessageBox.doButton(const input: TButtonCode);
begin
   if assigned(FButtonLock) then
   begin
      if (FButtonLock.timeRemaining = 0) then
         freeAndNil(FButtonLock)
      else Exit;
   end;

   if (input in [btn_enter, btn_cancel]) and (self.DoneWriting) then
   begin
      endMessage;
      FButtonLock := TRpgTimestamp.Create(180);
   end;
end;

procedure TMessageBox.setPortrait(const filename: string; const index: byte);
var
   image: TSdlImage;
begin
   image := Engine.Images.EnsureImage(format('portrait\%s', [filename]), filename);
   FPortrait.Visible := true;
   FPortrait.ImageName := image.name;
   FPortrait.ImageIndex := index;
end;

procedure TMessageBox.setRightside(const value: boolean);
begin
   FRightPortrait := value;
   case value of
      false: FPortrait.X := 8;
      true: FPortrait.X := FCorners[topRight].x - 56;
   end;
end;

procedure TMessageBox.ResetText;
begin
   inherited ResetText;
   FParsedText.Clear;
   FImmediate := false;
   FRemainder := 0;
   SetTextRate(1);
end;

{ TInputBox }

procedure TInputBox.BasicDrawText;
const
   TEXTV = 0.1;
   TEXTH = 0.025;
var
   dest: TRect;
   xVal, yVal: single;
begin
   dest := GetDrawCoords;
   xVal := dest.right * TEXTH;
   yVal := dest.Bottom * TEXTV;
   inc(dest.Left, round(xVal));
   dec(dest.right, round(xVal * 2));
   inc(dest.Top, round(yVal));
   dec(dest.Bottom, round(yVal * 2));
   SDL_RenderCopy(FTextTarget.parent.Renderer, FTextTarget.handle, nil, @dest);
end;

procedure TInputBox.DoDraw;
begin
   inherited DoDraw;
   PrepareText;
   TMenuSpriteEngine(Engine).cursor.Draw;
   DrawText;
end;

function TInputBox.Validate(const text: string): boolean;
begin
   if assigned(FOnValidate) then
      result := FOnValidate(text)
   else result := true;
end;

{ TPromptBox }

procedure TPromptBox.button(const input: TButtonCode);
begin
   case input of
      btn_enter:
      begin
         if Validate(FParsedText[FCursorPosition]) then
         begin
            TMenuSpriteEngine(Engine).MenuInt := FCursorPosition;
            endMessage;
            playSound(sfxAccept);
         end
         else playSound(sfxBuzzer);;
      end;
      btn_cancel:
      begin
         if FAcceptCancel then
         begin
            TMenuSpriteEngine(Engine).MenuInt := 3;
            endMessage;
            playSound(sfxCancel);
         end;
      end;
      btn_down, btn_up:
      begin
         if FCursorPosition = 2 then
         begin
            placeCursor(3);
         end
         else placeCursor(2);
         playSound(sfxCursor);
      end
      else ;
   end;
end;

{ TChoiceBox }

procedure TChoiceBox.button(const input: TButtonCode);
begin
   case input of
      btn_cancel:
      begin
         if FAcceptCancel then
         begin
            TMenuSpriteEngine(Engine).MenuInt := -1;
            endMessage;
            PlaySound(sfxCancel);
         end;
      end;
      else begin
         inherited button(input);
         if (input = btn_enter) and (FOptionEnabled[FCursorPosition]) then
            endMessage;
      end;
   end;
end;

procedure TChoiceBox.PrepareText;
var
   value: string;
   i: integer;
begin
   if not FTextDrawn then
   begin
      FTextTarget.parent.pushRenderTarget;
      FTextTarget.SetRenderer;
      try
         ResetText;
         for value in FParsedText do
            DrawChar(value);
         if FParsedText.Count > 0 then
            NewLine;
         FPromptLines:= FTextLine;
         for i := 0 to high(FChoices) do
         begin
            value := FChoices[i];
            FOptionEnabled[i] := validate(value);
            FParsedText.Clear;
            DoParseText(value, FParsedText);
            for value in FParsedText do
               DrawChar(value);
            NewLine;
         end;
      finally
         FTextTarget.parent.popRenderTarget;
      end;
      placeCursor(0);
      FTextDrawn := true;
   end;
end;

procedure TChoiceBox.DrawText;
begin
   BasicDrawText;
end;

procedure TChoiceBox.parseText(const input: string);
begin
   inherited ParseText(input);
   FTextDrawn := false;
end;

procedure TChoiceBox.SetChoices(const choices: TArray<string>);
begin
   FChoices := choices;
   SetLength(FOptionEnabled, length(choices));
end;

{ TValueInputBox }

procedure TValueInputBox.button(const input: TButtonCode);
begin
   case input of
      btn_enter:
      begin
         TMenuSpriteEngine(Engine).MenuInt := computeInputResult;
         endMessage;
         playSound(sfxAccept);
      end;
      btn_down:
      begin
         if inputResult[FCursorPosition] = 0 then
            inputResult[FCursorPosition] := 9
         else inputResult[FCursorPosition] := inputResult[FCursorPosition] - 1;
         playSound(sfxCursor);
      end;
      btn_up:
      begin
         if inputResult[FCursorPosition] = 9 then
            inputResult[FCursorPosition] := 0
         else inputResult[FCursorPosition] := inputResult[FCursorPosition] + 1;
         playSound(sfxCursor);
      end;
      btn_left:
      begin
         if FCursorPosition > 0 then
            placeCursor(FCursorPosition - 1);
         playSound(sfxCursor);
      end;
      btn_right:
      begin
         if FCursorPosition < high(FInputResult) then
            placeCursor(FCursorPosition + 1);
         playSound(sfxCursor);
      end
      else ;
   end;
end;

function TValueInputBox.computeInputResult: integer;
var
   i: integer;
begin
   result := 0;
   for i := high(FInputResult) downto 0 do
      result := (result * 10) + FInputResult[i];
end;

procedure TValueInputBox.DrawText;
begin
   BasicDrawText;
end;

function TValueInputBox.getInputResult(digit: byte): byte;
begin
   assert(digit < length(FInputResult));
   result := FInputResult[digit];
end;

procedure TValueInputBox.placeCursor(position: smallint);
var
   coords: TRect;
   width: word;
   max: smallint;
begin
   max := length(FInputResult);
   width := SEPARATOR;
   assert(length(FInputResult) > 0);
   position := min(position, high(FInputResult));
   coords := rect(8 + (position * (width + SEPARATOR)),
                  FPromptLines * 15 + FBounds.Top + (ord(FPosition) * 80) + 11,
                  width * 2, 18);
   inc(coords.Bottom, coords.Top);
   inc(coords.Right, coords.Left);
   if FCursorPosition > max then
      inc(coords.top, (FCursorPosition div FColumns) * 15);
   FCursorPosition := position;

   with TMenuSpriteEngine(FEngine).cursor do
   begin
      Visible := true;
      layout(coords);
   end;
end;

procedure TValueInputBox.PrepareText;
var
   value: string;
   digit: Byte;
begin
   if not FTextDrawn then
   begin
      FTextTarget.parent.pushRenderTarget;
      FTextTarget.SetRenderer;
      try
         ResetText;
         for value in FParsedText do
            DrawChar(value);
         if FParsedText.Count > 0 then
            NewLine;
         FPromptLines := FTextLine;
         for digit in FInputResult do
            DrawLine(IntToStr(digit) + ' ');
         placeCursor(FCursorPosition);
      finally
         FTextTarget.parent.popRenderTarget;
      end;
      FTextDrawn := true;
   end;
end;

procedure TValueInputBox.changeInputResult(digit, value: byte);
var
   dummy: string;
   i: integer;
begin
   assert(digit <= high(FInputResult));
   assert(value < 10);
   if FInputResult[digit] = value then
      Exit;

   FInputResult[digit] := value;
   dummy := intToStr(FInputResult[0]);
   for I := 1 to high(FInputResult) do
      dummy := dummy + '  ' + intToStr(FInputResult[i]);
   FTextDrawn := false;
end;

procedure TValueInputBox.setupInput(const digits: byte);
begin
   FInputResult := nil;
   setLength(FInputResult, digits);
end;

initialization
   TMenuEngine.RegisterMenuBoxClass(TMessageBox);
end.
