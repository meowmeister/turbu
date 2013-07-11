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
   types,
   sdl_sprite, sdl_canvas,
   turbu_defs, turbu_2k_frames;

type
   TMessageBox = class(TCustomMessageBox)
   private
      FPortrait: TSprite;
      FNextArrow: TTiledAreaSprite;
      FRightPortrait: boolean;

      procedure setRightside(const value: boolean);
   private //drawing section
      FTextRate: single;
      FRemainder: single;
      FSpecialText: string;
      FSpecialIndex: integer;
      FImmediate: boolean;

      procedure DrawNextChar;
      procedure SetTextRate(value: integer);
      function GetHeroName(value: integer): string;
   protected
      procedure NewLine; override;
      procedure DrawSpecialChar(const line: string); override;
      procedure parseText(const input: string); override;
      procedure DoDraw; override;
      procedure ResetText; override;
   public
      constructor Create(parent: TMenuSpriteEngine; const coords: TRect); override;
      procedure button(const input: TButtonCode); override;
      procedure moveTo(coords: TRect); override;
{      procedure realign; inline;
      procedure placeCursor(position: smallint); override;
      procedure tick;}
      procedure setPortrait(const filename: string; const index: byte);
//      procedure setupInput(const digits: byte);

      property portrait: TSprite read FPortrait;
      property rightside: boolean write setRightside;
   end;

   TInputBox = class(TCustomMessageBox)
   private
      FAcceptCancel: boolean;
      FOnValidate: TValidateEvent;
      FCursorPosition: smallint;
      FPromptLines: integer;
      FTextDrawn: boolean;
      function Validate(const text: string): boolean;
   protected
      procedure PrepareText; virtual; abstract;
      procedure DrawText; virtual; abstract;
      procedure DoDraw; override;
   public
      procedure button(const input: TButtonCode); override;
      procedure placeCursor(position: smallint); virtual;
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
   public
      procedure button(const input: TButtonCode); override;
      procedure setupInput(const digits: byte);
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

constructor TMessageBox.Create(parent: TMenuSpriteEngine; const coords: TRect);
begin
   //these lines go before the inherited constructor because they're
   //needed (the first, at least) by the virtual MoveTo function that gets
   //called from the inherited constructor
   FNextArrow := TSystemTile.Create(parent, parent.SystemGraphic.rects[srArrowD], ORIGIN, 1);
   FNextArrow.ImageName := parent.SystemGraphic.filename;
   FPortrait := TSprite.Create(parent);
   FPortrait.SetSpecialRender;
   FPortrait.Visible := false;
   inherited Create(parent, coords);
   if parent.SystemGraphic.translucent then
   begin
      SDL_SetTextureBlendMode(FBackground.Image.surface, [sdlbBlend]);
      FBackground.Alpha := 200;
   end;

   SetTextRate(1);
   SetPosition(mb_top);

end;

procedure TMessageBox.DoDraw;
const
   TEXTV = 0.1;
   TEXTH = 0.025;
var
   xVal, yVal: single;
   dest: TRect;
begin
   inherited DoDraw;

   FTextTarget.parent.pushRenderTarget;
   FTextTarget.SetRenderer;
   try
      FPortrait.Draw;
      DrawNextChar;
   finally
      FTextTarget.parent.popRenderTarget;
   end;
   dest := GetDrawCoords;
   setX(FNextArrow, dest.Right div 2);
   xVal := dest.right * TEXTH;
   yVal := dest.Bottom * TEXTV;
   inc(dest.Left, round(xVal));
   dec(dest.right, round(xVal * 2));
   inc(dest.Top, round(yVal));
   dec(dest.Bottom, round(yVal * 2));
   SDL_RenderCopy(FTextTarget.parent.Renderer, FTextTarget.handle, nil, @dest);
   setY(FNextArrow, dest.Bottom + dest.top);
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

function TMessageBox.GetHeroName(value: integer): string;
begin
   if value = 0 then
   begin
      if GEnvironment.partySize = 0 then
         result := ''
      else result := GEnvironment.party.hero[1].name;
   end
   else if clamp(value, 1, GEnvironment.HeroCount) <> value then
      Abort
   else result := GEnvironment.Heroes[value].name;
end;

procedure TMessageBox.DrawSpecialChar(const line: string);
const HALF_CHAR = 3;
begin
   assert(line[1] = '\');
   try
      case line[2] of
         '$': FSpecialText := IntToStr(GEnvironment.money);
         '!':; //TODO: implement this
         '.': FRemainder := FRemainder - 0.25;  //quarter-second delay
         '|': FRemainder := FRemainder - 1;     //full-second delay
         '>': FImmediate := true;
         '<': FImmediate := false;
         '^': endMessage;
         '_': FTextPosX := FTextPosX + HALF_CHAR;
         'E': Abort; //TODO: implement error reporting
         'e': Abort; //TODO: implement error reporting
         'C': FTextColor := clamp(GetIntegerValue, 1, 20);
         'S': SetTextRate(clamp(GetIntegerValue, 1, 20));
         'N': FSpecialText := GetHeroName(GetIntegerValue);
         'V': FSpecialText := IntToStr(GEnvironment.Ints[GetIntegerValue]);
         'T': Abort; //TODO: implement string array in Environment
         'F': Abort; //TODO: implement float array in Environment
         'O': Abort; //TODO: implement vocab display
      end;
   except
      on EAbort do ;
   end;
   if FSpecialText <> '' then
      FSpecialIndex := 1;
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
            if (FSpecialIndex > 0) and (FSpecialIndex <= length(FSpecialText)) then
            begin
               DrawChar(FSpecialText[FSpecialIndex]);
               inc(FSpecialIndex);
            end
            else begin
               if FSpecialIndex > 0 then
               begin
                  FSpecialIndex := 0;
                  FSpecialText := '';
               end;
               DrawChar(FParsedText[FTextCounter]);
               inc(FTextCounter);
            end;
            if not FImmediate then
               FRemainder := FRemainder - FTextRate;
         end;
      finally
         TMonitor.Exit(self);
      end;
end;

procedure TMessageBox.button(const input: TButtonCode);
begin
   if assigned(FButtonLock) then
      if (FButtonLock.timeRemaining = 0) then
         freeAndNil(FButtonLock)
      else Exit;

   if (input in [btn_enter, btn_cancel]) and (FTextCounter >= FParsedText.Count) then
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
   FSpecialText := '';
   FSpecialIndex := 0;
   FImmediate := false;
   FRemainder := 0;
   SetTextRate(1);
end;

procedure TMessageBox.parseText(const input: string);
var
   counter: integer;
begin
   TMonitor.Enter(self);
   try
      ResetText;
      counter := 1;
      while counter <= length(input) do
      begin
         if input[counter] = #13 then
         begin
            FParsedText.Add(#13#10);
            if (counter < length(input)) and (input[counter + 1] = #10) then
               inc(counter);
         end
         else if input[counter] <> '\' then
            FParsedText.Add(input[counter])
         else FParsedText.Add(ParseToken(input, counter));
         inc(counter);
      end;
   finally
      TMonitor.Exit(self);
   end;
end;

{ TInputBox }

procedure TInputBox.button(const input: TButtonCode);
var
   max, absMax, lPosition: smallint;
   ratio: byte;
begin
   if (FCursorPosition = -1) and (input in [btn_up, btn_down, btn_left, btn_right]) then
      Exit;
   if length(FOptionEnabled) = 0 then
      Exit;
   if assigned(FButtonLock) then
   begin
      if FButtonLock.timeRemaining = 0 then
         freeAndNil(FButtonLock)
      else Exit;
   end;

   lPosition := FCursorPosition;
   max := high(FOptionEnabled) - FLastLineColumns;
   absMax := max + FLastLineColumns;
   case input of
      btn_enter:
      begin
         if FOptionEnabled[FCursorPosition] then
         begin
            TMenuSpriteEngine(FEngine).MenuInt := FCursorPosition;
            PlaySound(sfxAccept);
         end
         else PlaySound(sfxBuzzer);
      end;
      btn_down:
      begin
         if FCursorPosition <= max - FColumns then
            lPosition := FCursorPosition + FColumns
         else if FColumns = 1 then
            lPosition := 0
         else if (FLastLineColumns > 0) and (FCursorPosition <= max) then
         begin
            lPosition := FCursorPosition mod FColumns;
            ratio := FColumns div FLastLineColumns;
            lPosition := (lPosition div ratio) + max + 1;
         end;
      end;
      btn_up:
      begin
         if FCursorPosition > max then
         begin
            ratio := FColumns div FLastLineColumns;
            lPosition := FCursorPosition - (max + 1);
            lPosition := (max + 1 - FColumns) + (lPosition * ratio) + (ratio div 2);
         end else if FCursorPosition >= FColumns then
            lPosition := FCursorPosition - FColumns
         else if FColumns = 1 then
            lPosition := high(FOptionEnabled);
      end;
      btn_right:
      begin
         if (FColumns > 1) and (FCursorPosition < absMax) then
            lPosition := FCursorPosition + 1;
      end;
      btn_left:
      begin
         if (FColumns > 1) and (FCursorPosition > 0) then
            lPosition := FCursorPosition - 1;
      end;
      else ;
   end;
   if (input in [btn_up, btn_down, btn_left, btn_right]) and (lPosition <> FCursorPosition) then
   begin
      FButtonLock := TRpgTimestamp.Create(180);
      placeCursor(lPosition);
      PlaySound(sfxCursor);
   end;
end;

procedure TInputBox.DoDraw;
begin
   inherited DoDraw;
   PrepareText;
   TMenuSpriteEngine(Engine).cursor.Draw;
   DrawText;
end;

procedure TInputBox.placeCursor(position: smallint);
var
   coords: TRect;
   column, columns: byte;
   width: word;
   max: smallint;
begin
   if self.FDontChangeCursor then
      position := (self.FCursorPosition);
   FCursorPosition := position;
   max := length(FOptionEnabled) - (FPromptLines + FLastLineColumns);
   if (position > max) and (FLastLineColumns > 0) then
   begin
      columns := FLastLineColumns;
      width := lastColumnWidth;
   end else
   begin
      columns := FColumns;
      width := columnWidth;
   end;
   if length(FOptionEnabled) = 0 then
      position := 0
   else position := min(position, high(FOptionEnabled));
   if position > max then
      dec(position, max + 1);
   column := position mod columns;
   inc(position, FPromptLines * columns);
   coords := rect(8 + (column * (width + SEPARATOR)),
                  (position div columns) * 15 + FBounds.Top + (ord(FPosition) * 80) + 8,
                  width, 18);
   inc(coords.Bottom, coords.Top);
   if FCursorPosition > max then
      inc(coords.top, (FCursorPosition div FColumns) * 15);

   with TMenuSpriteEngine(FEngine).cursor do
   begin
      Visible := true;
      layout(coords);
   end;
   FDontChangeCursor := false;
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
   value, line: string;
   i: integer;
begin
   if not FTextDrawn then
   begin
      FTextTarget.parent.pushRenderTarget;
      FTextTarget.SetRenderer;
      try
         ResetText;
         for value in FParsedText do
            DrawLine(value);
         if FParsedText.Count > 0 then
            NewLine;
         FPromptLines:= FTextLine;
         for i := 0 to high(FChoices) do
         begin
            value := FChoices[i];
            FOptionEnabled[i] := validate(value);
            parseText(value);
            for line in FParsedText do
               DrawLine(line);
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

function TValueInputBox.getInputResult(digit: byte): byte;
begin
   assert(digit < length(FInputResult));
   result := FInputResult[digit];
end;

procedure TValueInputBox.PrepareText;
var
   value, line: string;
   i: integer;
begin
   if not FTextDrawn then
   begin
      FTextTarget.parent.pushRenderTarget;
      FTextTarget.SetRenderer;
      try
         ResetText;
         for value in FParsedText do
            DrawLine(value);
         FPromptLines:= FTextLine;
         if FParsedText.Count > 0 then
            NewLine;
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
   text := dummy;
end;

procedure TValueInputBox.setupInput(const digits: byte);
begin
   FInputResult := nil;
   setLength(FInputResult, digits);
end;

end.
