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
      FTextTarget: TsdlRenderTarget;
      FTextRate: single;
      FRemainder: single;
      FTextCounter: integer;
      FTextPosX: single;
      FTextPosY: single;
      FTextColor: integer;
      FTextLine: integer;
      FSpecialText: string;
      FSpecialIndex: integer;
      FImmediate: boolean;

      procedure DrawChar(value: char); overload;
      procedure DrawChar(index: integer); overload;
      procedure DrawNextChar;
      procedure parseText(const input: string); override;
      procedure ParseToken(const input: string; var counter: integer);
      procedure ParseParamToken(const input: string; var counter: integer);
      procedure ParseInt(const input: string; var counter: integer);
      procedure NewLine;
      procedure DrawSpecialChar(const line: string);
      procedure SetTextRate(value: integer);
      function GetHeroName(value: integer): string;
      function GetIntegerValue: integer;
      function GetDrawCoords: TRect;
      procedure ResetText;
   public
      constructor Create(parent: TMenuSpriteEngine; const coords: TRect); override;
      destructor Destroy; override;
      procedure Draw; override;
      procedure button(const input: TButtonCode); override;
      procedure moveTo(coords: TRect); override;
{      procedure realign; inline;
      procedure placeCursor(position: smallint); override;
      procedure tick;}
      procedure setPortrait(const filename: string; const index: byte);
//      procedure setupInput(const digits: byte);

      property portrait: TSprite read FPortrait;
      property rightside: boolean write setRightside;
{      property canCancel: boolean  read FAcceptCancel write FAcceptCancel;}
   end;

   TInputBox = class(TCustomMessageBox)
   private
      FAcceptCancel: boolean;
      FOnValidate: TValidateEvent;
      FCursorPosition: smallint;
      function Validate(const text: string): boolean;
   public
      procedure Draw; override;
      procedure button(const input: TButtonCode); override;
      procedure placeCursor(position: smallint); virtual;
      property OnValidate: TValidateEvent read FOnValidate write FOnValidate;
   end;

   TChoiceBox = class(TInputBox)
   public
      procedure button(const input: TButtonCode); override;
   end;

   TValueInputBox = class(TInputBox)
   private
      FInputResult: array of byte;
      procedure changeInputResult(digit, value: byte);
      function getInputResult(digit: byte): byte;
      function computeInputResult: integer;
   public
      procedure button(const input: TButtonCode); override;
      property inputResult[x: byte]: byte read getInputResult write changeInputResult;
   end;

   TPromptBox = class(TInputBox)
   public
      procedure button(const input: TButtonCode); override;
   end;

implementation
uses
   SysUtils, Character,
   sdl_13, sg_defs, SDL_ImageManager,
   commons, timing, turbu_text_utils, turbu_2k_environment;

{ TMessageBox }

constructor TMessageBox.Create(parent: TMenuSpriteEngine; const coords: TRect);
const BORDER_THICKNESS = 16;
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

   FTextColor := 1;
   SetTextRate(1);
   SetPosition(mb_top);

   FTextTarget := TSdlRenderTarget.Create(sgPoint(self.Width - BORDER_THICKNESS, self.Height - BORDER_THICKNESS));
   ClearTarget(FTextTarget);
end;

destructor TMessageBox.Destroy;
begin
   FTextTarget.Free;
   inherited Destroy;
end;

function TMessageBox.GetDrawCoords: TRect;
begin
   result.Left := 0;
   result.Right := FTextTarget.parent.Width;
   result.Bottom := FTextTarget.parent.Height div 3;
   result.Top := result.Bottom * ord(FPosition);
end;

procedure TMessageBox.Draw;
const
   TEXTV = 0.1;
   TEXTH = 0.025;
var
   xVal, yVal: single;
   dest: TRect;
begin
   inherited Draw;

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

procedure TMessageBox.DrawChar(value: char);
var
   newPos: TSgFloatPoint;
begin
   newPos := GFontEngine.drawChar(value, FTextPosX, FTextPosY, FTextColor);
   FTextPosX := newPos.x;
   FTextPosY := newPos.y;
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

function TMessageBox.GetIntegerValue: integer;
begin
   inc(FTextCounter);
   if FParsedText[FTextCounter] = '\V' then
      result := GEnvironment.Ints[GetIntegerValue]
   else if not TryStrToInt(FParsedText[FTextCounter], result) then
      Abort;
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

procedure TMessageBox.DrawChar(index: integer);
var
   value: string;
begin
   value := FParsedText[index];
   if length(value) = 1 then
      drawChar(value[1])
   else if value = #13#10 then
      NewLine
   else DrawSpecialChar(value);
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
               DrawChar(FTextCounter);
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

procedure TMessageBox.ParseInt(const input: string; var counter: integer);
var
   start: integer;
begin
   inc(counter);
   start := counter;
   if UpperCase(copy(input, start, 3)) = '\V[' then
   begin
      FParsedText.Add('\V');
      inc(counter);
      ParseInt(input, counter);
   end
   else begin
      while (counter <= length(input)) and (TCharacter.IsDigit(input[counter])) do
         inc(counter);
      if (counter > length(input)) or (input[counter] <> ']') then
      begin
         FParsedText.Add('\e');
         counter := start;
      end
      else begin
         FParsedText.Add(copy(input, start, counter - start));
//         inc(counter);
      end;
   end;
end;

procedure TMessageBox.ParseParamToken(const input: string; var counter: integer);
var
   token: char;
begin
   token := UpCase(input[counter]);
   FParsedText.Add('\' + token);
   case token of
      'C','S','N','V','T','F':
      begin
         inc(counter);
         if input[counter] = '[' then
            ParseInt(input, counter)
         else FParsedText.Add('\E' + input[counter]);
      end;
      'O': FParsedText.Add('\E' + input[counter]); //TODO: support \O for vocab
      else assert(false);
   end;
end;

procedure TMessageBox.ParseToken(const input: string; var counter: integer);
var
   token: char;
begin
   assert(input[counter] = '\');
   inc(counter);
   token := UpCase(input[counter]);
   case token of
      '\': FParsedText.Add('\');
      '$','!','.','|','>','<','^','_': FParsedText.Add('\' + token);
      'C','S','N','V','T','F','O': ParseParamToken(input, counter);
      else FParsedText.Add('\E' + input[counter]);
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
   FSignal.ResetEvent;
   FParsedText.Clear;
   runThreadsafe(procedure begin ClearTarget(FTextTarget) end, true);
   FTextCounter := 0;
   FSpecialText := '';
   FSpecialIndex := 0;
   FImmediate := false;
   FRemainder := 0;
   FTextLine := -1;
   SetTextRate(1);
   NewLine;
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
         else ParseToken(input, counter);
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
   if FParsedText.Count = 0 then
      Exit;
   if assigned(FButtonLock) then
   begin
      if FButtonLock.timeRemaining = 0 then
         freeAndNil(FButtonLock);
      Exit;
   end;

   lPosition := FCursorPosition; //to suppress a compiler warning
   max := (FParsedText.Count - 1) - (FPromptLines + FLastLineColumns);
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
            lPosition := FParsedText.Count - 1;
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

procedure TInputBox.Draw;
begin
   inherited Draw;
   TMenuSpriteEngine(Engine).cursor.Draw;
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
   max := (FParsedText.Count - 1) - (FPromptLines + FLastLineColumns);
   if (position > max) and (FLastLineColumns > 0) then
   begin
      columns := FLastLineColumns;
      width := lastColumnWidth;
   end else
   begin
      columns := FColumns;
      width := columnWidth;
   end;
   if FParsedText.Count = 0 then
      position := 0
   else if position >= FParsedText.Count then
      position := FParsedText.Count - 1;
   if position > max then
      dec(position, max + 1);
   column := position mod columns;
   inc(position, FPromptLines * columns);
   coords := rect(8 + (column * (width + SEPARATOR)),
                  (position div columns) * 15 + origin.y + 8,
                  width, 18);
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
         if input = btn_enter then
            endMessage;
      end;
   end;
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

end.
