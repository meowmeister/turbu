unit turbu_2k_map_timer;

interface
uses
   rsImport,
   sdl_sprite;

type
   TRpgTimer = class(TObject)
   private
      FTimerSprite: TSprite;
      FSecs: word;
      FTimeRemaining: integer;
      FActivated: boolean;
      FVisible: boolean;
      FInBattle: boolean;

      procedure setVisible(value: boolean);
   public
      [NoImport]
      constructor create(sprite: TSprite);

      [NoImport]
      procedure start; overload;
      procedure start(const visible, inBattle: boolean); overload;
      procedure pause;
      procedure reset;
      [NoImport]
      procedure tick;

      property time: integer read FTimeRemaining write FTimeRemaining;
      property visible: boolean read FVisible write setVisible;
      property inBattle: boolean read FInBattle write FInBattle;
      property active: boolean read FActivated;
   end;


implementation
uses
   SysUtils;

{ TRpgTimer }

constructor TRpgTimer.create(sprite: TSprite);
var
   dummy, msec: word;
begin
//   assert(sprite is TSystemTimer);
   FTimerSprite := sprite;
   decodeTime(GetTime, dummy, dummy, FSecs, msec);
   if MSec >= 500 then
      inc(FSecs);
end;

procedure TRpgTimer.pause;
begin
   FActivated := false;
end;

procedure TRpgTimer.reset;
begin
   FActivated := false;
   FVisible := false;
   FTimeRemaining := 0;
end;

procedure TRpgTimer.setVisible(value: boolean);
begin
   if value = FVisible then
      Exit;

   FVisible := value;
   FTimerSprite.visible := value;
end;

procedure TRpgTimer.start(const visible, inBattle: boolean);
begin
   self.visible := visible;
   FInBattle := inBattle;
   FActivated := true;
end;

procedure TRpgTimer.start;
begin
   FActivated := true;
end;

procedure TRpgTimer.tick;
var
   dummy, secs: word;
begin
   if not FActivated then
      Exit;

   decodeTime(GetTime, dummy, dummy, secs, dummy);
   if (secs = 0) and (FSecs = 59) then
      secs := 60;
   if (secs - FSecs > 0) and (FTimeRemaining > 0) then
   begin
      dec(FTimeRemaining);
      FSecs := secs mod 60;
   end;
end;

end.
