unit turbu_2k_map_timer;

interface
uses
   turbu_2k_frames,
   rsImport,
   sdl_sprite,
   dwsJSON;

type
   TRpgTimer = class(TObject)
   private
      FTimerSprite: TSystemTimer;
      FSecs: word;
      FTimeRemaining: integer;
      FActivated: boolean;
      FVisible: boolean;
      FInBattle: boolean;

      procedure setVisible(value: boolean);
   public
      [NoImport]
      constructor create(sprite: TSystemTimer);
      [NoImport]
      procedure Serialize(writer: TdwsJSONWriter);
      [NoImport]
      procedure Deserialize(obj: TdwsJSONObject);

      [NoImport]
      procedure start; overload;
      procedure start(const visible, inBattle: boolean); overload;
      procedure pause;
      procedure reset;
      [NoImport]
      procedure tick;
      [NoImport]
      function GetTime: integer;

      property time: integer read FTimeRemaining write FTimeRemaining;
      property visible: boolean read FVisible write setVisible;
      property inBattle: boolean read FInBattle write FInBattle;
      property active: boolean read FActivated;
   end;


implementation
uses
   SysUtils,
   turbu_classes;

{ TRpgTimer }

constructor TRpgTimer.create(sprite: TSystemTimer);
var
   dummy, msec: word;
begin
   FTimerSprite := sprite;
   decodeTime(SysUtils.GetTime, dummy, dummy, FSecs, msec);
   if MSec >= 500 then
      inc(FSecs);
   sprite.OnGetTime := self.GetTime;
end;

procedure TRpgTimer.Serialize(writer: TdwsJSONWriter);
begin
   writer.BeginObject;
      writer.CheckWrite('TimeRemaining', FTimeRemaining, 0);
      writer.CheckWrite('Activated', FActivated, false);
      writer.CheckWrite('Visible', FVisible, false);
      writer.CheckWrite('InBattle', FInBattle, false);
   writer.EndObject;
end;

procedure TRpgTimer.Deserialize(obj: TdwsJSONObject);
begin
   obj.CheckRead('TimeRemaining', FTimeRemaining);
   obj.CheckRead('Activated', FActivated);
   obj.CheckRead('Visible', FVisible);
   obj.CheckRead('InBattle', FInBattle);
   obj.CheckEmpty;
end;

function TRpgTimer.GetTime: integer;
begin
   result := FTimeRemaining;
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

   decodeTime(SysUtils.GetTime, dummy, dummy, secs, dummy);
   if (secs = 0) and (FSecs = 59) then
      secs := 60;
   if (secs - FSecs > 0) and (FTimeRemaining > 0) then
   begin
      dec(FTimeRemaining);
      FSecs := secs mod 60;
   end;
end;

end.
