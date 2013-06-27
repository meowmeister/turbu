unit disharmony;

interface
type
{
  HarmonyRelease,

  HarmonyCacheSound, HarmonyCheckValidWave, HarmonyCheckValidMidi,
  HarmonyGetVersion;
}
   IDisharmony = interface
      procedure PlayMusic(FileName: string);
      procedure PlaySoundEx(FileName: string; Volume, Speed, Panpot: Cardinal);
      procedure FadeInMusic(TimeFactor: Cardinal);
      procedure FadeOutMusic(TimeFactor: Cardinal);
      function GetMusicPlaying: Boolean;
      function GetMusicLooping: cardinal;
      function GetMusicPosition: cardinal;
      procedure StopSound;
      procedure SetPanPos(Panpot: Cardinal);
      procedure SetMusicSpeed(Speed: Cardinal);
      procedure SetMusicVolume(Volume: Cardinal);
      procedure StopMusic;
      procedure InitMidi;
      procedure InitWave;
      procedure TermMidi;
      procedure TermWave;
      procedure ReserveSound(FileName: string);
      procedure CancelSound(FileName: string);
   end;

function LoadDisharmony: IDisharmony;

implementation
uses
   Windows, SysUtils;

type
   TDisharmony = class(TInterfacedObject, IDisharmony)
   private
      FHandle: THandle;
      FHarmonyRelease: procedure; stdcall;

      FPlayMusic: procedure (FileName: PAnsiChar); stdcall;
      FPlaySoundEx: procedure (FileName: PAnsiChar; Volume, Speed, Panpot: Cardinal); stdcall;
      FFadeInMusic: procedure (TimeFactor: Cardinal); stdcall;
      FFadeOutMusic: procedure (TimeFactor: Cardinal); stdcall;
      FGetMusicPlaying: function: Boolean; stdcall;
      FGetMusicLooping: function: cardinal; stdcall;
      FGetMusicPosition: function: cardinal; stdcall;
      FStopSound: procedure; stdcall;
      FSetPanPos: procedure (Panpot: Cardinal); stdcall;
      FSetMusicSpeed: procedure (Speed: Cardinal); stdcall;
      FSetMusicVolume: procedure (Volume: Cardinal); stdcall;
      FStopMusic: procedure; stdcall;
      FInitMidi: procedure; stdcall;
      FInitWave: procedure; stdcall;
      FTermMidi: procedure; stdcall;
      FTermWave: procedure; stdcall;
      FReserveSound: procedure (FileName: PAnsiChar); stdcall;
      FCancelSound: procedure (FileName: PAnsiChar); stdcall;

      procedure PlayMusic(FileName: string);
      procedure PlaySoundEx(FileName: string; Volume, Speed, Panpot: Cardinal);
      procedure FadeInMusic(TimeFactor: Cardinal);
      procedure FadeOutMusic(TimeFactor: Cardinal);
      function GetMusicPlaying: Boolean;
      function GetMusicLooping: cardinal;
      function GetMusicPosition: cardinal;
      procedure StopSound;
      procedure SetPanPos(Panpot: Cardinal);
      procedure SetMusicSpeed(Speed: Cardinal);
      procedure SetMusicVolume(Volume: Cardinal);
      procedure StopMusic;
      procedure InitMidi;
      procedure InitWave;
      procedure TermMidi;
      procedure TermWave;
      procedure ReserveSound(FileName: string);
      procedure CancelSound(FileName: string);
   public
      constructor Create;
      destructor Destroy; override;
   end;

var
   LDisharmony: TDisharmony;

function LoadDisharmony: IDisharmony;
begin
   if LDisharmony = nil then
      LDisharmony := TDisharmony.Create;
   result := LDisharmony;
end;

{ TDisharmony }

constructor TDisharmony.Create;

  function LoadProc(const name: string): pointer;
  begin
    result := GetProcAddress(FHandle, PChar(name));
    if result = nil then
      raise ENotSupportedException.CreateFmt('Procedure %s not found in disharmony.dll', [name]);
  end;

var
   HarmonyCreate: procedure; stdcall;
begin
   FHandle := LoadLibrary('disharmony.dll');
   if FHandle = 0 then
      raise EFileNotFoundException.Create('Unable to load Disharmony.dll');
   HarmonyCreate := LoadProc('HarmonyCreate');
   FPlayMusic := LoadProc('HarmonyPlayMusic');
   FPlaySoundEx := LoadProc('HarmonyPlaySoundEx');
   FFadeInMusic := LoadProc('HarmonyFadeInMusic');
   FFadeOutMusic := LoadProc('HarmonyFadeOutMusic');
   FGetMusicPlaying := LoadProc('HarmonyGetMusicPlaying');
   FGetMusicLooping := LoadProc('HarmonyGetMusicLooping');
   FGetMusicPosition := LoadProc('HarmonyGetMidiTick');
   FStopSound := LoadProc('HarmonyStopSound');
   FSetPanPos := LoadProc('HarmonySetMusicPanpot');
   FSetMusicSpeed := LoadProc('HarmonySetMusicSpeed');
   FSetMusicVolume := LoadProc('HarmonySetMusicVolume');
   FStopMusic := LoadProc('HarmonyStopMusic');
   FInitMidi := LoadProc('HarmonyInitMidi');
   FInitWave := LoadProc('HarmonyInitWave');
   FTermMidi := LoadProc('HarmonyTermMidi');
   FTermWave := LoadProc('HarmonyTermWave');
   FReserveSound := LoadProc('HarmonyReserveSound');
   FCancelSound := LoadProc('HarmonyCancelSound');
   FHarmonyRelease := LoadProc('HarmonyRelease');
   HarmonyCreate();
   FInitMidi();
   FinitWave();
   LDisharmony := self;
end;

destructor TDisharmony.Destroy;
begin
   FHarmonyRelease();
   FreeLibrary(FHandle);
   LDisharmony := nil;
   inherited Destroy;
end;

procedure TDisharmony.CancelSound(FileName: string);
begin
   FCancelSound(PAnsiChar(utf8String(filename)));
end;

procedure TDisharmony.FadeInMusic(TimeFactor: Cardinal);
begin
   FFadeInMusic(timeFactor);
end;

procedure TDisharmony.FadeOutMusic(TimeFactor: Cardinal);
begin
   FFadeOutMusic(TimeFactor);
end;

function TDisharmony.GetMusicLooping: cardinal;
begin
   result := FGetMusicLooping();
end;

function TDisharmony.GetMusicPlaying: Boolean;
begin
   result := FGetMusicPlaying();
end;

function TDisharmony.GetMusicPosition: cardinal;
begin
   result := FGetMusicPosition();
end;

procedure TDisharmony.InitMidi;
begin
   FInitMidi();
end;

procedure TDisharmony.InitWave;
begin
   FInitWave();
end;

procedure TDisharmony.PlayMusic(FileName: string);
begin
   FPlayMusic(PAnsiChar(utf8String(filename)));
end;

procedure TDisharmony.PlaySoundEx(FileName: string; Volume, Speed, Panpot: Cardinal);
begin
   FPlaySoundEx(PAnsiChar(utf8String(filename)), volume, speed, panpot);
end;

procedure TDisharmony.ReserveSound(FileName: string);
begin
   FReserveSound(PAnsiChar(utf8String(filename)));
end;

procedure TDisharmony.SetMusicSpeed(Speed: Cardinal);
begin
   FSetMusicSpeed(speed);
end;

procedure TDisharmony.SetMusicVolume(Volume: Cardinal);
begin
   FSetMusicVolume(volume);
end;

procedure TDisharmony.SetPanPos(Panpot: Cardinal);
begin
   FSetPanPos(panpot);
end;

procedure TDisharmony.StopMusic;
begin
   FStopMusic();
end;

procedure TDisharmony.StopSound;
begin
   FStopSound();
end;

procedure TDisharmony.TermMidi;
begin
   FTermMidi();
end;

procedure TDisharmony.TermWave;
begin
   FTermWave();
end;

end.
