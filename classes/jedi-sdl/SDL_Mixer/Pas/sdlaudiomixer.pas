unit sdlaudiomixer;
{******************************************************************************}
{
  $Id: sdlaudiomixer.pas,v 1.6 2007/12/20 22:38:53 savage Exp $
  
}
{                                                                              }
{       Borland Delphi SDL_Mixer - Simple DirectMedia Layer Mixer Library      }
{       Conversion of the Simple DirectMedia Layer Headers                     }
{                                                                              }
{ The initial developer of this Pascal code was :                              }
{ Ariel Jacob <ariel@global-rd.com>                                            }
{                                                                              }
{ Portions created by Ariel are                                                }
{ Copyright (C) 2005  Ariel Jacob.                                             }
{                                                                              }
{                                                                              }
{ Contributor(s)                                                               }
{ --------------                                                               }
{ Dominqiue Louis <Dominique@SavageSoftware.com.au>                            }
{                                                                              }
{ Obtained through:                                                            }
{ Joint Endeavour of Delphi Innovators ( Project JEDI )                        }
{                                                                              }
{ You may retrieve the latest version of this file at the Project              }
{ JEDI home page, located at http://delphi-jedi.org                            }
{                                                                              }
{ The contents of this file are used with permission, subject to               }
{ the Mozilla Public License Version 1.1 (the "License"); you may              }
{ not use this file except in compliance with the License. You may             }
{ obtain a copy of the License at                                              }
{ http://www.mozilla.org/MPL/MPL-1.1.html                                      }
{                                                                              }
{ Software distributed under the License is distributed on an                  }
{ "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or               }
{ implied. See the License for the specific language governing                 }
{ rights and limitations under the License.                                    }
{                                                                              }
{ Description                                                                  }
{ -----------                                                                  }
{   SDL_Mixer wrapper classed                                                  }
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{ Requires                                                                     }
{ --------                                                                     }
{   SDL_Mixer.pas somewhere within your search path.                           }
{                                                                              }
{ Programming Notes                                                            }
{ -----------------                                                            }
{                                                                              }
{                                                                              }
{ Revision History                                                             }
{ ----------------                                                             }
{
  $Log: sdlaudiomixer.pas,v $
  Revision 1.6  2007/12/20 22:38:53  savage
  Latest improvements to overall userbility.

  Revision 1.5  2007/12/05 22:53:57  savage
  Better Mac OS X support for Frameworks.

  Revision 1.4  2005/05/13 12:29:35  savage
  Added PlayByName capabilities and completed more of the other music functions.

  Revision 1.3  2005/05/11 12:21:34  savage
  Slight change for music playback.

  Revision 1.2  2005/05/09 22:55:53  savage
  Added MP3 playing capabilities.

  Revision 1.1  2005/02/24 20:42:24  savage
  SDL Audio Class - Thanks to Ariel for his help in the initial implementation.


}
{******************************************************************************}
{$DEFINE NOT_OO}
{$M+}
interface

uses
  Classes,
  Contnrs,
  SysUtils,
  sdl,
  {$IFNDEF DARWIN}
  {$IFNDEF no_smpeg}
  smpeg,
  {$ENDIF}
  {$ENDIF}
  sdl_mixer;

type
  ESDLAudioException = class( Exception );

  TSDLAudio = class
  private
    FPaused : boolean;
    FFading : TMix_Fading;
    FPlaying : boolean;

    FChannel : integer;
    FPanning : Uint8;
    FDistance : Uint8;
    FAngle : Sint16;
    function GetVolume : integer; virtual; abstract;
    procedure SetVolume( const Value : integer ); virtual; abstract;
    function GetFading : TMix_Fading; virtual; abstract;
    function GetPaused : boolean; virtual; abstract;
    function GetPlaying : boolean; virtual; abstract;
    procedure InitialiseFields;
  protected
    FRw: PSDL_RWops;
  public
    constructor Create( const aFileName : string ); overload;
    constructor Create( aStream : TStream ); overload;

    procedure Play( aLoops : integer = 0 ); virtual;
    procedure Stop; virtual;
    procedure Pause; virtual;
    procedure Resume; virtual;

    procedure SetPanning( val : Uint8 );
    procedure SetDistance( Distance : Uint8 );
    procedure SetPosition( Angle : Sint16; Distance : Uint8 );
    function SetReverseStereo( aFlip : integer ) : integer;

    procedure FadeIn( aTime : integer; aLoops : integer = 0 ); virtual;
    procedure FadeOut( aTime : integer ); virtual;

    procedure LoadFromFile( const aFileName : string ); virtual;
    procedure LoadFromStream( aStream : TStream ); virtual;

    procedure UnLoad; virtual;

    property IsPlaying : boolean read GetPlaying;
    property IsPaused : boolean read GetPaused;
    property Fading : TMix_Fading read GetFading;
    property Volume : integer read GetVolume write SetVolume;
    property Channel : integer read FChannel write FChannel;

    property Panning : Uint8 read FPanning write SetPanning;
    property Distance : Uint8 read FDistance write SetDistance;
    property Angle : SInt16 read FAngle;
  end;

  ESDLSoundEffect = class( ESDLAudioException );

  TSDLSoundEffect = class( TSDLAudio )
  private
    FMix_Chunk : PMix_Chunk;
    FGroup : integer;
    function GetFading : TMix_Fading; override;
    function GetPaused : boolean; override;
    function GetPlaying : boolean; override;
    function GetVolume : integer; override;
    procedure SetVolume( const Value : integer ); override;
  public
    destructor Destroy; override;

    procedure FadeIn( aTime : integer; aLoops : integer = 0 ); override;
    procedure FadeOut( aTime : integer ); override;

    procedure LoadFromFile( const aFileName : string ); override;
    procedure LoadFromStream( aStream : TStream ); override;

    procedure Play( aLoops : integer = 0 ); override;
    procedure Stop; override;
    procedure Pause; override;
    procedure Resume; override;

    procedure UnLoad; override;
  published
    property Group : integer read FGroup;
  end;

  TSDLSoundEffectManager = class( TObjectList )
  private
    FSoundEffectNames : TStringList;
    function GetSoundEffectByName(const aSoundEffectName: string): TSDLSoundEffect;
    procedure SetSoundEffectByName(const aSoundEffectName: string; const aValue: TSDLSoundEffect);
  protected
    function GetItem( aIndex : Integer ) : TSDLSoundEffect;
    procedure SetItem( aIndex : Integer; aObject : TSDLSoundEffect );
  public
    destructor Destroy; override;
    function Add( aObject : TSDLSoundEffect; const aSoundEffectName: string = '' ) : Integer;
    function Extract( aItem : TSDLSoundEffect ) : TSDLSoundEffect; overload;
    function Extract( const aSoundEffectName: string ) : TSDLSoundEffect; overload;
    function Remove( aObject : TSDLSoundEffect ) : Integer; overload;
    function Remove( const aSoundEffectName: string ) : Integer; overload;
    function IndexOf( aObject : TSDLSoundEffect ) : Integer; overload;
    function IndexOf( const aSoundEffectName: string ) : Integer; overload;
    procedure Insert( aIndex : Integer; aObject : TSDLSoundEffect );
    function First : TSDLSoundEffect;
    function Last : TSDLSoundEffect;
    property Items[ Index : Integer ] : TSDLSoundEffect read GetItem write SetItem; default;
    property SoundEffectNames[const SoundEffectName: string]: TSDLSoundEffect read GetSoundEffectByName write SetSoundEffectByName;
  end;

  TMusicFinishedEvent = procedure{$IFNDEF NOT_OO} of object{$ENDIF};

  ESDLMusic = class( ESDLAudioException );

  TSDLMusic = class( TSDLAudio )
  private
    FMix_Music : PMix_Music;
    {$IFNDEF DARWIN}
    {$IFNDEF no_smpeg}
    FPSMPEG : PSMPEG;
    {$ENDIF}
    {$ENDIF}
    FMusicFinishedEvent : TMusicFinishedEvent;
    FMusicPosition : double;
    FIsMP3 : boolean;
    function GetFading : TMix_Fading; override;
    function GetPaused : boolean; override;
    function GetPlaying : boolean; override;
    function GetVolume : integer; override;
    procedure SetVolume( const Value : integer ); override;
    function GetMusic : TMix_MusicType;
    procedure SetMusicPosition( const Value : double );
    procedure SetFinishedEvent( const Value : TMusicFinishedEvent );
  public
    destructor Destroy; override;

    procedure FadeIn( aTime : integer; aLoops : integer = 0 ); override;
    procedure FadeOut( aTime : integer ); override;

    procedure LoadFromFile( const aFileName : string ); override;
    procedure LoadFromStream( aStream : TStream ); override;

    procedure Play( aLoops : integer = 0 ); override;
    procedure Stop; override;
    procedure Pause; override;
    procedure Resume; override;
    procedure Rewind;

    procedure UnLoad; override;

    property OnMusicFinished : TMusicFinishedEvent read FMusicFinishedEvent write SetFinishedEvent;
    property MusicType : TMix_MusicType read GetMusic;
    property MusicPosition : double read FMusicPosition write SetMusicPosition;
  end;

  TSDLMusicManager = class( TObjectList )
  private
    FCurrentTrack: Integer;
    FTrackNames : TStringList;

    function GetTrackByName(const aTrackName: string): TSDLMusic;
    procedure SetTrackByName(const aTrackName: string; const aValue: TSDLMusic);
  protected
    function GetItem( aIndex : Integer ) : TSDLMusic;
    procedure SetItem( aIndex : Integer; aObject : TSDLMusic );
  public
    destructor Destroy; override;
    function Add( aObject : TSDLMusic; const aTrackName: string = '' ) : Integer;
    function Extract( aItem : TSDLMusic ) : TSDLMusic; overload;
    function Extract( const aTrackName: string ) : TSDLMusic; overload;
    function Remove( aObject : TSDLMusic ) : Integer; overload;
    function Remove( const aTrackName: string ) : Integer; overload;
    function IndexOf( aObject : TSDLMusic ) : Integer; overload;
    function IndexOf( const aTrackName: string ) : Integer; overload;
    procedure Insert( aIndex : Integer; aObject : TSDLMusic );
    function First : TSDLMusic;
    function Last : TSDLMusic;
    property Items[ Index : Integer ] : TSDLMusic read GetItem write SetItem; default;
    property TrackNames[const TrackName: string]: TSDLMusic read GetTrackByName write SetTrackByName;
  end;

  TSDLAudioManager = class( TObject )
  private
    FMusicManager : TSDLMusicManager;
    FSoundEffectManager : TSDLSoundEffectManager;
    procedure SetVolume( const Value: integer );
  public
    constructor Create( frequency : integer = MIX_DEFAULT_FREQUENCY; format : Uint16 = MIX_DEFAULT_FORMAT; channels : integer = MIX_DEFAULT_CHANNELS ; chunksize : integer = 1024 );
    destructor Destroy; override;

    procedure Stop;

    property MusicManager : TSDLMusicManager read FMusicManager write FMusicManager;
    property SoundEffectManager : TSDLSoundEffectManager read FSoundEffectManager write FSoundEffectManager;
    property Volume : integer write SetVolume;
  end;

implementation
uses
   sdlstreams, SDL_13;

{ TSDLAudio }

constructor TSDLAudio.Create( const aFileName : string );
begin
  inherited Create;

  InitialiseFields;

  LoadFromFile( aFileName );
end;

constructor TSDLAudio.Create( aStream : TStream );
begin
  inherited Create;

  InitialiseFields;

  LoadFromStream( aStream );
end;

procedure TSDLAudio.FadeIn( aTime, aLoops : integer );
begin
  FFading := MIX_FADING_IN;
end;

procedure TSDLAudio.FadeOut( aTime : integer );
begin
  FFading := MIX_FADING_OUT;
end;

procedure TSDLAudio.InitialiseFields;
begin
  FPaused := false;
  FFading := MIX_NO_FADING;
  FPlaying := false;
end;

procedure TSDLAudio.LoadFromFile(const aFileName: string);
begin
  UnLoad;
end;

procedure TSDLAudio.LoadFromStream(aStream: TStream);
begin
  UnLoad;
end;

procedure TSDLAudio.Pause;
begin
  FPlaying := false;
  FPaused := true;
end;

procedure TSDLAudio.Play( aLoops : integer );
begin
  FPlaying := true;
  FPaused := false;
end;

procedure TSDLAudio.Resume;
begin
  FPlaying := true;
  FPaused := false;
end;

procedure TSDLAudio.Stop;
begin
  FPlaying := false;
  FPaused := true;
end;

procedure TSDLAudio.UnLoad;
begin
  Stop;
end;

procedure TSdlAudio.SetPanning( val : Uint8 );
var
   l, r: Uint8;
begin
  FPanning := val;
  if (val = 127) or (val = 128) then
    Mix_SetPanning( FChannel, 255, 255 )
  else begin
    if val > 128 then
    begin
      r := 255;
      l := 255 - val
    end
    else begin
      l := 255;
      r := val;
    end;
    Mix_SetPanning( FChannel, l, r );
  end;
end;

procedure TSdlAudio.SetDistance( Distance : Uint8 );
begin
  FDistance := Distance;
  Mix_SetDistance( FChannel, FDistance );
end;

procedure TSdlAudio.SetPosition( Angle : Sint16; Distance : Uint8 );
begin
  FAngle := Angle;
  FDistance := Distance;
  Mix_SetPosition( FChannel, FAngle, FDistance );
end;

function TSDLAudio.SetReverseStereo( aFlip : integer ) : integer;
begin
  result := Mix_SetReverseStereo( FChannel, aFlip );
end;

{ TSDLSoundEffect }

destructor TSDLSoundEffect.Destroy;
begin
  UnLoad;
  inherited;
end;

function TSDLSoundEffect.GetFading : TMix_Fading;
begin
  result := Mix_FadingChannel( FChannel );
end;

procedure TSDLSoundEffect.LoadFromFile( const aFileName : string );
begin
  inherited;
  FMix_Chunk := Mix_LoadWAV( PAnsiChar( ansiString(aFileName) ) );
end;

procedure TSDLSoundEffect.LoadFromStream( aStream : TStream );
var
   rw: PSdl_Rwops;
begin
  inherited;
  rw := sdlstreams.SDLStreamSetup(aStream);
  try
    FMix_Chunk := Mix_LoadWav_RW(rw, false);
    if FMix_Chunk = nil then
      raise EBadHandle.Create(string(AnsiString(SDL_GetError)));
  finally
     sdlstreams.SDLStreamCloseRWops(rw);
  end;
end;

procedure TSDLSoundEffect.UnLoad;
begin
  inherited;
  Mix_FreeChunk( FMix_Chunk );
end;

procedure TSDLSoundEffect.Pause;
begin
  inherited;
  Mix_Pause( FChannel );
end;

procedure TSDLSoundEffect.Play( aLoops : integer );
begin
  inherited;
  Mix_PlayChannel( FChannel, FMix_Chunk, aLoops );
end;

procedure TSDLSoundEffect.Resume;
begin
  inherited;
  Mix_Resume( FChannel );
end;

procedure TSDLSoundEffect.Stop;
begin
  inherited;
  Mix_HaltChannel( FChannel );
end;

function TSDLSoundEffect.GetPaused : boolean;
begin
  result := ( Mix_Paused( FChannel ) = 1 );
end;

procedure TSDLSoundEffect.FadeIn( aTime, aLoops : integer );
begin
  inherited;
  Mix_FadeInChannel( FChannel, FMix_Chunk, aLoops, aTime );
end;

procedure TSDLSoundEffect.FadeOut( aTime : integer );
begin
  inherited;
  Mix_FadeOutChannel( FChannel, aTime );
end;

procedure TSDLSoundEffect.SetVolume( const Value : integer );
begin
  Mix_VolumeChunk( FMix_Chunk, Value );
end;

function TSDLSoundEffect.GetVolume : integer;
begin
  result := Mix_VolumeChunk( FMix_Chunk, -1 );
end;

function TSDLSoundEffect.GetPlaying: boolean;
begin
  result := ( Mix_Playing( FChannel ) = 1 );
end;

{ TSDLMusic }

destructor TSDLMusic.Destroy;
begin
  UnLoad;
  inherited;
end;

procedure TSDLMusic.FadeIn( aTime, aLoops : integer );
begin
  inherited;
  Mix_FadeInMusic( FMix_Music, aLoops, aTime , FChannel);
end;

procedure TSDLMusic.FadeOut( aTime : integer );
begin
  inherited;
  Mix_FadeOutMusic( aTime, FChannel);
end;

function TSDLMusic.GetFading : TMix_Fading;
begin
  result := Mix_FadingMusic(FChannel);
end;

function TSDLMusic.GetMusic : TMix_MusicType;
begin
  //result := Mix_GetMusicType( FMix_Music );
  {$IFNDEF DARWIN}
  {$IFNDEF no_smpeg}
  if FIsMP3 then
    result := MUS_MP3
  else
  {$ENDIF}
  {$ENDIF}
    result := FMix_Music.type_;
end;

function TSDLMusic.GetPaused : boolean;
begin
  {$IFNDEF DARWIN}
  {$IFNDEF no_smpeg}
  if FIsMP3 then
    result := SMPEG_status( FPSMPEG ) = STATUS_SMPEG_STOPPED
  else
  {$ENDIF}
  {$ENDIF}
    result := ( Mix_PausedMusic(FChannel) = 1 );
end;

function TSDLMusic.GetPlaying: boolean;
begin
  {$IFNDEF DARWIN}
  {$IFNDEF no_smpeg}
  if FIsMP3 then
    result := SMPEG_status( FPSMPEG ) = STATUS_SMPEG_PLAYING
  else
  {$ENDIF}
  {$ENDIF}
    result := ( Mix_PlayingMusic(FChannel) = 1 );
end;

function TSDLMusic.GetVolume : integer;
begin
  {$IFNDEF DARWIN}
  {$IFNDEF no_smpeg}
  if FIsMP3 then
    result := -1//SMPEG_setvolume( FPSMPEG, Value )
  else
  {$ENDIF}
  {$ENDIF}
    result := Mix_VolumeMusic( -1, FChannel );
end;

{$T-}
procedure TSDLMusic.LoadFromFile( const aFileName : string );
begin
  inherited;
  FIsMP3 := LowerCase( ExtractFileExt( aFileName ) ) = '.mp3';
(*  {$IFNDEF DARWIN}
  {$IFNDEF no_smpeg}
  if FIsMP3 then
  begin
    FPSMPEG := SMPEG_new( PAnsiChar( ansiString(aFileName) ), nil, 0 );

    // Disable Audio
    SMPEG_enableaudio( FPSMPEG, 0 );

    // Query Mixer
    Mix_QuerySpec( freq, format, chan );
    aspec.freq := freq;
    aspec.format := format;
    aspec.channels := chan;

    // Tell Smpeg what we want
    Smpeg_actualSpec( FPSMPEG, @aspec );

    // Hook the mixer audio playing function
    Mix_HookMusic( @SMPeg_PlayAudioSDL, FPSMPEG );

    // Reenable Audio
    SMPEG_enableaudio( FPSMPEG, 1 );
  end
  else
  {$ENDIF}
  {$ENDIF} *)
    FMix_Music := Mix_LoadMUS( PAnsiChar( ansiString(aFileName) ) );
end;
{$T+}

procedure TSDLMusic.LoadFromStream( aStream : TStream );
begin
  inherited;
  FRw := sdlstreams.SDLStreamSetup(aStream);
  try
    FMix_Music := Mix_LoadMUS_RW(FRw);
    if FMix_Music = nil then
      raise EBadHandle.Create(string(AnsiString(SDL_GetError)));
  except
     sdlstreams.SDLStreamCloseRWops(FRw);
     aStream.Free;
     raise;
  end;
end;

procedure TSDLMusic.Pause;
begin
  inherited;
  {$IFNDEF DARWIN}
  {$IFNDEF no_smpeg}
  if FIsMP3 then
    SMPEG_pause( FPSMPEG )
  else
  {$ENDIF}
  {$ENDIF}
    Mix_PauseMusic(FChannel);
end;

procedure TSDLMusic.Play( aLoops : integer );
begin
  inherited;
  {$IFNDEF DARWIN}
  {$IFNDEF no_smpeg}
  if FIsMP3 then
  begin
    SMPEG_play( FPSMPEG );
    SMPEG_loop( FPSMPEG, aLoops );
  end
  else
  {$ENDIF}
  {$ENDIF}
  begin
    if ( Mix_PlayingMusic(FChannel) = 0 ) then
      Mix_PlayMusic( FMix_Music, aLoops, FChannel );
  end;
end;

procedure TSDLMusic.Resume;
begin
  inherited;
  {$IFNDEF DARWIN}
  {$IFNDEF no_smpeg}
  if FIsMP3 then
    SMPEG_pause( FPSMPEG )
  else
  {$ENDIF}
  {$ENDIF}
    Mix_ResumeMusic(FChannel);
end;

procedure TSDLMusic.Rewind;
begin
  {$IFNDEF DARWIN}
  {$IFNDEF no_smpeg}
  if FIsMP3 then
    SMPEG_rewind( FPSMPEG )
  else
  {$ENDIF}
  {$ENDIF}
    Mix_RewindMusic(FChannel);
end;

procedure TSDLMusic.SetFinishedEvent( const Value : TMusicFinishedEvent );
begin
  FMusicFinishedEvent := Value;
  Mix_HookMusicFinished( @FMusicFinishedEvent );
end;

procedure TSDLMusic.SetMusicPosition( const Value : double );
begin
  if FMusicPosition <> Value then
  begin
    FMusicPosition := Value;
    if ( Mix_SetMusicPosition( Value, FChannel ) = -1 ) then
      raise ESDLMusic.CreateFmt( 'Mix_SetMusicPosition : %s', [ Mix_GetError ] );
  end;
end;

procedure TSDLMusic.SetVolume( const Value : integer );
begin
  {$IFNDEF DARWIN}
  {$IFNDEF no_smpeg}
  if FIsMP3 then
    SMPEG_setvolume( FPSMPEG, Value )
  else
  {$ENDIF}
  {$ENDIF}
    Mix_VolumeMusic( Value, FChannel );
end;

procedure TSDLMusic.Stop;
begin
  inherited;
  {$IFNDEF DARWIN}
  {$IFNDEF no_smpeg}
  if FIsMP3 then
    SMPEG_stop( FPSMPEG )
  else
  {$ENDIF}
  {$ENDIF}
    Mix_HaltMusic(FMix_Music);
end;

procedure TSDLMusic.UnLoad;
begin
  inherited;
  {$IFNDEF DARWIN}
  {$IFNDEF no_smpeg}
  if FIsMP3 then
  begin
    // Unhook mixer audio playback function
    Mix_HookMusic( nil, nil );
    
    SMPEG_delete( FPSMPEG )
  end
  else
  {$ENDIF}
  {$ENDIF}
    Mix_FreeMusic( FMix_Music );
  if assigned(FRw) then
  begin
     TObject(FRw.unknown.data1).Free;
     SDLStreamCloseRWops(FRw);
     FRw := nil;
  end;
end;

{ TSDLSoundEffectManager }

function TSDLSoundEffectManager.Add( aObject : TSDLSoundEffect; const aSoundEffectName: string ) : Integer;
begin
  if FSoundEffectNames = nil then
    FSoundEffectNames := TStringList.Create;

  if ( aSoundEffectName <> '' ) then
  begin
    if ( FSoundEffectNames.IndexOf( aSoundEffectName ) <> -1 ) then
    begin
      aObject.Free;
      result := -1;
      exit;
    end;
    FSoundEffectNames.Add( aSoundEffectName );
  end;

  result := inherited Add( aObject );
end;

function TSDLSoundEffectManager.Extract( aItem : TSDLSoundEffect ) : TSDLSoundEffect;
begin
  result := TSDLSoundEffect( inherited Extract( aItem ) );
  FSoundEffectNames.Delete( IndexOf( aItem ) );
end;

destructor TSDLSoundEffectManager.Destroy;
begin
  if FSoundEffectNames <> nil then
    FSoundEffectNames.Free;
  inherited;
end;

function TSDLSoundEffectManager.Extract( const aSoundEffectName: string): TSDLSoundEffect;
begin
  result := Extract( Items[ FSoundEffectNames.IndexOf( aSoundEffectName ) ] );
end;

function TSDLSoundEffectManager.First : TSDLSoundEffect;
begin
  result := TSDLSoundEffect( inherited First );
end;

function TSDLSoundEffectManager.GetItem( aIndex : Integer ) : TSDLSoundEffect;
begin
  result := TSDLSoundEffect( inherited GetItem( aIndex ) );
end;

function TSDLSoundEffectManager.GetSoundEffectByName(
  const aSoundEffectName: string): TSDLSoundEffect;
begin
  result := Items[ FSoundEffectNames.IndexOf( aSoundEffectName ) ];
end;

function TSDLSoundEffectManager.IndexOf( aObject : TSDLSoundEffect ) : Integer;
begin
  result := inherited IndexOf( aObject );
end;

function TSDLSoundEffectManager.IndexOf( const aSoundEffectName: string): Integer;
begin
  result := FSoundEffectNames.IndexOf( aSoundEffectName );
end;

procedure TSDLSoundEffectManager.Insert( aIndex : Integer; aObject : TSDLSoundEffect );
begin
  inherited Insert( aIndex, aObject );
end;

function TSDLSoundEffectManager.Last : TSDLSoundEffect;
begin
  result := TSDLSoundEffect( inherited Last );
end;

function TSDLSoundEffectManager.Remove( aObject : TSDLSoundEffect ) : Integer;
begin
  result := inherited Remove( aObject );
  FSoundEffectNames.Delete( result );
end;

function TSDLSoundEffectManager.Remove( const aSoundEffectName: string): Integer;
begin
  result := Remove( Items[ FSoundEffectNames.IndexOf( aSoundEffectName ) ] );
end;

procedure TSDLSoundEffectManager.SetItem( aIndex : Integer; aObject : TSDLSoundEffect );
begin
  inherited SetItem( aIndex, aObject );
end;

procedure TSDLSoundEffectManager.SetSoundEffectByName(const aSoundEffectName: string; const aValue: TSDLSoundEffect);
var
  I: Integer;
begin
  I := FSoundEffectNames.IndexOf(aSoundEffectName);
  if aValue <> nil then
  begin
    if I < 0 then
      Add( aValue, aSoundEffectName )
    else
      Items[ I ] := aValue;
  end
  else
  begin
    if I >= 0 then
      Remove( Items[ I ] );
  end;
end;

{ TSDLMusicManager }

function TSDLMusicManager.Add( aObject : TSDLMusic; const aTrackName: string ) : Integer;
begin
  if FTrackNames = nil then
    FTrackNames := TStringList.Create;

  if ( aTrackName <> '' ) then
  begin
    if ( FTrackNames.IndexOf( aTrackName ) <> -1 ) then
    begin
      aObject.Free;
      result := -1;
      Exit;
    end;
    FTrackNames.Add( aTrackName );
  end;

  result := inherited Add( aObject );
  FCurrentTrack := result;
end;

destructor TSDLMusicManager.Destroy;
begin
  if FTrackNames <> nil then
    FTrackNames.Free;
  inherited;
end;

function TSDLMusicManager.Extract( aItem : TSDLMusic ) : TSDLMusic;
begin
  result := TSDLMusic( inherited Extract( aItem ) );
  FTrackNames.Delete( IndexOf( aItem ) );
end;

function TSDLMusicManager.Extract(const aTrackName: string): TSDLMusic;
begin
  result := Extract( Items[ FTrackNames.IndexOf( aTrackName ) ] );
end;

function TSDLMusicManager.First : TSDLMusic;
begin
  result := TSDLMusic( inherited First );
end;

function TSDLMusicManager.GetItem( aIndex : Integer ) : TSDLMusic;
begin
  result := TSDLMusic( inherited GetItem( aIndex ) );
end;

function TSDLMusicManager.GetTrackByName(const aTrackName: string): TSDLMusic;
begin
  result := Items[ FTrackNames.IndexOf( aTrackName ) ];
end;

function TSDLMusicManager.IndexOf( aObject : TSDLMusic ) : Integer;
begin
  result := inherited IndexOf( aObject );
end;

function TSDLMusicManager.IndexOf(const aTrackName: string): Integer;
begin
  result := FTrackNames.IndexOf( aTrackName );
end;

procedure TSDLMusicManager.Insert( aIndex : Integer; aObject : TSDLMusic );
begin
  inherited Insert( aIndex, aObject );
end;

function TSDLMusicManager.Last : TSDLMusic;
begin
  result := TSDLMusic( inherited Last );
end;

function TSDLMusicManager.Remove( aObject : TSDLMusic ) : Integer;
begin
  result := inherited Remove( aObject );
  FTrackNames.Delete( result );
end;

function TSDLMusicManager.Remove(const aTrackName: string): Integer;
begin
  result := Remove( Items[ FTrackNames.IndexOf( aTrackName ) ] );
end;

procedure TSDLMusicManager.SetItem( aIndex : Integer; aObject : TSDLMusic );
begin
  inherited SetItem( aIndex, aObject );
end;

procedure TSDLMusicManager.SetTrackByName(const aTrackName: string; const aValue: TSDLMusic);
var
  I: Integer;
begin
  I := FTrackNames.IndexOf( aTrackName );
  if aValue <> nil then
  begin
    if I < 0 then
      Add( aValue, aTrackName )
    else
      Items[ I ] := aValue;
  end
  else
  begin
    if I >= 0 then
      Remove( Items[ I ] );
  end;
end;

{ TSDLAudioManager }

constructor TSDLAudioManager.Create( frequency : integer; format : Uint16;
  channels, chunksize : integer );
begin
  inherited Create;

  if ( Mix_OpenAudio( frequency, format, channels, chunksize ) < 0 ) then
    raise ESDLAudioException.CreateFmt( 'Mix_OpenAudio : %s', [ Mix_GetError ] );

  FMusicManager := TSDLMusicManager.Create;
  FSoundEffectManager := TSDLSoundEffectManager.Create;
end;

destructor TSDLAudioManager.Destroy;
begin
  if FMusicManager <> nil then
    FMusicManager.Free;
  if FSoundEffectManager <> nil then
    FSoundEffectManager.Free;

  inherited;
end;

procedure TSDLAudioManager.SetVolume(const Value: integer);
var
  i : integer;
begin
  for i := 0 to FMusicManager.Count - 1 do
  begin
    FMusicManager[ i ].Volume := Value;
  end;

  for i := 0 to FSoundEffectManager.Count - 1 do
  begin
    FSoundEffectManager[ i ].Volume := Value;
  end;
end;

procedure TSDLAudioManager.Stop;
var
  i : integer;
begin
  for i := 0 to FMusicManager.Count - 1 do
  begin
    FMusicManager[ i ].Stop;
  end;

  for i := 0 to FSoundEffectManager.Count - 1 do
  begin
    FSoundEffectManager[ i ].Stop;
  end;
end;

initialization
  if ( SDL_WasInit( SDL_INIT_AUDIO ) = 0 ) then
    SDL_InitSubSystem( SDL_INIT_AUDIO );
  Mix_Init([mifFlac, mifMod, mifMp3, mifOgg]);


finalization
  SDL_QuitSubSystem( SDL_INIT_AUDIO );

end.

