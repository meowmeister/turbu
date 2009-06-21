unit turbu_animations;
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

interface
uses
   classes, DB,
   commons, turbu_sounds, turbu_classes;

type
   TAnimYTarget = (at_top, at_center, at_bottom);
   TFlashTarget = (fl_none, fl_target, fl_screen);

   TAnimEffects = class(TRpgDatafile)
   private
      FSound: TRpgSound;
      FFlashWhere: TFlashTarget;
      FColor: TRpgColor;
   protected
      class function keyChar: ansiChar; override;
   public
      constructor Load(savefile: TStream);
      procedure save(savefile: TStream); override;
      destructor Destroy; override;

      property sound: TRpgSound read FSound write FSound;
      property flashWhere: TFlashTarget read FFlashWhere write FFlashWhere;
      property color: TRpgColor read FColor write FColor;
   end;

   TAnimCell = class(TRpgDatafile)
   private
      FPosition: TRpgPoint;
      FZoom: TRpgPoint;
      FColor: TRpgColor;
      FSaturation: byte;
   protected
      class function keyChar: ansiChar; override;
   public
      constructor Load(savefile: TStream);
      procedure save(savefile: TStream); override;

      property position: TRpgPoint read FPosition write FPosition;
      property zoom: TRpgPoint read FZoom write FZoom;
      property color: TRpgColor read FColor write FColor;
      property saturation: byte read FSaturation write FSaturation;
   end;

   TAnimFrame = array of TAnimCell;
   TAnimFrameSet = array of TAnimFrame;
   TAnimEffectSet = array of TAnimEffects;

   TAnimTemplate = class(TRpgDatafile)
   private
      FFilename: string;
      FTimingSec: TAnimEffectSet;
      FFrameSec: TAnimFrameSet;
      FHitsAll: boolean;
      FYTarget: TAnimYTarget;
   protected
      function getDatasetName: string; override;
      class function keyChar: ansiChar; override;
   public
      constructor Load(savefile: TStream);
      procedure save(savefile: TStream); override;
      destructor Destroy; override;
      procedure upload(db: TDataSet); override;

      property filename: string read FFilename write FFilename;
      property hitsAll: boolean read FHitsAll write FHitsAll;
      property yTarget: TAnimYTarget read FYTarget write FYTarget;
      property effect: TAnimEffectSet read FTimingSec write FTimingSec;
      property frame: TAnimFrameSet read FFrameSec write FFrameSec;
   end;

implementation

{ TAnimEffects }

class function TAnimEffects.keyChar: ansiChar;
begin
   result := 'a';
end;

constructor TAnimEffects.Load(savefile: TStream);
begin
   inherited Load(savefile);
   if savefile.readBool then
      FSound := TRpgSound.Load(savefile);
   savefile.readBuffer(FFlashWhere, sizeof(TFlashTarget));
   savefile.readBuffer(FColor, sizeof(TRpgColor));
   lassert(savefile.readChar = 'E');
end;

procedure TAnimEffects.save(savefile: TStream);
begin
   inherited save(savefile);
   savefile.writeBool(assigned(FSound));
   if assigned(FSound) then
      FSound.save(savefile);
   savefile.WriteBuffer(FFlashWhere, sizeof(TFlashTarget));
   savefile.WriteBuffer(FColor, sizeof(TRpgColor));
   savefile.writeChar('E');
end;

destructor TAnimEffects.Destroy;
begin
   FSound.free;
   inherited;
end;

{ TAnimTemplate }

destructor TAnimTemplate.Destroy;
var
   i, j: integer;
begin
   for I := low(FTimingSec) to high(FTimingSec) do
      FTimingSec[i].free;
   for j := low(FFrameSec) to high(FFrameSec) do
   begin
      for I := low(FFrameSec[j]) to high(FFrameSec[j]) do
         FFrameSec[j, i].free;
   end;
   inherited;
end;

function TAnimTemplate.getDatasetName: string;
begin
   result := 'animations';
end;

class function TAnimTemplate.keyChar: ansiChar;
begin
   result := 'a';
end;

constructor TAnimTemplate.Load(savefile: TStream);
var
   i, j: integer;
begin
   inherited Load(savefile);
   FFilename := savefile.readString();
   setLength(FTimingSec, savefile.readInt);
   for I := 1 to high(FTimingSec) do
      FTimingSec[i] := TAnimEffects.load(savefile);
   lassert(savefile.readChar = 't');
   setLength(FFrameSec, savefile.readInt);
   for I := 0 to high(FFrameSec) do
   begin
      setLength(FFrameSec[i], savefile.readInt);
      for j := 1 to high(FFrameSec[i]) do
         FFrameSec[i, j] := TAnimCell.load(savefile);
      lassert(savefile.readChar = 'f');
   end;
   lassert(savefile.readChar = 's');
   FHitsAll := savefile.readBool;
   savefile.readBuffer(FYTarget, sizeof(TAnimYTarget));
   lassert(savefile.readChar = 'A');
end;

procedure TAnimTemplate.save(savefile: TStream);
var
   i, j: integer;
begin
   inherited save(savefile);
   savefile.writeString(FFilename);
   savefile.writeInt(length(FTimingSec));
   for I := 1 to high(FTimingSec) do
      FTimingSec[i].save(savefile);
   savefile.writeChar('t');
   savefile.writeInt(length(FFrameSec));
   for I := 0 to high(FFrameSec) do
   begin
      savefile.writeInt(length(FFrameSec[i]));
      for j := 1 to high(FFrameSec[i]) do
         FFrameSec[i, j].save(savefile);
      savefile.writeChar('f');
   end;
   savefile.writeChar('s');
   savefile.writeBool(FHitsAll);
   savefile.WriteBuffer(FYTarget, sizeof(TAnimYTarget));
   savefile.writeChar('A');
end;

procedure TAnimTemplate.upload(db: TDataSet);
begin
   inherited;
   db.FieldByName('hitsAll').AsBoolean := FHitsAll;
   db.FieldByName('yTarget').AsInteger := ord(FYTarget);
   //upload object arrays later
end;

{ TAnimCell }

class function TAnimCell.keyChar: ansiChar;
begin
   result := 'c';
end;

constructor TAnimCell.Load(savefile: TStream);
begin
   inherited Load(savefile);
   savefile.readBuffer(FPosition, sizeof(TRpgPoint));
   savefile.readBuffer(FZoom, sizeof(TRpgPoint));
   savefile.readBuffer(FColor, sizeof(TRpgColor));
   FSaturation := savefile.readByte;
   lassert(savefile.readChar = 'C');
end;

procedure TAnimCell.save(savefile: TStream);
begin
   inherited save(savefile);
   savefile.WriteBuffer(FPosition, sizeof(TRpgPoint));
   savefile.WriteBuffer(FZoom, sizeof(TRpgPoint));
   savefile.WriteBuffer(FColor, sizeof(TRpgColor));
   savefile.writeByte(FSaturation);
   savefile.writeChar('C');
end;

end.
