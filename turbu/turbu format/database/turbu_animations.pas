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
   classes, DB, RTTI,
   commons, turbu_sounds, turbu_classes, turbu_containers, turbu_serialization,
   sg_defs;

type
   TAnimYTarget = (at_top, at_center, at_bottom);
   TFlashTarget = (fl_none, fl_target, fl_screen);

   UploadColorAttribute = class(TDBUploadAttribute)
      procedure upload(db: TDataset; field: TRttiField; instance: TObject); override;
      procedure download(db: TDataset; field: TRttiField; instance: TObject); override;
   end;

   UploadCellColorAttribute = class(TDBUploadAttribute)
      procedure upload(db: TDataset; field: TRttiField; instance: TObject); override;
      procedure download(db: TDataset; field: TRttiField; instance: TObject); override;
   end;

   TAnimEffects = class(TRpgDatafile)
   private
      FFrame: word;
      FSound: TRpgSound;
      FFlashWhere: TFlashTarget;
      [UploadColor]
      FColor: TRpgColor;
      FShakeWhere: TFlashTarget;
      function GetColor(const index: integer): byte; inline;
   protected
      class function keyChar: ansiChar; override;
   public
      constructor Create; override;
      constructor Load(savefile: TStream); override;
      procedure save(savefile: TStream); override;
      destructor Destroy; override;

      property frame: word read FFrame write FFrame;
      property sound: TRpgSound read FSound write FSound;
      property flashWhere: TFlashTarget read FFlashWhere write FFlashWhere;
      property color: TRpgColor read FColor write FColor;
      property shakeWhere: TFlashTarget read FShakeWhere write FShakeWhere;
      property r: byte index 1 read GetColor;
      property g: byte index 2 read GetColor;
      property b: byte index 3 read GetColor;
      property a: byte index 4 read GetColor;
   end;

   TAnimCell = class(TRpgDatafile)
   protected
      FFrame: word;
      FPosition: TSgPoint;
      FZoom: TSgPoint;
      [UploadCellColor]
      FColor: TRpgColor;
      FSaturation: byte;
      FImageIndex: integer;
   protected
      class function keyChar: ansiChar; override;
   public
      constructor Load(savefile: TStream); override;
      procedure save(savefile: TStream); override;

      property frame: word read FFrame;
      property position: TSgPoint read FPosition;
      property zoom: TSgPoint read FZoom;
      property color: TRpgColor read FColor;
      property saturation: byte read FSaturation;
      property ImageIndex: integer read FImageIndex;
   end;

   TAnimFrameList = class(TRpgObjectList<TAnimCell>);
   TAnimEffectList = class(TRpgObjectList<TAnimEffects>);

   TAnimTemplate = class(TRpgDatafile)
   private
      FFilename: string;
      FTimingSec: TAnimEffectList;
      FFrameSec: TAnimFrameList;
      FHitsAll: boolean;
      FYTarget: TAnimYTarget;
   protected
      class function getDatasetName: string; override;
      class function keyChar: ansiChar; override;
   public
      constructor Create; override;
      constructor Load(savefile: TStream); override;
      procedure save(savefile: TStream); override;
      destructor Destroy; override;

      property filename: string read FFilename write FFilename;
      property hitsAll: boolean read FHitsAll write FHitsAll;
      property yTarget: TAnimYTarget read FYTarget write FYTarget;
      property effect: TAnimEffectList read FTimingSec;
      property frame: TAnimFrameList read FFrameSec;
   end;

   TBattleCharData = class(TRpgDatafile)
   protected
      FFilename: string;
      FFrame: integer;
      FUnk04: integer;
      FUnk05: integer;
   public
      property filename: string read FFilename;
      property frame: integer read FFrame;
   end;

   TBattleCharDataList = class(TRpgObjectList<TBattleCharData>);

   TBattleCharAnim = class(TRpgDatafile)
   protected
      FSpeed: integer;
      FPoses: TBattleCharDataList;
      FWeapons: TBattleCharDataList;
   public
      constructor Create; override;
      destructor Destroy; override;
   end;

implementation

{ TAnimEffects }

function TAnimEffects.GetColor(const index: integer): byte;
begin
   result := FColor.rgba[index];
end;

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

constructor TAnimEffects.Create;
begin
   inherited Create;
   FSound := TRpgSound.Create;
end;

destructor TAnimEffects.Destroy;
begin
   FSound.free;
   inherited;
end;

{ TAnimTemplate }

constructor TAnimTemplate.Create;
begin
   inherited Create;
   FTimingSec := TAnimEffectList.Create;
   FFrameSec := TAnimFrameList.Create;
end;

destructor TAnimTemplate.Destroy;
begin
   FTimingSec.Free;
   FFrameSec.free;
   inherited;
end;

class function TAnimTemplate.getDatasetName: string;
begin
   result := 'animations';
end;

class function TAnimTemplate.keyChar: ansiChar;
begin
   result := 'a';
end;

constructor TAnimTemplate.Load(savefile: TStream);
var
   i: integer;
begin
   inherited Load(savefile);
   FFilename := savefile.readString();
   FTimingSec := TAnimEffectList.Create;
   FTimingSec.Capacity := savefile.readInt;
   FTimingSec.Add(TAnimEffects.Create);
   for I := 1 to FTimingSec.Capacity - 1 do
      FTimingSec.Add(TAnimEffects.load(savefile));
   lassert(savefile.readChar = 't');
   FFrameSec := TAnimFrameList.Create;
   FFrameSec.Capacity := savefile.readInt;
   FFrameSec.Add(TAnimCell.Create);
   for I := 1 to FFrameSec.Capacity - 1 do
      FFrameSec.Add(TAnimCell.load(savefile));
   lassert(savefile.readChar = 's');
   FHitsAll := savefile.readBool;
   savefile.readBuffer(FYTarget, sizeof(TAnimYTarget));
   lassert(savefile.readChar = 'A');
end;

procedure TAnimTemplate.save(savefile: TStream);
var
   i: integer;
begin
   inherited save(savefile);
   savefile.writeString(FFilename);
   savefile.writeInt(FTimingSec.Count);
   for I := 1 to FTimingSec.High do
      FTimingSec[i].save(savefile);
   savefile.writeChar('t');
   savefile.writeInt(FFrameSec.Count);
   for I := 1 to FFrameSec.High do
      FFrameSec[i].save(savefile);
   savefile.writeChar('s');
   savefile.writeBool(FHitsAll);
   savefile.WriteBuffer(FYTarget, sizeof(TAnimYTarget));
   savefile.writeChar('A');
end;

{ TAnimCell }

class function TAnimCell.keyChar: ansiChar;
begin
   result := 'c';
end;

constructor TAnimCell.Load(savefile: TStream);
begin
   inherited Load(savefile);
   FFrame := savefile.ReadWord;
   savefile.readBuffer(FPosition, sizeof(TSgPoint));
   savefile.readBuffer(FZoom, sizeof(TSgPoint));
   savefile.readBuffer(FColor, sizeof(TRpgColor));
   FSaturation := savefile.readByte;
   lassert(savefile.readChar = 'C');
end;

procedure TAnimCell.save(savefile: TStream);
begin
   inherited save(savefile);
   savefile.writeWord(FFrame);
   savefile.WriteBuffer(FPosition, sizeof(TSgPoint));
   savefile.WriteBuffer(FZoom, sizeof(TSgPoint));
   savefile.WriteBuffer(FColor, sizeof(TRpgColor));
   savefile.writeByte(FSaturation);
   savefile.writeChar('C');
end;

{ TBattleCharAnim }

constructor TBattleCharAnim.Create;
begin
   inherited;
   FPoses := TBattleCharDataList.Create;
   FWeapons := TBattleCharDataList.Create;
end;

destructor TBattleCharAnim.Destroy;
begin
   FWeapons.Free;
   FPoses.Free;
   inherited;
end;

{ UploadColorAttribute }

procedure UploadColorAttribute.download(db: TDataset; field: TRttiField;
  instance: TObject);
var
   anim: TAnimEffects absolute instance;
begin
   assert(instance is TAnimEffects);
   anim.FColor.color := cardinal(db.FieldByName('color').AsInteger);
end;

procedure UploadColorAttribute.upload(db: TDataset; field: TRttiField;
  instance: TObject);
var
   anim: TAnimEffects absolute instance;
begin
   assert(instance is TAnimEffects);
   db.FieldByName('color').AsInteger := integer(anim.FColor.color);
end;

{ UploadCellColorAttribute }

procedure UploadCellColorAttribute.download(db: TDataset; field: TRttiField;
  instance: TObject);
var
   anim: TAnimCell absolute instance;
begin
   assert(instance is TAnimCell);
   anim.FColor.color := cardinal(db.FieldByName('color').AsInteger);
end;

procedure UploadCellColorAttribute.upload(db: TDataset; field: TRttiField;
  instance: TObject);
var
   anim: TAnimCell absolute instance;
begin
   assert(instance is TAnimCell);
   db.FieldByName('color').AsInteger := integer(anim.FColor.color);
end;

end.
