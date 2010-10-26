unit turbu_characters;
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
   types, classes, DB, Generics.Collections, rtti,
   turbu_constants, turbu_defs, turbu_classes, turbu_skills, turbu_containers,
   turbu_script_interface, turbu_serialization;

type
   TExpCalcFunc = function(int1, int2, int3, int4, int5: integer): integer of object;
   TStatCalcFunc = function(a, b, c: smallint; d: shortint): integer of object;

   TCommandStyle = (cs_weapon, cs_skill, cs_defend, cs_item, cs_flee, cs_skillgroup, cs_special, cs_script);

   TBattleCommand = class(TRpgDatafile)
   private
      FStyle: TCommandStyle;
      FValue: smallint;
   protected
      class function keyChar: ansiChar; override;
   public
      constructor Load(savefile: TStream); override;
      procedure save(savefile: TStream); override;

      property style: TCommandStyle read FStyle write FStyle;
      property value: smallint read FValue write FValue;
   end;

   TStatBlock = class;
   IStatBlock = interface;
   TStatSet = class;

   IStatBlock = interface //to allow reference counting done right
      procedure setSize(const value: word);
      function getSize: word;
      function getBlock: TIntArray;
      procedure setBlock(const Value: TIntArray);
      function getIndex: integer;

      property size: word read getSize write setSize;
      property block: TIntArray read getBlock write setBlock;
      property index: integer read getIndex;
   end;

   TStatBlockUploadAttribute = class(TDBUploadAttribute)
   private
      function getField(db: TDataset; i: integer): TField;
   public
      procedure upload(db: TDataset; field: TRttiField; instance: TObject); override;
      procedure download(db: TDataset; field: TRttiField; instance: TObject); override;
   end;

   TStatBlock = class(TInterfacedObject, IStatBlock)
   private
      FBlock: TIntArray;
      FParent: TStatSet;

      procedure setSize(const value: word);
      function getSize: word;
      function getBlock: TIntArray;
      procedure setBlock(const Value: TIntArray);
      function getIndex: integer;
   public
      constructor Create(size: word; parent: TStatSet);
      function compare(other: TStatBlock): boolean;

      property size: word read getSize write setSize;
      property block: TIntArray read getBlock write setBlock;
      property index: integer read getIndex;
   end;

   TStatSet = class(TRpgDatafile)
   private
      FBlocks: array of TStatBlock;
      function getSize: integer;
   protected
      class function keyChar: ansiChar; override;
   public
      constructor Load(savefile: TStream); override;
      procedure save(savefile: TStream); override;

      function add(var newBlock: TStatBlock): integer;
      function indexOf(value: TStatBlock): smallint;

      property size: integer read getSize;
   end;

   TCommandSet = packed array[1..COMMAND_COUNT] of smallint;
   TEqArray = packed array[1..TOTAL_SLOTS] of smallint;

   TClassTemplate = class(TRpgDatafile)
   private
      FMapSprite: string;
      FTranslucent: boolean;
      FActionMatrix: integer;
      FBattleSprite: integer;
      FBattleMatrix: integer;
      FPortrait: string;
      FPortraitIndex: integer;
      FCommand: TCommandSet;
      FCommands: byte;
      [TStatBlockUpload]
      FStatBlocks: array[1..STAT_COUNT] of IStatBlock;
      FExpFunc: string;
      FExpVars: T4IntArray;
      FSkillset: TSkillsetList;
      FResists: TPointArray;
      FConditions: TPointArray;
      FEquip: TEqArray;
      FDualWield: TWeaponStyle;
      FStaticEq: boolean;
      FStrongDef : boolean;
      FUnarmedAnim: integer;
      FOnJoin: TScriptEvent;

      function getCommand(x: byte): smallint; inline;
      procedure setCommand(x: byte; const Value: smallint); inline;
      function getStatBlock(x: byte): IStatBlock; inline;
      procedure setStatBlock(x: byte; const Value: IStatBlock); inline;
      function getExpVar(x: byte): integer;
      procedure setExpVar(x: byte; const Value: integer);
      function getEq(x: byte): smallint;
      procedure setEq(x: byte; const Value: smallint);
   protected
      class function getDatasetName: string; override;
      class function keyChar: ansiChar; override;
   public
      constructor Load(savefile: TStream); override;
      constructor Create;
      destructor Destroy; override;
      procedure save(savefile: TStream); override;

      procedure addResist(const value: TPoint); inline;
      procedure addCondition(const value: TPoint); inline;

      property command[x: byte]: smallint read getCommand write setCommand;
      property statblock[x: byte]: IStatBlock read getStatBlock write setStatBlock;
      property expVars[x: byte]: integer read getExpVar write setExpVar;
      property eq[x: byte]: smallint read getEq write setEq;
      property skillset: TSkillsetList read FSkillset write FSkillset;
      property clsName: string read FName write FName;
      property mapSprite: string read FMapSprite write FMapSprite;
      property translucent: boolean read FTranslucent write FTranslucent;
      property battleSprite: integer read FBattleSprite write FBattleSprite;
      property portrait: string read FPortrait write FPortrait;
      property portraitIndex: integer read FPortraitIndex write FPortraitIndex;
      property commands: byte read FCommands write FCommands;
      property expFunc: string read FExpFunc write FExpFunc;
      property resist: TPointArray read FResists write FResists;
      property condition: TPointArray read FConditions write FConditions;
      property dualWield: TWeaponStyle read FDualWield write FDualWield;
      property staticEq: boolean read FStaticEq write FStaticEq;
      property strongDef: boolean read FStrongDef write FStrongDef;
      property unarmedAnim: integer read FUnarmedAnim write FUnarmedAnim;
   published
      [EventType('TPartyEvent')]
      property OnJoin: TScriptEvent read FOnJoin write FOnJoin;
   end;

   THeroTemplate = class(TClassTemplate)
   private
      FTitle: string;
      FClass: integer;
      FPortraitShift: TColorShift;
      FSpriteShift: TColorShift;
      FBattleSpriteShift: TColorShift;
      FMinLevel: word;
      FMaxLevel: word;
      FGuest: boolean;
      FCanCrit: boolean;
      FCritRate: integer;
   protected
      class function getDatasetName: string; override;
      class function keyChar: ansiChar; override;
   public
      constructor Load(savefile: TStream); override;
      procedure save(savefile: TStream); override;

      property title: string read FTitle write FTitle;
      property charClass: integer read FClass write FClass;
      property portraitShift: TColorShift read FPortraitShift write FPortraitShift;
      property spriteShift: TColorShift read FSpriteShift write FSpriteShift;
      property battleSpriteShift: TColorShift read FBattleSpriteShift write FBattleSpriteShift;
      property minLevel: word read FMinLevel write FMinLevel;
      property maxLevel: word read FMaxLevel write FMaxLevel;
      property guest: boolean read FGuest write FGuest;
      property canCrit: boolean read FCanCrit write FCanCrit;
      property critRate: integer read FCritRate write FCritRate;
   end;

//procedure SetDatabase(value: TRpgDatafile);

implementation

uses
   sysUtils,
   turbu_database;

{var
   GDatabase: TRpgDatabase;

procedure SetDatabase(value: TRpgDatafile);
begin
   GDatabase := value as TRpgDatabase;
end; }

resourcestring
   NOT_IN_BLOCK = 'Stat block not found in stat set!';

{ TStatBlock }

constructor TStatBlock.Create(size: word; parent: TStatSet);
begin
   setLength(FBlock, size);
   FParent := parent;
end;

function TStatBlock.getBlock: TIntArray;
begin
   result := FBlock;
end;

function TStatBlock.getIndex: integer;
begin
   result := FParent.indexOf(self);
end;

function TStatBlock.getSize: word;
begin
   result := length(FBlock);
end;

procedure TStatBlock.setBlock(const Value: TIntArray);
begin
   FBlock := Value;
end;

procedure TStatBlock.setSize(const value: word);
begin
   setLength(FBlock, value);
end;

function TStatBlock.compare(other: TStatBlock): boolean;
var
   i: integer;
begin
   if self.size <> other.size then
   begin
      result := false;
      Exit;
   end;

   i := 0;
   while (i < size) and (FBlock[i] = other.FBlock[i]) do
      inc(i);
   result := i = size;
end;

{ TStatSet }

constructor TStatSet.Load(savefile: TStream);
var
   len: word;
   i: word;
   newblock: TStatBlock;
begin
   inherited Load(savefile);
   lassert ((FId = 0) and (FName = ''));
   len := savefile.readWord;
   for I := 0 to len - 1 do
   begin
      lassert(savefile.readWord = i);
      newblock := TStatBlock.Create(saveFile.readWord, self);
      savefile.ReadBuffer(newblock.FBlock[0], newblock.Size * 4);
      lassert(self.add(newblock) = i);
   end;
   lassert(savefile.readChar = 'S');
end;

procedure TStatSet.save(savefile: TStream);
var
   len: word;
   i: word;
begin
   inherited save(savefile);
   len := length(FBlocks);
   savefile.writeWord(len);
   for I := 0 to len - 1 do
   begin
      savefile.writeWord(i);
      savefile.writeWord(FBlocks[i].size);
      savefile.WriteBuffer(FBlocks[i].FBlock[0], FBlocks[i].size * 4);
   end;
   savefile.writeChar('S');
end;

function TStatSet.add(var newBlock: TStatBlock): integer;
var
   i: integer;
begin
   i := 0;
   while (i <= high(FBlocks)) and not (newBlock.compare(FBlocks[i])) do
      inc(i);
   if i > high(FBlocks) then
   begin
      setLength(FBlocks, length(FBlocks) + 1);
      FBlocks[high(FBlocks)] := newblock;
   end
   else begin
      newBlock.Free;
      newBlock := FBlocks[i];
   end;
   result := i;
end;

function TStatSet.indexOf(value: TStatBlock): smallint;
begin
   result := high(FBlocks);
   while (result >= 0) and (self.FBlocks[result] <> value) do
      dec(result);
end;

class function TStatSet.keyChar: ansiChar;
begin
   result := 's';
end;

function TStatSet.getSize: integer;
begin
   result := length(FBlocks);
end;

{ TBattleCommand }

class function TBattleCommand.keyChar: ansiChar;
begin
   result := 'b';
end;

constructor TBattleCommand.Load(savefile: TStream);
begin
   inherited Load(savefile);
   savefile.ReadBuffer(self.FStyle, sizeof(TCommandStyle));
end;

procedure TBattleCommand.save(savefile: TStream);
begin
   inherited save(savefile);
   savefile.WriteBuffer(self.FStyle, sizeof(TCommandStyle));
end;

{ TCharClass }

constructor TClassTemplate.Create;
var
   i: integer;
begin
   inherited Create;
   FSkillset := TSkillsetList.Create;
   FSkillSet.add(TSkillGainInfo.Create);
   for I := 1 to TOTAL_SLOTS do
      self.eq[i] := -1;
end;

constructor TClassTemplate.Load(savefile: TStream);
var
   i: integer;
begin
   inherited Load(savefile);
   FSkillset := TSkillsetList.Create;
   FSkillSet.add(TSkillGainInfo.Create);

   FMapSprite := savefile.readString;
   FTranslucent := savefile.readBool;
   FActionMatrix := savefile.readInt;
   FBattleSprite := savefile.readInt;
   FBattleMatrix := savefile.readInt;
   FPortrait := savefile.readString;
   FPortraitIndex := savefile.readInt;
   lassert(savefile.readWord = COMMAND_COUNT);
   savefile.readBuffer(FCommand[1], sizeof(FCommand));
   FCommands := savefile.readByte;
   lassert(savefile.readByte = STAT_COUNT);
   for I := 1 to STAT_COUNT do
      FStatBlocks[i] := GDatabase.statSet.FBlocks[savefile.readInt];
   lassert(savefile.readChar = 'p');
   FExpFunc := savefile.readString;
   savefile.readBuffer(FExpVars[1], 16);
   for I := 1 to savefile.readInt do
      FSkillSet.add(TSkillGainInfo.Load(savefile));
   lassert(savefile.readChar = 's');
   setLength(FResists, savefile.readInt);
   if length(FResists) > 0 then
      savefile.readBuffer(FResists[0], sizeof(TPoint) * length(FResists));
   setLength(FConditions, savefile.readInt);
   if length(FConditions) > 0 then
      savefile.readBuffer(FConditions[0], sizeof(TPoint) * length(FConditions));
   lassert(savefile.readByte = TOTAL_SLOTS);
   savefile.readBuffer(FEquip[1], sizeof(smallint) * TOTAL_SLOTS);
   savefile.readBuffer(FDualWield, sizeof(TWeaponStyle));
   FStaticEq := savefile.readBool;
   FStrongDef := savefile.readBool;
   FUnarmedAnim := savefile.readInt;
   lassert(savefile.readChar = 'C');
end;

procedure TClassTemplate.save(savefile: TStream);
var
   i: integer;
begin
   inherited save(savefile);
   savefile.writeString(FMapSprite);
   savefile.writeBool(FTranslucent);
   savefile.writeInt(FActionMatrix);
   savefile.writeInt(FBattleSprite);
   savefile.writeInt(FBattleMatrix);
   savefile.writeString(FPortrait);
   savefile.writeInt(FPortraitIndex);
   savefile.writeWord(COMMAND_COUNT);
   savefile.WriteBuffer(FCommand[1], sizeof(FCommand));
   savefile.writeByte(FCommands);
   savefile.writeByte(STAT_COUNT);
   for I := 1 to STAT_COUNT do
      savefile.WriteInt(FStatBlocks[i].index);
   savefile.writeChar('p');
   savefile.writeString(FExpFunc);
   savefile.WriteBuffer(FExpVars[1], 16);
   savefile.writeInt(FSkillset.High);
   for I := 1 to FSkillset.high do
      FSkillSet[i].save(savefile);
   savefile.writeChar('s');
   savefile.writeInt(length(FResists));
   if length(FResists) > 0 then
      savefile.WriteBuffer(FResists[0], sizeof(TPoint) * length(FResists));
   savefile.writeInt(length(FConditions));
   if length(FConditions) > 0 then
      savefile.WriteBuffer(FConditions[0], sizeof(TPoint) * length(FConditions));
   savefile.writeByte(TOTAL_SLOTS);
   savefile.WriteBuffer(FEquip[1], sizeof(smallint) * TOTAL_SLOTS);
   savefile.WriteBuffer(FDualWield, sizeof(TWeaponStyle));
   savefile.writeBool(FStaticEq);
   savefile.writeBool(FStrongDef);
   savefile.writeInt(FUnarmedAnim);
   savefile.writechar('C');
end;

destructor TClassTemplate.Destroy;
begin
   FSkillset.Free;
   inherited Destroy;
end;

procedure TClassTemplate.addCondition(const value: TPoint);
begin
   setLength(FConditions, length(FConditions) + 1);
   FConditions[high(FConditions)] := value;
end;

procedure TClassTemplate.addResist(const value: TPoint);
begin
   setLength(FResists, length(FResists) + 1);
   FResists[high(FResists)] := value;
end;

function TClassTemplate.getCommand(x: byte): smallint;
begin
   result := FCommand[x];
end;

class function TClassTemplate.getDatasetName: string;
begin
   result := 'charClasses';
end;

function TClassTemplate.getEq(x: byte): smallint;
begin
   result := FEquip[x];
end;

function TClassTemplate.getExpVar(x: byte): integer;
begin
   result := FExpVars[x];
end;

function TClassTemplate.getStatBlock(x: byte): IStatBlock;
begin
   result := FStatBlocks[x];
end;

class function TClassTemplate.keyChar: ansiChar;
begin
   result := 'c';
end;

procedure TClassTemplate.setCommand(x: byte; const Value: smallint);
begin
   FCommand[x] := value;
end;

procedure TClassTemplate.setEq(x: byte; const Value: smallint);
begin
   FEquip[x] := value;
end;

procedure TClassTemplate.setExpVar(x: byte; const Value: integer);
begin
   FExpVars[x] := value;
end;

procedure TClassTemplate.setStatBlock(x: byte; const Value: IStatBlock);
begin
   FStatBlocks[x] := value;
end;

{ THeroTemplate }

class function THeroTemplate.getDatasetName: string;
begin
   result := 'heroes';
end;

class function THeroTemplate.keyChar: ansiChar;
begin
   result := 'h';
end;

constructor THeroTemplate.Load(savefile: TStream);
begin
   inherited Load(savefile);
   FTitle := savefile.readString;
   FClass := savefile.readInt;
   savefile.ReadBuffer(FPortraitShift, sizeof(TColorShift));
   savefile.ReadBuffer(FSpriteShift, sizeof(TColorShift));
   savefile.ReadBuffer(FBattleSpriteShift, sizeof(TColorShift));
   FMinLevel := savefile.readWord;
   FMaxLevel := savefile.readWord;
   FGuest := savefile.readBool;
   lassert(savefile.readChar = 'H');
end;

procedure THeroTemplate.save(savefile: TStream);
begin
   inherited save(savefile);
   savefile.writeString(FTitle);
   savefile.writeInt(FClass);
   savefile.WriteBuffer(FPortraitShift, sizeof(TColorShift));
   savefile.WriteBuffer(FSpriteShift, sizeof(TColorShift));
   savefile.WriteBuffer(FBattleSpriteShift, sizeof(TColorShift));
   savefile.writeWord(FMinLevel);
   savefile.writeWord(FMaxLevel);
   savefile.writeBool(FGuest);
   savefile.writeChar('H');
end;

{ TStatBlockUploadAttribute }

function TStatBlockUploadAttribute.getField(db: TDataset; i: integer): TField;
begin
   result := db.FieldByName(format('statblock[%d]', [i]));
end;

procedure TStatBlockUploadAttribute.download(db: TDataset; field: TRttiField;
  instance: TObject);
var
   i: integer;
   template: TClassTemplate absolute instance;
begin
   assert(template is TClassTemplate);
   for i := 1 to high(template.FStatBlocks) do
      template.FStatBlocks[i] := GDatabase.statSet.FBlocks[getField(db, i).AsInteger];
end;

procedure TStatBlockUploadAttribute.upload(db: TDataset; field: TRttiField;
  instance: TObject);
var
   i: integer;
   template: TClassTemplate absolute instance;
begin
   assert(template is TClassTemplate);
   for i := 1 to high(template.FStatBlocks) do
      getField(db, i).AsInteger := template.FStatBlocks[i].index;
end;

end.
