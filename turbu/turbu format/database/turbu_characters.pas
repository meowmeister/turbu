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
   types, classes, DB, Generics.Collections,
   turbu_constants, turbu_defs, turbu_classes, turbu_skills;

type
   TStatBlock = class;

   TExpCalcFunc = function(int1, int2, int3, int4, int5: integer): integer of object;
   TStatCalcFunc = function(a, b, c: smallint; d: shortint): integer of object;

   TCommandStyle = (cs_weapon, cs_skill, cs_defend, cs_item, cs_flee, cs_skillgroup, cs_special, cs_script);
   TWeaponStyle = (ws_single, ws_shield, ws_dual, ws_all);

   TPosNegPointer = packed record
   case byte of
      0: ({$IFNDEF 64BIT}dummy: integer;{$ENDIF}
              address: TStatBlock);
      1: (index: shortint; //must be negative
             val8: shortint;
             val16: array[1..3] of smallint);
      2: (fullvalue: int64); //last three values are for convenience only and
      3: (bytes: array[1..8] of byte); //not to be used as actual options
      4: (sBytes: array[1..8] of shortint);
   end;

   TExpCalcRecord = class(TScriptRecord)
   private
      function getMethod: TExpCalcFunc; inline;
      class function getExpFunc(name: string): TExpCalcFunc;
   protected
      procedure setName(const Value: string); override;
   public
      constructor Create(name, designName: string);
      property method: TExpCalcFunc read getMethod;
   end;

   TExpRecordList = TRpgObjectList<TExpCalcRecord>;

   TBattleCommand = class(TRpgDatafile)
   private
      FStyle: TCommandStyle;
      FValue: smallint;
   public
      constructor Load(savefile: TStream);
      procedure save(savefile: TStream); override;
      procedure upload(db: TDataSet); override;

      property style: TCommandStyle read FStyle write FStyle;
      property value: smallint read FValue write FValue;
   end;

   TStatBlock = class(TObject)
   private
      FRefcount: word;
      FSize: word;
      FBlock: TIntArray;

      procedure setSize(const value: word);
   public
      constructor Create(size: word);
      destructor Destroy; override;
      function compare(other: TStatBlock): boolean;

      property size: word read FSize write setSize;
      property count: word read FRefCount;
      property block: TIntArray read FBlock write FBlock;
   end;

   TStatSet = class(TRpgDatafile)
   private
      FBlocks: array of TStatBlock;
      function getSize: integer;
      function inArray(index: TStatBlock): boolean;
   public
      constructor Load(savefile: TStream);
      destructor Destroy; override;
      procedure save(savefile: TStream); override;

      function add(var newBlock: TStatBlock): integer;
      function indexOf(value: TStatBlock): smallint;
      procedure incCount(index: TStatBlock);
      procedure decCount(index: TStatBlock);

      property size: integer read getSize;
   end;

   TClassTemplate = class(TRpgDatafile)
   private
      FMapSprite: integer;
      FBattleSprite: integer;
      FPortrait: integer;
      FCommandSet: packed array[1..COMMAND_COUNT] of smallint;
      FCommands: byte;
      FStatBlocks: array[1..STAT_COUNT] of TPosNegPointer;
      FExpCalc: string;
      FExpVars: array[1..4] of integer;
      FSkillset: TSkillsetList;
      FResists: TPointArray;
      FConditions: TPointArray;
      FInitialEq: packed array[1..TOTAL_SLOTS] of smallint;
      FDualWield: TWeaponStyle;
      FStaticEq: boolean;
      FStrongDefense: boolean;
      FUnarmedAnim: integer;

      function getCommand(x: byte): smallint; inline;
      procedure setCommand(x: byte; const Value: smallint); inline;
      function getStatBlock(x: byte): TPosNegPointer; inline;
      procedure setStatBlock(x: byte; const Value: TPosNegPointer); inline;
      function getExpVar(x: byte): integer;
      procedure setExpVar(x: byte; const Value: integer);
      function getEq(x: byte): smallint;
      procedure setEq(x: byte; const Value: smallint);
   protected
      function getDatasetName: string; override;
   public
      constructor Load(savefile: TStream);
      constructor Create;
      destructor Destroy; override;
      procedure save(savefile: TStream); override;
      procedure upload(db: TDataSet); override;
      procedure download(db: TDataset); override;

      procedure addResist(const value: TPoint); inline;
      procedure addCondition(const value: TPoint); inline;

      property clsName: string read FName write FName;
      property mapSprite: integer read FMapSprite write FMapSprite;
      property battleSprite: integer read FBattleSprite write FBattleSprite;
      property portrait: integer read FPortrait write FPortrait;
      property command[x: byte]: smallint read getCommand write setCommand;
      property commands: byte read FCommands write FCommands;
      property statblock[x: byte]: TPosNegPointer read getStatBlock write setStatBlock;
      property expFunc: string read FExpCalc write FExpCalc;
      property expVars[x: byte]: integer read getExpVar write setExpVar;
      property skillset: TSkillsetList read FSkillset write FSkillset;
      property resist: TPointArray read FResists write FResists;
      property condition: TPointArray read FConditions write FConditions;
      property eq[x: byte]: smallint read getEq write setEq;
      property dualWield: TWeaponStyle read FDualWield write FDualWield;
      property staticEq: boolean read FStaticEq write FStaticEq;
      property strongDef: boolean read FStrongDefense write FStrongDefense;
      property unarmedAnim: integer read FUnarmedAnim write FUnarmedAnim;
   end;

   THeroTemplate = class(TClassTemplate)
   private
      FCharName: string;
      FTitle: string;
      FClass: integer;
      FPortraitShift: TColorShift;
      FSpriteShift: TColorShift;
      FBattleSpriteShift: TColorShift;
      FMinLevel: word;
      FMaxLevel: word;
      FGuest: boolean;
   public
      constructor Load(savefile: TStream);
      procedure save(savefile: TStream); override;

      property charName: string read FCharName write FCharName;
      property title: string read FTitle write FTitle;
      property charClass: integer read FClass write FClass;
      property portraitShift: TColorShift read FPortraitShift write FPortraitShift;
      property spriteShift: TColorShift read FSpriteShift write FSpriteShift;
      property battleSpriteShift: TColorShift read FBattleSpriteShift write FBattleSpriteShift;
      property minLevel: word read FMinLevel write FMinLevel;
      property maxLevel: word read FMaxLevel write FMaxLevel;
      property guest: boolean read FGuest write FGuest;
   end;

var
   GStatSet: TStatSet;

implementation

uses
   sysUtils,
   turbu_database, design_script_engine;

type
   EStatBlockError = class(Exception);

resourcestring
   NOT_IN_BLOCK = 'Stat block not found in stat set!';

{ TStatBlock }

constructor TStatBlock.Create(size: word);
begin
   setLength(FBlock, size);
   FSize := size;
   FRefcount := 1;
end;

destructor TStatBlock.Destroy;
begin
   assert(FRefcount = 0);
   Finalize(FBlock);
end;

procedure TStatBlock.setSize(const value: word);
begin
   FSize := Value;
   setLength(FBlock, value);
end;

function TStatBlock.compare(other: TStatBlock): boolean;
var
   i: integer;
begin
   if FSize <> other.FSize then
   begin
      result := false;
      Exit;
   end;

   i := 0;
   while (i < FSize) and (FBlock[i] = other.FBlock[i]) do
      inc(i);
   result := i = FSize;
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
      newblock := TStatBlock.Create(saveFile.readWord);
      newblock.FRefcount := 0;
      savefile.ReadBuffer(newblock.FBlock[0], newblock.FSize * 4);
      lassert(self.add(newblock) = i);
   end;
   lassert(savefile.readChar = 'S');
end;

destructor TStatSet.Destroy;
var
   i: integer;
begin
   for I := 0 to high(FBlocks) do
      FBlocks[i].Free;
   finalize(FBlocks);
   inherited Destroy;
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
      savefile.writeWord(FBlocks[i].FSize);
      savefile.WriteBuffer(FBlocks[i].FBlock[0], FBlocks[i].FSize * 4);
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
      dec(newBlock.FRefcount);
      newBlock.Free;
      newBlock := FBlocks[i];
      inc(FBlocks[i].FRefcount);
   end;
   result := i;
end;

function TStatSet.indexOf(value: TStatBlock): smallint;
begin
   result := high(FBlocks);
   while (result >= 0) and (self.FBlocks[result] <> value) do
      dec(result);
end;

function TStatSet.getSize: integer;
begin
   result := length(FBlocks);
end;

function TStatSet.inArray(index: TStatBlock): boolean;
var
   i: integer;
begin
   i := 0;
   while (i <= high(FBlocks)) and (FBlocks[i] <> index) do
      inc(i);
   result := i <= high(FBlocks);
end;

procedure TStatSet.incCount(index: TStatBlock);
begin
   if not inArray(index) then
      raise EStatBlockError.Create(NOT_IN_BLOCK);
   inc(index.FRefcount);
end;

procedure TStatSet.decCount(index: TStatBlock);
begin
   if not inArray(index) then
      raise EStatBlockError.Create(NOT_IN_BLOCK);
   assert(index.FRefcount > 0);
   dec(index.FRefcount);
end;

{ TBattleCommand }

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

procedure TBattleCommand.upload(db: TDataSet);
begin
   inherited upload(db);
   db.FieldByName('style').AsInteger := ord(FStyle);
   db.FieldByName('value').AsInteger := FValue;
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

   FMapSprite := savefile.readInt;
   FBattleSprite := savefile.readInt;
   FPortrait := savefile.readInt;
   lassert(savefile.readWord = COMMAND_COUNT);
   savefile.readBuffer(FCommandSet[1], sizeof(FCommandSet));
   FCommands := savefile.readByte;
   lassert(savefile.readByte = STAT_COUNT);
   for I := 1 to STAT_COUNT do
   begin
      savefile.ReadBuffer(FStatBlocks[i].fullvalue, sizeof(int64));
      if FStatBlocks[i].index = 0 then
      begin
         FStatBlocks[i].address := GStatSet.FBlocks[integer(FStatBlocks[i].val16[3])];
         inc(FStatBlocks[i].address.FRefcount);
      end;
   end;
   lassert(savefile.readChar = 'p');
   FExpCalc := savefile.readString;
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
   savefile.readBuffer(FInitialEQ[1], sizeof(smallint) * TOTAL_SLOTS);
   savefile.readBuffer(FDualWield, sizeof(TWeaponStyle));
   FStaticEq := savefile.readBool;
   FStrongDefense := savefile.readBool;
   FUnarmedAnim := savefile.readInt;
   lassert(savefile.readChar = 'C');
end;

procedure TClassTemplate.save(savefile: TStream);
var
   i: integer;
begin
   inherited save(savefile);
   savefile.writeInt(FMapSprite);
   savefile.writeInt(FBattleSprite);
   savefile.writeInt(FPortrait);
   savefile.writeWord(COMMAND_COUNT);
   savefile.WriteBuffer(FCommandSet[1], sizeof(FCommandSet));
   savefile.writeByte(FCommands);
   savefile.writeByte(STAT_COUNT);
   for I := 1 to STAT_COUNT do
   begin
      if FStatBlocks[i].index < 0 then
         savefile.WriteBuffer(FStatBlocks[i].fullvalue, sizeof(int64))
      else begin
         savefile.writeInt(0);
         savefile.writeWord(0);
         savefile.writeWord(GStatSet.indexOf(FStatBlocks[i].address));
      end;
   end;
   savefile.writeChar('p');
   savefile.writeString(FExpCalc);
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
   savefile.WriteBuffer(FInitialEQ[1], sizeof(smallint) * TOTAL_SLOTS);
   savefile.WriteBuffer(FDualWield, sizeof(TWeaponStyle));
   savefile.writeBool(FStaticEq);
   savefile.writeBool(FStrongDefense);
   savefile.writeInt(FUnarmedAnim);
   savefile.writechar('C');
end;

destructor TClassTemplate.Destroy;
var
   i: integer;
begin
   for i := 1 to STAT_COUNT do
      if (FStatBlocks[i].index >= 0) and (FStatBlocks[i].fullvalue <> 0) then
         GStatSet.decCount(FStatBlocks[i].address);
   FSkillset.Free;
   inherited Destroy;
end;

procedure TClassTemplate.download(db: TDataset);
begin
   inherited;
end;

procedure TClassTemplate.upload(db: TDataSet);
var
   i: integer;
   tempDB: TDataSet;
begin
   inherited upload(db);
   db.FieldByName('mapSprite').AsInteger := FMapSprite;
   db.FieldByName('battleSprite').AsInteger := FBattleSprite;
   db.FieldByName('portrait').AsInteger := FPortrait;
   for i := 1 to high(FCommandSet) do
      (db.FieldByName('command') as TArrayField)[i - 1] := FCommandSet[i];
   for i := 1 to high(FStatBlocks) do
      (db.FieldByName('statblock') as TArrayField).fields[i - 1].Value := FStatBlocks[i].fullvalue;
   db.FieldByName('expFunc').AsString := FExpCalc;
   for i := 1 to 4 do
      (db.FieldByName('expVars') as TArrayField)[i - 1] := FExpVars[i];
   if FSkillSet.length > 0 then
   begin
      tempDB := db.Owner.FindComponent(db.Name + FSkillSet[0].datasetName) as TDataSet;
      for I := 1 to FSkillset.high do
      begin
         FSkillset[i].upload(tempDB);
         tempDB.FieldByName('master').AsInteger := self.id;
         tempDB.Post;
      end;
   end;
   if length(FResists) > 0 then
   begin
      tempDB := db.Owner.FindComponent(db.Name + '_Resist') as TDataset;
      for i := 0 to high(FResists) do
      begin
         tempDB.Append;
         tempDB.FieldByName('master').AsInteger := self.id;
         tempDB.FieldByName('x').AsInteger := FResists[i].X;
         tempDB.FieldByName('y').AsInteger := FResists[i].Y;
         tempDB.Post;
      end;
   end;
   if length(FConditions) > 0 then
   begin
      tempDB := db.Owner.FindComponent(db.Name + '_Condition') as TDataset;
      for i := 0 to high(FConditions) do
      begin
         tempDB.Append;
         tempDB.FieldByName('master').AsInteger := self.id;
         tempDB.FieldByName('x').AsInteger := FConditions[i].X;
         tempDB.FieldByName('y').AsInteger := FConditions[i].Y;
         tempDB.Post;
      end;
   end;
   for i := 1 to 5 do
      (db.FieldByName('equip') as TArrayField)[i - 1] := FInitialEq[i];
   db.FieldByName('dualWield').AsInteger := ord(FDualWield);
   db.FieldByName('staticEq').AsBoolean := FStaticEq;
   db.FieldByName('strongDef').AsBoolean := FStrongDefense;
   db.FieldByName('unarmedAnim').AsInteger := FUnarmedAnim;
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
   result := FCommandSet[x];
end;

function TClassTemplate.getDatasetName: string;
begin
   result := 'charClasses';
end;

function TClassTemplate.getEq(x: byte): smallint;
begin
   result := FInitialEq[x];
end;

function TClassTemplate.getExpVar(x: byte): integer;
begin
   result := FExpVars[x];
end;

function TClassTemplate.getStatBlock(x: byte): TPosNegPointer;
begin
   result := FStatBlocks[x];
end;

procedure TClassTemplate.setCommand(x: byte; const Value: smallint);
begin
   FCommandSet[x] := value;
end;

procedure TClassTemplate.setEq(x: byte; const Value: smallint);
begin
   FInitialEq[x] := value;
end;

procedure TClassTemplate.setExpVar(x: byte; const Value: integer);
begin
   FExpVars[x] := value;
end;

procedure TClassTemplate.setStatBlock(x: byte; const Value: TPosNegPointer);
begin
   FStatBlocks[x] := value;
end;

{ THeroTemplate }

constructor THeroTemplate.Load(savefile: TStream);
begin
   inherited Load(savefile);
   FCharName := savefile.readString;
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
   savefile.writeString(FCharName);
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

{ TExpCalcRecord }

constructor TExpCalcRecord.Create(name, designName: string);
begin
   inherited Create(name, designName, TRpgMethod(getExpFunc(name)));
end;

class function TExpCalcRecord.getExpFunc(name: string): TExpCalcFunc;
begin
   result := TExpCalcFunc(GDScriptEngine.exec.GetProcAsMethodN(rawByteString(name)));
end;

function TExpCalcRecord.getMethod: TExpCalcFunc;
begin
   result := TExpCalcFunc(FMethod);
end;

procedure TExpCalcRecord.setName(const Value: string);
begin
   inherited setName(value);
   FMethod := TRpgMethod(getExpFunc(value));
end;

initialization
begin
   GStatSet := TStatSet.Create;
end;

finalization
begin
   GStatSet.Free;
end;

end.
