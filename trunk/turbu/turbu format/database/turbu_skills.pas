unit turbu_skills;
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
   types, classes, DB,
   turbu_constants, turbu_defs, turbu_classes, turbu_sounds, turbu_containers;

type
   TSkillGainDisplayFunc = function(one, two, three, four: integer): string of object;
   TSkillGainDisplayArrayFunc = function(args: T4IntArray): string of object;

   TSkillFuncStyle = (sf_bool, sf_percent, sf_level, sf_both);
   TSkillRange = (sr_self, sr_target, sr_area);

   TSkillMessages = record
      useString: string;
      useString2: string;
      failureMessage: word;
   end;

   PSkillMessages = ^TSkillMessages;

   TSkillGainRecord = class(TScriptRecord)
   private
      FStyle: TSkillFuncStyle;
      FArray: boolean;
      FDisplayMethod: TSkillGainDisplayFunc;
   protected
      procedure setName(const Value: string); override;
   public
      constructor Create(name, designName: string; style: TSkillFuncStyle; arrayArgs: boolean);
      procedure upload(db: TDataSet); override;

      property style: TSkillFuncStyle read FStyle write FStyle;
      property arrayArgs: boolean read FArray write FArray;
      property displayMethod: TSkillGainDisplayFunc read FDisplayMethod;
   end;

   TSkillGainInfo = class(TRpgDatafile)
   private
      FStyle: TSkillFuncStyle;
      FPointer: TSkillGainRecord;
      FSkill: integer;
      FNums: array[1..4] of integer;
      function getNum(x: byte): integer; inline;
      procedure setNum(x: byte; const Value: integer); inline;
   protected
      function getDatasetName: string; override;
      class function keyChar: ansiChar; override;
   public
      constructor Load(savefile: TStream);
      procedure save(savefile: TStream); override;
      procedure upload(db: TDataSet); override;

      property style: TSkillFuncStyle read FStyle write FStyle;
      property method: TSkillGainRecord read  FPointer write FPointer;
      property skill: integer read FSkill write FSkill;
      property num[x: byte]: integer read getNum write setNum;
   end;

   TSkillsetList = TRpgObjectList<TSkillGainInfo>;

   TSkillTemplate = class abstract(TRpgDatafile)
   private
      FCost: integer;
      FCostPercent: boolean;
      FDesc: string;
      FSkillMessages: PSkillMessages;
      FUsableWhere: TUsableWhere;
      FTag: T4IntArray;
   protected
      class function keyChar: ansiChar; override;
   public
      constructor Load(savefile: TStream); virtual;
      procedure save(savefile: TStream); override;
      procedure upload(db: TDataSet); override;
      destructor Destroy; override;

      procedure createMessages;

      property cost: integer read FCost write FCost;
      property desc: string read FDesc write FDesc;
      property costAsPercentage: boolean read FCostPercent write FCostPercent;
      property messages: PSkillMessages read FSkillMessages write FSkillMessages;
      property usableWhere: TUsableWhere read FUsableWhere write FUsableWhere;
      property tag: T4IntArray read FTag write FTag;
   end;

   TNormalSkillTemplate = class(TSkillTemplate)
   private
      FRange: TSkillRange;
      FOffensive: boolean;
      FAnim: word;
      FSkillPower: T4IntArray;
      FSuccessRate: integer;
      FStat: array[1..STAT_COUNT] of boolean;
      FVampire: boolean;
      FPhased: boolean;
      FCondition: TByteSet;
      FAttribute: TPointArray;
      FResistMod: boolean;

      function getStat(x: byte): boolean;
      procedure setStat(x: byte; const Value: boolean);
      procedure setSkillPower(x: byte; const Value: integer);
      function getSkillPower(x: byte): integer;
   public
      constructor Load(savefile: TStream); override;
      procedure save(savefile: TStream); override;

      property range: TSkillRange read FRange write FRange;
      property offensive: boolean read FOffensive write FOffensive;
      property animation: word read FAnim write FAnim;
      property skillPower[x: byte]: integer read getSkillPower write setSkillPower;
      property successRate: integer read FSuccessRate write FSuccessRate;
      property stat[x: byte]: boolean read getStat write setStat;
      property drain: boolean read FVampire write FVampire;
      property phased: boolean read FPhased write FPhased;
      property condition: TByteSet read FCondition write FCondition;
      property attribute: TPointArray read FAttribute write FAttribute;
      property resistMod: boolean read FResistMod write FResistMod;
   end;

   TSpecialSkillTemplate = class(TSkillTemplate)
   private
      FSfx: TSoundTemplate;
   public
      constructor Load(savefile: TStream); override;
      procedure save(savefile: TStream); override;
      destructor Destroy; override;
      
      property sfx: TSoundTemplate read FSfx write FSfx;
   end;

   TTeleportSkillTemplate = class(TSpecialSkillTemplate)
   private
      FTarget: byte;   
   public
      constructor Load(savefile: TStream); override;
      procedure save(savefile: TStream); override;

      property teleportTarget: byte read FTarget write FTarget;
   end;

   TVariableSkillTemplate = class(TSpecialSkillTemplate)
   private
      FWhich: word;
      FMagnitude: smallint;
      FStyle: TVarSets;
      FOperation: TVarOps;
   public
      constructor Load(savefile: TStream); override;
      procedure save(savefile: TStream); override;

      property which: word read FWhich write FWhich;
      property magnitude: smallint read FMagnitude write FMagnitude;
      property style: TVarSets read FStyle write FStyle;
      property operation: TVarOps read FOperation write FOperation;
   end;

   TScriptSkillTemplate = class(TSkillTemplate)
   private
      FEvent: TScriptEvent;
   public
      constructor Load(savefile: TStream); override;
      procedure save(savefile: TStream); override;

      property event: TScriptEvent read FEvent write FEvent;
   end;

procedure SetDatabase(value: TRpgDatafile);

implementation
uses
   sysutils,
   turbu_database {$IFDEF EDITOR}, design_script_engine{$ENDIF};

var
   GDatabase: TRpgDatabase;

procedure SetDatabase(value: TRpgDatafile);
begin
   GDatabase := value as TRpgDatabase;
end;

{ TSkillTemplate }

procedure TSkillTemplate.createMessages;
begin
   assert(not assigned(FSkillMessages));
   new(FSkillMessages);
end;

destructor TSkillTemplate.Destroy;
begin
   if assigned(FSkillMessages) then
      dispose(FSkillMessages);
   inherited;
end;

class function TSkillTemplate.keyChar: ansiChar;
begin
   result := 's';
end;

constructor TSkillTemplate.Load(savefile: TStream);
begin
   inherited Load(savefile);
   FCost := savefile.readInt;
   FCostPercent := savefile.readBool;
   FDesc := savefile.readString;
   if savefile.readBool then
   begin
      self.createMessages;
      FSkillMessages.useString := savefile.readString;
      FSkillMessages.useString2 := savefile.readString;
      FSkillMessages.failureMessage := savefile.readWord;
   end;
   savefile.readBuffer(FUsableWhere, sizeof(TUsableWhere));
   savefile.readBuffer(FTag[1], sizeof(T4IntArray));
   lassert(savefile.readChar = 'S');
end;

procedure TSkillTemplate.save(savefile: TStream);
begin
   inherited save(savefile);
   savefile.writeInt(FCost);
   savefile.writeBool(FCostPercent);
   savefile.writeString(FDesc);
   savefile.writeBool(assigned(FSkillMessages));
   if assigned(FSkillMessages) then
   begin
      savefile.writeString(FSkillMessages.useString);
      savefile.writeString(FSkillMessages.useString2);
      savefile.writeWord(FSkillMessages.failureMessage);
   end;
   savefile.WriteBuffer(FUsableWhere, sizeof(TUsableWhere));
   savefile.WriteBuffer(FTag[1], sizeof(T4IntArray));
   savefile.writeChar('S');
end;

procedure TSkillTemplate.upload(db: TDataSet);
var
   i: integer;
begin
   inherited upload(db);
   db.FieldByName('cost').AsInteger := FCost;
   db.FieldByName('costPercent').AsBoolean := FCostPercent;
   db.FieldByName('desc').asString := FDesc;
   if assigned(FSkillMessages) then
   begin
      db.FieldByName('messages.useString').AsString := Self.messages.useString;
      db.FieldByName('messages.useString2').AsString := Self.messages.useString2;
      db.FieldByName('messages.failureMessage').AsInteger := Self.messages.failureMessage;
   end
   else db.FieldByName('messages').clear;
   db.FieldByName('usableWhere').AsInteger := ord(FUsableWhere);
   for i := 1 to 4 do
      (db.FieldByName('tag') as TArrayField)[i - 1] := FTag[i];
end;

{ TNormalSkillTemplate }

constructor TNormalSkillTemplate.Load(savefile: TStream);
var
   dummy: byte;
begin
   inherited Load(savefile);
   savefile.readBuffer(FRange, sizeof(TSkillRange));
   FOffensive := savefile.readBool();
   FAnim := savefile.readWord();
   savefile.readBuffer(FSkillPower[1], sizeof(T4IntArray));
   FSuccessRate := savefile.readInt();
   lassert(savefile.readWord = STAT_COUNT);
   savefile.readBuffer(FStat[1], sizeof(FStat));
   FVampire := savefile.readBool();
   FPhased := savefile.readBool();
   dummy := savefile.readInt;
   if dummy > 0 then
      savefile.readBuffer(self.FCondition, dummy);
   setLength(FAttribute, savefile.readInt);
   if length(FAttribute) > 0 then
      savefile.readBuffer(FAttribute[0], length(FAttribute) * sizeof(TPoint));
   FResistMod := savefile.readBool();
   lassert(savefile.readChar = 'n');
end;

procedure TNormalSkillTemplate.save(savefile: TStream);
var
   dummy: byte;
begin
   savefile.writeChar('N');
   inherited save(savefile);
   savefile.WriteBuffer(FRange, sizeof(TSkillRange));
   savefile.writeBool(FOffensive);
   savefile.writeWord(FAnim);
   savefile.WriteBuffer(FSkillPower[1], sizeof(T4IntArray));
   savefile.writeInt(FSuccessRate);
   savefile.writeWord(STAT_COUNT);
   savefile.WriteBuffer(FStat[1], sizeof(FStat));
   savefile.writeBool(FVampire);
   savefile.writeBool(FPhased);
   dummy := getSetLength(FCondition);
   savefile.writeInt(dummy);
   if dummy > 0 then
      savefile.WriteBuffer(self.FCondition, dummy);
   savefile.writeInt(length(FAttribute));
   if length(FAttribute) > 0 then
      savefile.WriteBuffer(FAttribute[0], length(FAttribute) * sizeof(TPoint));
   savefile.writeBool(FResistMod);
   savefile.writeChar('n');
end;

function TNormalSkillTemplate.getStat(x: byte): boolean;
begin
   assert(x in [1..STAT_COUNT]);
   result := FStat[x];
end;

function TNormalSkillTemplate.getSkillPower(x: byte): integer;
begin
   assert(x in [1..4]);
   result := FSkillPower[x];
end;

procedure TNormalSkillTemplate.setSkillPower(x: byte; const Value: integer);
begin
   assert(x in [1..4]);
   FSkillPower[x] := value;
end;

procedure TNormalSkillTemplate.setStat(x: byte; const Value: boolean);
begin
   assert(x in [1..STAT_COUNT]);
   FStat[x] := value;
end;

{ TSpecialSkillTemplate }

destructor TSpecialSkillTemplate.Destroy;
begin
   FSfx.Free;
   inherited Destroy;
end;

constructor TSpecialSkillTemplate.Load(savefile: TStream);
begin
   inherited load(savefile);
   if savefile.readBool then
   begin
      FSfx := TRpgSound.Load(savefile);
      lassert(savefile.readChar = 's');
   end;
   lassert(savefile.readChar = 'P');
end;

procedure TSpecialSkillTemplate.save(savefile: TStream);
begin
   inherited save(savefile);
   savefile.writeBool(assigned(FSfx));
   if Assigned(FSfx) then
   begin
      FSfx.save(savefile);
      savefile.writeChar('s');
   end;
   savefile.writeChar('P');
end;

{ TSkillGainInfo }

constructor TSkillGainInfo.Load(savefile: TStream);
begin
   inherited Load(savefile);
   lassert((FId = 0) and (FName = ''));
   savefile.ReadBuffer(FStyle, sizeof(FStyle));
   FPointer := GDatabase.skillFunc[savefile.readInt];
   FSkill := savefile.readInt;
   savefile.ReadBuffer(FNums[1], 16);
   lassert(savefile.readChar = 'G');
end;

procedure TSkillGainInfo.save(savefile: TStream);
begin
   inherited save(savefile);
   savefile.WriteBuffer(FStyle, sizeof(FStyle));
   savefile.writeInt(GDatabase.skillFunc.indexOf(FPointer));
   savefile.writeInt(FSkill);
   savefile.WriteBuffer(FNums[1], 16);
   savefile.writeChar('G');
end;

procedure TSkillGainInfo.setNum(x: byte; const Value: integer);
begin
   FNums[x] := Value;
end;

procedure TSkillGainInfo.upload(db: TDataSet);
var
   i: integer;
begin
   //do not call inherited
   db.Append;
   db.FieldByName('style').AsInteger := ord(FStyle);
   db.FieldByName('method.methodName').AsString := FPointer.name;
   db.FieldByName('method.arrayArgs').AsBoolean := FPointer.FArray;
   db.FieldByName('method.methodStyle').AsInteger := ord(FPointer.FStyle);
   db.FieldByName('method.address').asPSMethod := TMethod(FPointer.FMethod);
   db.FieldByName('method.displayAddress').asPSMethod := TMethod(FPointer.displayMethod);
   db.FieldByName('skill').AsInteger := self.FSkill;
   for I := 1 to 4 do
      (db.FieldByName('num') as TArrayField)[i - 1] := FNums[i];
   db.FieldByName('modified').AsBoolean := false;
end;

function TSkillGainInfo.getDatasetName: string;
begin
   result := '_skillset';
end;

function TSkillGainInfo.getNum(x: byte): integer;
begin
   result := FNums[x];
end;

class function TSkillGainInfo.keyChar: ansiChar;
begin
   result := 'g';
end;

{ TSkillGainFunction }

constructor TSkillGainRecord.Create(name, designName: string; style: TSkillFuncStyle; arrayArgs: boolean);
begin
   setName(name);
   inherited Create(name, designName, TRpgMethod(FMethod));
   FStyle := style;
   FArray := arrayArgs;
end;

procedure TSkillGainRecord.setName(const Value: string);
begin
   inherited setName(Value);
   {$IFDEF EDITOR}
   FMethod := TRpgMethod(GDScriptEngine.exec.GetProcAsMethodN(ansiString(Value)));
   FDisplayMethod := TSkillGainDisplayFunc(GDScriptEngine.exec.GetProcAsMethodN(ansiString(Value) + '_display'))
   {$ENDIF}
end;

procedure TSkillGainRecord.upload(db: TDataSet);
begin
   inherited upload(db);
   db.FieldByName('style').AsInteger := Ord(style);
   db.FieldByName('arrayArgs').AsBoolean := arrayArgs;
   db.FieldByName('displayMethod').asPSMethod := TMethod(displayMethod);
   db.FieldByName('displayName').AsString := name + '_display';
end;

{ TTeleportSkillTemplate }

constructor TTeleportSkillTemplate.Load(savefile: TStream);
begin
   inherited Load(savefile);
   FTarget := savefile.readByte();
   lassert(savefile.readChar = 't');
end;

procedure TTeleportSkillTemplate.save(savefile: TStream);
begin
   savefile.writeChar('T');
   inherited save(savefile);
   savefile.writeByte(FTarget);
   savefile.writeChar('t');
end;

{ TVariableSkillTemplate }

constructor TVariableSkillTemplate.Load(savefile: TStream);
begin
   inherited Load(savefile);
   FWhich := savefile.readWord;
   FMagnitude := savefile.readWord;
   savefile.readBuffer(FStyle, sizeof(TVarSets));
   savefile.readBuffer(FOperation, sizeof(TVarOps));
   lassert(savefile.readChar = 'v');
end;

procedure TVariableSkillTemplate.save(savefile: TStream);
begin
   savefile.writeChar('V');
   inherited save(savefile);
   savefile.writeWord(FWhich);
   savefile.writeWord(FMagnitude);
   savefile.WriteBuffer(FStyle, sizeof(TVarSets));
   savefile.WriteBuffer(FOperation, sizeof(TVarOps));
   savefile.writeChar('v');
end;

{ TScriptSkillTemplate }

constructor TScriptSkillTemplate.Load(savefile: TStream);
begin
   assert(false, 'Can''t load this!');
end;

procedure TScriptSkillTemplate.save(savefile: TStream);
begin
   assert(false, 'Can''t save this!');
end;

end.
