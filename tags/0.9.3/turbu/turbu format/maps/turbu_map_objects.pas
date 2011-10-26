unit turbu_map_objects;

interface
uses
   Classes, DB, RTTI,
   turbu_defs, turbu_classes, turbu_containers, charset_data, turbu_pathing,
   turbu_serialization, turbu_map_interface,
   SG_defs;

type
   UploadConditionsAttribute = class(TDBUploadAttribute)
   protected
      procedure upload(db: TDataset; field: TRttiField; instance: TObject); override;
      procedure download(db: TDataset; field: TRttiField; instance: TObject); override;
   end;

   UploadTimerAttribute = class(TDBUploadAttribute)
   private
      FIndex: integer;
   protected
      procedure upload(db: TDataset; field: TRttiField; instance: TObject); override;
      procedure download(db: TDataset; field: TRttiField; instance: TObject); override;
   public
      constructor Create(index: integer);
   end;

   UploadBattleConditionsAttribute = class(TDBUploadAttribute)
   protected
      procedure upload(db: TDataset; field: TRttiField; instance: TObject); override;
      procedure download(db: TDataset; field: TRttiField; instance: TObject); override;
   end;

type
   TMoveType = (mt_still, mt_randomMove, mt_cycleUD, mt_cycleLR, mt_chaseHero, mt_fleeHero, mt_byRoute);
   TStartCondition = (by_key, by_touch, by_collision, automatic, parallel, on_call);
   TAnimType = (at_sentry, at_jogger, at_fixedDir, at_fixedJog, at_statue, at_spinRight);
   TPageConditions = (pc_switch1, pc_switch2, pc_var1, pc_var2, pc_item, pc_hero, pc_timer1, pc_timer2);
   TBattlePageConditions = (bp_turns, bp_monsterTime, bp_heroTime, bp_exhaustion, bp_monsterHP, bp_HeroHP, bp_commandUsed);

   TPageConditionSet = set of TPageConditions;
   TBattlePageConditionSet = set of TBattlePageConditions;

   TRpgMapObject = class;

   TRpgEventConditions = class(TObject)
   protected
      [UploadConditions]
      FConditions: TPageConditionSet;
      FSwitch1: integer;
      FSwitch2: integer;
      FVariable1: integer;
      FVariable2: integer;
      FVar1Op: TComparisonOp;
      FVar2Op: TComparisonOp;
      FVarValue1: integer;
      FVarValue2: integer;
      FItem: integer;
      FHero: integer;
      [UploadTimer(1)]
      FClock1: integer;
      [UploadTimer(2)]
      FClock2: integer;
      FClock1Op: TComparisonOp;
      FClock2Op: TComparisonOp;
      FScript: string;
//      function evaluate: boolean;
   public
      constructor Load(savefile: TStream);
      procedure Save(savefile: TStream);

//      property valid: boolean read evaluate;
      property conditions: TPageConditionSet read FConditions write FConditions;
      property switch1Set: integer read FSwitch1 write FSwitch1;
      property switch2Set: integer read FSwitch2 write FSwitch2;
      property variable1Set: integer read FVariable1 write FVariable1;
      property variable1Value: integer read FVarValue1 write FVarValue1;
      property variable1Op: TComparisonOp read FVar1Op write FVar1Op;
      property variable2Set: integer read FVariable2 write FVariable2;
      property variable2Value: integer read FVarValue2 write FVarValue2;
      property variable2Op: TComparisonOp read FVar2Op write FVar2Op;
      property itemNeeded: integer read FItem write FItem;
      property heroNeeded: integer read FHero write FHero;
      property timeRemaining: integer read FClock1 write FClock1;
      property timeRemaining2: integer read FClock2 write FClock2;
      property timer1Op: TComparisonOp read FClock1Op write FClock1Op;
      property timer2Op: TComparisonOp read FClock2Op write FClock2Op;
      property script: string read FScript write FScript;
   end;

   TBattleEventConditions = class(TRpgEventConditions)
   protected
      [UploadBattleConditions]
      FBattleConditions: TBattlePageConditionSet;
      FMonsterHP: integer;
      FTurnsMultiple: integer;
      FMonsterHPMax: integer;
      FHeroCommandWhich: integer;
      FHeroHP: integer;
      FHeroHPMax: integer;
      FExhaustionMax: integer;
      FMonsterTurnsMultiple: integer;
      FHeroTurnsMultiple: integer;
      FMonsterHPMin: integer;
      FHeroHPMin: integer;
      FExhaustionMin: integer;
      FTurnsConst: integer;
      FMonsterTurnsConst: integer;
      FHeroTurnsConst: integer;
      FMonsterTurn: integer;
      FHeroCommandWho: integer;
      FHeroTurn: integer;
   public
      property battleConditions: TBattlePageConditionSet read FBattleConditions;
      property turnsConst: integer read FTurnsConst;
      property turnsMultiple: integer read FTurnsMultiple;
      property monsterTurn: integer read FMonsterTurn;
      property monsterTurnsConst: integer read FMonsterTurnsConst;
      property monsterTurnsMultiple: integer read FMonsterTurnsMultiple;
      property heroTurn: integer read FHeroTurn;
      property heroTurnsConst: integer read FHeroTurnsConst;
      property heroTurnsMultiple: integer read FHeroTurnsMultiple;
      property exhaustionMin: integer read FExhaustionMin;
      property exhaustionMax: integer read FExhaustionMax;
      property monsterHP: integer read FMonsterHP;
      property monsterHPMin: integer read FMonsterHPMin;
      property monsterHPMax: integer read FMonsterHPMax;
      property heroHP: integer read FHeroHP;
      property heroHPMin: integer read FHeroHPMin;
      property heroHPMax: integer read FHeroHPMax;
      property heroCommandWho: integer read FHeroCommandWho;
      property heroCommandWhich: integer read FHeroCommandWhich;
   end;

   TRpgEventPage = class(TRpgDatafile)
   private
      FFrame: word;
      FTransparent: boolean;
      FDirection: TFacing;
      FMoveType: TMoveType;
      FMoveFrequency: byte;
      FStartCondition: TStartCondition;
      FEventHeight: byte;
      FNoOverlap: boolean;
      FAnimType: TAnimType;
      FMoveSpeed: byte;
      [NoUpload]
      FPath: TPath;
      FMoveIgnore: boolean;
      FMatrix: word;
{      FEventScript: ansiString;
      FScriptText: ansiString;
      FCompiledScript: ansiString; }
      function isValid: boolean; inline;

{      function getEventScript: ansiString;
      function getCompiledScript: ansiString;}
      function hasScriptFunction: boolean; inline;

{      function getOpcode(x: word): TEventCommand;
      function getLength: word; }
      function GetTileGroup: integer;
   protected
      [NoUpload]
      FConditions: TRpgEventConditions;
      [NoUpload]
      FParent: TRpgMapObject;
      [NoUpload]
      FEventText: string;
      class function keyChar: AnsiChar; override;
   public
      constructor Create(parent: TRpgMapObject; id: smallint); reintroduce;
      constructor Load(savefile: TStream); overload; override;
      constructor Load(savefile: TStream; parent: TRpgMapObject); reintroduce; overload;
      destructor Destroy; override;
      procedure save(savefile: TStream); override;

      function isTile: boolean; inline;

      property conditionBlock: TRpgEventConditions read FConditions;
      property whichTile: word read FFrame write FFrame;
      property direction: TFacing read FDirection write FDirection;
      property transparent: boolean read FTransparent write FTransparent;
      property path: TPath read FPath write FPath;
      property moveIgnore: boolean read FMoveIgnore write FMoveIgnore;
      property moveType: TMoveType read FMoveType write FMoveType;
      property moveFrequency: byte read FMoveFrequency write FMoveFrequency;
      property startCondition: TStartCondition read FStartCondition write FStartCondition;
      property zOrder: byte read FEventHeight write FEventHeight;
      property isBarrier: boolean read FNoOverlap write FNoOverlap;
      property animType: TAnimType read FAnimType write FAnimType;
      property moveSpeed: byte read FMoveSpeed write FMoveSpeed;
      property scriptName: string read FEventText write FeventText;
      property actionMatrix: word read FMatrix;
//      property eventScript: ansiString read getEventScript;
      property parent: TRpgMapObject read FParent;
      property hasScript: boolean read hasScriptFunction;
{      property compiledScript: tbtString read getCompiledScript;
      property parseStack: TStack read FParseStack write FParseStack;
      property opcode[x: word]: TEventCommand read getOpcode;
      property len: word read getLength;}
      property valid: boolean read isValid;
      property tilegroup: integer read GetTileGroup;
   end;

   TPageList = class(TRpgObjectList<TRpgEventPage>);

   TRpgMapObject = class(TRpgDatafile, IRpgMapObject)
   private
      FLocation: TSgPoint;
      FPages: TPageList;
      function getPage(x: integer): TRpgEventPage;
   private
      FCurrentPage: TRpgEventPage;
      FPageChanged: boolean;
      FLocked: boolean;
      function getCurrentPage: TRpgEventPage;
      function GetPageCount: integer;
   public //no idea why, but marking this protected generates a bad VMT.
      class function keyChar: ansiChar; override;
   public
      constructor Create; overload; override;
      constructor Create(id: smallint); reintroduce; overload;
      constructor Load(savefile: TStream); override;
      procedure save(savefile: TStream); override;
      destructor Destroy; override;

      procedure AddPage(value: TRpgEventPage);
      function isTile: boolean;

      property location: TSgPoint read FLocation write FLocation;
      property pages: TPageList read FPages;
      property page[x: integer]: TRpgEventPage read getPage; default;
      property newCurrentPage: TRpgEventPage read getCurrentPage;
{     property parent: TEventBlock read FParent;
      property playing: boolean read isCurrentlyPlaying write setCurrentlyPlaying;
      property deleted: boolean read FDeleted write FDeleted;
}
      property locked: boolean read FLocked write FLocked;
      property currentPage: TRpgEventPage read FCurrentPage;
      property updated: boolean read FPageChanged;
   end;

implementation
uses
   SysUtils;

{ TRpgMapObject }

constructor TRpgMapObject.Load(savefile: TStream);
var
   i: integer;
begin
   inherited load(savefile);
   FLocation.x := savefile.readInt;
   FLocation.y := savefile.readInt;
   FPages := TPageList.Create;
   for i := 0 to savefile.readInt - 1 do
      FPages.Add(TRpgEventPage.Load(savefile, self));
   readEnd(savefile);
   if FPages.Count > 0 then
      FCurrentPage := FPages[0];
end;

procedure TRpgMapObject.save(savefile: TStream);
var
   page: TRpgEventPage;
begin
   inherited save(savefile);
   savefile.writeInt(FLocation.x);
   savefile.writeInt(FLocation.y);
   savefile.writeInt(FPages.Count);
   for page in FPages do
      page.save(savefile);
   writeEnd(savefile);
end;

constructor TRpgMapObject.Create(id: smallint);
begin
   inherited Create;
   FId := id;
   FPages := TPageList.Create;
   AddPage(TRpgEventPage.Create(self, 0));
   FCurrentPage := FPages[0];
end;

constructor TRpgMapObject.Create;
begin
   inherited Create;
   FPages := TPageList.Create;
end;

destructor TRpgMapObject.Destroy;
begin
   FPages.Free;
   inherited Destroy;
end;

procedure TRpgMapObject.AddPage(value: TRpgEventPage);
begin
   FPages.Add(value);
end;

function TRpgMapObject.getCurrentPage: TRpgEventPage;
var i: integer;
begin
   result := nil;
   I := FPages.High;
   while (result = nil) and (i >= 0) do
   begin
      if FPages[i].valid then
         result := FPages[i];
      dec(i);
   end;
   FPageChanged := (FCurrentPage = result);
   FCurrentPage := result;
end;

function TRpgMapObject.getPage(x: integer): TRpgEventPage;
begin
   result := FPages[x];
end;

function TRpgMapObject.GetPageCount: integer;
begin
   result := FPages.Count;
end;

function TRpgMapObject.isTile: boolean;
begin
   result := FCurrentPage.isTile;
end;

class function TRpgMapObject.keyChar: ansiChar;
begin
   result := 'o';
end;

{ TRpgEventConditions }

constructor TRpgEventConditions.Load(savefile: TStream);
begin
   savefile.ReadBuffer(FConditions, sizeof(FConditions));
   FSwitch1 := savefile.readInt;
   FSwitch2 := savefile.readInt;
   FVariable1 := savefile.readInt;
   FVariable2 := savefile.readInt;
   savefile.ReadBuffer(FVar1Op, sizeof(TComparisonOp));
   savefile.ReadBuffer(FVar2Op, sizeof(TComparisonOp));
   FVarValue1 := savefile.readInt;
   FVarValue2 := savefile.readInt;
   FItem := savefile.readInt;
   FHero := savefile.readInt;
   FClock1 := savefile.readInt;
   FClock2 := savefile.readInt;
   savefile.ReadBuffer(FClock1Op, sizeof(TComparisonOp));
   savefile.ReadBuffer(FClock2Op, sizeof(TComparisonOp));
   FScript := savefile.readString;
end;

procedure TRpgEventConditions.Save(savefile: TStream);
begin
   savefile.WriteBuffer(FConditions, sizeof(FConditions));
   savefile.writeInt(FSwitch1);
   savefile.writeInt(FSwitch2);
   savefile.writeInt(FVariable1);
   savefile.writeInt(FVariable2);
   savefile.WriteBuffer(FVar1Op, sizeof(TComparisonOp));
   savefile.WriteBuffer(FVar2Op, sizeof(TComparisonOp));
   savefile.writeInt(FVarValue1);
   savefile.writeInt(FVarValue2);
   savefile.writeInt(FItem);
   savefile.writeInt(FHero);
   savefile.writeInt(FClock1);
   savefile.writeInt(FClock2);
   savefile.WriteBuffer(FClock1Op, sizeof(TComparisonOp));
   savefile.WriteBuffer(FClock2Op, sizeof(TComparisonOp));
   savefile.writeString(FScript);
end;

{ TRpgEventPage }

constructor TRpgEventPage.Create(parent: TRpgMapObject; id: smallint);
begin
   FParent := parent;
   FId := id;
   FPath := TPath.Create;
   FConditions := TRpgEventConditions.Create;
end;

destructor TRpgEventPage.Destroy;
begin
   FPath.Free;
   FConditions.Free;
   inherited Destroy;
end;

function TRpgEventPage.isTile: boolean;
begin
   result := self.tilegroup <> -1;
end;

function TRpgEventPage.isValid: boolean;
begin
   assert(false, 'not implemented'); result := false;
//   result := self.FConditions.Valid;
end;

class function TRpgEventPage.keyChar: AnsiChar;
begin
   result := 'e';
end;

constructor TRpgEventPage.Load(savefile: TStream);
begin
   self.Load(savefile, nil);
end;

constructor TRpgEventPage.Load(savefile: TStream; parent: TRpgMapObject);
begin
   FParent := parent;
   inherited Load(savefile);
   FConditions := TRpgEventConditions.Load(savefile);
   FFrame := savefile.readWord;
   FTransparent := savefile.readBool;
   savefile.ReadBuffer(FDirection, sizeof(FDirection));
   savefile.ReadBuffer(FMoveType, sizeof(FMoveType));
   FMoveFrequency := savefile.readByte;
   savefile.ReadBuffer(FStartCondition, sizeof(FStartCondition));
   FEventHeight := savefile.readByte;
   FNoOverlap := savefile.readBool;
   savefile.ReadBuffer(FAnimType, sizeof(FAnimType));
   FMoveSpeed := savefile.readByte;
   FPath := TPath.Load(savefile);
   FMoveIgnore := savefile.readBool;
   FEventText := savefile.readString;
   FMatrix := savefile.readWord;
   self.readEnd(savefile);
end;

procedure TRpgEventPage.Save(savefile: TStream);
begin
   inherited Save(savefile);
   FConditions.Save(savefile);
   savefile.writeWord(FFrame);
   savefile.writeBool(FTransparent);
   savefile.WriteBuffer(FDirection, sizeof(FDirection));
   savefile.WriteBuffer(FMoveType, sizeof(FMoveType));
   savefile.writeByte(FMoveFrequency);
   savefile.WriteBuffer(FStartCondition, sizeof(FStartCondition));
   savefile.writeByte(FEventHeight);
   savefile.writeBool(FNoOverlap);
   savefile.WriteBuffer(FAnimType, sizeof(FAnimType));
   savefile.writeByte(FMoveSpeed);
   FPath.Save(savefile);
   savefile.writeBool(FMoveIgnore);
   savefile.writeString(FEventText);
   savefile.writeWord(FMatrix);
   self.writeEnd(savefile);
end;

function TRpgEventPage.GetTileGroup: integer;
begin
   if FName[1] <> '*' then
      Exit(-1);
   result := StrToInt(Copy(FName, 2, MAXINT));
end;

function TRpgEventPage.hasScriptFunction: boolean;
begin
   result := FEventText <> '';
end;

{ UploadConditionsAttribute }

procedure UploadConditionsAttribute.download(db: TDataset; field: TRttiField;
  instance: TObject);
var
   conditions: TPageConditionSet;

   procedure setCondition(field: boolean; value: TPageConditions);
   begin
      if field then
         include(conditions, value)
      else exclude(conditions, value);
   end;

begin
   conditions := [];
   setCondition(db.FieldByName('bSwitch1').AsBoolean, pc_switch1);
   setCondition(db.FieldByName('bSwitch2').AsBoolean, pc_switch2);
   setCondition(db.FieldByName('BVar1').AsBoolean, pc_var1);
   setCondition(db.FieldByName('bVar2').AsBoolean, pc_var2);
   setCondition(db.FieldByName('bItem').AsBoolean, pc_item);
   setCondition(db.FieldByName('bHero').AsBoolean, pc_hero);
   setCondition(db.FieldByName('bTimer1').AsBoolean, pc_timer1);
   setCondition(db.FieldByName('bTimer2').AsBoolean, pc_timer2);
   field.SetValue(instance, TValue.From(conditions));
end;

procedure UploadConditionsAttribute.upload(db: TDataset; field: TRttiField;
  instance: TObject);
var
   conditions: TPageConditionSet;
begin
   conditions := field.GetValue(instance).AsType<TPageConditionSet>;
   db.FieldByName('bSwitch1').AsBoolean := (pc_switch1 in conditions);
   db.FieldByName('bSwitch2').AsBoolean := (pc_switch2 in conditions);
   db.FieldByName('BVar1').AsBoolean := (pc_var1 in conditions);
   db.FieldByName('bVar2').AsBoolean := (pc_var2 in conditions);
   db.FieldByName('bItem').AsBoolean := (pc_item in conditions);
   db.FieldByName('bHero').AsBoolean := (pc_hero in conditions);
   db.FieldByName('bTimer1').AsBoolean := (pc_timer1 in conditions);
   db.FieldByName('bTimer2').AsBoolean := (pc_timer2 in conditions);
end;

{ UploadTimerAttribute }

constructor UploadTimerAttribute.Create(index: integer);
begin
   FIndex := index;
end;

procedure UploadTimerAttribute.download(db: TDataset; field: TRttiField;
  instance: TObject);
var
   time: integer;
begin
   time := (db.FieldByName(format('Clock%dMins', [FIndex])).AsInteger * 60) +
     db.FieldByName(format('Clock%dSecs', [FIndex])).AsInteger;
   field.SetValue(instance, time);
end;

procedure UploadTimerAttribute.upload(db: TDataset; field: TRttiField;
  instance: TObject);
var
   time: integer;
begin
   time := field.GetValue(instance).AsInteger;
   db.FieldByName(format('Clock%dMins', [FIndex])).AsInteger := time div 60;
   db.FieldByName(format('Clock%dSecs', [FIndex])).AsInteger := time mod 60;
end;

{ UploadBattleConditionsAttribute }

procedure UploadBattleConditionsAttribute.download(db: TDataset; field: TRttiField; instance: TObject);
var
   conditions: TBattlePageConditionSet;

   procedure setCondition(field: boolean; value: TBattlePageConditions);
   begin
      if field then
         include(conditions, value)
      else exclude(conditions, value);
   end;

begin
   conditions := [];
   setCondition(db.FieldByName('bTurns').AsBoolean, bp_turns);
   setCondition(db.FieldByName('bMonsterTime').AsBoolean, bp_monsterTime);
   setCondition(db.FieldByName('BHeroTime').AsBoolean, bp_heroTime);
   setCondition(db.FieldByName('bExhaustion').AsBoolean, bp_exhaustion);
   setCondition(db.FieldByName('bMonsterHP').AsBoolean, bp_monsterHP);
   setCondition(db.FieldByName('bHeroHP').AsBoolean, bp_HeroHP);
   setCondition(db.FieldByName('bCommandUsed').AsBoolean, bp_commandUsed);
   field.SetValue(instance, TValue.From(conditions));
end;

procedure UploadBattleConditionsAttribute.upload(db: TDataset; field: TRttiField; instance: TObject);
var
   conditions: TBattlePageConditionSet;
begin
   conditions := field.GetValue(instance).AsType<TBattlePageConditionSet>;
   db.FieldByName('bTurns').AsBoolean := (bp_turns in conditions);
   db.FieldByName('bMonsterTime').AsBoolean := (bp_monsterTime in conditions);
   db.FieldByName('BHeroTime').AsBoolean := (bp_heroTime in conditions);
   db.FieldByName('bExhaustion').AsBoolean := (bp_exhaustion in conditions);
   db.FieldByName('bMonsterHP').AsBoolean := (bp_monsterHP in conditions);
   db.FieldByName('bHeroHP').AsBoolean := (bp_HeroHP in conditions);
   db.FieldByName('bCommandUsed').AsBoolean := (bp_commandUsed in conditions);
end;

end.
