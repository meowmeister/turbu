unit turbu_map_objects;

interface
uses
   classes,
   turbu_defs, turbu_classes, turbu_containers, charset_data, turbu_pathing,
   SG_defs;

type
   TMoveType = (mt_still, mt_randomMove, mt_cycleUD, mt_cycleLR, mt_chaseHero, mt_fleeHero, mt_byRoute);
   TStartCondition = (by_key, by_touch, by_collision, automatic, parallel, on_call);
   TAnimType = (at_sentry, at_jogger, at_fixedDir, at_fixedJog, at_statue, at_spinRight);
   TPageConditions = (pc_switch1, pc_switch2, pc_var1, pc_var2, pc_item, pc_hero, pc_timer, pc_script);
   TPageConditionSet = set of TPageConditions;

   TRpgMapObject = class;

   TRpgEventConditions = class(TObject)
   private
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
      FClock: integer;
      FScript: string;
//      FScriptCompiled: boolean;
//      function evaluate: boolean;
//      procedure compileScript;
   public
      constructor Load(savefile: TStream);
      procedure Save(savefile: TStream);

//      property valid: boolean read evaluate;
      property conditions: TPageConditionSet read FConditions write FConditions;
      property switch1Set: integer read FSwitch1 write FSwitch1;
      property switch2Set: integer read FSwitch2 write FSwitch2;
      property variable1Set: integer read FVariable1 write FVariable1;
      property variable1Value: integer read FVarValue1 write FVarValue1;
      property variable2Set: integer read FVariable2 write FVariable2;
      property variable2Value: integer read FVarValue2 write FVarValue2;
      property itemNeeded: integer read FItem write FItem;
      property heroNeeded: integer read FHero write FHero;
      property timeRemaining: integer read FClock write FClock;
      property script: string read FScript write FScript;
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
      FPath: TPath;
      FMoveIgnore: boolean;
{      FEventScript: ansiString;
      FEventText: ansiString;
      FScriptText: ansiString;
      FCompiledScript: ansiString; }
//      FCommands: TRpgObjectList<TEventCommand>;
      function isValid: boolean; inline;

{      function getEventScript: ansiString;
      function getCompiledScript: ansiString;
      function hasScriptFunction: boolean; inline; }

{      function getOpcode(x: word): TEventCommand;
      function getLength: word; }
      function GetTileGroup: integer;
   protected
      FConditions: TRpgEventConditions;
      FParent: TRpgMapObject;

      class function keyChar: AnsiChar; override;
   public
      constructor Load(savefile: TStream; parent: TRpgMapObject);
      destructor Destroy; override;
      procedure save(savefile: TStream); override;

      property conditionBlock: TRpgEventConditions read FConditions;
      property whichTile: word read FFrame write FFrame;
      property direction: TFacing read FDirection write FDirection;
      property transparent: boolean read FTransparent write FTransparent;
      property path: TPath read FPath write FPath;
      property moveType: TMoveType read FMoveType write FMoveType;
      property moveFrequency: byte read FMoveFrequency write FMoveFrequency;
      property startCondition: TStartCondition read FStartCondition write FStartCondition;
      property zOrder: byte read FEventHeight write FEventHeight;
      property isBarrier: boolean read FNoOverlap write FNoOverlap;
      property animType: TAnimType read FAnimType write FAnimType;
      property moveSpeed: byte read FMoveSpeed write FMoveSpeed;
//      property eventScript: ansiString read getEventScript;
      property parent: TRpgMapObject read FParent;
{      property compiledScript: tbtString read getCompiledScript;
      property hasScript: boolean read hasScriptFunction;
      property parseStack: TStack read FParseStack write FParseStack;
      property opcode[x: word]: TEventCommand read getOpcode;
      property len: word read getLength;}
      property valid: boolean read isValid;
      property tilegroup: integer read GetTileGroup;
   end;

   TPageList = class(TRpgObjectList<TRpgEventPage>);

   TRpgMapObject = class(TRpgDatafile)
   private
      FLocation: TSgPoint;
      FPages: TPageList;
      function getPage(x: integer): TRpgEventPage;
   private
      FCurrentPage: TRpgEventPage;
      FPageChanged: boolean;
   public //no idea why, but marking this protected generates a bad VMT.
      class function keyChar: ansiChar; override;
   public
      constructor Create;
      constructor Load(savefile: TStream);
      procedure save(savefile: TStream); override;
      destructor Destroy; override;

      procedure AddPage(value: TRpgEventPage);
      function isTile: boolean;

      property location: TSgPoint read FLocation write FLocation;
      property page[x: integer]: TRpgEventPage read getPage; default;
{      constructor createGlobal(input: TStream; expected: word; parent: TEventBlock);
      constructor create(var input: TStringStream; parent: TEventBlock); overload;
      constructor create(newScript: string); overload;
      destructor Destroy; override;
      property len: smallint read FLength write FLength;
      property parent: TEventBlock read FParent;
      property newCurrentPage: TRpgEventPage read getCurrentPage;
      property playing: boolean read isCurrentlyPlaying write setCurrentlyPlaying;
      property locked: boolean read FLocked write FLocked;
      property deleted: boolean read FDeleted write FDeleted;
}
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

function TRpgMapObject.getPage(x: integer): TRpgEventPage;
begin
   result := FPages[x];
end;

function TRpgMapObject.isTile: boolean;
begin
   result := FCurrentPage.tilegroup <> -1;
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
   FHero := savefile.readInt;
   FClock := savefile.readInt;
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
   savefile.writeInt(FHero);
   savefile.writeInt(FClock);
   savefile.writeString(FScript);
end;

{ TRpgEventPage }

destructor TRpgEventPage.Destroy;
begin
   FPath.Free;
   FConditions.Free;
   inherited Destroy;
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
   self.writeEnd(savefile);
end;

function TRpgEventPage.GetTileGroup: integer;
begin
   if FName[1] <> '*' then
      Exit(-1);
   result := StrToInt(Copy(FName, 2, MAXINT));
end;

end.
