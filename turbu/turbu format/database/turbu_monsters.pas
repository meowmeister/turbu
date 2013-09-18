unit turbu_monsters;

interface
uses
   SysUtils, Generics.Collections, DB, RTTI,
   turbu_classes, turbu_defs, turbu_containers, turbu_map_objects, turbu_serialization,
   SG_defs;

type
   TRpgMonster = class(TRpgDatafile)
   protected
      FFilename: string;
      FTransparent: boolean;
      FFlying: boolean;
      FColorShift: integer;
      FStats: TStatArray;
      FExp: integer;
      FMoney: integer;
      FItem: integer;
      FItemChance: integer;
      FCanCrit: boolean;
      FCritChance: integer;
      FOftenMiss: boolean;
      FConditions: TPointArray;
      FResists: TPointArray;
      FTag: T4IntArray;
      function getStat(i: byte): integer;
   public
      property filename: string read FFilename;
      property transparent: boolean read FTransparent;
      property flying: boolean read FFlying;
      property colorShift: integer read FColorShift;
      property stat[i: byte]: integer read getStat;
      property exp: integer read FExp;
      property money: integer read FMoney;
      property item: integer read FItem;
      property itemChance: integer read FItemChance;
      property canCrit: boolean read FCanCrit;
      property critChance: integer read FCritChance;
      property oftenMiss: boolean read FOftenMiss;
      property condition: TPointArray read FConditions;
      property resist: TPointArray read FResists;
      property tag: T4IntArray read FTag;
   end;

   TRpgMonsterElement = class(TObject)
      id: integer;
      monster: integer;
      position: TSgPoint;
      invisible: boolean;
      constructor Create(id: integer; monster: integer; position: TSgPoint;
         invisible: boolean);
   end;

   TBattleEventPage = class
   protected
      FID: integer;
      FConditions: TBattleEventConditions;
      FEventText: string;
   public
      constructor Create;
      destructor Destroy; override;

      property id: integer read FID;
      property conditions: TBattleEventConditions read FConditions;
      property eventText: string read FEventText;
   end;

   TBattleEventList = class(TRpgObjectList<TBattleEventPage>);
   TMonsterElementList = class(TObjectList<TRpgMonsterElement>);

   HabitatUploadAttribute = class(TDBUploadAttribute)
   public
      procedure upload(db: TDataset; field: TRttiField; instance: TObject); override;
      procedure download(db: TDataset; field: TRttiField; instance: TObject); override;
   end;

   TRpgMonsterParty = class(TRpgDatafile)
   protected
      FAutoAlign: boolean;
      FRandom: boolean;
      [HabitatUpload]
      FHabitats: TBytes;
      FMonsters: TMonsterElementList;
      FEvents: TBattleEventList;
   public
      constructor Create; override;
      destructor Destroy; override;
      property autoAlign: boolean read FAutoAlign;
      property random: boolean read FRandom;
      property habitats: TBytes read FHabitats;
      property monsters: TMonsterElementList read FMonsters;
      property events: TBattleEventList read FEvents;
   end;

implementation

{ TRpgMonster }

function TRpgMonster.getStat(i: byte): integer;
begin
   result := FStats[i];
end;

{ TRpgMonsterParty }

constructor TRpgMonsterParty.Create;
begin
   inherited Create;
   FMonsters := TMonsterElementList.Create;
   FEvents := TBattleEventList.Create;
end;

destructor TRpgMonsterParty.Destroy;
begin
   FMonsters.Free;
   FEvents.Free;
   inherited;
end;

{ TBattleEventPage }

constructor TBattleEventPage.Create;
begin
   inherited Create;
   FConditions := TBattleEventConditions.Create;
end;

destructor TBattleEventPage.Destroy;
begin
   FConditions.Free;
   inherited;
end;

{ TRpgMonsterElement }

constructor TRpgMonsterElement.Create(id, monster: integer; position: TSgPoint;
  invisible: boolean);
begin
   self.id := id;
   self.monster := monster;
   self.position := position;
   self.invisible := invisible;
end;

{ HabitatUploadAttribute }

procedure HabitatUploadAttribute.download(db: TDataset; field: TRttiField; instance: TObject);
begin
   (instance as TRpgMonsterParty).FHabitats := db.FieldByName('habitats').AsBytes;
end;

procedure HabitatUploadAttribute.upload(db: TDataset; field: TRttiField; instance: TObject);
begin
   db.FieldByName('habitats').AsBytes := (instance as TRpgMonsterParty).FHabitats;
end;

end.
