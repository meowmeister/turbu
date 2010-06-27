unit turbu_heroes;

interface
uses
   turbu_classes, turbu_containers, turbu_defs;

type
   //stub declaration that will be filled in later.
   TRpgHero = class(TRpgObject)
   private
      FName: string;
      FClass: string;
      FSprite: string;
      FTransparent: boolean;
      FLevel: integer;
      FCritRate: integer;
      FFaceName: string;
      FFaceNum: integer;

      FDualWield: TWeaponStyle;
      FStaticEq: boolean;
      FComputerControlled: boolean;
      FStrongDefense: boolean;

      FExpTable: array[1..50] of integer;
      FExpTotal: integer;
{      FEquipment: array[1..5] of TRpgItem;
      FStat: array[TStatComponents, 1..4] of smallint;}
      FConditionModifier: array of integer;
      FCondition: array of boolean;
      FDtypeModifiers: array of integer;
      FHitPoints: integer;
      FManaPoints: integer;
      FMaxHitPoints: integer;
      FMaxManaPoints: integer;
      FHpModifier: integer;
      FMpModifier: integer;
      FSkill: array of boolean;

      FLevelUpdated: boolean;

   protected
      class function templateClass: TDatafileClass; override;
   public
      constructor Create(base: TRpgDatafile);
   end;

   TRpgParty = class(TRpgObjectList<TRpgHero>)
   private
      FCash: integer;
//      FInventory: TRpgInventory;
   end;

   TPartyEvent = procedure(hero: TRpgHero; party: TRpgParty) of object;

implementation
uses
   turbu_characters, turbu_database,
   upsRuntime;

{ TRpgHero }

constructor TRpgHero.Create(base: TRpgDatafile);
var
  I: Integer;
  dummy: byte;
  exec: TPSExec;
  calc: TExpCalcEvent;
  template: THeroTemplate absolute base;
begin
   inherited Create(base);
   if base = nil then
      Exit;

   FName := template.name;
   FClass := GDatabase.charClass[template.charClass].clsName;
   FSprite := template.MapSprite;
   FTransparent := template.translucent;
   FExpTable[1] := 0;

   calc := TExpCalcEvent(GScriptEngine.Exec.GetProcAsMethodN(template.expFunc));
   for I := 2 to 50 do
      FExpTable[i] := calc(i, template.expVars[1], template.expVars[2], template.expVars[3], template.expVars[4]);
   if template.canCrit then
      FCritRate := template.critRate
   else FCritRate := 0;
   FFaceName := template.portrait;
   FFaceNum := template.portraitIndex;
   FDualWield := template.dualWield;
   FStaticEq := template.staticEq;
   setLength(FSkill, GDatabase.skill.count + 1);
{   for I := 1 to template.skillset.count do
      if template.skillset[i].le <= FLevel then
         FSkill[template.skill[i].id] := true;
      //end if
   //end for
   FLevel := 1;
   self.levelAdjustUp(0);
   level := template.startLevel;
   FExpTotal := FExpTable[FLevel];
   for I := low(FEquipment) to high(FEquipment) do
      if template.initialEq[i] <> 0 then
         self.equip(template.initialEq[i]);
   //end for
   i := template.conditionModifiers;
   setLength(FConditionModifier, i);
   setLength(FCondition, i);
   for I := 0 to high(FCondition) do
   begin
      dummy := template.conditionModifier[i];
      FConditionModifier[i] := GDatabase.condition[i].chance[dummy]; //fix this
      FCondition[i] := false;
   end;
   FHitPoints := maxHp;
   FManaPoints := maxMp;}
end;

class function TRpgHero.templateClass: TDatafileClass;
begin
   result := TClassTemplate;
end;

end.
