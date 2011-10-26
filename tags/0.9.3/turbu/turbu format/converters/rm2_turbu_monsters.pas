unit rm2_turbu_monsters;

interface
uses
   types, Generics.Collections,
   turbu_monsters, turbu_map_objects, monster, events, conversion_report;

type
   TEventScanner = procedure (list: TEventCommandList) of object;

   T2kMonster = class helper for TRpgMonster
   private
      procedure addResist(const value: TPoint); inline;
      procedure addCondition(const value: TPoint); inline;
   public
      constructor convert(base: TMonster; id: integer);
   end;

   T2kMonsterParty = class helper for TRpgMonsterParty
   public
      constructor convert(base: TMonsterParty; id: integer; scanner: TEventScanner);
   end;

   T2kBattleEventPage = class helper for turbu_monsters.TBattleEventPage
   public
      constructor convert(base: events.TBattleEventPage; baseid, id: integer; scanner: TEventScanner);
   end;

   T2kBattleEventConditions = class helper for turbu_map_objects.TBattleEventConditions
   public
      constructor convert(base: events.TBattleEventConditions);
   end;

   procedure ScanMPartiesForDuplicates(list: TArray<TMonsterParty>;
     scanner: TEventScanner);
   procedure ReportMPartyDuplicates(report: IConversionReport);

implementation
uses
   SysUtils, Classes,
   turbu_defs, rm2_turbu_database, rm2_turbu_map_objects, turbu_database,
   ArchiveInterface, EB_RpgScript, EB_System;

var
   globalScripts, duplicateRefs, totalPages: integer;

{ T2kMonster }

procedure T2kMonster.addCondition(const value: TPoint);
begin
   setLength(FConditions, length(FConditions) + 1);
   FConditions[high(FConditions)] := value;
end;

procedure T2kMonster.addResist(const value: TPoint);
begin
   setLength(FResists, length(FResists) + 1);
   FResists[high(FResists)] := value;
end;

constructor T2kMonster.convert(base: TMonster; id: integer);
var
   i: integer;
   resistVal: integer;
begin
   inherited Create;
   FId := id;
   FName := string(base.name);
   FFilename := string(base.filename);
   FTransparent := base.transparent;
   FFlying := base.flying;
   FColorShift := base.colorShift;
   for i := Low(TStatArray) to High(TStatArray) do
      FStats[i] := base.stats[i];
   FExp := base.exp;
   FMoney := base.money;
   FItem := base.item;
   FItemChance := base.itemChance;
   FCanCrit := base.canCrit;
   FCritChance := base.critChance;
   FOftenMiss := base.oftenMiss;
   for I := 1 to GLcfDatabase.attributes do
   begin
      if i <= high(base.dtypeModifiers) then
         resistVal := base.dtypeModifiers[i] + 1
      else resistVal := 2;
      resistVal := -(GLcfDatabase.attribute[i].rate[resistVal] - 100);
      if resistVal <> GLcfDatabase.attribute[i].rate[3] then
         self.addResist(point(i, resistVal));
   end;
   for I := 1 to GLcfDatabase.conditions do
   begin
      if i <= high(base.conditionModifiers) then
         resistVal := base.conditionModifiers[i] + 1
      else resistVal := 2;
      resistVal := -(GLcfDatabase.condition[i].chance[resistVal] - 100);
      if resistVal <> 0 then
         self.addCondition(point(i, resistVal));
   end;
   GDatabase.AddLegacy('monster', id, $2a, base.actions);
end;

{ T2kMonsterParty }

constructor T2kMonsterParty.convert(base: TMonsterParty; id: integer; scanner: TEventScanner);
var
   i: integer;
begin
   inherited Create;
   self.id := id;
   self.name := string(base.name);
   FAutoAlign := base.autoAlign;
   FRandom := base.rTag;

   self.monsters.Capacity := length(base.monsters);
   for I := 1 to High(base.monsters) do
      self.monsters.Add(TRpgMonsterElement.Create(i, base.monsters[i].id,
        base.monsters[i].position, base.monsters[i].invisible));

   setLength(FHabitats, length(base.habitats));
   for I := 0 to High(base.habitats) do
      if base.habitats[i] then
         self.habitats[i] := 1;

   self.events.Capacity := length(base.events);
   for i := 1 to high(base.events) do
      self.events.Add(turbu_monsters.TBattleEventPage.convert(base.events[i], id, i, scanner))
end;

{ T2kBattleEventPage }

constructor T2kBattleEventPage.convert(base: events.TBattleEventPage; baseid, id: integer; scanner: TEventScanner);
var
   proc: TEBProcedure;
   call: TEBCallEvent;
   pagename: string;
begin
   inherited Create;
   FId := id;
   FConditions.convert(base.Conditions);
   pagename := format('mparty%.4d_%d', [baseid, id]);
   if (base.commands.count = 1) and (base.Commands[0].opcode = 9999) then
   begin
      proc := TEBProcedure.Create(nil);
      call := TEBCallEvent.Create(proc);
      call.Values.AddRange([3, base.Commands[0].data[0]]);
   end
   else begin
      scanner(base.Commands);
      proc := rm2_turbu_map_objects.ConvertEventScript(base.Commands, pagename);
   end;
   try
      FEventText := proc.Serialize;
   finally
      proc.Free;
   end;
end;

{ T2kBattleEventConditions }

constructor T2kBattleEventConditions.convert(base: events.TBattleEventConditions);
begin
   inherited Create;
   if bp_switch1 in base.conditions then
      include(FConditions, pc_switch1);
   if bp_switch2 in base.conditions then
      include(FConditions, pc_switch2);
   if bp_variable1 in base.conditions then
      include(FConditions, pc_var1);
   if bp_turns in base.conditions then
      include(FBattleConditions, turbu_map_objects.bp_turns);
   if bp_monsterTime in base.conditions then
      include(FBattleConditions, turbu_map_objects.bp_monsterTime);
   if bp_heroTime in base.conditions then
      include(FBattleConditions, turbu_map_objects.bp_heroTime);
   if bp_exhaustion in base.conditions then
      include(FBattleConditions, turbu_map_objects.bp_exhaustion);
   if bp_monsterHP in base.conditions then
      include(FBattleConditions, turbu_map_objects.bp_monsterHP);
   if bp_heroHP in base.conditions then
      include(FBattleConditions, turbu_map_objects.bp_heroHP);
   if bp_commandUsed in base.conditions then
      include(FBattleConditions, turbu_map_objects.bp_commandUsed);
   FSwitch1 := base.switch1Set;
   FSwitch2 := base.switch2Set;
   FVariable1 := base.variableSet;
   FVarValue1 := base.variableValue;
   FVar1Op := co_gt;
   FVar2Op := co_gt;
   FTurnsConst := base.turnsConst;
   FTurnsMultiple := base.turnsConst;
   FMonsterTurn := base.monsterTurn;
   FMonsterTurnsConst := base.monsterTurnsConst;
   FMonsterTurnsMultiple := base.monsterTurnsMultiple;
   FHeroTurn := base.heroTurn;
   FHeroTurnsConst := base.heroTurnsConst;
   FHeroTurnsMultiple := base.heroTurnsMultiple;
   FExhaustionMin := base.exhaustionMin;
   FExhaustionMax := base.exhaustionMax;
   FMonsterHP := base.monsterHP;
   FMonsterHPMin := base.monsterHPMin;
   FMonsterHPMax := base.monsterHPMax;
   FHeroHP := base.heroHP;
   FHeroHPMin := base.heroHPMin;
   FHeroHPMax := base.heroHPMax;
   FHeroCommandWho := base.heroCommandWho;
   FHeroCommandWhich := base.heroCommandWhich;
end;

procedure SaveGlobalScript(global: TEBUnit);
var
   saver: TStringStream;
   folder: string;
   archive: IArchive;
begin
   archive := GArchives[SCRIPT_ARCHIVE];
   folder := archive.currentFolder;
   saver := TStringStream.Create(global.Serialize);
   try
      archive.createFolder('global');
      archive.currentFolder := 'global';
      archive.writeFile('BattleGlobalScripts.trs', saver);
   finally
      archive.currentFolder := folder;
      saver.Free;
   end;
end;

function InsertDupe(list: TArray<TMonsterParty>; scanner: TEventScanner; duplicates: TDictionary<ansiString, integer>; base: ansiString;
   orig: TPoint; var counter: integer): TEBProcedure;
var
   commands: TEventCommandList;
begin
   commands := list[orig.X].events[orig.Y].Commands;
   scanner(Commands);
   result := (ConvertEventScript(commands, format('battleGlobal%.4d', [counter])));
   duplicates.Add(base, counter);
   commands.Clear;
   commands.Add(TEventCommand.Create(9999, counter));
   inc(counter);
   inc(globalScripts);
   inc(duplicateRefs);
end;

procedure ScanMPartiesForDuplicates(list: TArray<TMonsterParty>; scanner: TEventScanner);
var
   originals: TDictionary<ansiString, TPoint>;
   duplicates: TDictionary<ansiString, integer>;
   i, j, counter: integer;
   commands: TEventCommandList;
   base: ansiString;
   global: TEBUnit;
begin
   globalScripts := 0;
   duplicateRefs := 0;
   totalPages := 0;
   originals := TDictionary<ansiString, TPoint>.Create;
   duplicates := TDictionary<ansiString, integer>.Create;
   global := TEBUnit.Create(nil);
   try
      counter := 1;
      for i := 1 to high(list) do
         for j := 1 to High(list[i].events) do
         begin
            if list[i].events[j].commands.Count <= 1 then
               Continue;
            base := list[i].events[j].Base;
            if originals.ContainsKey(base) then
            begin
               if not duplicates.ContainsKey(base) then
                  global.Add(InsertDupe(list, scanner, duplicates, base, originals[base], counter));
               commands := list[i].events[j].Commands;
               commands.Clear;
               commands.Add(TEventCommand.Create(9999, duplicates[base]));
               inc(duplicateRefs);
            end
            else originals.Add(base, point(i, j));
            list[i].events[j].Base := '';
            inc(totalPages);
         end;
      SaveGlobalScript(global);
   finally
      global.Free;
      originals.Free;
      duplicates.Free;
   end;
end;

procedure ReportMPartyDuplicates(report: IConversionReport);
begin
   if duplicateRefs > 0 then
      report.makeHint(format('Found %d exact duplicates of %d battle script pages; moved to global battle scripts section.', [duplicateRefs, globalScripts]));
   report.makeHint(format('Processed %d total battle script pages', [totalpages]));
end;

end.
