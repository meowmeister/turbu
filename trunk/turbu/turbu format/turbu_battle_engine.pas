unit turbu_battle_engine;
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
   turbu_plugin_interface, turbu_versioning,
   sdl_13;

type
   TBattleResult = (br_victory, br_escaped, br_defeated);
   TBattleView = (bv_firstPerson = 1, bv_side, bv_top);
   TBattleTiming = (bt_turns = 1, bt_atb, bt_counter);
   TBattleConditions = class;
   TBattleEngineData = class;

   TBattleResultData = record
      result: TBattleResult;
      data: TObject;
   end;

   IBattleEngine = interface(IInterface)
   ['{B4F083F0-B3F3-465D-930F-C9542CF1B9C0}']
      function startBattle(party: {TRpgParty} TObject; foes: TObject; conditions: TBattleConditions): TBattleResultData;
      procedure initialize(window: TSdlWindowId);
      function getData: TBattleEngineData;
      property data: TBattleEngineData read getData;
   end;

   TBattleConditions = class(TObject)
   private
      FTimer: TObject;
      FCanFlee: boolean;
   public
      constructor Create(timer: TObject; canFlee: boolean);
      property timer: TObject read FTimer;
      property canFlee: boolean read FCanFlee;
   end;

   {TBattleEngineData is used to describe metadata about the type of battle
   engine. This is useful to the editor, to understand how to lay out certain
   tabs. The "name" field provides a user-readable name that can be used from
   scripts to switch battle engines.  Because TBattleEngineData structures are
   created dynamically at run-time, they aren't saved in the database.
   To prevent refcount problems, TBattleEngineData does not contain a reference
   to the engine it describes.  Instead, engine references will be stored in a
   list, and a function similar to indexOf can be used to find engines by name.}
   TBattleEngineData = class(TRpgMetadata)
   private
      FView: TBattleView;
      FTiming: TBattleTiming;
   public
      constructor Create(name: string; version: TVersion; view: TBattleView; timing: TBattleTiming);
      property view: TBattleView read FView;
      property timing: TBattleTiming read FTiming;
   end;

   TBattleEngine = class abstract(TRpgPlugBase, IBattleEngine)
   private
      FData: TBattleEngineData;
      function getData: TBattleEngineData;
   protected
      FInitialized: boolean;
      procedure cleanup; virtual; abstract;
   public
      destructor Destroy; override;
      procedure AfterConstruction; override;
      procedure initialize(window: TSdlWindowId); virtual; abstract;
      function startBattle(party: {TRpgParty} TObject; foes: TObject; conditions: TBattleConditions): TBattleResultData; virtual; abstract;

      property data: TBattleEngineData read getData write FData;
   end;

const
   NEED_BATTLE_SPRITES: set of TBattleView = [bv_side, bv_top];

implementation

{ TBattleEngine }

procedure TBattleEngine.AfterConstruction;
begin
   inherited AfterConstruction;
   assert(assigned(data));
   assert(data.name <> '');
   assert(data.version > TVersion.create(0, 0, 0));
   assert(ord(data.view) > 0);
   assert(ord(data.timing) > 0);
end;

destructor TBattleEngine.Destroy;
begin
   if FInitialized then
      self.cleanup;
   inherited Destroy;
end;

function TBattleEngine.getData: TBattleEngineData;
begin
   result := FData;
end;

{ TBattleEngineData }

constructor TBattleEngineData.Create(name: string; version: TVersion; view: TBattleView; timing: TBattleTiming);
begin
   inherited Create(name, version);
   FView := view;
   FTiming := timing;
end;

{ TBattleConditions }

constructor TBattleConditions.Create(timer: TObject; canFlee: boolean);
begin
   inherited Create;
   FTimer := timer;
   FCanFlee := canFlee;
end;

end.
