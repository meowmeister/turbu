unit rm2X_menu_engine;
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
   windows,
   chipset_data, frames, rpg_list, item_data, skill_code, script_backend,
   menu_basis, rm2X_menu_components, rm2X_main_menu, rm2x_item_menu,
   rm2x_skill_menu, rm2x_party_target_menu, rm2x_inventory_menu, rm2x_shop_menu,
   rm2x_name_menu,
   {asphyreSprite,} SDL_sprite,
   uPSRuntime;

type
   TRmMenuScriptEngine = class(TMenuScriptEngine)
   protected
      procedure setup; override;
      procedure setScript(const value: string); override;
   public
      property exec: TPSExec read FScriptEngine;
   end;

   TGameMenu = class(TMenuEngine)
   private
      FMainPage: TMainMenuPage;
      FInventoryPage: TItemMenuPage;
      FSkillPage: TSkillMenuPage;
      FPartyTargetPage: TPartyTargetPage;
      FEquipmentPage: TEquipmentPage;
      FTargetPage: TPartyTargetPage;
      FShopPage: TShopMenuPage;
      FTextInputPage: TNameMenuPage;
      FOrigin: TPoint;
      FScriptEngine: TRmMenuScriptEngine;

      procedure setState(const Value: TMenuState);
      procedure move;
      procedure initialize;
   protected
      procedure setVisible(const Value: boolean); override;
   public
      constructor Create(parent: TSpriteEngine);
      destructor Destroy; override;
      procedure draw;
      procedure showMain;
      procedure showShop;
      procedure showName;
      procedure activate;
      procedure shutdown;

      property state: TMenuState read FState write setState;
   end;

const
   FULLSCREEN: TRect = (left: 0; top: 0; right: 320; bottom: 240);

var
   GMainMenu: TMainMenuPage;
   GInventoryPage: TItemMenuPage;
   GSkillPage: TSkillMenuPage;
   GPartyTargetPage: TPartyTargetPage;
   GEquipmentPage: TEquipmentPage;
   GTargetPage: TPartyTargetPage;
   GShopPage: TShopMenuPage;
   GTextInputPage: TNameMenuPage;
   GScriptExec: TPSExec;

implementation

uses
   types, sysUtils, forms,
   LDB, commons, chipset_graphics, script_engine, tiles, text_graphics, item_code,
   skill_data, script_interface,
   uPSI_menus, uPSI_menu_components, uPSI_item_menu, uPSI_rm2x_inventory_menu,
   uPSI_script_interface, uPSI_item_code, uPSI_party_target_menu, uPSI_shop_menu,
   uPSCompiler,
   {asphyredef} turbu_defs;

function scriptOnUses(Sender: TPSPascalCompiler; const Name: AnsiString): Boolean; forward;

{ TGameMenu }

procedure TGameMenu.activate;
begin
   assert(self.fstate = ms_fading);
   self.FState := ms_main;
   FOrigin := point(round(FParent.WorldX), round(FParent.WorldY));
end;

constructor TGameMenu.Create(parent: TSpriteEngine);
var
   dummy: string;
begin
   inherited Create(parent);
   FScriptEngine := TRmMenuScriptEngine.create(self);
//   assert(GDataArchive.ReadString('menuscripts', dummy));
   FScriptEngine.script := dummy;
   GScriptExec := FScriptEngine.exec;

   FMainPage := TMainMenuPage.Create(parent, FULLSCREEN, self);
   GMainMenu := FMainPage;
   FInventoryPage := TItemMenuPage.Create(parent, FULLSCREEN, self);
   GInventoryPage := FInventoryPage;
   FSkillPage := TSkillMenuPage.Create(parent, FULLSCREEN, self);
   GSkillPage := FSkillPage;
   FPartyTargetPage := TPartyTargetPage.Create(parent, FULLSCREEN, self);
   GPartyTargetPage := FPartyTargetPage;
   FEquipmentPage := TEquipmentPage.Create(parent, FULLSCREEN, self);
   GEquipmentPage := FEquipmentPage;
   FTargetPage := TPartyTargetPage.Create(parent, FULLSCREEN, self);
   GTargetPage := FTargetPage;
   FShopPage := TShopMenuPage.Create(parent, FULLSCREEN, self);
   GShopPage := FShopPage;
   FTextInputPage := TNameMenuPage.Create(parent, FULLSCREEN, self);
   GTextInputPage := FTextInputPage;
end;

destructor TGameMenu.Destroy;
begin
   FMainPage.Free;
   FInventoryPage.free;
   FSkillPage.free;
   FPartyTargetPage.free;
   FEquipmentPage.free;
   FTargetPage.free;
   FShopPage.free;
   FTextInputPage.free;
   FScriptEngine.free;
   inherited;
end;

procedure TGameMenu.draw;
begin
   if (FOrigin.X <> round(FParent.WorldX)) or (FOrigin.Y <> round(FParent.WorldY)) then
      self.move;
//fixme
//   FParent.Canvas.FillRect(FULLSCREEN, TGameMap(FParent).systemGraphic.menuBg, fxNone);
   if FCurrentPage = nil then
      raise EFatalError.create('Tried to draw the menu while it was off!')
   else FCurrentPage.Draw;
   FOrigin := point(round(FParent.WorldX), round(FParent.WorldY));
end;

procedure TGameMenu.initialize;
begin
   TGameMap(FParent).enterLock := true;
   self.move;
   GParty.sort;
   self.visible := true;
   self.FState := ms_fading;
   placeCursor(0);
end;

procedure TGameMenu.move;
begin
   FCurrentPage.move;
end;

procedure TGameMenu.setState(const Value: TMenuState);
begin
   FState := Value;
   case value of
      ms_off: FCurrentPage := nil;
//      ms_main, ms_fading:  FCurrentMenu := FMenu;
{      ms_party_skill, ms_party_eq: FCurrentMenu := FPartyPanel;
      ms_item: FCurrentMenu := FItemMenu;
      ms_item_target, ms_skill_target: FCurrentMenu := FItemTargetMenu;
      ms_skill: FCurrentMenu := FSkillMenu;
      ms_eq: FCurrentMenu := FEquipmentMenu;}
   end;
end;

procedure TGameMenu.setVisible(const Value: boolean);
begin
   FVisible := Value;
   if assigned(FCurrentPage) then
      FCurrentPage.Visible := value;
   FCursor.Visible := value;
end;

procedure TGameMenu.showMain;
begin
   self.FMainPage.setup(0);
   self.FCurrentPage := FMainPage;
   self.initialize;
end;

procedure TGameMenu.showName;
begin
   self.FTextInputPage.setup(GGameEngine.menuInt);
   self.FCurrentPage := FTextInputPage;
   self.initialize;
end;

procedure TGameMenu.showShop;
begin
   self.FShopPage.setup(0);
   self.FCurrentPage := FShopPage;
   self.initialize;
end;

procedure TGameMenu.shutdown;
begin
   assert((self.fstate = ms_fading) or (application.Terminated));
   self.FState := ms_off;
end;

{ TRmMenuScriptEngine }

procedure TRmMenuScriptEngine.setScript(const value: string);
begin
   inherited;
   SetPointerToData('GPartyTargetPage', @GPartyTargetPage, FScriptEngine.GetTypeNo(FScriptEngine.GetType('TPartyTargetPage')));
end;

procedure TRmMenuScriptEngine.setup;
begin
   FCompiler.onUses := scriptOnUses;
   RIRegister_script_interface(FImporter);
   RIRegister_item_code(FImporter);
   RIRegister_menu_basis(FImporter);
   RIRegister_rm2X_menu_components(FImporter);
   RIRegister_rm2x_item_menu(FImporter);
   RIRegister_rm2x_inventory_menu(FImporter);
   RIRegister_rm2x_party_target_menu(FImporter);
   RIRegister_rm2x_shop_menu(FImporter);
end;

{ Classless }

function scriptOnUses(Sender: TPSPascalCompiler; const Name: AnsiString): Boolean;
{}
   function AddPointerVariable(const VarName, VarType: string): Boolean;
   var
      FVar: TPSVar;
   begin
      FVar := sender.AddUsedVariableN(varname, vartype);
      if fvar = nil then
         result := False
      else begin
         fvar.exportname := fvar.Name;
         fvar.SaveAsPointer := true;
         Result := True;
      end;
   end;
{}

var dummy: TPSType;
begin
   result := true;
   if Name = 'SYSTEM' then begin
      dummy := sender.addtypeS('varArray', 'array of integer');
      dummy.ExportName := true;
      dummy := sender.AddTypeS('switchArray', 'array of boolean');
      dummy.ExportName := true;
      sender.AddTypeS('TButtonCode', '(btn_enter, btn_cancel, btn_up, btn_down, btn_left, btn_right);');
      sender.AddTypeS('TShopState', '(ss_selling, ss_buying, ss_transaction);');
      SIRegister_script_interface(sender);
      SIRegister_item_code(sender);
      SIRegister_menu_basis(sender);
      SIRegister_rm2X_menu_components(sender);
      SIRegister_rm2x_item_menu(sender);
      SIRegister_rm2x_inventory_menu(sender);
      SIRegister_rm2x_party_target_menu(sender);
      SIRegister_rm2x_shop_menu(sender);
      AddPointerVariable('variable', 'varArray');
      AddPointerVariable('switch', 'switchArray');
      AddPointerVariable('GPartyTargetPage', 'TPartyTargetPage');
      sender.AddDelphiFunction('function random(one, two: integer): integer;');
   end
end;

end.
