program mapshow;


{Build count rolled over at 84 from version 0.0.1 to 0.0.2}

uses
  fastmm4,
  Forms,
  mapview in 'mapview.pas' {frmGameForm},
  LMU in '..\LMU.pas',
  LDB in '..\LDB.pas',
  LMT in '..\LMT.pas',
  openformThrowaway in 'openformThrowaway.pas' {openForm},
  events in '..\events.pas',
  locate_files in '..\locate_files.pas',
  console in 'console.pas' {frmConsole},
  rm_sound in '..\rm_sound.pas',
  item_code in '..\item_code.pas',
  skill_code in '..\skill_code.pas',
  rm2x_skill_menu in 'libs\menus\rm2x\rm2x_skill_menu.pas',
  rm2x_inventory_menu in 'libs\menus\rm2x\rm2x_inventory_menu.pas',
  rm2x_item_menu in 'libs\menus\rm2x\rm2x_item_menu.pas',
  rm2x_main_menu in 'libs\menus\rm2x\rm2x_main_menu.pas',
  rm2X_menu_components in 'libs\menus\rm2x\rm2X_menu_components.pas',
  rm2X_menu_engine in 'libs\menus\rm2x\rm2X_menu_engine.pas',
  rm2x_party_target_menu in 'libs\menus\rm2x\rm2x_party_target_menu.pas',
  rm2x_shop_menu in 'libs\menus\rm2x\rm2x_shop_menu.pas',
  menu_basis in 'libs\menus\menu_basis.pas',
  shop_data in '..\shop_data.pas',
  rm2x_name_menu in 'libs\menus\rm2x\rm2x_name_menu.pas',
  addition_sprite in 'libs\map engine\addition_sprite.pas',
  charset_graphics in 'libs\map engine\charset_graphics.pas',
  chipset_graphics in 'libs\map engine\chipset_graphics.pas',
  script_engine in 'libs\script engine\script_engine.pas',
  tiles in '..\engines\map engine\tiles.pas',
  uPSI_script_interface in 'libs\script engine\uPSI_script_interface.pas',
  rpg_list in 'libs\script engine\rpg_list.pas',
  script_backend in 'libs\script engine\script_backend.pas',
  script_interface in 'libs\script engine\script_interface.pas',
  rs_system in 'libs\script engine\RPG Script library\rs_system.pas',
  rs_message in 'libs\script engine\RPG Script library\rs_message.pas',
  rs_menu in 'libs\script engine\RPG Script library\rs_menu.pas',
  rs_map in 'libs\script engine\RPG Script library\rs_map.pas',
  transitions in 'libs\map engine\transitions.pas',
  transition_graphics in 'libs\map engine\transition_graphics.pas',
  distortions in 'libs\map engine\distortions.pas',
  weather in 'libs\general graphics\weather.pas',
  rpg_image in 'libs\general graphics\rpg_image.pas',
  frames in 'libs\menus\frames.pas',
  timing in 'libs\timing.pas',
  rpg_anim in 'libs\general graphics\rpg_anim.pas',
  xyz_lib in '..\xyz_lib.pas',
  move_data in '..\move_data.pas',
  map_unit in 'libs\map engine\map_unit.pas',
  formats in '..\formats.pas',
  logs in '..\logs.pas',
  SG_defs in '..\..\classes\sdl custom\SG_defs.pas',
  sdl_sprite in '..\..\classes\sdl custom\sdl_sprite.pas',
  text_graphics in 'libs\general graphics\text_graphics.pas',
  turbu_heroes in '..\engines\map engine\turbu_heroes.pas',
  archiveInterface in '..\archiveInterface.pas',
  uPSI_shop_menu in 'libs\menus\rm2x\upsi\uPSI_shop_menu.pas',
  uPSI_item_code in 'libs\menus\rm2x\upsi\uPSI_item_code.pas',
  uPSI_item_menu in 'libs\menus\rm2x\upsi\uPSI_item_menu.pas',
  uPSI_menu_components in 'libs\menus\rm2x\upsi\uPSI_menu_components.pas',
  uPSI_menus in 'libs\menus\rm2x\upsi\uPSI_menus.pas',
  uPSI_party_target_menu in 'libs\menus\rm2x\upsi\uPSI_party_target_menu.pas',
  uPSI_rm2x_inventory_menu in 'libs\menus\rm2x\upsi\uPSI_rm2x_inventory_menu.pas';

{$R *.res}

 begin
  Application.Initialize;
//  ReportMemoryLeaksOnShutdown := DebugHook <> 0;
  Application.CreateForm(TopenForm, openForm);
  Application.CreateForm(TfrmGameForm, frmGameForm);
  Application.CreateForm(TfrmConsole, frmConsole);
  Application.Run;
end.
