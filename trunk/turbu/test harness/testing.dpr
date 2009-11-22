program testing;

uses
  Forms,
  types,
  SDL,
  sdl_canvas,
  test_console in 'test_console.pas' {frmTestConsole},
  turbu_database in '..\turbu format\database\turbu_database.pas',
  turbu_skills in '..\turbu format\database\turbu_skills.pas',
  turbu_constants in '..\turbu format\turbu_constants.pas',
  uPSI_script_interface in '..\mapview\libs\script engine\uPSI_script_interface.pas',
  turbu_2k3_battle_engine in '..\engines\turbu_2k3_battle_engine.pas',
  turbu_2k_battle_engine in '..\engines\turbu_2k_battle_engine.pas',
  turbu_decl_utils in '..\turbu format\turbu_decl_utils.pas',
  turbu_characters in '..\turbu format\database\turbu_characters.pas',
  turbu_script_basis in '..\turbu format\turbu_script_basis.pas',
  rpg_list in '..\mapview\libs\script engine\rpg_list.pas',
  turbu_items in '..\turbu format\database\turbu_items.pas',
  script_interface in '..\mapview\libs\script engine\script_interface.pas',
  script_backend in '..\mapview\libs\script engine\script_backend.pas',
  addition_sprite in '..\mapview\libs\map engine\addition_sprite.pas',
  timing in '..\mapview\libs\timing.pas',
  turbu_unit_dictionary in '..\turbu format\turbu_unit_dictionary.pas',
  findfile in '..\..\classes\findfile\findfile.pas',
  turbu_classes in '..\turbu format\database\turbu_classes.pas',
  turbu_defs in '..\turbu format\turbu_defs.pas',
  turbu_sounds in '..\turbu format\database\turbu_sounds.pas',
  turbu_resists in '..\turbu format\database\turbu_resists.pas',
  turbu_animations in '..\turbu format\database\turbu_animations.pas',
  turbu_engines in '..\turbu format\turbu_engines.pas',
  turbu_sdl_image in '..\turbu format\turbu_sdl_image.pas',
  turbu_sprites in '..\turbu format\turbu_sprites.pas',
  turbu_tbi_lib in '..\turbu format\turbu_tbi_lib.pas',
  turbu_vartypes in '..\turbu format\turbu_vartypes.pas',
  test_project in 'test_project.pas' {frmTestProjLocation},
  conversion_table in '..\turbu format\converters\conversion_table.pas',
  rm2_turbu_animations in '..\turbu format\converters\rm2_turbu_animations.pas',
  rm2_turbu_characters in '..\turbu format\converters\rm2_turbu_characters.pas',
  rm2_turbu_database in '..\turbu format\converters\rm2_turbu_database.pas',
  rm2_turbu_items in '..\turbu format\converters\rm2_turbu_items.pas',
  rm2_turbu_resists in '..\turbu format\converters\rm2_turbu_resists.pas',
  rm2_turbu_skills in '..\turbu format\converters\rm2_turbu_skills.pas',
  rm2_turbu_sounds in '..\turbu format\converters\rm2_turbu_sounds.pas',
  xyz_lib in '..\xyz_lib.pas',
  archiveInterface in '..\archiveInterface.pas',
  battle_anims in '..\battle_anims.pas',
  BER in '..\BER.pas',
  charset_data in '..\charset_data.pas',
  chipset_data in '..\chipset_data.pas',
  commons in '..\commons.pas',
  condition_data in '..\condition_data.pas',
  discInterface in '..\discInterface.pas',
  events in '..\events.pas',
  fileIO in '..\fileIO.pas',
  formats in '..\formats.pas',
  hero_data in '..\hero_data.pas',
  item_data in '..\item_data.pas',
  LDB in '..\LDB.pas',
  LMT in '..\LMT.pas',
  LMU in '..\LMU.pas',
  locate_files in '..\locate_files.pas',
  logs in '..\logs.pas',
  rm_sound in '..\rm_sound.pas',
  skill_data in '..\skill_data.pas',
  strtok in '..\strtok.pas',
  attributes_editor in '..\editor\attributes_editor.pas' {frmAttributesEditor},
  database in '..\editor\database.pas' {frmDatabase},
  design_script_engine in '..\editor\design_script_engine.pas',
  dm_database in '..\editor\dm_database.pas' {dmDatabase: TDataModule},
  frame_class in '..\editor\frame_class.pas' {frameClass: TFrame},
  frame_commands in '..\editor\frame_commands.pas' {frameHeroCommands: TFrame},
  frame_items in '..\editor\frame_items.pas' {frameItems: TFrame},
  frame_params in '..\editor\frame_params.pas' {frameParams: TFrame},
  generic_algorithm_editor in '..\editor\generic_algorithm_editor.pas' {frmAlgorithmEditor},
  skill_settings in '..\editor\skill_settings.pas' {frmSkillLearning},
  turbu_containers in '..\turbu format\turbu_containers.pas',
  turbu_heroes in '..\engines\map engine\turbu_heroes.pas',
  function_header in '..\editor\function_header.pas' {frmFuncHeader},
  rm2_turbu_map_metadata in '..\turbu format\converters\rm2_turbu_map_metadata.pas',
  conversion_report in '..\turbu format\converters\conversion_report.pas',
  conversion_report_form in '..\turbu format\converters\conversion_report_form.pas' {frmConversionReport},
  rm2_turbu_converter_thread in '..\turbu format\converters\rm2_turbu_converter_thread.pas',
  rm2_turbu_maps in '..\turbu format\converters\rm2_turbu_maps.pas',
  turbu_maps in '..\turbu format\turbu_maps.pas',
  turbu_tilesets in '..\turbu format\database\turbu_tilesets.pas',
  rm2_turbu_tilesets in '..\turbu format\converters\rm2_turbu_tilesets.pas',
  tiles in '..\engines\map engine\tiles.pas',
  turbu_2k_map_engine in '..\engines\map engine\turbu_2k_map_engine.pas',
  turbu_2k_sprite_engine in '..\engines\map engine\turbu_2k_sprite_engine.pas',
  map_tree_controller in '..\editor\map_tree_controller.pas',
  test_map_tree in 'test_map_tree.pas' {frmMapTree},
  ps_pointer in '..\engines\basis\ps_pointer.pas';

{$R *.res}
{$R 'turbures.res' '..\turbures.rc'}

begin
   ReportMemoryLeaksOnShutdown := true;
   if (SDL_Init(SDL_INIT_VIDEO) <> 0) then
      raise EParseMessage.create('Unable to initialize graphics converter.');
   Application.Initialize;
   Application.MainFormOnTaskbar := True;
   Application.CreateForm(TdmDatabase, dmDatabase);
  Application.CreateForm(TfrmTestConsole, frmTestConsole);
  Application.CreateForm(TfrmDatabase, frmDatabase);
  Application.CreateForm(TfrmFuncHeader, frmFuncHeader);
  Application.CreateForm(TfrmAlgorithmEditor, frmAlgorithmEditor);
  Application.CreateForm(TfrmSkillLearning, frmSkillLearning);
  Application.CreateForm(TfrmAttributesEditor, frmAttributesEditor);
  Application.CreateForm(TfrmTestProjLocation, frmTestProjLocation);
  Application.CreateForm(TfrmConversionReport, frmConversionReport);
  Application.Run;
end.
