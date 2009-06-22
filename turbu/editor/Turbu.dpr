program Turbu;

uses
  SimpleShareMem,
  Forms,
  dialogs,
  SysUtils,
  sdl,
  sdl_canvas,
  types,
  LMT in '..\LMT.pas',
  fileIO in '..\fileIO.pas',
  commons in '..\commons.pas',
  LDB in '..\LDB.pas',
  BER in '..\BER.pas',
  chipset in '..\chipset.pas',
  LMU in '..\LMU.pas',
  skill_data in '..\skill_data.pas',
  condition_data in '..\condition_data.pas',
  battle_anims in '..\battle_anims.pas',
  events in '..\events.pas',
  database in 'database.pas' {frmDatabase},
  turbu_main in 'turbu_main.pas' {frmTurbuMain},
  formats in '..\formats.pas',
  rm_converter in 'rm_converter.pas' {frmRmConverter},
  archiveInterface in '..\archiveInterface.pas',
  rm2_turbu_characters in '..\turbu format\converters\rm2_turbu_characters.pas',
  turbu_characters in '..\turbu format\database\turbu_characters.pas',
  turbu_constants in '..\turbu format\turbu_constants.pas',
  turbu_database in '..\turbu format\database\turbu_database.pas',
  skill_settings in 'skill_settings.pas' {frmSkillLearning},
  generic_algorithm_editor in 'generic_algorithm_editor.pas' {frmAlgorithmEditor},
  rm2_turbu_database in '..\turbu format\converters\rm2_turbu_database.pas',
  conversion_table in '..\turbu format\converters\conversion_table.pas',
  turbu_defs in '..\turbu format\turbu_defs.pas',
  turbu_sdl_image in '..\turbu format\turbu_sdl_image.pas',
  turbu_tbi_lib in '..\turbu format\turbu_tbi_lib.pas',
  turbu_db_var_arrays in '..\turbu format\database\turbu_db_var_arrays.pas',
  frame_commands in 'frame_commands.pas' {frameHeroCommands: TFrame},
  frame_class in 'frame_class.pas' {frameClass: TFrame},
  turbu_engines in '..\turbu format\turbu_engines.pas',
  turbu_sprites in '..\turbu format\turbu_sprites.pas',
  rm2_turbu_items in '..\turbu format\converters\rm2_turbu_items.pas',
  turbu_debugging in '..\debug\turbu_debugging.pas',
  turbu_items in '..\turbu format\database\turbu_items.pas',
  turbu_sounds in '..\turbu format\database\turbu_sounds.pas',
  rm2_turbu_sounds in '..\turbu format\converters\rm2_turbu_sounds.pas',
  rm2_turbu_skills in '..\turbu format\converters\rm2_turbu_skills.pas',
  logs in '..\logs.pas',
  turbu_animations in '..\turbu format\database\turbu_animations.pas',
  rm2_turbu_animations in '..\turbu format\converters\rm2_turbu_animations.pas',
  turbu_resists in '..\turbu format\database\turbu_resists.pas',
  rm2_turbu_resists in '..\turbu format\converters\rm2_turbu_resists.pas',
  turbu_classes in '..\turbu format\database\turbu_classes.pas',
  discInterface in '..\discInterface.pas',
  turbu_vartypes in '..\turbu format\turbu_vartypes.pas',
  frame_params in 'frame_params.pas' {frameParams: TFrame},
  dm_database in 'dm_database.pas' {dmDatabase: TDataModulet},
  attributes_editor in 'attributes_editor.pas' {frmAttributesEditor},
  frame_items in 'frame_items.pas' {frameItems: TFrame},
  turbu_script_basis in '..\turbu format\turbu_script_basis.pas',
  design_script_engine in 'design_script_engine.pas',
  turbu_decl_utils in '..\turbu format\turbu_decl_utils.pas',
  turbu_unit_dictionary in '..\turbu format\turbu_unit_dictionary.pas',
  rpg_list in '..\mapview\libs\script engine\rpg_list.pas',
  script_backend in '..\mapview\libs\script engine\script_backend.pas',
  script_interface in '..\mapview\libs\script engine\script_interface.pas',
  uPSI_script_interface in '..\mapview\libs\script engine\uPSI_script_interface.pas',
  addition_sprite in '..\mapview\libs\map engine\addition_sprite.pas',
  timing in '..\mapview\libs\timing.pas',
  turbu_battle_engine in '..\turbu format\turbu_battle_engine.pas',
  turbu_versioning in '..\turbu format\turbu_versioning.pas',
  turbu_plugin_interface in '..\turbu format\plugins\turbu_plugin_interface.pas',
  DB in '..\..\vclpatch\DB.pas',
  findfile in '..\..\classes\findfile\findfile.pas',
  turbu_heroes in '..\engines\map engine\turbu_heroes.pas',
  function_header in 'function_header.pas' {frmFuncHeader},
  rm2_turbu_map_metadata in '..\turbu format\converters\rm2_turbu_map_metadata.pas',
  turbu_map_metadata in '..\turbu format\database\turbu_map_metadata.pas',
  turbu_maps in '..\turbu format\turbu_maps.pas',
  rm2_turbu_maps in '..\turbu format\converters\rm2_turbu_maps.pas',
  conversion_report_form in '..\turbu format\converters\conversion_report_form.pas' {frmConversionReport},
  conversion_report in '..\turbu format\converters\conversion_report.pas',
  turbu_skills in '..\turbu format\database\turbu_skills.pas',
  rm2_turbu_converter_thread in '..\turbu format\converters\rm2_turbu_converter_thread.pas',
  turbu_functional in '..\turbu_functional.pas',
  turbu_tilesets in '..\turbu format\database\turbu_tilesets.pas',
  rm2_turbu_tilesets in '..\turbu format\converters\rm2_turbu_tilesets.pas';

{$R *.res}
{$R 'turbures.res' '..\turbures.rc'}

{$WARN SYMBOL_PLATFORM OFF}
begin
   Application.Initialize;
   {$IFDEF DEBUG}
   ReportMemoryLeaksOnShutdown := DebugHook <> 0;
   {$ELSE IFDEF TEST_BUILD}
   ReportMemoryLeaksOnShutdown := true;
   {$ENDIF}
   Application.CreateForm(TfrmTurbuMain, frmTurbuMain);
  Application.CreateForm(TdmDatabase, dmDatabase);
  Application.CreateForm(TfrmDatabase, frmDatabase);
  Application.CreateForm(TfrmRmConverter, frmRmConverter);
  Application.CreateForm(TfrmSkillLearning, frmSkillLearning);
  Application.CreateForm(TfrmFuncHeader, frmFuncHeader);
  Application.CreateForm(TfrmAlgorithmEditor, frmAlgorithmEditor);
  Application.CreateForm(TfrmAttributesEditor, frmAttributesEditor);
  if (SDL_Init(SDL_INIT_VIDEO) <> 0) then
      raise Exception.create('Unable to initialize graphics converter.');
   {$IFDEF DEBUG}
   //Create an SDL window for testing images on.
   TSdlCanvas.Create(cmHardware, false, rect(100, 100, 640, 480), 32);
   {$ENDIF}
   Application.Run;
end.
