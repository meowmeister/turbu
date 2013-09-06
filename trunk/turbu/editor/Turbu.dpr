program Turbu;

{$R *.dres}

uses
  FastMM4,
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  Forms,
  Variants,
  dialogs,
  SysUtils,
  MidasLib,
  sdl,
  sdl_canvas,
  types,
  dm_database,
  LMT in '..\LMT.pas',
  LDB in '..\LDB.pas',
  chipset in '..\chipset.pas',
  LMU in '..\LMU.pas',
  skill_data in '..\skill_data.pas',
  condition_data in '..\condition_data.pas',
  battle_anims in '..\battle_anims.pas',
  turbu_main in 'turbu_main.pas' {frmTurbuMain},
  rm_converter in 'rm_converter.pas' {frmRmConverter},
  rm2_turbu_characters in '..\turbu format\converters\rm2_turbu_characters.pas',
  rm2_turbu_database in '..\turbu format\converters\rm2_turbu_database.pas',
  conversion_table in '..\turbu format\converters\conversion_table.pas',
  rm2_turbu_items in '..\turbu format\converters\rm2_turbu_items.pas',
  rm2_turbu_sounds in '..\turbu format\converters\rm2_turbu_sounds.pas',
  rm2_turbu_skills in '..\turbu format\converters\rm2_turbu_skills.pas',
  rm2_turbu_animations in '..\turbu format\converters\rm2_turbu_animations.pas',
  rm2_turbu_resists in '..\turbu format\converters\rm2_turbu_resists.pas',
  discInterface in '..\discInterface.pas',
  frame_params in 'frame_params.pas' {frameParams: TFrame},
  findfile in '..\..\classes\findfile\findfile.pas',
  function_header in 'function_header.pas' {frmFuncHeader},
  rm2_turbu_map_metadata in '..\turbu format\converters\rm2_turbu_map_metadata.pas',
  rm2_turbu_maps in '..\turbu format\converters\rm2_turbu_maps.pas',
  rm2_turbu_converter_thread in '..\turbu format\converters\rm2_turbu_converter_thread.pas',
  turbu_functional in '..\turbu_functional.pas',
  rm2_turbu_tilesets in '..\turbu format\converters\rm2_turbu_tilesets.pas',
  delete_map in 'delete_map.pas' {dlgDeleteMap},
  rm2_turbu_map_objects in '..\turbu format\converters\rm2_turbu_map_objects.pas',
  rm2_turbu_event_builder in '..\turbu format\converters\rm2_turbu_event_builder.pas',
  dm_ProjectBoot in 'dm_ProjectBoot.pas' {dmProjectBoot: TDataModule},
  monster in '..\monster.pas',
  rm2_turbu_monsters in '..\turbu format\converters\rm2_turbu_monsters.pas',
  rm2_turbu_terrain in '..\turbu format\converters\rm2_turbu_terrain.pas';

{$R *.res}

{$WARN SYMBOL_PLATFORM OFF}
begin
   Application.Initialize;
   Variants.NullStrictConvert := false;
   {$IFDEF DEBUG}
   ReportMemoryLeaksOnShutdown := DebugHook <> 0;
   {$ELSE IFDEF TEST_BUILD}
   ReportMemoryLeaksOnShutdown := true;
   {$ENDIF}
   Application.CreateForm(TfrmTurbuMain, frmTurbuMain);
  if (SDL_Init(SDL_INIT_VIDEO) <> 0) then
      raise Exception.create('Unable to initialize graphics converter.');
   {$IFDEF DEBUG}
   //Create an SDL window for testing images on.
//   TSdlCanvas.Create(cmHardware, false, rect(100, 100, 640, 480), 32);
   {$ENDIF}
   Application.Run;
end.
