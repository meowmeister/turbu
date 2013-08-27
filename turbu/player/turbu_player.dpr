program turbu_player;

{$R *.dres}

uses
  FastMM4 in '..\..\classes\FastMM4\FastMM4.pas',
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  FastMM4Messages in '..\..\classes\FastMM4\FastMM4Messages.pas',
  Forms,
  Variants,
  mainform in 'mainform.pas' {frmMain},
  dm_ProjectBoot in '..\editor\dm_ProjectBoot.pas' {dmProjectBoot: TDataModule},
  discInterface in '..\discInterface.pas',
  findfile in '..\..\classes\findfile\findfile.pas';

{$R *.res}

begin
  Variants.NullStrictConvert := false;
  {$IFDEF RELEASE}
  ReportMemoryLeaksOnShutdown := false;
  {$ENDIF}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
