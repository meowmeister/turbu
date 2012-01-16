program turbu_player;

{$R *.dres}

uses
  FastMM4 in '..\..\classes\FastMM4\FastMM4.pas',
  Forms,
  mainform in 'mainform.pas' {frmMain},
  dm_ProjectBoot in '..\editor\dm_ProjectBoot.pas' {dmProjectBoot: TDataModule},
  discInterface in '..\discInterface.pas';

{$R *.res}

begin
  {$IFDEF RELEASE}
  ReportMemoryLeaksOnShutdown := false;
  {$ENDIF}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
