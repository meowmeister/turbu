program Turbu;

{%File 'specs.txt'}
{%ToDo 'Turbu.todo'}

uses
  Forms,
  dialogs,
  eval in 'eval.pas' {frmMapProperties},
  LMT in 'LMT.pas',
  fileIO in 'fileIO.pas',
  commons in 'commons.pas',
  LDB in 'LDB.pas',
  StreamRoutines in '..\classes\streamroutines\StreamRoutines.pas',
  UnframedRadioGroup in '..\components\Radio\UnframedRadioGroup.pas',
  BER in 'BER.pas',
  chipdata in 'chipdata.pas' {frmChipData},
  chipset in 'chipset.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMapProperties, frmMapProperties);
  Application.CreateForm(TfrmChipData, frmChipData);
  Application.Run;
end.
