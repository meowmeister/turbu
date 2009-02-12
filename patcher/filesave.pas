unit filesave;

interface

uses Forms, Controls, StdCtrls, ExtDlgs, Dialogs, Classes;

type
  TdlgSave = class(TForm)
    btnSave: TButton;
    btnCancel: TButton;
    Bevel1: TGroupBox;
    Label1: TLabel;
    btnSaveAs: TButton;
    dlgSaveAs: TSaveTextFileDialog;
    procedure btnSaveAsClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    function Run(var filename: string): boolean;
  end;

var
  dlgSave: TdlgSave;

implementation

{$R *.dfm}

{ TdlgSave }

procedure TdlgSave.btnSaveAsClick(Sender: TObject);
begin
   if dlgSaveAs.Execute then
      self.ModalResult := mrOk;
end;

function TdlgSave.Run(var filename: string): boolean;
begin
   dlgSaveAs.FileName := filename;
   if self.ShowModal = mrOk then
   begin
      filename := dlgSaveAs.FileName;
      result := true;
   end
   else result := false;
end;

end.
