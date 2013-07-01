unit test_map_tree;

interface

uses
  SysUtils, Controls, Forms, ComCtrls, Classes;

type
  TfrmMapTree = class(TForm)
    trvMapTree: TTreeView;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMapTree: TfrmMapTree;

implementation

{$R *.dfm}

procedure TfrmMapTree.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   self.Release;
end;

end.
