unit test_map_size;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Spin;

type
  TfrmTestMapSize = class(TForm)
    spnX: TSpinEdit;
    spnY: TSpinEdit;
    spnMode: TSpinEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    btnOK: TButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmTestMapSize: TfrmTestMapSize;

implementation

{$R *.dfm}

end.
