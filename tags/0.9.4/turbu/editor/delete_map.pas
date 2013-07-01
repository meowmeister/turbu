unit delete_map;

interface

uses
  SysUtils, Controls, Forms, StdCtrls, ExtCtrls, Classes,
  turbu_map_engine;

type
  TdlgDeleteMap = class(TForm)
    lblDeleteMap: TLabel;
    RadioGroup1: TRadioGroup;
    Button1: TButton;
    Button2: TButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  function deleteMapConfirm(hasChildren: boolean): TDeleteMapMode;

implementation

{$R *.dfm}

var
  dlgDeleteMap: TdlgDeleteMap;

function deleteMapConfirm(hasChildren: boolean): TDeleteMapMode;
const
   YES_CHILDREN = 'Delete this map and...';
   NO_CHILDREN = 'Delete this map?';
begin
   dlgDeleteMap := TDlgDeleteMap.Create(nil);
   try
      case hasChildren of
         true: dlgDeleteMap.lblDeleteMap.Caption := YES_CHILDREN;
         false: dlgDeleteMap.lblDeleteMap.Caption := NO_CHILDREN;
      end;
      dlgDeleteMap.RadioGroup1.Enabled := hasChildren;
      if dlgDeleteMap.ShowModal = mrOK then
         result := TDeleteMapMode(dlgDeleteMap.RadioGroup1.ItemIndex)
      else result := dmNone;
   finally
      freeAndNil(dlgDeleteMap);
   end;
end;

end.
