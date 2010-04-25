unit dataset_viewer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, DBGrids, ExtCtrls, DBCtrls;

type
  TfrmDatasetViewer = class(TForm)
    DBGrid1: TDBGrid;
    DBGrid2: TDBGrid;
    DBNavigator1: TDBNavigator;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

end.
