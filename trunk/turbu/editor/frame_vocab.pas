unit frame_vocab;

interface

uses
   Forms, StdCtrls, Grids, DB, DBGrids, Controls, ExtCtrls, Classes,
   turbu_listGrid, dm_database, DBCtrls;

type
  TframeVocab = class(TFrame)
    pnlVocab: TPanel;
    Splitter1: TSplitter;
    lstCustomVocab: TRpgListGrid;
    StaticText1: TStaticText;
    dsSysVocab: TDataSource;
    DBNavigator1: TDBNavigator;
  end;

implementation

{$R *.dfm}

end.
