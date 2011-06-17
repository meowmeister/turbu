unit attributes_editor;
{*****************************************************************************
* The contents of this file are used with permission, subject to
* the Mozilla Public License Version 1.1 (the "License"); you may
* not use this file except in compliance with the License. You may
* obtain a copy of the License at
* http://www.mozilla.org/MPL/MPL-1.1.html
*
* Software distributed under the License is distributed on an
* "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
* implied. See the License for the specific language governing
* rights and limitations under the License.
*
*****************************************************************************
*
* This file was created by Mason Wheeler.  He can be reached for support at
* www.turbu-rpg.com.
*****************************************************************************}

interface

uses
  SysUtils, Classes, Controls, Forms, StdCtrls, Mask, DBCtrls,
  Grids, DBGrids, JvExDBGrids, JvDBGrid, JvDBUltimGrid, DB;

type
   TfrmAttributesEditor = class(TForm)
    DataSource: TDataSource;
      JvDBUltimGrid1: TJvDBUltimGrid;
    btnOK: TButton;
    btnCancel: TButton;
    procedure FormShow(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
   private
      FBookmark: integer;
   public
      { Public declarations }
   end;

implementation

uses
   DBClient,
   dm_database;

{$R *.dfm}

procedure TfrmAttributesEditor.btnOKClick(Sender: TObject);
begin
   (DataSource.DataSet as TClientDataset).SavePoint := FBookmark;
end;

procedure TfrmAttributesEditor.FormShow(Sender: TObject);
begin
   FBookmark := (dataSource.DataSet as TClientDataset).SavePoint;
end;

end.
