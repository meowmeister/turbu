unit frame_items;
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
  SysUtils, Classes, Controls, Forms, DB, Grids, DBGrids, ExtCtrls,
  dm_database, StdCtrls;

type
   {This frame was originally used to inspect the items table of the database
   now, for the time being at least, it's a general-purpose table viewer
   It will be turned into a proper item frame soon enough.}

   TframeItems = class(TFrame)
      pnlItems: TPanel;
      DBGrid1: TDBGrid;
      dsWeapons: TDataSource;
      cboDatasets: TComboBox;
    procedure cboDatasetsChange(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
   private
      { Private declarations }
   public
      { Public declarations }
      procedure onShow;
   end;

implementation

uses
   DBClient, typInfo, turbu_defs;

{$R *.dfm}

{ TframeItems }

procedure TframeItems.btnSaveClick(Sender: TObject);
var
   dataset: TClientDataset;
begin
   dataset := dsWeapons.dataset as TClientDataSet;
   dataset.SaveToFile('c:\dataset_' + dataset.Name + '.xml', dfXMLUTF8);
end;

procedure TframeItems.cboDatasetsChange(Sender: TObject);
begin
   dsWeapons.DataSet := dmDatabase.FindComponent(cboDatasets.Text) as TDataSet;
end;

procedure TframeItems.onShow;
var
   iterator: TDataset;
begin
   for iterator in dmDatabase.datasets do
      cboDatasets.Items.Add(iterator.Name);
   cboDatasets.ItemIndex := 0;
   cboDatasetsChange(self)
end;

end.
