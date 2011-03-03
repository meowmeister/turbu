unit eval;

interface

uses
  SysUtils, Forms, Menus, ExtCtrls, StdCtrls, ComCtrls, Controls, Grids, Mask,
  Graphics, DBGrids, Classes, // system libs
  turbu_map_metadata, turbu_maps, dm_database,
  turbu_listGrid, DBIndexComboBox, // custom controls
  JvDBSpinEdit, DBCtrls, JvExMask, JvSpin, JvExControls, JvDBLookup,
  DB, DBClient; //JVCL

type
   TfrmMapProperties = class(Tform)
      pnlBack: TPanel;
      grpMonsters: TGroupBox;
      imgBG: TImage;
      lbHPanlSpeed: TLabel;
      txtName: TDBEdit;
      txtBGM: TDBEdit;
      txtBattleBG: TDBEdit;
      txtBGName: TDBEdit;
      spnWidth: TJvDBSpinEdit;
      spnHeight: TJvDBSpinEdit;
      spnVar1: TJvDBSpinEdit;
      spnVar2: TJvDBSpinEdit;
      spnVar3: TJvDBSpinEdit;
      spnVar4: TJvDBSpinEdit;
      spnHPanSpeed: TJvDBSpinEdit;
      spnVPanSpeed: TJvDBSpinEdit;
      grdMonsterParties: TRpgListGrid;
      cboTileset: TDBLookupComboBox;
      cboEncounterScript: TDBLookupComboBox;
      cboWraparoundType: TDBIndexComboBox;
      cboTeleport: TDBIndexComboBox;
      cboEscape: TDBIndexComboBox;
      cboSave: TDBIndexComboBox;
      cboBGM: TDBIndexComboBox;
      cboBattleBG: TDBIndexComboBox;
      cboHPan: TDBIndexComboBox;
      cboVPan: TDBIndexComboBox;
      lblVar1: TDBText;
      lblVar2: TDBText;
      lblVar3: TDBText;
      lblVar4: TDBText;
      chkUseBG: TDBCheckBox;
      btnSet: TButton;
      btnSetBGMTo: TButton;
      btnSetBGTo: TButton;
      btnOK: TButton;
      btnCancel: TButton;
      btnApply: TButton;
      btnHelp: TButton;
      srcMetadata: TDataSource;
      dsMap: TClientDataSet;
      srcMap: TDataSource;
      dsMapid: TIntegerField;
      dsMapname: TStringField;
      dsMapmodified: TBooleanField;
      dsMapdepth: TByteField;
      dsMaptileMap: TBlobField;
      srcTileset: TDataSource;
      srcScript: TDataSource;
      StaticText1: TStaticText;
      procedure FormShow(Sender: TObject);
      procedure btnApplyClick(Sender: TObject);
      procedure FormCreate(Sender: TObject);
      procedure btnOKClick(Sender: TObject);
   private
      FMetadata: TMapMetadata;
      FMap: TRpgMap;
   public
      class function EditMap(metadata: TMapMetadata; map: TRpgMap): boolean;
   end;


implementation
uses
   sg_defs, turbu_database;

var
   frmMapProperties: TfrmMapProperties;

{$R *.dfm}

{ TfrmMapProperties }

procedure TfrmMapProperties.btnApplyClick(Sender: TObject);
begin
   FMap.download(dsMap);
end;

procedure TfrmMapProperties.btnOKClick(Sender: TObject);
begin
   FocusControl(btnOK);
end;

class function TfrmMapProperties.EditMap(metadata: TMapMetadata; map: TRpgMap): boolean;
var
   ds: TDataset;
   dimensions: TSgPoint;
begin
   assert(frmMapProperties = nil);
   frmMapProperties := Self.Create(nil);
   try
      frmMapProperties.FMetadata := metadata;
      frmMapProperties.FMap := map;
      ds := frmMapProperties.srcMetadata.DataSet;
      ds.DisableControls;
      try
         ds.Filter := format('id = %d', [metadata.id]);
         if ds.RecordCount = 0 then
            metadata.upload(ds);
         map.upload(frmMapProperties.dsMap);
         dimensions := map.size;
         GDatabase.copyTypeToDB(dmDatabase, rd_tileset);
      finally
         ds.EnableControls;
      end;
      result := frmMapProperties.ShowModal = mrOK;
      if result then
      begin
         frmMapProperties.btnApplyClick(frmMapProperties.btnOK);
         map.adjustSize(sgPoint(trunc(frmMapProperties.spnWidth.Value),
                                trunc(frmMapProperties.spnHeight.value)), 7);
      end;
   finally
      freeAndNil(frmMapProperties);
   end;
end;

procedure TfrmMapProperties.FormCreate(Sender: TObject);
var
   ds: TClientDataset;
begin
   //interpose a cloned dataset so the filtering in FormShow doesn't affect
   //the original
   ds := TClientDataset.Create(self);
   ds.CloneCursor(dmDatabase.metadata, false);
   srcMetadata.DataSet := ds;
   ds.name := dmDatabase.metadata.name;
end;

procedure TfrmMapProperties.FormShow(Sender: TObject);
var
   ds: TDataset;
   field: TField;
   val: integer;
begin
   imgBG.Canvas.Brush.Color := clBlack;
   imgBG.Canvas.FillRect(rect(0, 0, imgBG.Width, imgBG.Height));
   ds := srcMap.DataSet;
   field := ds.FieldByName('tileset');
   val := field.AsInteger;
   ds.Edit;
   field.AsInteger := 9999;
   ds.Post;
   ds.Edit;
   field.AsInteger := val;
   ds.Post;
end;

end.
