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
      spnWidth: TJvDBSpinEdit;
      spnHeight: TJvDBSpinEdit;
      grdMonsterParties: TRpgListGrid;
      cboTileset: TDBLookupComboBox;
      cboTeleport: TDBIndexComboBox;
      lblVar1: TDBText;
      chkUseBG: TDBCheckBox;
      btnOK: TButton;
      srcMetadata: TDataSource;
      dsMap: TClientDataSet;
      srcMap: TDataSource;
      dsMaptileMap: TBlobField;
      StaticText1: TStaticText;
      metadata: TClientDataSet;
      metadataid: TIntegerField;
      metadataname: TWideStringField;
      metadataparent: TSmallintField;
      metadatatreeOpen: TBooleanField;
      metadatabgmState: TByteField;
      metadatamapEngine: TShortintField;
      metadata_regions: TClientDataSet;
      procedure FormShow(Sender: TObject);
      procedure btnApplyClick(Sender: TObject);
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
   FMap.download(GDatabase.serializer, dsMap);
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
         metadata.upload(GDatabase.serializer, ds);
         map.upload(GDatabase.serializer, frmMapProperties.dsMap);
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
