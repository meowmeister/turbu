unit MapObject_Editor;

interface

uses
   Windows, SysUtils, Classes, Controls, Forms, DB, DBClient, StdCtrls, ExtCtrls,
   ComCtrls, DBCtrls, Messages, DBIndexComboBox, sdl_frame,
   turbu_tilesets, turbu_map_objects, turbu_serialization, turbu_constants,
   frame_conditions, dataset_viewer, SDL_ImageManager, Mask;

type
  TfrmObjectEditor = class(TForm)
    pnlBackground: TPanel;
    grpName: TGroupBox;
    txtName: TEdit;
    btnNew: TButton;
    btnCopy: TButton;
    btnPaste: TButton;
    btnDelete: TButton;
    tabEventPages: TTabControl;
    gbxEventText: TGroupBox;
    btnOK: TButton;
    btnCancel: TButton;
    btnApply: TButton;
    btnHelp: TButton;
    Panel1: TPanel;
    gbxGraphic: TGroupBox;
    imgEventSprite: TSdlFrame;
    chkTransparent: TDBCheckBox;
    btnSetImage: TButton;
    gbxEventTrigger: TGroupBox;
    cbxEventTrigger: TDBIndexComboBox;
    gbxPosition: TGroupBox;
    cbxZOrder: TDBIndexComboBox;
    chkSolidEvent: TDBCheckBox;
    gbxAnimationStyle: TGroupBox;
    cbxAnimStyle: TDBIndexComboBox;
    gbxMoveSpeed: TGroupBox;
    cbxMoveSpeed: TDBIndexComboBox;
    gbxMoveData: TGroupBox;
    lblMoveFreq: TLabel;
    cbxMoveType: TDBIndexComboBox;
    cbxMoveFreq: TDBIndexComboBox;
    btnEditRoute: TButton;
    frameConditions: TframeConditions;
    dsPages: TClientDataSet;
    srcPages: TDataSource;
    dsPagesId: TIntegerField;
    dsPagesName: TWideStringField;
    dsPagesModified: TBooleanField;
    dsPagesframe: TWordField;
    dsPagesDirection: TByteField;
    ListView1: TListView;
    StaticText1: TStaticText;
    Button1: TButton;
    procedure tabEventPagesChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure imgEventSpriteAvailable(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure SetDirty(DataSet: TDataSet);
    procedure btnSetImageClick(Sender: TObject);
  private
    procedure WMRender(var message: TMessage); message WM_RENDER;
  private
    { Private declarations }
    FViewer: TfrmDatasetViewer;
    FSerializer: TDatasetSerializer;
    FTileset: TTileset;
    FImageList: TSdlImages;
    FMapObject: TRpgMapObject;

    procedure UploadMapObject(obj: TRpgMapObject);
    procedure DownloadMapObject(obj: TRpgMapObject);
    procedure ClearViewer(sender: TObject);
    function GetSpriteIndex(name: string): integer;
  public
    { Public declarations }
    class procedure EditMapObject(obj: TRpgMapObject; const tilesetName: string);
  end;

implementation
uses
   commons, turbu_tbi_lib, sdl_13,
   sprite_selector,
   dm_database, turbu_database, archiveInterface, turbu_sdl_image,
   sg_defs;

{$R *.dfm}

{ TfrmObjectEditor }

procedure TfrmObjectEditor.btnApplyClick(Sender: TObject);
begin
   DownloadMapObject(FMapObject);
   btnApply.Enabled := false;
end;

procedure TfrmObjectEditor.btnSetImageClick(Sender: TObject);
var
   filename: string;
   frame: integer;
begin
   filename := dsPagesName.Value;
   frame := dsPagesFrame.Value;
   TfrmSpriteSelector.SelectSprite(FTileset, filename, frame);
   dsPages.Edit;
   dsPagesName.Value := filename;
   dsPagesFrame.Value := frame;
   dsPages.Post;
end;

procedure TfrmObjectEditor.Button1Click(Sender: TObject);
begin
   if not assigned(FViewer) then
   begin
      FViewer := TFrmDatasetViewer.Create(self);
      FViewer.OnDestroy := self.ClearViewer;
      FViewer.DBGrid1.DataSource := srcPages;
      FViewer.DBGrid2.DataSource := frameConditions.srcConditions;
      FViewer.DBNavigator1.DataSource := srcPages;
      FViewer.Show;
   end;
end;

procedure TfrmObjectEditor.ClearViewer(sender: TObject);
begin
   FViewer := nil;
end;

class procedure TfrmObjectEditor.EditMapObject(obj: TRpgMapObject; const tilesetName: string);
var
   form: TfrmObjectEditor;
begin
   form := TfrmObjectEditor.Create(nil);
   try
      form.UploadMapObject(obj);
      form.FTileset := GDatabase.tileset.firstWhere(
         function(arg1: TTileset): boolean
         begin
            result := arg1.name = tilesetName;
         end);

      if form.ShowModal = mrOK then
         form.DownloadMapObject(obj);
   finally
      form.Free;
   end;
end;

procedure TfrmObjectEditor.FormCreate(Sender: TObject);
begin
   FSerializer := TDatasetSerializer.Create;
   FImageList := TSdlImages.Create;
   Button1.Visible := DebugHook <> 0;
end;

procedure TfrmObjectEditor.FormDestroy(Sender: TObject);
begin
   FSerializer.Free;
   FImageList.Free;
end;

function TfrmObjectEditor.GetSpriteIndex(name: string): integer;
const
   FILENAME_STRING = '%s\%s.png';
var
   stream: TStream;
   image : TRpgSdlImage;
   size: TSgPoint;
   spriteRect, destRect: TRect;
begin
   if name[1] = '*' then
   begin
      name := FTileset.Records[strToInt(copy(name, 2, 3))].group.filename;
      name := format(FILENAME_STRING, ['tileset', name]);
      size := TILE_SIZE;
   end
   else begin
      name := format(FILENAME_STRING, ['mapsprite', name]);
      size := SPRITE_SIZE * sgPoint(1, 2);
   end;
   if not imgEventSprite.ContainsName(name) then
   begin
      stream := GArchives[IMAGE_ARCHIVE].getFile(name);
      try
         image := TRpgSdlImage.CreateSprite(loadFromTBI(stream), name, FImageList);
         assert(image.Texture.ID > 0);
         imgEventSprite.AddTexture(image.surface, name);
      finally
         stream.Free;
      end;
   end
   else image := FImageList.Image[name] as TRpgSdlImage;
   result := imgEventSprite.IndexOfName(name);

   spriteRect := image.spriteRect[dsPagesframe.Value];
   destRect.left := (imgEventSprite.Width div 2) - (spriteRect.right);
   destRect.top := (imgEventSprite.height div 2) - (spriteRect.bottom);
   destRect.BottomRight := TSgPoint(spriteRect.BottomRight) * 2;
   imgEventSprite.fillColor(image.surface.Format.palette.colors[image.surface.ColorKey], 255);
   imgEventSprite.DrawTexture(image.Texture, @spriteRect, @destRect);
   imgEventSprite.Flip;
end;

procedure TfrmObjectEditor.imgEventSpriteAvailable(Sender: TObject);
begin
   PostMessage(self.Handle, WM_RENDER, 0, 0);
end;

procedure TfrmObjectEditor.tabEventPagesChange(Sender: TObject);
begin
   dsPages.Locate('id', tabEventPages.TabIndex, []);
   GetSpriteIndex(dsPagesName.Value);
end;

procedure TfrmObjectEditor.UploadMapObject(obj: TRpgMapObject);
var
   page: TRpgEventPage;
begin
   FMapObject := obj;
   dsPages.DisableControls;
   frameConditions.dsConditions.DisableControls;
   try
      GDatabase.copyToDB(dmDatabase, [rd_switch, rd_int, rd_hero, rd_item]);
      for page in obj.pages do
      begin
         FSerializer.upload(page, dsPages);
         FSerializer.upload(page.conditionBlock, frameConditions.dsConditions);
         tabEventPages.Tabs.Add(intToStr(tabEventPages.Tabs.Count + 1));
      end;
      txtName.Text := obj.name;
   finally
      dsPages.EnableControls;
      frameConditions.dsConditions.EnableControls;
   end;
   btnApply.Enabled := false;
end;

procedure TfrmObjectEditor.DownloadMapObject(obj: TRpgMapObject);
var
   page: TRpgEventPage;
begin
   dsPages.DisableControls;
   frameConditions.dsConditions.DisableControls;
   try
      dsPages.First;
      while not dsPages.Eof do
      begin
         page := obj.pages[dsPagesId.Value];
         FSerializer.download(page, dsPages);
         FSerializer.download(page.conditionBlock, frameConditions.dsConditions);
      end;
      obj.name := txtName.Text;
   finally
      dsPages.EnableControls;
      frameConditions.dsConditions.EnableControls;
   end;
end;

procedure TfrmObjectEditor.SetDirty(DataSet: TDataSet);
begin
   btnApply.Enabled := true;
end;

procedure TfrmObjectEditor.WMRender(var message: TMessage);
begin
   dsPages.First;
   tabEventPagesChange(self);
end;

end.
