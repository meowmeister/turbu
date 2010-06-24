unit MapObject_Editor;

interface

uses
   Windows, SysUtils, Classes, Controls, Forms, DB, DBClient, StdCtrls, ExtCtrls,
   ComCtrls, DBCtrls, Messages, DBIndexComboBox, sdl_frame, Mask, EBListView,
   turbu_tilesets, turbu_map_objects, turbu_serialization, turbu_constants,
   turbu_maps, frame_conditions, dataset_viewer, SDL_ImageManager;

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
    trvEvents: TEBTreeView;
    Button1: TButton;
    dsPagesEventText: TWideMemoField;
    btnScript: TButton;
    procedure tabEventPagesChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure imgEventSpriteAvailable(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure SetDirty(DataSet: TDataSet);
    procedure btnSetImageClick(Sender: TObject);
    procedure imgEventSpriteKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure btnNewClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnCopyClick(Sender: TObject);
    procedure btnPasteClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnScriptClick(Sender: TObject);
  private
    procedure WMRender(var message: TMessage); message WM_RENDER;
    procedure OnClipboardChange(Sender: TObject);
  private
    { Private declarations }
    FViewer: TfrmDatasetViewer;
    FSerializer: TDatasetSerializer;
    FTileset: TTileset;
    FMapObject: TRpgMapObject;
    FMap: TRpgMap;

    procedure UploadMapObject(obj: TRpgMapObject);
    procedure DownloadMapObject(obj: TRpgMapObject);
    procedure ClearViewer(sender: TObject);
    function GetSpriteIndex(name: string): integer;
    function DoEdit(obj: TRpgMapObject; const tilesetName: string): integer;
    procedure DisableDatasets;
    procedure EnableDatasets;
    procedure AdjustRemainingEntries(incIDs: boolean);
    procedure CheckDeleteEnabled;
    procedure InsertPage(page: TRpgEventPage);
  public
    { Public declarations }
    class procedure EditMapObject(obj: TRpgMapObject; map: TRpgMap; const tilesetName: string);
    class function NewMapObject(id: integer; const tilesetName: string): TRpgMapObject;
  end;

implementation
uses
   clipbrd,
   sprite_selector, ClipboardWatcher,
   commons, turbu_tbi_lib,
   dm_database, turbu_database, archiveInterface, turbu_sdl_image, EB_RpgScript,
   sdl_13, sg_defs;

{$R *.dfm}

var
   eventClipFormat: cardinal;

{ TfrmObjectEditor }

procedure TfrmObjectEditor.btnApplyClick(Sender: TObject);
begin
   DownloadMapObject(FMapObject);
   btnApply.Enabled := false;
end;

procedure TfrmObjectEditor.btnCopyClick(Sender: TObject);
var
   page: TRpgEventPage;
begin
   page := TRpgEventPage.Create(nil, 0);
   try
      FSerializer.download(page, dsPages);
      FSerializer.download(page.conditionBlock, frameConditions.dsConditions);
      page.CopyToClipboard;
   finally
      page.Free;
   end;
end;

procedure TfrmObjectEditor.btnPasteClick(Sender: TObject);
begin
   InsertPage(TRpgEventPage.PasteFromClipboard);
end;

procedure TfrmObjectEditor.DisableDatasets;
begin
   dsPages.DisableControls;
   frameConditions.dsConditions.DisableControls;
   frameConditions.dsConditions.MasterSource := nil;
end;

procedure TfrmObjectEditor.EnableDatasets;
begin
   dsPages.EnableControls;
   frameConditions.dsConditions.EnableControls;
   frameConditions.dsConditions.MasterSource := srcPages;
end;

procedure TfrmObjectEditor.AdjustRemainingEntries(incIDs: boolean);
begin
   while not dsPages.Eof do
   begin
      assert(frameConditions.dsConditions.Locate('Master', dsPagesID.Value, []));
      frameConditions.dsConditions.Edit;
      if incIDs then
         frameConditions.dsConditionsMaster.Value := frameConditions.dsConditionsMaster.Value + 1
      else frameConditions.dsConditionsMaster.Value := frameConditions.dsConditionsMaster.Value - 1;
      frameConditions.dsConditions.Post;
      dsPages.Edit;
      if incIDs then
         dsPagesID.Value := dsPagesID.Value + 1
      else dsPagesID.Value := dsPagesID.Value - 1;
      dsPages.Next;
   end;
end;

procedure TfrmObjectEditor.btnDeleteClick(Sender: TObject);
begin
   assert(tabEventPages.Tabs.Count > 1);
   DisableDatasets;
   try
      dsPages.Delete;
      frameConditions.dsConditions.Delete;
      AdjustRemainingEntries(false);
   finally
      EnableDatasets;
   end;
   tabEventPages.Tabs.Delete(tabEventPages.Tabs.Count - 1);
   CheckDeleteEnabled;
   btnApply.Enabled := true;
end;

procedure TfrmObjectEditor.InsertPage(page: TRpgEventPage);
var
   id: integer;
begin
   DisableDatasets;
   try
      id := dsPagesId.Value + 1;
      page.id := id;
      dsPages.Next;
      AdjustRemainingEntries(true);
      FSerializer.upload(page, dsPages);
      FSerializer.upload(page.conditionBlock, frameConditions.dsConditions);
      frameConditions.dsConditions.Edit;
      frameConditions.dsConditionsMaster.Value := id;
      frameConditions.dsConditions.Post;
      tabEventPages.Tabs.Add(intToStr(FMapObject.pages.Count));
      tabEventPages.TabIndex := id;
      tabEventPagesChange(self);
   finally
      EnableDatasets;
      page.Free;
   end;
   CheckDeleteEnabled;
   btnApply.Enabled := true;
end;

procedure TfrmObjectEditor.btnNewClick(Sender: TObject);
var
   id: integer;
   page, newpage: TRpgEventPage;
begin
   id := dsPagesId.Value;
   page := FMapObject.pages[id];
   newpage := TRpgEventPage.Create(nil, 0);
   newpage.name := page.name;
   newpage.direction := page.direction;
   newpage.whichTile := page.whichTile;
   InsertPage(newpage);
end;

procedure TfrmObjectEditor.btnScriptClick(Sender: TObject);
begin
   Application.MessageBox(PChar(self.trvEvents.proc.GetScript(0)), 'Script');
end;

procedure TfrmObjectEditor.btnSetImageClick(Sender: TObject);
var
   filename: string;
   frame: integer;
begin
   if dsPagesName.Value = '' then
      GetSpriteIndex('');
   filename := dsPagesName.Value;
   frame := dsPagesFrame.Value;
   TfrmSpriteSelector.SelectSprite(FTileset, filename, frame);
   SDL_SelectRenderer(imgEventSprite.SdlWindow);
   dsPages.Edit;
   dsPagesName.Value := filename;
   dsPagesFrame.Value := frame;
   dsPages.Post;
   GetSpriteIndex(filename);
end;

procedure TfrmObjectEditor.Button1Click(Sender: TObject);
begin
   if not assigned(FViewer) then
   begin
      FViewer := TFrmDatasetViewer.Create(self);
      FViewer.OnDestroy := self.ClearViewer;
      FViewer.DBGrid1.DataSource := srcPages;
      FViewer.DBGrid2.DataSource := frameConditions.srcConditions;
      FViewer.DBMemo1.DataSource := srcPages;
      FViewer.DBMemo1.DataField := 'EventText';
      FViewer.DBNavigator1.DataSource := srcPages;
      FViewer.Show;
   end;
end;

procedure TfrmObjectEditor.ClearViewer(sender: TObject);
begin
   FViewer := nil;
end;

procedure TfrmObjectEditor.FormCreate(Sender: TObject);
begin
   FSerializer := TDatasetSerializer.Create;
   Button1.Visible := DebugHook <> 0;
   ClipboardWatcher.RegisterClipboardViewer(self.OnClipboardChange);
end;

procedure TfrmObjectEditor.FormDestroy(Sender: TObject);
begin
   ClipboardWatcher.UnregisterClipboardViewer(self.OnClipboardChange);
   FSerializer.Free;
end;

procedure TfrmObjectEditor.FormShow(Sender: TObject);
begin
   Self.OnClipboardChange(self);
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
   if name = '' then
   begin
      name := '*' + intToStr(FTileset.Records.firstIndexWhere(turbu_tilesets.upperLayerFilter));
      dsPages.Edit;
      dsPagesName.Value := name;
      dsPages.Post;
   end;
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
         image := TRpgSdlImage.CreateSprite(loadFromTBI(stream), name, imgEventSprite.Images);
         assert(image.Texture.ID > 0);
      finally
         stream.Free;
      end;
   end
   else image := imgEventSprite.Images.Image[name] as TRpgSdlImage;
   result := imgEventSprite.IndexOfName(name);

   spriteRect := image.spriteRect[dsPagesframe.Value];
   destRect.left := (imgEventSprite.Width div 2) - (spriteRect.right);
   destRect.top := (imgEventSprite.height div 2) - (spriteRect.bottom);
   destRect.BottomRight := TSgPoint(spriteRect.BottomRight) * 2;
   imgEventSprite.fillColor(image.surface.Format.palette.colors[image.surface.ColorKey], 255);
   imgEventSprite.DrawTexture(image.Texture, @spriteRect, @destRect);
   imgEventSprite.Flip;
end;

procedure TfrmObjectEditor.CheckDeleteEnabled;
begin
   btnDelete.Enabled := tabEventPages.Tabs.Count > 1;
end;

procedure TfrmObjectEditor.imgEventSpriteAvailable(Sender: TObject);
begin
   PostMessage(self.Handle, WM_RENDER, 0, 0);
end;

procedure TfrmObjectEditor.imgEventSpriteKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
   outputDebugString(PChar(format('Key pressed: %d', [key])));
end;

procedure TfrmObjectEditor.tabEventPagesChange(Sender: TObject);
begin
   dsPages.Locate('id', tabEventPages.TabIndex, []);
   GetSpriteIndex(dsPagesName.Value);
   if dsPagesEventText.BlobSize > 0 then
      trvEvents.proc := FMap.ScriptObject.FindComponent(dsPagesEventText.Value) as TEBProcedure;
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
   CheckDeleteEnabled;
end;

procedure TfrmObjectEditor.DownloadMapObject(obj: TRpgMapObject);
var
   page: TRpgEventPage;
   last: integer;
begin
   DisableDatasets;
   last := 0;
   try
      dsPages.First;
      frameConditions.dsConditions.First;
      while not dsPages.Eof do
      begin
         assert(frameConditions.dsConditions.Locate('Master', dsPagesID.Value, []));
         last := dsPagesID.Value;
         if last >= obj.pages.Count then
            obj.AddPage(TRpgEventPage.Create(obj, last));
         page := obj.pages[last];
         FSerializer.download(page, dsPages);
         FSerializer.download(page.conditionBlock, frameConditions.dsConditions);
         dsPages.Next;
         frameConditions.dsConditions.Next;
      end;
      obj.name := txtName.Text;
      inc(last);
      if last < obj.pages.Count then
         obj.pages.DeleteRange(last, obj.pages.Count - last);
   finally
      EnableDatasets;
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

function TfrmObjectEditor.DoEdit(obj: TRpgMapObject; const tilesetName: string): integer;
begin
   UploadMapObject(obj);
   FTileset := GDatabase.tileset.firstWhere(
      function(arg1: TTileset): boolean
      begin
         result := arg1.name = tilesetName;
      end);

   result := self.ShowModal;
   if result = mrOK then
      DownloadMapObject(obj);
end;

class procedure TfrmObjectEditor.EditMapObject(obj: TRpgMapObject; map: TRpgMap; const tilesetName: string);
var
   form: TfrmObjectEditor;
begin
   form := TfrmObjectEditor.Create(nil);
   try
      form.FMap := map;
      form.DoEdit(obj, tilesetName);
   finally
      form.Free;
   end;
end;

class function TfrmObjectEditor.NewMapObject(id: integer;
  const tilesetName: string): TRpgMapObject;
var
   form: TfrmObjectEditor;
begin
   form := TfrmObjectEditor.Create(nil);
   result := TRpgMapObject.Create(id);
   result.name := format('Map%.4d', [id]);
   try
      if form.DoEdit(result, tilesetName) <> mrOK then
         FreeAndNil(result);
   finally
      form.Free;
   end;
end;

procedure TfrmObjectEditor.OnClipboardChange(Sender: TObject);
begin
   btnPaste.Enabled := clipboard.HasFormat(eventClipFormat);
end;

initialization
   eventClipFormat := RegisterClipboardFormat('CF_TRpgEventPage');
   if eventClipFormat = 0 then
      RaiseLastOSError;

end.
