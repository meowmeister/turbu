unit MapObject_Editor;

interface

uses
   Classes, Forms, DB, DBClient, StdCtrls, ExtCtrls, ComCtrls, DBCtrls,
   Messages, DBIndexComboBox, Mask, Controls,
   sdl_frame, EBListView,
   turbu_tilesets, turbu_map_objects, turbu_serialization, turbu_constants,
   turbu_maps, frame_conditions, dataset_viewer, sdl_frame_helper,
   SDL_ImageManager;

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
    dsPagesMatrix: TWordField;
    dsContext: TClientDataSet;
    dsContextName: TWideStringField;
    dsContextType: TWideStringField;
    dsContextid: TAutoIncField;
    dsContextDisplayName: TWideStringField;
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
    procedure tabEventPagesChanging(Sender: TObject; var AllowChange: Boolean);
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
    FRenameProc: TRenameProc;

    procedure UploadMapObject(obj: TRpgMapObject);
    procedure DownloadMapObject(obj: TRpgMapObject);
    procedure ClearViewer(sender: TObject);
    function DoEdit(obj: TRpgMapObject; const tilesetName: string): integer;
    procedure DisableDatasets;
    procedure EnableDatasets;
    procedure AdjustRemainingEntries(incIDs: boolean);
    procedure CheckDeleteEnabled;
    procedure InsertPage(page: TRpgEventPage);
    function Procname(id: integer): string;
    procedure PostProc;
  public
    { Public declarations }
    class procedure EditMapObject(obj: TRpgMapObject; map: TRpgMap; const tilesetName: string);
    class function NewMapObject(id: integer; const tilesetName: string): TRpgMapObject;
  end;

implementation
uses
   Windows, Clipbrd, SysUtils,
   sprite_selector, ClipboardWatcher,
   commons, turbu_tbi_lib, dm_database, turbu_database, archiveInterface,
   turbu_sdl_image, EB_RpgScript, EventBuilder,
   sdl_13, sg_defs;

{$R *.dfm}

var
   eventClipFormat: cardinal;

{ TfrmObjectEditor }

procedure TfrmObjectEditor.btnApplyClick(Sender: TObject);
begin
   PostProc;
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
      page.scriptName := trvEvents.proc.Serialize;
      page.CopyToClipboard;
   finally
      page.Free;
   end;
end;

procedure TfrmObjectEditor.btnPasteClick(Sender: TObject);
var
   page: TRpgEventPage;
   script: string;
begin
   page := TRpgEventPage.PasteFromClipboard;
   script := page.scriptName;
   InsertPage(page);
   dsPages.Edit;
   dsPagesEventText.Value := script;
   dsPages.Post;
   tabEventPagesChange(self);
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
   proc: TEBProcedure;
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
      dsPages.Edit;
      proc := TEBProcedure.Create(nil);
      dsPagesEventText.Value := proc.Serialize;
      proc.Free;
      dsPages.Post;

      tabEventPages.Tabs.Add(IntToStr(tabEventPages.Tabs.Count + 1));
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
   filename := dsPagesName.Value;
   frame := dsPagesFrame.Value;
   TfrmSpriteSelector.SelectSpriteInto(imgEventSprite, filename, frame, FTileset, FRenameProc);
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
   FRenameProc :=
     procedure(const name: string)
     begin
        dsPages.Edit;
        dsPagesName.Value := name;
        dsPages.Post;
     end;
end;

procedure TfrmObjectEditor.FormDestroy(Sender: TObject);
begin
   ClipboardWatcher.UnregisterClipboardViewer(self.OnClipboardChange);
   FSerializer.Free;
   trvEvents.proc.Free;
end;

procedure TfrmObjectEditor.FormShow(Sender: TObject);
begin
   Self.OnClipboardChange(self);
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
   imgEventSprite.SetSprite(dsPagesName.Value, dsPagesFrame.Value, FTileset, FRenameProc);
   assert(dsPagesEventText.BlobSize > 0);
   trvEvents.proc.Free;
   trvEvents.proc := TEBObject.Load(dsPagesEventText.Value) as TEBProcedure;
end;

procedure TfrmObjectEditor.tabEventPagesChanging(Sender: TObject; var AllowChange: Boolean);
begin
   PostProc;
end;

procedure TfrmObjectEditor.UploadMapObject(obj: TRpgMapObject);
var
   page: TRpgEventPage;
   proc: TEBProcedure;
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
         proc := FMap.ScriptObject.FindComponent(page.scriptName) as TEBProcedure;
         dsPages.Edit;
         if assigned(proc) then
            dsPagesEventText.Value := proc.Serialize
         else begin
            proc := TEBProcedure.Create(nil);
            dsPagesEventText.Value := proc.Serialize;
            proc.Free;
         end;
         dsPages.Post;
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
   proc: TEBProcedure;
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
         FMap.ScriptObject.FindComponent(page.scriptName).Free;
         proc := TEBObject.Load(dsPagesEventText.Value) as TEBProcedure;
         if proc.ComponentCount > 0 then
         begin
            proc.Name := procname(page.id);
            FMap.ScriptObject.Add(proc);
            page.scriptName := proc.Name;
         end
         else begin
            page.scriptName := '';
            proc.Free;
         end;
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

procedure SetImage(sdlFrame: TSdlFrame; name: string; frame: integer; tileset: TTileset);
const
   FILENAME_STRING = '%s\%s.png';
var
   stream: TStream;
   image : TRpgSdlImage;
   size: TSgPoint;
   spriteRect, destRect: TRect;
begin
   if name = '' then
      name := '*' + intToStr(tileset.Records.firstIndexWhere(turbu_tilesets.upperLayerFilter));
   if name[1] = '*' then
   begin
      name := tileset.Records[strToInt(copy(name, 2, 3))].group.filename;
      name := format(FILENAME_STRING, ['tileset', name]);
      size := TILE_SIZE;
   end
   else begin
      name := format(FILENAME_STRING, ['mapsprite', name]);
      size := SPRITE_SIZE * sgPoint(1, 2);
   end;
   if not sdlFrame.ContainsName(name) then
   begin
      stream := GArchives[IMAGE_ARCHIVE].getFile(name);
      try
         image := TRpgSdlImage.CreateSprite(loadFromTBI(stream), name, sdlFrame.Images);
         assert(image.Texture.ID > 0);
      finally
         stream.Free;
      end;
   end
   else image := sdlFrame.Images.Image[name] as TRpgSdlImage;

   spriteRect := image.spriteRect[frame];
   destRect.left := (sdlFrame.Width div 2) - (spriteRect.right);
   destRect.top := (sdlFrame.height div 2) - (spriteRect.bottom);
   destRect.BottomRight := TSgPoint(spriteRect.BottomRight) * 2;
   sdlFrame.fillColor(image.surface.Format.palette.colors[image.surface.ColorKey], 255);
   sdlFrame.DrawTexture(image.Texture, @spriteRect, @destRect);
   sdlFrame.Flip;
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

procedure TfrmObjectEditor.PostProc;
begin
   dsPages.Edit;
   dsPagesEventText.Value := trvEvents.proc.Serialize;
   dsPages.Post;
end;

function TfrmObjectEditor.Procname(id: integer): string;
begin
   result := format('%s_page%d', [txtName.Text, id]);
end;

initialization
   eventClipFormat := RegisterClipboardFormat('CF_TRpgEventPage');
   if eventClipFormat = 0 then
      RaiseLastOSError;

end.
