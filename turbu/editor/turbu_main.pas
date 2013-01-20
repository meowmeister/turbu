unit turbu_main;
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
   Controls, Classes, Forms, Menus, ExtCtrls, StdCtrls, ComCtrls, Dialogs,
   Generics.Collections, ImgList, ToolWin, ActnList, ActnMan, SyncObjs,
   PlatformDefaultStyleActnCtrls, Messages,
   JvPluginManager, JvExControls, JvxSlider,
   turbu_plugin_interface, turbu_engines, turbu_map_engine,
   MusicSelector, scrollbox_manager,
   turbu_map_interface, turbu_map_metadata, turbu_database_interface,
   sdl_frame, sdl, sdl_13, sg_defs;

type
   TfrmTurbuMain = class(TForm, ITurbuController, ITurbuControllerEx)
      mnuMain: TMainMenu;
      mnuDatabase: TMenuItem;
      dlgOpen: TOpenDialog;
      sbxPallette: TScrollBox;
      splSidebar: TSplitter;
      imgPalette: TSdlFrame;
      trvMapTree: TTreeView;
      sbPalette: TScrollBar;
      sbHoriz: TScrollBar;
      sbVert: TScrollBar;
      ilToolbarIcons: TImageList;
      mnuAutosaveMaps: TMenuItem;
      ToolBar2: TToolBar;
      btnLayer1: TToolButton;
      mnuTreePopup: TPopupMenu;
      mnuEditMapProperties: TMenuItem;
      mnuDeleteMap: TMenuItem;
      ActionManager: TActionManager;
      imgLogo: TSdlFrame;
      imgBackground: TPaintBox;
      actRun: TAction;
      actPause: TAction;
      pnlZoom: TPanel;
      sldZoom: TJvxSlider;
      actPlayMusic: TAction;
      Label1: TLabel;
      tbLayerSplitter: TToolButton;
      tbSaveSplitter: TToolButton;
      tbPlaySplitter: TToolButton;
      tbDrawMode: TToolBar;
      ilDrawTools: TImageList;
      tbPen: TToolButton;
      N2: TMenuItem;
      mnuUndo: TMenuItem;
      procedure mnu2KClick(Sender: TObject);
      procedure FormShow(Sender: TObject);
      procedure mnuDatabaseClick(Sender: TObject);
      procedure mnuExitClick(Sender: TObject);
      procedure mnuOpenClick(Sender: TObject);
      procedure FormCreate(Sender: TObject);
      procedure imgLogoAvailable(Sender: TObject);
      procedure FormDestroy(Sender: TObject);
      procedure sbPaletteScroll(Sender: TObject; ScrollCode: TScrollCode;
        var ScrollPos: Integer);
      procedure splSidebarMoved(Sender: TObject);
      procedure OnScrollMap(Sender: TObject; ScrollCode: TScrollCode;
        var ScrollPos: Integer);
      procedure imgPaletteMouseDown(Sender: TObject; Button: TMouseButton;
        Shift: TShiftState; X, Y: Integer);
      procedure imgPaletteMouseMove(Sender: TObject; Shift: TShiftState; X,
        Y: Integer);
      procedure imgLogoMouseDown(Sender: TObject; Button: TMouseButton;
        Shift: TShiftState; X, Y: Integer);
      procedure imgLogoMouseUp(Sender: TObject; Button: TMouseButton;
        Shift: TShiftState; X, Y: Integer);
      procedure imgLogoMouseMove(Sender: TObject; Shift: TShiftState; X,
        Y: Integer);
      procedure btnLayer1Click(Sender: TObject);
      procedure btnLayer2Click(Sender: TObject);
      procedure trvMapTreeChange(Sender: TObject; Node: TTreeNode);
      procedure mnuAutosaveMapsClick(Sender: TObject);
      procedure btnSaveClick(Sender: TObject);
      procedure btnSaveAllClick(Sender: TObject);
      procedure mnuAddNewMapClick(Sender: TObject);
      procedure mnuEditMapPropertiesClick(Sender: TObject);
      procedure trvMapTreeContextPopup(Sender: TObject; MousePos: TPoint;
        var Handled: Boolean);
      procedure mnuDeleteMapClick(Sender: TObject);
      procedure mnuTestbugrepsClick(Sender: TObject);
      procedure btnRunClick(Sender: TObject);
      procedure btnPauseClick(Sender: TObject);
      procedure btnMapObjClick(Sender: TObject);
      procedure imgLogoDblClick(Sender: TObject);
      procedure imgLogoKeyDown(Sender: TObject; var Key: Word;
        Shift: TShiftState);
      procedure imgLogoKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
      procedure imgBackgroundClick(Sender: TObject);
      procedure imgLogoPaint(Sender: TObject);
      procedure FormResize(Sender: TObject);
      procedure pnlZoomResize(Sender: TObject);
      procedure sldZoomChanged(Sender: TObject);
      procedure imgBackgroundPaint(Sender: TObject);
      procedure actPlayMusicExecute(Sender: TObject);
      procedure imgPalettePaint(Sender: TObject);
      procedure DrawToolButtonClick(Sender: TObject);
      procedure mnuUndoClick(Sender: TObject);
   private
      pluginManager: TJvPluginManager;
      FLogoIndex: integer;
      FScrollboxManager: TScrollboxManager;
      FMapEngine: IDesignMapEngine;
      FTileSize: TsgPoint;
      FCurrentLayer: integer;
      FLastGoodLayer: integer;
      FPaletteTexture: integer;
      FPaletteSelection: TRect;
      FPaletteSelectionTiles: TRect;
      FLastPalettePos: integer;
      FCurrPalettePos: integer;
      FPaletteImages: TDictionary<integer, integer>;
      FIgnoreMouseDown: boolean;
      FZoom: single;
      FMusicPlayer: TfrmMusicSelector;
      FDBName: string;
      procedure loadEngine(data: TEngineData);
      procedure openProject(const filename: string);
      procedure closeProject;

      procedure loadMap(const value: IMapMetadata);

      procedure assignPaletteImage(surface: PSdlSurface);
      procedure displayPalette(height: integer); overload;
      procedure displayPalette; overload; inline;
      procedure resizePalette;
      function calculatePaletteRect: TRect;
      procedure bindPaletteCursor;

      procedure setLayer(const value: integer);

      procedure RequireMapEngine;
      procedure enableRunButtons(running: boolean);
      procedure SetZoom(const value: single);
      procedure EnableZoom(value: boolean);
      procedure NormalizeMousePosition(image: TSdlFrame; var x, y: integer; scale: single);
   private //ITurbuController declarations
      function MapResize(const size: TSgPoint): TSgPoint;
      procedure TilesetChanged;
      procedure HandleTilesetChanged(var message); message WM_USER;
      procedure SetButton(button: TToolButton; position: TButtonPosition);
      procedure ClearMapMenuItem(item: TMenuItem);
      procedure SetMapMenuItem(item: TMenuItem);
      procedure UpdateEngine(const filename: string);
      function AddTileImage(il: TImageList; index: integer): integer;
   public
      { Public declarations }
   end;

var
  frmTurbuMain: TfrmTurbuMain;

implementation

uses
   SysUtils, Math, Graphics, Windows, OpenGL, IOUtils, ShellAPI,
   commons, rm_converter, archiveInterface, dm_ProjectBoot, engine_manager,
   turbu_constants, database, turbu_battle_engine, project_folder, logs,
   turbu_classes, turbu_versioning, turbu_defs, delayedAction,
   dm_database, discInterface, formats, map_tree_controller, delete_map,
   sdl_image, sdlstreams, sg_utils;

{$R *.dfm}

procedure TfrmTurbuMain.displayPalette(height: integer);

   procedure DrawPaletteCursor(const aRect: TRect);
   begin
      imgPalette.DrawRect(aRect, SDL_BLACK);
      imgPalette.DrawRect(constrictRect(aRect, 1), SDL_WHITE);
      imgPalette.DrawRect(constrictRect(aRect, 2), SDL_WHITE, $B0);
      imgPalette.DrawRect(constrictRect(aRect, 3), SDL_BLACK, $10);
   end;

var
   texture: TSdlTexture;
   displayRect: TRect;
begin
   if FPaletteTexture = -1 then
      Exit;

   texture := imgPalette.images[FPaletteTexture].surface;
   height := min(height, texture.size.Y - sbPalette.pageSize);
   FCurrPalettePos := height;
   displayRect := rect(0, height, imgPalette.logicalWidth, imgPalette.Logicalheight);
   imgPalette.drawTexture(texture, @displayRect, nil);
   drawPaletteCursor(calculatePaletteRect);
   imgPalette.Flip;
end;

procedure TfrmTurbuMain.displayPalette;
begin
   displayPalette(sbPalette.Position);
end;

procedure TfrmTurbuMain.enableRunButtons(running: boolean);
begin
   actRun.Enabled := not running;
   trvMapTree.Enabled := not running;
   actPause.Enabled := running;
//   btnStop.Enabled := running;
   enableZoom(not running);
end;

procedure TfrmTurbuMain.EnableZoom(value: boolean);
var
   i: integer;
begin
   for i := 0 to pnlZoom.ControlCount - 1 do
      pnlZoom.Controls[i].Enabled := value;
end;

procedure TfrmTurbuMain.resizePalette;
var
   texture: TSdlTexture;
begin
   texture := imgPalette.images[FPaletteTexture].surface;
   sbPalette.Max := round(texture.size.y / (imgPalette.Width / imgPalette.LogicalWidth));
   sbPalette.PageSize := imgPalette.LogicalHeight;
   sbPalette.LargeChange := sbPalette.PageSize - FTileSize.y;
   displayPalette;
end;

procedure TfrmTurbuMain.mnuEditMapPropertiesClick(Sender: TObject);
begin
   RequireMapEngine;
   fMapEngine.EditMapProperties(trvMapTree.currentMapID);
end;

procedure TfrmTurbuMain.mnuAddNewMapClick(Sender: TObject);
var
   newMap: IMapMetadata;
begin
   RequireMapEngine;
   newMap := FMapEngine.AddNewMap(trvMapTree.currentMapID);
   if assigned(newMap) then
      trvMapTree.addChildMap(newMap);
end;

procedure TfrmTurbuMain.mnuDeleteMapClick(Sender: TObject);
var
   deleteResult: TDeleteMapMode;
begin
   RequireMapEngine;
   deleteResult := delete_map.deleteMapConfirm(trvMapTree.Selected.HasChildren);
   if deleteResult <> dmNone then
   begin
      FMapEngine.DeleteMap(trvMapTree.currentMapID, deleteResult);
      trvMapTree.buildMapTree(FMapEngine.mapTree);
   end;
end;

procedure TfrmTurbuMain.actPlayMusicExecute(Sender: TObject);
begin
   RequireMapEngine;
   if FMusicPlayer = nil then
   begin
      FMusicPlayer := TFrmMusicSelector.Create(self);
      FMusicPlayer.Setup(GArchives[MUSIC_ARCHIVE].root, true);
   end;
   FMusicPlayer.ShowModal;
end;

function TfrmTurbuMain.AddTileImage(il: TImageList; index: integer): integer;
begin
   result := ilToolbarIcons.AddImage(il, index);
end;

procedure TfrmTurbuMain.assignPaletteImage(surface: PSdlSurface);
var
   texture: TSdlTexture;
begin
   FPaletteTexture := imgPalette.AddTexture(surface);
   texture := imgPalette.images[FPaletteTexture].surface;
   imgPalette.LogicalWidth := texture.size.X;
   imgPalette.LogicalHeight := round(imgPalette.LogicalWidth * (imgPalette.Height / imgPalette.Width));
   bindPaletteCursor;
   resizePalette;
end;

procedure TfrmTurbuMain.closeProject;
begin
   //check for modifications first
   if assigned(FMapEngine) then
      FMapEngine.Reset;
   GArchives.clearFrom(1);
   FPaletteTexture := -1;
end;

procedure TfrmTurbuMain.FormCreate(Sender: TObject);
begin
   if getProjectFolder = '' then
      createProjectFolder;
   FPaletteTexture := -1;
   FPaletteImages := TDictionary<integer, integer>.Create;
   FZoom := 1;
   self.pluginManager := dmEngineManager.pluginManager;
end;

procedure TfrmTurbuMain.FormDestroy(Sender: TObject);
begin
   cleanupEngines;
   FMapEngine := nil;
   FPaletteImages.Free;
   FScrollboxManager.Free;
end;

procedure TfrmTurbuMain.FormResize(Sender: TObject);
var
   availableSize: TSgPoint;
begin
   if assigned(FMapEngine) then
   begin
      availableSize := sgPoint(imgBackground.Width, imgBackground.Height) / FZoom;
      FMapEngine.ResizeWindow(classes.rect(ORIGIN, availableSize));
      FMapEngine.ScrollMap(sgPoint(sbHoriz.Position, sbVert.Position));
   end;
end;

procedure TfrmTurbuMain.FormShow(Sender: TObject);
var
   infile: TStream;
   plugins: TStringList;
   engines: TEngineDataList;
   plugStr, pf: string;
   i, j: integer;
   pluginIntf: ITurbuPlugin;
begin
   pf := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
   assert(GArchives.Add(openFolder(pf + DESIGN_DB)) = BASE_ARCHIVE);
   inFile := nil;
   plugins := TStringList.Create;
   try
      inFile := GArchives[BASE_ARCHIVE].getFile('plugins');
      plugins.LoadFromStream(inFile);
      for plugStr in plugins do
      try
         pluginManager.LoadPlugin(plugStr, plgPackage);
      except
         on E: EPackageError do
            raise; //TODO: Add code here to handle package loading errors
      end;
   finally
      plugins.free;
      inFile.free;
   end;
   for I := 0 to pluginManager.PluginCount - 1 do
   begin
      assert(pluginManager.Plugins[i].GetInterface(ITurbuPlugin, pluginIntf));
      engines := pluginIntf.listPlugins;
      for j := 0 to engines.Count - 1 do
         loadEngine(engines[j]);
      engines.free;
   end;
   pnlZoomResize(self);
   focusControl(trvMapTree);
end;

procedure TfrmTurbuMain.HandleTilesetChanged(var message);
begin
   imgPalette.ClearTextures;
   FPaletteImages.Clear;
   setLayer(FCurrentLayer);
end;

procedure TfrmTurbuMain.imgBackgroundClick(Sender: TObject);
begin
   FocusControl(imgLogo);
end;

procedure TfrmTurbuMain.imgLogoAvailable(Sender: TObject);
var
   surface: PSdlSurface;
   convert1, convert2: PSdlSurface;
   rw: PSDL_RWops;
   stream: TResourceStream;
begin
   stream := TResourceStream.Create(HInstance, 'logo', RT_RCDATA);
   rw := SDLStreamSetup(stream);
   surface := pointer(IMG_LoadPNG_RW(rw));
   assert(assigned(surface));
   convert1 := TSdlSurface.Create(1, 1, 32);
   convert2 := TSdlSurface.Convert(surface, convert1.Format);

   FLogoIndex := imgLogo.AddTexture(convert2);
   SDLStreamCloseRWops(rw);
   stream.Free;
   surface.Free;
   convert1.Free;

   imgLogo.DrawTexture(FLogoIndex);
   FScrollboxManager := TScrollboxManager.Create(imgBackground, imgLogo, sbHoriz, sbVert,
      function: single begin result := FZoom end,
      function: integer begin result := FTileSize.x end,
      function: TSgPoint begin result := FMapEngine.mapPosition end);
   TThread.NameThreadForDebugging('TURBU main thread');
end;

procedure TfrmTurbuMain.imgLogoDblClick(Sender: TObject);
begin
   RequireMapEngine;
   FMapEngine.doubleClick;

   //Needed because of the order of Windows messages: the single Click message
   //posts first, then the MouseDown after it, and this can do strange
   //things to the engine.
   FIgnoreMouseDown := true;
end;

procedure TfrmTurbuMain.imgLogoKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
   RequireMapEngine;
   FMapEngine.KeyDown(key, shift);
end;

procedure TfrmTurbuMain.imgLogoKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
   RequireMapEngine;
   FMapEngine.KeyUp(key, shift);
end;

procedure TfrmTurbuMain.imgLogoMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
   point: TSgPoint;
begin
   RequireMapEngine;
   if (FTileSize.x = 0) then
      Abort;
   if FIgnoreMouseDown then
      FIgnoreMouseDown := false
   else begin
      NormalizeMousePosition(imgLogo, x, y, FZoom);
      point := pointToGridLoc(sgPoint(x, y), FTileSize, sbHoriz.Position, sbVert.Position, FZoom);
      case button of
         mbLeft: FMapEngine.draw(point, true);
         mbRight: FMapEngine.RightClick(point);
         else ;
      end;
   end;
end;

procedure TfrmTurbuMain.imgLogoMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
   RequireMapEngine;
   if (FTileSize.x = 0) then
      Abort;
   if (ssLeft in shift) then
   begin
      NormalizeMousePosition(imgLogo, x, y, FZoom);
      FMapEngine.draw(pointToGridLoc(sgPoint(x, y), FTileSize, sbHoriz.Position, sbVert.Position, FZoom),
                      false);
   end;
end;

procedure TfrmTurbuMain.imgLogoMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
   RequireMapEngine;
   FMapEngine.doneDrawing;
end;

procedure TfrmTurbuMain.imgLogoPaint(Sender: TObject);
begin
   if assigned(FMapEngine) then
      FMapEngine.Repaint
   else begin
      imgLogo.DrawTexture(FLogoIndex);
      imgLogo.Flip;
   end;
end;

procedure TfrmTurbuMain.imgPaletteMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   if (button = mbLeft) then
   begin
      NormalizeMousePosition(imgPalette, x, y, 2);
      FPaletteSelection.TopLeft := point(x, y);
      imgPaletteMouseMove(sender, shift, x, y);
   end;
end;

procedure TfrmTurbuMain.NormalizeMousePosition(image: TSdlFrame; var x, y: integer; scale: single);
begin
   x := clamp(x, 0, image.Width - Ceil(scale));
   y := clamp(y, 0, image.height - Ceil(scale));
end;

procedure TfrmTurbuMain.imgPaletteMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
   if ssLeft in shift then
   begin
      NormalizeMousePosition(imgPalette, x, y, 2);
      FPaletteSelection.BottomRight := point(x, y);
      bindPaletteCursor;
      displayPalette;
   end;
end;

procedure TfrmTurbuMain.imgPalettePaint(Sender: TObject);
begin
   displayPalette;
end;

procedure TfrmTurbuMain.loadEngine(data: TEngineData);
var
   iClass: TRpgPlugBase;
   bEngine: IBattleEngine;
   mEngine: IMapEngine;
begin
   iClass := data.engine.Create;
   case data.style of
      et_map:
      begin
         assert(iclass.GetInterface(IMapEngine, mEngine));
         addEngine(et_map, mEngine.getData, mEngine);
      end;
      et_battle:
      begin
         assert(iclass.GetInterface(IBattleEngine, bEngine));
         addEngine(et_battle, bEngine.getData, bEngine);
      end;
      et_menu: assert(false);
      et_minigame: assert(false);
      else assert(false);
   end;
end;

procedure TfrmTurbuMain.loadMap(const value: IMapMetadata);
const
   SDL_BLACK: SDL_Color = ();
var
   i: integer;
   ctrl: TControl;
begin
   FMapEngine := retrieveEngine(et_map, value.mapEngine,
                 TVersion.Create(0, 0, 0)) as IDesignMapEngine;
   FMapEngine.SetController(self);
   FMapEngine.initialize(imgLogo.sdlWindow, FDBName);
   FMapEngine.autosaveMaps := mnuAutosaveMaps.checked;
   for i := 0 to tbDrawMode.ControlCount - 1 do
   begin
      ctrl := tbDrawMode.Controls[i];
      if (ctrl as TToolButton).Down then
      begin
         DrawToolButtonClick(ctrl);
         Break;
      end;
   end;
   FMapEngine.loadMap(value);
   FMapEngine.ScrollMap(sgPoint(sbHoriz.Position, sbVert.Position));
   FTileSize := FMapEngine.GetTileSize;
   imgPalette.ClearTextures;
   FPaletteImages.Clear;
   setLayer(FCurrentLayer);
   EnableZoom(true);
end;

function TfrmTurbuMain.MapResize(const size: TSgPoint): TSgPoint;
begin
   result := FScrollboxManager.SetMapSize(size);
end;

procedure TfrmTurbuMain.mnu2KClick(Sender: TObject);
var
   frmRmConverter: TFrmRmConverter;
   filename: string;
begin
   closeProject;
   frmRmConverter := TFrmRmConverter.Create(nil);
   try
      if frmRmConverter.loadProject <> SUCCESSFUL_IMPORT then
         Exit;
      filename := frmRmConverter.dbName;
   finally
      frmRmConverter.Free;
   end;
   self.openProject(filename);
end;

procedure TfrmTurbuMain.mnuAutosaveMapsClick(Sender: TObject);
var
   menu: TMenuItem absolute Sender;
begin
   assert(menu = mnuAutosaveMaps);
   menu.checked := not menu.checked;
   RequireMapEngine;
   FMapEngine.autosaveMaps := menu.checked;
end;

procedure TfrmTurbuMain.mnuDatabaseClick(Sender: TObject);
begin
   RequireMapEngine;
   FMapEngine.EditDatabase;
end;

procedure TfrmTurbuMain.mnuExitClick(Sender: TObject);
begin
   self.Close;
end;

procedure TfrmTurbuMain.mnuOpenClick(Sender: TObject);
var
   filename: string;
begin
   dlgOpen.InitialDir := getProjectFolder;
   if dlgOpen.Execute then
   begin
      filename := dlgOpen.FileName;
      closeProject;
      openProject(filename);
   end;
end;

procedure TfrmTurbuMain.mnuTestbugrepsClick(Sender: TObject);
begin
   RequireMapEngine;
   (FMapEngine as IBreakable).BreakSomething;
end;

procedure TfrmTurbuMain.mnuUndoClick(Sender: TObject);
begin
   RequireMapEngine;
   FMapEngine.Undo;
end;

procedure TfrmTurbuMain.OnScrollMap(Sender: TObject; ScrollCode: TScrollCode;
  var ScrollPos: Integer);
var
   scrollbar: TScrollBar absolute sender;
begin
   RequireMapEngine;
   assert(scrollbar is TScrollBar);
   scrollPos := min(scrollPos, scrollbar.Max - scrollbar.PageSize);
   fMapEngine.ScrollMap(sgPoint(sbHoriz.Position, sbVert.Position));
   trvMapTree.Selections[0].selected := true;
end;

procedure TfrmTurbuMain.openProject(const filename: string);
var
   location: string;

   procedure openArchive(folderName: string; index: integer);
   var
      filename: string;
   begin
      filename := location + folderName;
      ForceDirectories(filename);
      assert(GArchives.Add(openFolder(filename)) = index);
   end;

var
   boot: TdmProjectBoot;
begin
   location := IncludeTrailingPathDelimiter(ExtractFilePath(filename));
   logs.closeLog;
   GProjectFolder := location;
   openArchive(MAP_DB, MAP_ARCHIVE);
   openArchive(IMAGE_DB, IMAGE_ARCHIVE);
   openArchive(SCRIPT_DB, SCRIPT_ARCHIVE);
   openArchive(MUSIC_DB, MUSIC_ARCHIVE);
   openArchive(SFX_DB, SFX_ARCHIVE);
   openArchive(VIDEO_DB, VIDEO_ARCHIVE);

   boot := TdmProjectBoot.Create(nil);
   try
      FMapEngine := boot.Boot(filename) as IDesignMapEngine;
   finally
      boot.Free;
   end;
   FDbName := filename;
   try
      FMapEngine.initialize(imgLogo.sdlWindow, FDBName);
   except
      on EAbort do
      begin
         FMapEngine := nil;
         Exit;
      end;
   end;
   mnuDatabase.Enabled := true;
   trvMapTree.buildMapTree(FMapEngine.mapTree);
   if trvMapTree.Selected.IsFirstNode then
      trvMapTree.Select(trvMapTree.Selected.getFirstChild);
end;

procedure TfrmTurbuMain.pnlZoomResize(Sender: TObject);

   procedure CenterControl(ctrl: TControl; position: integer);
   begin
      ctrl.Left := sldZoom.GetOffsetByValue(position) - (ctrl.Width div 4);
   end;

var
   i: integer;
   ctrl: TControl;
begin
   for i := 0 to pnlZoom.ControlCount -1 do
   begin
      ctrl := pnlZoom.Controls[i];
      if (ctrl.tag <> 0) and (ctrl is TLabel) then
         CenterControl(ctrl, ctrl.tag);
   end;
end;

procedure TfrmTurbuMain.RequireMapEngine;
begin
   if not assigned(FMapEngine) then
      Abort;
end;

procedure TfrmTurbuMain.sbPaletteScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
   if (FLastPalettePos = ScrollPos) or (FPaletteTexture = -1) then
      Exit;
   displayPalette(ScrollPos);
   FLastPalettePos := ScrollPos;
end;

//thanks to Joseph Styons on StackOverflow for this routine (heavily modified)
//http://stackoverflow.com/a/4093723/32914
procedure AddButtonToToolbar(bar: TToolBar; button: TToolButton; addBeforeIdx: integer);
var
   prevBtnIdx: integer;
begin
   if (addBeforeIdx = -1) or (bar.ButtonCount <= addBeforeIdx) then
      prevBtnIdx := -1
   else prevBtnIdx := addBeforeIdx;

   if prevBtnIdx < 0 then
      button.Left := bar.Buttons[bar.ButtonCount - 1].Left + bar.Buttons[bar.ButtonCount - 1].Width
   else button.Left := bar.Buttons[prevBtnIdx].Left;

   button.Parent := bar;
end;

procedure TfrmTurbuMain.SetButton(button: TToolButton; position: TButtonPosition);
var
   idx: integer;
begin
   if (button = nil) or (button.Parent = ToolBar2) then
      Exit;
   case position of
      bpLayer: idx := tbLayerSplitter.Index;
      bpSave: idx := tbSaveSplitter.Index;
      bpPlay: idx := tbPlaySplitter.Index;
      else idx := -1;
   end;
   AddButtonToToolbar(ToolBar2, button, idx);
end;

procedure TfrmTurbuMain.TilesetChanged;
begin
   windows.PostMessage(self.Handle, WM_USER, 0, 0);
end;

procedure TfrmTurbuMain.DrawToolButtonClick(Sender: TObject);
begin
   RequireMapEngine;
   FMapEngine.SetPaintMode(TPaintMode((sender as TToolButton).Tag));
end;

procedure TfrmTurbuMain.setLayer(const value: integer);

   procedure LoadImage(const value: integer);
   begin
      if not FPaletteImages.ContainsKey(value) then
      begin
         assignPaletteImage(FMapEngine.tilesetImage[value]);
         FPaletteImages.add(value, FPaletteTexture);
      end
      else begin
         FPaletteTexture := FPaletteImages[value];
         bindPaletteCursor;
         resizePalette;
      end;
   end;

begin
   RequireMapEngine;
   FCurrentLayer := value;
   if value >= 0 then
      FLastGoodLayer := value;
   imgPalette.Enabled := value >= 0;
   LoadImage(FLastGoodLayer);
   FMapEngine.SetCurrentLayer(value);
end;

procedure TfrmTurbuMain.imgBackgroundPaint(Sender: TObject);
var
   bg, fg: TColor;
   box: TPaintBox;
begin
   box := sender as TPaintBox;
   GetColorsForDate(bg, fg);
   box.canvas.brush.Style := bsDiagCross;
   box.canvas.brush.color := fg;
   Windows.SetBkColor(box.Canvas.Handle, ColorToRgb(bg));
   box.canvas.FillRect(box.ClientRect);
end;

procedure TfrmTurbuMain.SetMapMenuItem(item: TMenuItem);
begin
   if mnuTreePopup.Items.IndexOf(item) = -1 then
      mnuTreePopup.Items.Add(item);
end;

procedure TfrmTurbuMain.ClearMapMenuItem(item: TMenuItem);
begin
   if mnuTreePopup.Items.IndexOf(item) <> -1 then
      mnuTreePopup.Items.Remove(item);
end;

procedure TfrmTurbuMain.SetZoom(const value: single);
begin
   if abs(value - FZoom) < 0.01 then //comparing floats with = doesn't always work
      Exit;
   FZoom := value;
   SDL_RenderDrawLine(imgLogo.Renderer, -1, -1, -1, -1); //hack: can't use SelectRenderer anymore
   GLLineWidth(value);
   FormResize(self);
end;

procedure TfrmTurbuMain.bindPaletteCursor;
var
   height, width: integer;
   i, j: integer;
   list: TList<integer>;
   ratio: single;
begin
   RequireMapEngine;
   ratio := imgPalette.Width / imgPalette.LogicalWidth;
   FPaletteSelectionTiles.TopLeft := pointToGridLoc(
     sgPoint(min(FPaletteSelection.left, FPaletteSelection.right),
             min(FPaletteSelection.top, FPaletteSelection.bottom)),
     FTileSize, 0, FCurrPalettePos, ratio);
   FPaletteSelectionTiles.BottomRight := pointToGridLoc(
     sgPoint(max(FPaletteSelection.left, FPaletteSelection.right),
             max(FPaletteSelection.top, FPaletteSelection.bottom)),
     FTileSize, 0, FCurrPalettePos, ratio);

   width := (FPaletteSelectionTiles.right - FPaletteSelectionTiles.left) + 1;
   height := (FPaletteSelectionTiles.bottom - FPaletteSelectionTiles.top) + 1;
   list := TList<integer>.Create;
   try
      list.capacity := (height * width) + 1;
      list.add(width);
      for j := 0 to height - 1 do
         for i := 0 to width - 1 do
            list.add((6 * (FPaletteSelectionTiles.top + j)) + FPaletteSelectionTiles.Left + i);
      FMapEngine.setPaletteList(list.ToArray);
   finally
      list.Free;
   end;
end;

function TfrmTurbuMain.calculatePaletteRect: TRect;
var
   ratio: double;
   offset: integer;
begin
//   ratio := imgPalette.Width / imgPalette.LogicalWidth;
   ratio := 1;
   offset := round(FCurrPalettePos * ratio);
   result.TopLeft := gridLocToPoint(FPaletteSelectionTiles.TopLeft,
     FTileSize, 0, offset, ratio);
   result.BottomRight := gridLocToPoint(
     sgPoint(FPaletteSelectionTiles.right + 1, FPaletteSelectionTiles.bottom + 1),
     FTileSize, 1, offset + 1, ratio); //adding 1 to X and Y values to subtract
                                       //so that it takes it in by 1 pixel
{
   cellsize := round(FTileSize.x * ratio);
   result := multiplyRect(FPaletteSelectionTiles, FTileSize.x * ratio);
   dec(result.top, offset);
   dec(result.bottom, offset);
   inc(result.right, cellsize - 1);
   inc(result.bottom, cellsize - 1);}
end;

procedure TfrmTurbuMain.sldZoomChanged(Sender: TObject);
begin
   SetZoom(sldZoom.Value / 100);
end;

procedure TfrmTurbuMain.splSidebarMoved(Sender: TObject);
begin
//QC 89303: Can't call Abort inside a splitter's OnMoved event
//   RequireMapEngine;
   if assigned(FMapEngine) then
   begin
      imgPalette.ClearTextures;
      FPaletteImages.Clear;
      setLayer(FCurrentLayer);
   end;
   trvMapTree.Invalidate;
end;

procedure TfrmTurbuMain.btnMapObjClick(Sender: TObject);
begin
   setLayer(-1);
end;

procedure TfrmTurbuMain.btnLayer1Click(Sender: TObject);
begin
   setLayer(0);
end;

procedure TfrmTurbuMain.btnLayer2Click(Sender: TObject);
begin
   setLayer(1);
end;

procedure TfrmTurbuMain.btnPauseClick(Sender: TObject);
begin
   RequireMapEngine;
   FMapEngine.Pause;
   enableRunButtons(false);
end;

procedure TfrmTurbuMain.btnRunClick(Sender: TObject);
begin
   RequireMapEngine;
   FMapEngine.Play;
   FocusControl(imgLogo);
   enableRunButtons(true);
end;

procedure TfrmTurbuMain.btnSaveAllClick(Sender: TObject);
begin
   RequireMapEngine;
   FMapEngine.SaveAll;
end;

procedure TfrmTurbuMain.btnSaveClick(Sender: TObject);
begin
   RequireMapEngine;
   FMapEngine.SaveCurrent;
end;

procedure TfrmTurbuMain.trvMapTreeChange(Sender: TObject; Node: TTreeNode);
begin
   if not node.IsFirstNode then
      loadMap(IInterface(node.Data) as IMapMetadata);
end;

procedure TfrmTurbuMain.trvMapTreeContextPopup(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
var
   topNode: boolean;
begin
   topNode := trvMapTree.selected = trvMapTree.items[0];
   mnuDeleteMap.Enabled := not topNode;
   mnuEditMapProperties.Enabled := not topNode;
end;

procedure TfrmTurbuMain.UpdateEngine(const filename: string);
begin
   DelayExec(
     procedure
     var
        info: SHELLEXECUTEINFO;
        ver: OSVERSIONINFO;
     begin
        CloseProject;
        ver.dwOSVersionInfoSize := sizeof(ver);
        GetVersionEx(ver);
        info := default(SHELLEXECUTEINFO);
        info.cbSize := sizeof(SHELLEXECUTEINFO);
        if ver.dwMajorVersion >= 6 then
          info.lpVerb := 'runas'
        else info.lpVerb := 'open';
        info.lpFile := PChar(TPath.Combine(ExtractFilePath(Application.ExeName), 'update.exe'));
        info.lpParameters := PChar('"' + filename + '"');
        info.nShow := SW_SHOW;
        info.fMask := SEE_MASK_NOASYNC;
        if ShellExecuteEx(@info) then
           Application.Terminate
        else Application.MessageBox('Unable to launch the updater', 'Update error');
     end);
   Abort;
end;

initialization
finalization
   frmTurbuMain := nil;

//The major difference between a thing that might go wrong and a thing that
//cannot possibly go wrong is that when a thing that cannot possibly go wrong
//goes wrong it usually turns out to be impossible to get at or repair.

end.