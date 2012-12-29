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

unit EditTeleport;

interface

uses
   Forms, StdCtrls, ExtCtrls, ComCtrls, Controls, Classes, Generics.Collections,
   Windows, Messages,
   EventBuilder, EbEdit, sdl_frame, turbu_map_interface, turbu_map_engine, EB_Maps,
   scrollbox_manager,
   sg_defs;

const
   WM_LOADED = WM_USER + 1;

type
   [EditorCategory('Map', 'Teleport')]
   [EditorContext('RM2K')]
   TfrmEBEditTeleport = class(TfrmEbEditBase, ITurbuController)
      trvMapTree: TTreeView;
      sbxMain: TScrollBox;
      imgBackground: TPaintBox;
      pnlHorizScroll: TPanel;
      sbHoriz: TScrollBar;
      sbVert: TScrollBar;
      imgView: TSdlFrame;
      radFacing: TRadioGroup;
      procedure FormCreate(Sender: TObject);
      procedure FormDestroy(Sender: TObject);
      procedure OnScrollMap(Sender: TObject; ScrollCode: TScrollCode;
        var ScrollPos: Integer);
      procedure imgBackgroundPaint(Sender: TObject);
      procedure imgViewMouseDown(Sender: TObject; Button: TMouseButton;
         Shift: TShiftState; X, Y: Integer);
      procedure imgViewMouseMove(Sender: TObject; Shift: TShiftState; X,
         Y: Integer);
      procedure imgViewMouseUp(Sender: TObject; Button: TMouseButton;
         Shift: TShiftState; X, Y: Integer);
      procedure imgViewAvailable(Sender: TObject);
   private
      FCurrentMap: integer;
      FPosition: TsgPoint;
      FMapEngine: IDesignMapEngine;
      FCache: TDictionary<string, IDesignMapEngine>;
      FScrollboxManager: TScrollboxManager;
      procedure WMLoaded(var msg: TMessage); message WM_LOADED;
      procedure LoadMap(Sender: TObject; Node: TTreeNode);
      function LoadEngine(const name: string): IDesignMapEngine;
      procedure DrawPosition(x, y: integer; new: boolean);

      function MapResize(const size: TSgPoint): TSgPoint;
   protected
      procedure UploadObject(obj: TEbObject); override;
      procedure DownloadObject(obj: TEbObject); override;
      function NewClassType: TEbClass; override;
   public
      procedure SetupMap(const map: IRpgMap); override;
      function EditExternal(value: TEBTeleport; hideMapTree: boolean = false): boolean;
   end;

implementation
uses
   Math, Graphics,
   dm_database,
   turbu_database, map_tree_controller, turbu_engines, turbu_plugin_interface,
   turbu_versioning, commons,
   sg_utils;

{$R *.dfm}

const TILE_SIZE = 16;

procedure TfrmEBEditTeleport.FormCreate(Sender: TObject);
begin
   trvMapTree.buildMapTree(GDatabase.mapTree);
   FCache := TDictionary<string, IDesignMapEngine>.Create;
   FScrollboxManager := TScrollboxManager.Create(imgBackground, imgView, sbHoriz, sbVert,
      function: single begin result := 1 end,
      function: integer begin result := TILE_SIZE end,
      function: TSgPoint begin result := FMapEngine.mapPosition end);
end;

procedure TfrmEBEditTeleport.FormDestroy(Sender: TObject);
begin
   FCache.Free;
   FScrollboxManager.Free;
end;

procedure TfrmEBEditTeleport.imgBackgroundPaint(Sender: TObject);
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

procedure NormalizeMousePosition(image: TSdlFrame; var x, y: integer; scale: single);
begin
   x := clamp(x, 0, image.Width - Ceil(scale));
   y := clamp(y, 0, image.height - Ceil(scale));
end;

procedure TfrmEBEditTeleport.DrawPosition(x, y: integer; new: boolean);
begin
   NormalizeMousePosition(imgView, x, y, 1);
   FPosition := pointToGridLoc(sgPoint(x, y), sgPoint(16, 16), sbHoriz.Position, sbVert.Position, 1);
   FMapEngine.draw(FPosition, new);
end;

function TfrmEBEditTeleport.EditExternal(value: TEBTeleport; hideMapTree: boolean): boolean;
begin
   radFacing.Visible := false;
   if hideMapTree then
      trvMapTree.Visible := false;
   UploadObject(value);
   result := self.ShowModal = mrOk;
   if result then
      DownloadObject(value);
end;

procedure TfrmEBEditTeleport.imgViewAvailable(Sender: TObject);
begin
   PostMessage(self.Handle, WM_LOADED, 0, 0);
end;

procedure TfrmEBEditTeleport.imgViewMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   if FMapEngine = nil then
      Exit;
   DrawPosition(x, y, true);
end;

procedure TfrmEBEditTeleport.imgViewMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
   if FMapEngine = nil then
      Exit;
   if ([ssLeft, ssRight] * shift <> []) then
      DrawPosition(x, y, false);
end;

procedure TfrmEBEditTeleport.imgViewMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   if assigned(FMapEngine) then
      FMapEngine.doneDrawing;
end;

procedure TfrmEBEditTeleport.SetupMap(const map: IRpgMap);
begin
   FCurrentMap := map.id;
end;

function TfrmEBEditTeleport.LoadEngine(const name: string): IDesignMapEngine;
var
   base: IDesignMapEngine;
   baseObj: TObject;
begin
   if not FCache.ContainsKey(name) then
   begin
      base := retrieveEngine(et_map, name, TVersion.Create(0,0,0)) as IDesignMapEngine;
      baseObj := TObject(base);
      if (base = nil) or not (base is TRpgPlugBase) then
         raise ERpgPlugin.CreateFmt('Map engine ''%s'' is not compatible with this editor.', [name]);
      FCache.Add(name, TPlugClass(baseObj.ClassType).Create as IDesignMapEngine);
   end;
   result := FCache[name];
end;

procedure TfrmEBEditTeleport.LoadMap(Sender: TObject; Node: TTreeNode);
var
   metadata: IMapMetadata;
   current: TPoint;
begin
   metadata := IMapMetadata(node.Data);
   FMapEngine := LoadEngine(metadata.mapEngine);
   FMapEngine.initialize(imgView.sdlWindow, dmDatabase.dbName);
   FMapEngine.SetController(self);
   FMapEngine.loadMap(metadata);
   FMapEngine.SetCurrentLayer(-1);
   current := (FPosition * TILE_SIZE) - (sgPoint(sbHoriz.PageSize, sbVert.PageSize) / 2);
   sbHoriz.Position := clamp(current.X, 0, sbHoriz.Max - sbHoriz.PageSize);
   sbvert.Position := clamp(current.Y, 0, sbvert.Max - sbVert.PageSize);
   FMapEngine.ScrollMap(sgPoint(sbHoriz.Position, sbvert.Position));
   FCurrentMap := metadata.id;
   FMapEngine.Draw(FPosition, true);
   FMapEngine.DoneDrawing;
end;

function TfrmEBEditTeleport.MapResize(const size: TSgPoint): TSgPoint;
begin
   result := FScrollboxManager.SetMapSize(size);
end;

procedure TfrmEBEditTeleport.OnScrollMap(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
var
   scrollbar: TScrollBar absolute sender;
begin
   assert(scrollbar is TScrollBar);
   scrollPos := min(scrollPos, scrollbar.Max - scrollbar.PageSize);
   FMapEngine.ScrollMap(sgPoint(sbHoriz.Position, sbVert.Position));
   trvMapTree.Selections[0].selected := true;
end;

procedure TfrmEBEditTeleport.UploadObject(obj: TEbObject);
begin
   FCurrentMap := obj.Values[0];
   FPosition := sgPoint(obj.Values[1], obj.Values[2]);
   radFacing.ItemIndex := obj.Values[3];
end;

procedure TfrmEBEditTeleport.DownloadObject(obj: TEbObject);
begin
   obj.clear;
   obj.values.add(FCurrentMap);
   obj.values.add(FPosition.x);
   obj.values.add(FPosition.y);
   obj.values.add(radFacing.ItemIndex);
end;

function TfrmEBEditTeleport.NewClassType: TEbClass;
begin
   result := TEBTeleport;
end;

procedure TfrmEBEditTeleport.WMLoaded(var msg: TMessage);
var
   node: TTreeNode;
begin
   imgView.Clear; //make sure that a renderer is loaded first
   for node in trvMapTree.Items do
      if IMapMetadata(node.Data).ID = FCurrentMap then
      begin
         trvMapTree.Select(node);
         trvMapTree.OnChange := self.LoadMap;
         Loadmap(self, node);
         Break;
      end;
end;

initialization
   RegisterEbEditor(TEBTeleport, TfrmEBEditTeleport);
finalization
   UnRegisterEbEditor(TEBTeleport);
end.
