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
unit turbu_2k_menu_basis;

interface
uses
   Types, Classes, Generics.collections,
   commons, turbu_2k_frames, turbu_defs, turbu_heroes,
   SDL_sprite, sg_defs,
   dwsJSON;

type
   TMenuState = (ms_off, ms_main, ms_fading);
   TMenuScripts = (msc_button, msc_cursor);

   TGameMenuBox = class;

   TMenuEngine = class;

   TMenuPage = class;
   TMenuPageStack = class(TStack<TMenuPage>);

   TButtonFunc = procedure (which: TButtonCode; theMenu: TGameMenuBox; parent: TMenuPage) of object;

   TCursorFunc = procedure (position: smallint; theMenu: TGameMenuBox; parent: TMenuPage) of object;

   TSetupFunc = procedure (value: integer; theMenu: TGameMenuBox; parent: TMenuPage) of object;
   TCloseMessageEvent = procedure of object;

   TGameMenuBox = class abstract(TCustomMessageBox)
   private
      FTextDrawn: boolean;
      FOnButton: TButtonFunc;
      FOnCursor: TCursorFunc;
      FOnSetup: TSetupFunc;

      function getOptionEnabled(which: word): boolean;
      procedure setOptionEnabled(which: word; const Value: boolean);
      function getCursorPosition: smallint;
   protected
      FMenuEngine: TMenuEngine;
      FOwner: TMenuPage;
      FReferrer: TGameMenuBox;
      FSetupValue: integer;
      FBlank: boolean;

      function getRightSide: integer;
      procedure return; inline;
      function focused: boolean; inline;
      procedure doButton(const input: TButtonCode); virtual;
      procedure doCursor(position: smallint); virtual;
      procedure doSetup(value: integer); virtual;
      procedure DrawText; virtual; abstract;
      procedure DoDraw; override; final;
      procedure InvalidateText;
   public
      constructor Create(parent: TMenuSpriteEngine; coords: TRect; main: TMenuEngine; owner: TMenuPage); reintroduce; virtual;
      procedure Draw; override; final;
      procedure setup(value: integer);
      procedure button(const input: TButtonCode); override; final;
      procedure placeCursor(position: smallint); override; final;
      procedure focusPage(const which: string; cursorValue: integer);
      procedure focusMenu(const which: string; setupValue: integer);
      procedure moveTo(coords: TRect); override;

      property cursorPosition: smallint read getCursorPosition;
      property parent: TMenuEngine read FMenuEngine;
      property onButton: TButtonFunc read FOnButton write FOnButton;
      property onCursor: TCursorFunc read FOnCursor write FOnCursor;
      property onSetup: TSetupFunc read FOnSetup write FOnSetup;
      property optionEnabled[which: word]: boolean read getOptionEnabled write setOptionEnabled;
   end;

   TGameMenuBoxClass = class of TGameMenuBox;

   TMenuPage = class abstract (TObject)
   private
      FOwner: TMenuEngine;
   protected
      FVisible: boolean;
      FMainMenu: TGameMenuBox;
      FCurrentMenu: TGameMenuBox;
      FComponents: TDictionary<string, TGameMenuBox>;
      FComponentList: TArray<TGameMenuBox>;
      FBounds: TRect;
      FBackground: string;

      class function ShiftX(const value: TRect; distance, cutoff: integer): TRect; static;
      procedure setVisible(const value: boolean); virtual;
      procedure registerComponent(const name: string; which: TGameMenuBox);
      procedure LoadComponent(obj: TdwsJSONObject);
      procedure LoadComponents(const layout: string);
      procedure SetBG(const filename, imagename: string);
      procedure LoadFullImage(const filename, imagename: string; opaque: boolean = true);
      procedure DoDraw; virtual;
      procedure Cleanup; virtual;
   public
      constructor Create(parent: TMenuSpriteEngine; coords: TRect; main: TMenuEngine;
        const layout: string); virtual;
      destructor Destroy; override;
      procedure focusMenu(referrer: TGameMenuBox; const which: string;
        setupValue: integer; unchanged: boolean = false); overload;
      procedure focusMenu(referrer, which: TGameMenuBox;
        unchanged: boolean = false); overload; virtual;
      procedure focusPage(const which: string; cursorValue: integer); inline;
      procedure backTo(which: TGameMenuBox); inline;
      procedure Draw;
      procedure placeCursor(value: integer);
      procedure setup(value: integer); virtual;
      procedure setupEx(const data: TObject); virtual;
      procedure move;
      procedure button(input: TButtonCode); virtual;
      function menu(const name: string): TGameMenuBox;

      property currentMenu: TGameMenuBox read FCurrentMenu;
      property visible: boolean read FVisible write setVisible;
   end;

   TMenuPageClass = class of TMenuPage;

   TMenuEngine = class(TInterfacedObject, IMenuEngine)
   private
      type
         TMenuPageData = record
            cls: TMenuPageClass;
            layout: string;
            constructor Create(aClass: TMenuPageClass; const aLayout: string);
         end;

      class var
         FMenuBoxes: TDictionary<string, TGameMenuBoxClass>;
         FMenuLayouts: TDictionary<string, TMenuPageData>;
      class constructor Create;
      class destructor Destroy;
   private
      FCursor: TSysFrame;
      FParent: TMenuSpriteEngine;
      FVisible: boolean;
      FStack: TMenuPageStack;
      FCurrentPage: TMenuPage;
      FState: TMenuState;
      FOrigin: TsgPoint;
      FMenus: TDictionary<string, TMenuPage>;
      FCloseMenu: TCloseMessageEvent;
      FCurrentHero: TRpgHero;

      procedure setVisible(const Value: boolean);

      procedure move;
      procedure initialize;
   private //IMenuEngine implementation
      procedure OpenMenu(const name: string; cursorValue: integer = 0);
      procedure OpenMenuEx(const name: string; const data: TObject);
      procedure button(const input: TButtonCode);
      procedure Draw;
   public
      constructor Create(parent: TMenuSpriteEngine; callback: TCloseMessageEvent);
      destructor Destroy; override;
      procedure focusMenu(sender, which: TMenuPage); overload;
      procedure focusMenu(sender: TMenuPage; const which: string; cursorValue: integer); overload;
      procedure return;
      procedure placeCursor(value: smallint);
      procedure leave(const playSound: boolean = true);

      procedure activate;
      procedure shutdown;

      class procedure RegisterMenuBoxClass(cls: TGameMenuBoxClass);
      class procedure RegisterMenuPage(const name, layout: string);
      class procedure RegisterMenuPageEx(cls: TMenuPageClass; const name, layout: string);

      property visible: boolean read FVisible write setVisible;
      property currentMenu: TMenuPage read FCurrentPage;
      property cursor: TSysFrame read FCursor;
      property CurrentHero: TRpgHero read FCurrentHero write FCurrentHero;
   end;

const
   CURSOR_UNCHANGED = 9999;

implementation
uses
   SysUtils, SyncObjs, Math,
   turbu_script_engine, turbu_2k_map_engine, turbu_2k_environment,
   rs_media,
   sg_utils, SDL_ImageManager;

var
   GSetupDrawLock: TCriticalSection;

{ TGameMenuBox }

constructor TGameMenuBox.Create(parent: TMenuSpriteEngine; coords: TRect; main: TMenuEngine; owner: TMenuPage);
begin
   FMenuEngine := main;
   FOwner := owner;
   inherited Create(parent, rect(ORIGIN, TRectToSdlRect(coords).BottomRight));
   self.moveTo(coords);
end;

procedure TGameMenuBox.button(const input: TButtonCode);
begin
   assert(self <> nil);
   self.doButton(input);
   if assigned(onButton) then
      onButton(input, self, FOwner);
end;

procedure TGameMenuBox.doButton(const input: TButtonCode);
begin
   inherited Button(input);
   if input = btn_cancel then
   begin
      playSystemSound(sfxCancel);
      self.return;
   end;
end;

procedure TGameMenuBox.Draw;
begin
   if Visible then
      inherited Draw;
end;

function TGameMenuBox.focused: boolean;
begin
   result := (FMenuEngine.FCurrentPage = FOwner) and (FOwner.CurrentMenu = self);
end;

procedure TGameMenuBox.focusMenu(const which: string; setupValue: integer);
begin
   FOwner.focusMenu(self, which, setupValue);
end;

procedure TGameMenuBox.focusPage(const which: string; cursorValue: integer);
begin
   FOwner.focusPage(which, cursorValue);
end;

function TGameMenuBox.getCursorPosition: smallint;
begin
   result := FCursorPosition;
end;

procedure TGameMenuBox.placeCursor(position: smallint);
begin
   assert(self <> nil);
   if position = CURSOR_UNCHANGED then
      position := FCursorPosition;
   self.doCursor(position);
   if assigned(onCursor) then
      onCursor(FCursorPosition, self, FOwner);
end;

procedure TGameMenuBox.doCursor(position: smallint);
begin
//checkpoint to keep entire-page setups from placing the cursor
//in the wrong box.
   if self.focused then
      inherited placeCursor(position);
end;

procedure TGameMenuBox.DoDraw;
const TEXT_TARGET: TSgPoint = (x: 8; y: 8);
begin
   inherited DoDraw;
   if not FTextDrawn then
   begin
      FTextTarget.parent.pushRenderTarget;
      FTextTarget.SetRenderer;
      FTextTarget.parent.Clear(SDL_BLACK, 0);
      DrawText;
      FTextTarget.parent.popRenderTarget;
      FTextDrawn := true;
   end;
   if (self.focused) and (not FBlank) then
      FMenuEngine.FCursor.Draw;
   FTextTarget.parent.Draw(FTextTarget, FOrigin + TEXT_TARGET);
end;

function TGameMenuBox.getOptionEnabled(which: word): boolean;
begin
   result := FOptionEnabled[which];
end;

function TGameMenuBox.getRightSide: integer;
begin
   result := FTextTarget.width;
end;

procedure TGameMenuBox.InvalidateText;
begin
   FTextDrawn := false;
end;

procedure TGameMenuBox.moveTo(coords: TRect);
begin
   FOrigin := coords.TopLeft;
   inherited moveTo(rect(ORIGIN, TRectToSdlRect(coords).BottomRight));
   FBounds := coords;
   FCoords := TRectToSdlRect(coords);
end;

procedure TGameMenuBox.setOptionEnabled(which: word; const Value: boolean);
begin
   FOptionEnabled[which] := value;
end;

procedure TGameMenuBox.setup(value: integer);
begin
   doSetup(value);
   if assigned(FOnSetup) then
      FOnSetup(value, self, FOwner);
end;

procedure TGameMenuBox.doSetup(value: integer);
begin
   if value <> CURSOR_UNCHANGED then
      FSetupValue := value
   else FDontChangeCursor := true;
end;

procedure TGameMenuBox.return;
var dummy: TGameMenuBox;
begin
   if FReferrer = nil then
      FMenuEngine.return
   else begin
      dummy := FReferrer;
      FReferrer := nil;
      FOwner.backTo(dummy);
   end;
end;

{ TMenuEngine }

class constructor TMenuEngine.Create;
begin
   FMenuBoxes := TDictionary<string, TGameMenuBoxClass>.Create;
   FMenuLayouts := TDictionary<string, TMenuPageData>.Create;
end;

class destructor TMenuEngine.Destroy;
begin
   FMenuBoxes.Free;
   FMenuLayouts.Free;
end;

constructor TMenuEngine.Create(parent: TMenuSpriteEngine; callback: TCloseMessageEvent);
var
   pair: TPair<string, TMenuPageData>;
begin
   inherited Create;
   FParent := parent;
   FCursor := parent.cursor;
   FStack := TMenuPageStack.create;
   self.visible := false;
   GSetupDrawLock := TCriticalSection.Create;
   FMenus := TObjectDictionary<string, TMenuPage>.Create([doOwnsValues]);

   for pair in FMenuLayouts do
      FMenus.Add(pair.Key,
        pair.Value.cls.Create(FParent,
                         rect(0, 0, FParent.Canvas.Width, FParent.Canvas.Height),
                         self,
                         pair.Value.layout));
   FCloseMenu := callback;
end;

destructor TMenuEngine.Destroy;
begin
   FStack.free;
   GSetupDrawLock.Free;
   FMenus.Free;
   inherited;
end;

procedure TMenuEngine.Draw;
begin
   FCurrentPage.Draw;
end;

procedure TMenuEngine.focusMenu(sender: TMenuPage; const which: string; cursorValue: integer);
var
   menu: TMenuPage;
begin
   if not FMenus.TryGetValue(which, menu) then
      raise Exception.CreateFmt('No menu named "%s" is registered.', [which]);
   FocusMenu(sender, menu);
   menu.setup(cursorValue);
end;

procedure TMenuEngine.focusMenu(sender, which: TMenuPage);
begin
   assert(sender <> which);
   FStack.Push(sender);
   self.FCurrentPage := which;
   sender.visible := false;
   which.visible := true;
end;

procedure TMenuEngine.initialize;
begin
   GGameEngine.enterLock := true;
   self.move;
   GEnvironment.Party.Pack;
   self.visible := true;
   self.FState := ms_fading;
   placeCursor(0);
end;

procedure TMenuEngine.activate;
begin
   assert(FState = ms_fading);
   FState := ms_main;
   FOrigin := sgPoint(round(FParent.WorldX), round(FParent.WorldY));
end;

procedure TMenuEngine.shutdown;
begin
   assert(FState = ms_fading);
   FState := ms_off;
end;

procedure TMenuEngine.move;
begin
   FCurrentPage.move;
end;

procedure TMenuEngine.OpenMenuEx(const name: string; const data: TObject);
begin
   if not FMenus.TryGetValue(name, FCurrentPage) then
      raise Exception.CreateFmt('No menu named "%s" is registered.', [name]);
   FCurrentPage.setupEx(data);
   self.initialize;
end;

procedure TMenuEngine.OpenMenu(const name: string; cursorValue: integer);
begin
   if not FMenus.TryGetValue(name, FCurrentPage) then
      raise Exception.CreateFmt('No menu named "%s" is registered.', [name]);
   FCurrentPage.setup(cursorValue);
   self.initialize;
end;

procedure TMenuEngine.button(const input: TButtonCode);
begin
   case FState of
      ms_off: raise EParseMessage.create('Tried to send a menu command when the menu was not active!');
      else FCurrentPage.button(input);
   end;
end;

procedure TMenuEngine.placeCursor(value: smallint);
begin
   if FCurrentPage = nil then
      raise EFatalError.create('Tried to place a system menu cursor when the menu was not active!')
   else FCurrentPage.placeCursor(value);
end;

class procedure TMenuEngine.RegisterMenuPage(const name, layout: string);
begin
   FMenuLayouts.Add(name, TMenuPageData.Create(TMenuPage, layout));
end;

class procedure TMenuEngine.RegisterMenuPageEx(cls: TMenuPageClass;
  const name, layout: string);
begin
   FMenuLayouts.Add(name, TMenuPageData.Create(cls, layout));
end;

class procedure TMenuEngine.RegisterMenuBoxClass(cls: TGameMenuBoxClass);
begin
   FMenuBoxes.Add(cls.ClassName, cls);
end;

procedure TMenuEngine.leave(const playSound: boolean = true);
begin
   while FStack.Count > 0 do
      return;
   GGameEngine.enterLock := true;
   if playSound then
      rs_media.playSystemSound(sfxCancel);
   FState := ms_fading;
   self.visible := false;
   GGameEngine.CurrentMap.wake;
   FCloseMenu();
end;

procedure TMenuEngine.return;
var
   page: TMenuPage;
begin
   if FStack.Count = 0 then
      self.leave(false)
   else begin
      page := FStack.pop;
      FCurrentPage.Visible := false;
      FCurrentPage.Cleanup;
      page.visible := true;
      FCurrentPage := page;
      page.setup(CURSOR_UNCHANGED);
   end;
end;

procedure TMenuEngine.setVisible(const Value: boolean);
begin
   FVisible := Value;
   if assigned(FCurrentPage) then
      FCurrentPage.Visible := value;
   FCursor.Visible := value;
end;

{ TMenuPage }

procedure TMenuPage.Cleanup;
begin
   //this virtual method intentionally left blank
end;

constructor TMenuPage.Create(parent: TMenuSpriteEngine; coords: TRect;
  main: TMenuEngine; const layout: string);
begin
   inherited Create;
   FBounds := coords;
   FOwner := main;
   FComponents := TDictionary<string, TGameMenuBox>.Create;
   loadComponents(layout);
end;

destructor TMenuPage.Destroy;
begin
   Cleanup;
   FComponents.free;
   inherited;
end;

procedure TMenuPage.DoDraw;
begin
   if FBackground <> '' then
      FOwner.FParent.Images.Image[FBackground].Draw;
end;

procedure TMenuPage.Draw;
var
   frame: TGameMenuBox;
begin
   GSetupDrawLock.Enter;
   try
      assert(FOwner.FCurrentPage = self);
      DoDraw;
      for frame in FComponentList do
         if frame.Visible then
            frame.draw;
   finally
      GSetupDrawLock.Leave;
   end;
end;

function TMenuPage.menu(const name: string): TGameMenuBox;
begin
   if not FComponents.TryGetValue(name, result) then
      raise Exception.CreateFmt('No menu box named "%s" is available.', [name]);
end;

procedure TMenuPage.focusMenu(referrer: TGameMenuBox; const which: string;
  setupValue: integer; unchanged: boolean);
var
   frame: TGameMenuBox;
begin
   frame := self.menu(which);
   if not (frame is TGameMenuBox) then
      raise Exception.CreateFmt('Menu box "%s" can''t be focused.', [which]);
   FocusMenu(referrer, TGameMenuBox(frame), unchanged);
   TGameMenuBox(frame).setup(setupValue);
end;

procedure TMenuPage.focusMenu(referrer, which: TGameMenuBox; unchanged: boolean);
begin
   self.FCurrentMenu := which;
   which.Visible := true;
   if referrer <> nil then
      FCurrentMenu.FReferrer := referrer;
   if unchanged then
      self.placeCursor(CURSOR_UNCHANGED)
   else self.placeCursor(0);
end;

procedure TMenuPage.focusPage(const which: string; cursorValue: integer);
begin
   FOwner.focusMenu(self, which, cursorValue);
end;

procedure TMenuPage.LoadComponent(obj: TdwsJSONObject);
var
   name, cls: string;
   boxClass: TGameMenuBoxClass;
   coordsArr: TdwsJSONArray;
   coords: TRect;
   box: TGameMenuBox;
begin
   cls := obj.Items['Class'].AsString;
   if not TMenuEngine.FMenuBoxes.TryGetValue(cls, boxClass) then
      raise Exception.CreateFmt('Menu class "%s" is not reigstered.', [cls]);
   name := obj.Items['Name'].AsString;
   coordsArr := obj.Items['Coords'] as TdwsJSONArray;
   coords.Left   := coordsArr.Elements[0].AsInteger;
   coords.Top    := coordsArr.Elements[1].AsInteger;
   coords.Right  := coordsArr.Elements[2].AsInteger;
   coords.Bottom := coordsArr.Elements[3].AsInteger;
   box := boxClass.Create(FOwner.FParent, coords, FOwner, self);
   self.registerComponent(name, box);
end;

procedure TMenuPage.LoadComponents(const layout: string);
var
   arr: TdwsJSONArray;
   i: integer;
   obj: TdwsJSONObject;
begin
   arr := TdwsJSONArray.ParseString(layout) as TdwsJSONArray;
   try
      for i := 0 to arr.ElementCount - 1 do
      begin
         obj := arr.Elements[i] as TdwsJSONObject;
         LoadComponent(obj);
      end;
   finally
      arr.Free;
   end;
end;

procedure TMenuPage.move;
begin
   if assigned(FCurrentMenu) then
      placeCursor(CURSOR_UNCHANGED);
end;

procedure TMenuPage.placeCursor(value: integer);
begin
   FCurrentMenu.placeCursor(value);
end;

procedure TMenuPage.registerComponent(const name: string; which: TGameMenuBox);
begin
   FComponents.Add(name, which);
   SetLength(FComponentList, length(FComponentList) + 1);
   FComponentList[high(FComponentList)] := which;
   which.Name := name;
   if (FMainMenu = nil) and (which is TGameMenuBox) then
      FMainMenu := TGameMenuBox(which);
end;

procedure TMenuPage.LoadFullImage(const filename, imagename: string; opaque: boolean = true);
var
   cls: TSdlImageClass;
   images: TSdlImages;
begin
   images := FOwner.FParent.Images;
   if not images.Contains(imagename) then
   begin
      cls := Images.SpriteClass;
      if opaque then
         Images.SpriteClass := TSdlOpaqueImage
      else Images.SpriteClass := TSdlImage;
      try
         Images.EnsureImage(filename, imagename);
      finally
         Images.SpriteClass := cls;
      end;
   end;
end;

procedure TMenuPage.SetBG(const filename, imagename: string);
begin
   LoadFullImage(filename, imagename);
   FBackground := imagename;
end;

procedure TMenuPage.setup(value: integer);
var
   frame: TGameMenuBox;
begin
   GSetupDrawLock.Enter;
   try
      FCurrentMenu := FMainMenu;
      for frame in FComponents.Values do
         if frame is TGameMenuBox then
            TGameMenuBox(frame).setup(value);
      self.move;
   finally
      GSetupDrawLock.Leave;
   end;
end;

procedure TMenuPage.setupEx(const data: TObject);
begin
   setup(GMenuEngine.MenuInt);
end;

class function TMenuPage.ShiftX(const value: TRect; distance, cutoff: integer): TRect;
begin
   if distance < 0 then
      distance := max(distance, -(value.Left - cutoff))
   else distance := min(distance, cutoff - value.Left);
   result := rect(value.Left + distance, value.Top, value.Right + distance, value.Bottom);
end;

procedure TMenuPage.setVisible(const value: boolean);
var
   frame: TGameMenuBox;
begin
   for frame in FComponents.Values do
      frame.Visible := value;
   self.FVisible := value;
end;

procedure TMenuPage.backTo(which: TGameMenuBox);
var
   frame: TGameMenuBox;
begin
   for frame in FComponents.Values do
      frame.InvalidateText;
   self.focusMenu(nil, which, true);
end;

procedure TMenuPage.button(input: TButtonCode);
begin
   FCurrentMenu.button(input);
end;

{ TMenuEngine.TMenuPageData }

constructor TMenuEngine.TMenuPageData.Create(aClass: TMenuPageClass;
  const aLayout: string);
begin
   Self.cls := aClass;
   self.layout := aLayout;
end;

end.
