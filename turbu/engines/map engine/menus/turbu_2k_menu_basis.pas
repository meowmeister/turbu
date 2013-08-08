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
   Types, SyncObjs, Generics.collections,
   commons, turbu_2k_frames, turbu_defs,
   SDL_sprite;

type
   TMenuState = (ms_off, ms_main, ms_fading);
   TMenuScripts = (msc_button, msc_cursor);

   TGameMenuBox = class;

   TMenuEngine = class;

   TMenuPage = class;
   TRpgStack = class(TStack<TMenuPage>);

   TButtonFunc = procedure (which: TButtonCode; theMenu: TGameMenuBox; parent: TMenuPage) of object;

   TCursorFunc = procedure (position: smallint; theMenu: TGameMenuBox; parent: TMenuPage) of object;

   TSetupFunc = procedure (value: integer; theMenu: TGameMenuBox; parent: TMenuPage) of object;

   TGameMenuBox = class abstract(TCustomMessageBox)
   private
      FOnButton: TButtonFunc;
      FOnCursor: TCursorFunc;
      FOnSetup: TSetupFunc;

      function getOptionEnabled(which: word): boolean;
      procedure setOptionEnabled(which: word; const Value: boolean);
      function getCursorPosition: smallint;
   protected
      FCursorPosition: smallint;
      FMenuEngine: TMenuEngine;
      FOwner: TMenuPage;
      FReferrer: TGameMenuBox;
      FSetupValue: integer;
      FBlank: boolean;

      procedure return; inline;
      function focused: boolean; inline;
      procedure doButton(const input: TButtonCode); virtual;
      procedure doCursor(position: smallint); virtual;
      procedure doSetup(value: integer); virtual;
   public
      constructor Create(parent: TMenuSpriteEngine; coords: TRect; main: TMenuEngine; owner: TMenuPage); reintroduce;
      procedure Draw; override;
      procedure setup(value: integer);
      procedure button(const input: TButtonCode); override; final;
      procedure placeCursor(position: smallint);
      procedure focusPage(which: TMenuPage);
      procedure focusMenu(which: TGameMenuBox);

      property cursorPosition: smallint read getCursorPosition;
      property parent: TMenuEngine read FMenuEngine;
      property onButton: TButtonFunc read FOnButton write FOnButton;
      property onCursor: TCursorFunc read FOnCursor write FOnCursor;
      property onSetup: TSetupFunc read FOnSetup write FOnSetup;
      property optionEnabled[which: word]: boolean read getOptionEnabled write setOptionEnabled;
   end;

   TMenuPage = class abstract (TObject)
   private
      FVisible: boolean;
      FOwner: TMenuEngine;
   protected
      FCurrentMenu: TGameMenuBox;
      FComponent: array of TSysFrame;
      FBounds: TRect;

      procedure setVisible(const value: boolean); virtual;
      procedure registerComponent(which: TSysFrame);
   public
      constructor Create(parent: TMenuSpriteEngine; coords: TRect; main: TMenuEngine); virtual;
      destructor Destroy; override;
      procedure focusMenu(referrer, which: TGameMenuBox; unchanged: boolean = false); virtual;
      procedure focusPage(which: TMenuPage); inline;
      procedure backTo(which: TGameMenuBox); inline;
      procedure Draw; virtual;
      procedure placeCursor(value: smallint); virtual;
      procedure setup(value: integer); virtual;
      procedure move; virtual;

      property currentMenu: TGameMenuBox read FCurrentMenu;
      property visible: boolean read FVisible write setVisible;
   end;

{$MESSAGE WARN 'Commented out code in live unit'}
(*
   TMenuScriptEngine = class(TObject)
   private
      FParent: TMenuEngine;
      FCompiledScript: AnsiString;
   protected
{      FScriptEngine: TPSExec;
      FImporter: TPSRuntimeClassImporter;
      FCompiler: TPSPascalCompiler;
}

      procedure setScript(const Value: string); virtual;
      procedure setup; virtual; abstract;
   public
      //class functions
      constructor create(parent: TMenuEngine);
      destructor Destroy; override;

      //properties
//      property compiler: TPSPascalCompiler read FCompiler;
      property parent: TMenuEngine read FParent;
      property script: string write setScript;
   end;
*)

   TMenuEngine = class(TObject)
   protected
      FCursor: TSysFrame;
      FParent: TSpriteEngine;
      FVisible: boolean;
      FStack: TRpgStack;
      FCurrentPage: TMenuPage;
      FState: TMenuState;
      FButtonLock: boolean;

      procedure setVisible(const Value: boolean); virtual; abstract;
   public
      constructor Create(parent: TMenuSpriteEngine);
      destructor Destroy; override;
      procedure focusMenu(sender, which: TMenuPage);
      procedure return;
      procedure placeCursor(value: smallint);
      procedure leave(const playSound: boolean = true);
      procedure button(const input: TButtonCode);

      property visible: boolean read FVisible write setVisible;
      property currentMenu: TMenuPage read FCurrentPage;
      property cursor: TSysFrame read FCursor;
      property buttonLock: boolean write FButtonLock;
   end;

var
   GSetupDrawMutex: TCriticalSection;
{$MESSAGE WARN 'Commented out code in live unit'}
//   GMenuScriptEngine: TMenuScriptEngine;

implementation
uses
   turbu_script_engine, turbu_2k_map_engine,
   rs_media;

const
   CURSOR_UNCHANGED = 9999;

{ TGameMenuBox }

procedure TGameMenuBox.button(const input: TButtonCode);
begin
   assert(self <> nil);
   self.doButton(input);
   if assigned(onButton) then
      onButton(input, self, FOwner);
end;

procedure TGameMenuBox.doButton(const input: TButtonCode);
begin
   inherited button(input);
   if input = btn_cancel then
   begin
      playSystemSound(sfxCancel);
      self.return;
   end;
end;

constructor TGameMenuBox.Create(parent: TMenuSpriteEngine; coords: TRect; main: TMenuEngine; owner: TMenuPage);
begin
   FMenuEngine := main;
   FOwner := owner;
   name := self.ClassName;
   inherited Create(parent, coords);
end;

procedure TGameMenuBox.Draw;
begin
   if not Visible then
      Exit;

   inherited Draw;
   if (self.focused) and (not FBlank) then
      FMenuEngine.FCursor.Draw;
end;

function TGameMenuBox.focused: boolean;
begin
   result := (FMenuEngine.FCurrentPage = FOwner) and (FOwner.FCurrentMenu = self);
end;

procedure TGameMenuBox.focusMenu(which: TGameMenuBox);
begin
   FOwner.focusMenu(self, which);
end;

procedure TGameMenuBox.focusPage(which: TMenuPage);
begin
   FOwner.focusPage(which);
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
      placeCursor(position);
end;

function TGameMenuBox.getOptionEnabled(which: word): boolean;
begin
   result := FOptionEnabled[which];
end;

procedure TGameMenuBox.setOptionEnabled(which: word; const Value: boolean);
begin
   FOptionEnabled[which] := value;
end;

procedure TGameMenuBox.setup(value: integer);
begin
   doSetup(value);
   if assigned(onSetup) then
      onSetup(value, self, FOwner);
end;

procedure TGameMenuBox.doSetup(value: integer);
begin
   if value <> CURSOR_UNCHANGED then
      FSetupValue := value
   else
      FDontChangeCursor := true;
   //end if
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

procedure TMenuEngine.button(const input: TButtonCode);
begin
   if FButtonLock then
      Exit;

   FButtonLock := true;
   case FState of
      ms_off: raise EParseMessage.create('Tried to send a menu command when the menu was not active!');
      else FCurrentPage.currentMenu.button(input);
   end;
end;

constructor TMenuEngine.Create(parent: TMenuSpriteEngine);
begin
   inherited Create;
   FParent := parent;
   FCursor := parent.cursor;
   FStack := TRpgStack.create;
   self.visible := false;
   GSetupDrawMutex := TCriticalSection.Create;
end;

destructor TMenuEngine.Destroy;
begin
   FStack.free;
   GSetupDrawMutex.Free;
   inherited;
end;

procedure TMenuEngine.focusMenu(sender, which: TMenuPage);
begin
   FStack.Push(sender);
   self.FCurrentPage := which;
end;

procedure TMenuEngine.placeCursor(value: smallint);
begin
   if FCurrentPage = nil then
      raise EFatalError.create('Tried to place a system menu cursor when the menu was not active!')
   else FCurrentPage.currentMenu.placeCursor(value);
end;

procedure TMenuEngine.leave(const playSound: boolean = true);
begin
   GGameEngine.enterLock := true;
   if playSound then
      rs_media.playSystemSound(sfxCancel);
   FState := ms_fading;
   self.visible := false;
   GGameEngine.CurrentMap.wake;
end;

procedure TMenuEngine.return;
var page: TMenuPage;
begin
   page := FStack.pop;
   if page = nil then
      self.leave(false)
   else begin
      FCurrentPage := page;
      page.setup(CURSOR_UNCHANGED);
   end;
end;

{ TMenuPage }

procedure TMenuPage.backTo(which: TGameMenuBox);
begin
   self.focusMenu(nil, which, true);
end;

constructor TMenuPage.Create(parent: TMenuSpriteEngine; coords: TRect;
  main: TMenuEngine);
begin
   inherited Create;
   FBounds := coords;
   FOwner := main;
end;

destructor TMenuPage.Destroy;
var
   i: Integer;
begin
   for i := 0 to high(FComponent) do
      FComponent[i].free;
   inherited;
end;

procedure TMenuPage.Draw;
var
   i: Integer;
begin
   GSetupDrawMutex.Enter;
   try
      assert(FOwner.FCurrentPage = self);
      for i := low(FComponent) to high(FComponent) do
         if FComponent[i].Visible then
            FComponent[i].draw;
   finally
      GSetupDrawMutex.Leave;
   end;
end;

procedure TMenuPage.focusMenu(referrer, which: TGameMenuBox; unchanged: boolean = false);
begin
   self.FCurrentMenu := which;
   if referrer <> nil then
      FCurrentMenu.FReferrer := referrer;
   if not unchanged then
      self.placeCursor(0)
   else self.placeCursor(CURSOR_UNCHANGED);
end;

procedure TMenuPage.focusPage(which: TMenuPage);
begin
   FOwner.focusMenu(self, which);
end;

procedure TMenuPage.move;
var
   i: integer;
begin
   for i := low(FComponent) to high(FComponent) do
      FComponent[i].realign;
   if assigned(FCurrentMenu) then
      placeCursor(CURSOR_UNCHANGED);
end;

procedure TMenuPage.placeCursor(value: smallint);
begin
   FCurrentMenu.placeCursor(value);
end;

procedure TMenuPage.registerComponent(which: TSysFrame);
begin
   setLength(FComponent, length(FComponent) + 1);
   FComponent[high(FComponent)] := which;
end;

procedure TMenuPage.setup(value: integer);
var
   i: Integer;
begin
   GSetupDrawMutex.Enter;
   try
      FCurrentMenu := FComponent[0] as TGameMenuBox;
      for i := low(FComponent) to high(FComponent) do
         if FComponent[i] is TGameMenuBox then
            TGameMenuBox(FComponent[i]).setup(value);
      self.move;
   finally
      GSetupDrawMutex.Leave;
   end;
end;

procedure TMenuPage.setVisible(const value: boolean);
var
   i: Integer;
begin
   for i := low(FComponent) to high(FComponent) do
      FComponent[i].Visible := value;
   self.FVisible := value;
end;

(*
{ TMenuScriptEngine }

constructor TMenuScriptEngine.create(parent: TMenuEngine);
begin
   inherited create;
   GMenuScriptEngine := self;
   FCompiler := TPSPascalCompiler.Create;
   FCompiler.BooleanShortCircuit := true;
   FImporter := TPSRuntimeClassImporter.Create;
   FScriptEngine := TPSExec.Create;
   FParent := parent;
   self.setup;
   RegisterClassLibraryRuntime(FScriptEngine, FImporter);
end;

destructor TMenuScriptEngine.Destroy;
begin
   FCompiler.free;
   FImporter.Free;
   FScriptEngine.Free;
   inherited;
end;

procedure TMenuScriptEngine.SetPointerToData(const VarName: string; Data: Pointer; aType: TIFTypeRec);
var
   v: PIFVariant;
   t: TPSVariantIFC;
begin
   v := FScriptEngine.GetVar2(VarName);
   if (Atype = nil) or (v = nil) then raise EParseMessage.Create('Unable to find variable');
   t.Dta := @PPSVariantData(v).Data;
   t.aType := v.FType;
   t.VarParam := false;
   VNSetPointerTo(t, Data, aType);
end;

procedure TMenuScriptEngine.setScript(const Value: string);
begin
   if not FCompiler.Compile(value) then
      raise EFatalError.create('Could not compile menu script file!');
   assert(FCompiler.GetOutput(FCompiledScript));
   if not FScriptEngine.LoadData(FCompiledScript) then
      raise EFatalError.create('Could not load compiled menu script file!');
end;
*)

end.
