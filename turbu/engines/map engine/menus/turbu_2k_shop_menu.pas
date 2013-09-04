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
unit turbu_2k_shop_menu;

interface
uses
   types,
   turbu_2k_frames, turbu_2k_menu_basis, turbu_2k_menu_components, turbu_2k_items,
   turbu_constants, turbu_defs, turbu_heroes,
   SDL_sprite;

type
   TStoreInventory = TIntArray;

   TShopModeBox = class(TGameMenuBox)
   private
      FAccessed: boolean;
      procedure return;
   public
      constructor Create(parent: TMenuSpriteEngine; coords: TRect;
        main: TMenuEngine; owner: TMenuPage); override;
      procedure doSetup(value: integer); override;
      procedure DrawText; override;
      procedure doButton(const input: TButtonCode); override;
      procedure doCursor(position: smallint); override;
   end;

   TStockMenu = class(TCustomScrollBox)
   private
      procedure update(cash: integer);
   protected
      procedure drawItem(id, x, y, color: integer); override;
   public
      constructor Create(parent: TMenuSpriteEngine; coords: TRect;
        main: TMenuEngine; owner: TMenuPage); override;
      procedure doSetup(value: integer); override;
      procedure doButton(const input: TButtonCode); override;
      procedure doCursor(position: smallint); override;
   end;

   TTransactionState = (ts_off, ts_buying, ts_selling);

   TTransactionMenu = class(TGameMenuBox)
   private
      FState: TTransactionState;
      FItem: TRpgItem;
      FExistingQuantity: byte;

      procedure setItem(const Value: TRpgItem);
   public
      procedure DrawText; override;
      procedure doCursor(position: smallint); override;
      procedure doButton(const input: TButtonCode); override;
      procedure doSetup(value: integer); override;

      property state: TTransactionState read FState write FState;
      property item: TRpgItem read FItem write setItem;
   end;

   TCompatSprite = class(TSprite)
   private
      FTemplate: TRpgHero;
      FItem: TRpgItem;
      FTickCount: integer;
      FHeartbeat: boolean;
      procedure DrawGrayscale;
   public
      constructor Create(const AParent: TSpriteEngine; const template: TRpgHero); reintroduce;
      procedure Draw; override;

      property item: TRpgItem write FITem;
   end;

   TShopCompatBox = class(TGameMenuBox)
   private
      FItem: TRpgItem;
      FParty: array [1..MAXPARTYSIZE] of TCompatSprite;

      procedure setItem(const Value: TRpgItem);
   public
      procedure DrawText; override;
      procedure doSetup(value: integer); override;

      property item: TRpgItem write setItem;
   end;

   TShopQuantityBox = class(TGameMenuBox)
   private
      FItem: TRpgItem;
   public
      procedure DrawText; override;
      property item: TRpgItem write FItem;
   end;

   TShopItemMenu = class(TCustomGameItemMenu)
   private
      procedure ShopButton(which: TButtonCode; theMenu: TGameMenuBox;
        theOwner: TMenuPage);
      procedure shopCursor(position: smallint; theMenu: TGameMenuBox;
        theOwner: TMenuPage);
      procedure shopSetup(position: integer; theMenu: TGameMenuBox;
        theOwner: TMenuPage);
   public
      constructor Create(parent: TMenuSpriteEngine; coords: TRect;
        main: TMenuEngine; owner: TMenuPage); override;
   end;

   TShopState = (ss_selling, ss_buying, ss_transaction);

   TShopMenuPage = class(TMenuPage)
   private
      FMainBox: TShopModeBox;
      FDescBox: TOnelineLabelBox;
      FPromptBox: TOnelineLabelBox;
      FStockBox: TStockMenu;
      FInventoryBox: TShopItemMenu;
      FTransactionBox: TTransactionMenu;
      FCompat: TShopCompatBox;
      FQuantities: TShopQuantityBox;
      FCash: TGameCashMenu;
      FStyle: TShopTypes;
      FState: TShopState;
      FInventory: TStoreInventory;
      FFormat: byte;
      FShopStyle: byte;

      FTransactionComplete: boolean;
      FOngoing: boolean;

      procedure setState(const Value: TShopState);
      function topRect(input: TRect): TRect; inline;
      function midRect(input: TRect): TRect; inline;
      function bottomRect(input: TRect): TRect; inline;
      function bottomOverlapRect(input: TRect): TRect; inline;
      function midLeftRect(input: TRect): TRect; inline;
      function midRightTRect(input: TRect): TRect; inline;
      function midRightMRect(input: TRect): TRect; inline;
      function midRightBRect(input: TRect): TRect; inline;
   protected
      procedure setVisible(const value: boolean); override;
   public
      constructor Create(parent: TMenuSpriteEngine; coords: TRect; main: TMenuEngine;
        const layout: string); override;
      procedure setup(value: integer); override;
      procedure setupEx(const data: TObject); override;
      procedure focusMenu(referrer, which: TGameMenuBox; unchanged: boolean = false); override;

      property descBox: TOnelineLabelBox read FDescBox;
      property itemMenu: TShopItemMenu read FInventoryBox;
      property mainBox: TShopModeBox read FMainBox;
      property transactionBox: TTransactionMenu read FTransactionBox;
      property state: TShopState read FState write setState;
      property inventory: TStoreInventory read FInventory;
   end;

const SELLBACK_RATIO = 0.5;

implementation

uses
   classes, sysUtils, OpenGL, Math,
   dm_shaders, turbu_2k_map_engine,
   turbu_text_utils, turbu_database, turbu_items, turbu_shops, turbu_OpenGL,
   turbu_2k_environment,
   delayedAction,
   sg_defs, sg_utils;

{ TShopModeBox }

constructor TShopModeBox.Create(parent: TMenuSpriteEngine; coords: TRect;
  main: TMenuEngine; owner: TMenuPage);
var
   I: Integer;
begin
   inherited Create(parent, coords, main, owner);
   setLength(FOptionEnabled, 4);
   for I := 0 to 3 do
      FOptionEnabled[i] := true;
   FColumns := 2;
   FPromptLines := 1;
end;

procedure TShopModeBox.doButton(const input: TButtonCode);
var owner: TShopMenuPage;
begin
   inherited doButton(input);
   owner := TShopMenuPage(FOwner);
   if (input = btn_enter) and (FOptionEnabled[FCursorPosition]) then
   begin
      case FCursorPosition of
         0: //buy
         begin
            owner.state := ss_buying;
            self.focusMenu('Stock', 0);
         end;
         1: //sell
         begin
            owner.FInventoryBox.inventory := GEnvironment.Party.inventory;
            self.focusMenu('Inventory', 0);
            owner.FPromptBox.text := GDatabase.vocabNum(V_SHOP_NUM_SELL_WHAT, owner.FFormat);
         end;
         2: //equip
         begin
            self.focusPage('Equipment', GEnvironment.Party[1].template.id);
         end;
         3: self.return; //leave
      end;
   end
   else if input = btn_cancel then
      self.return;
end;

procedure TShopModeBox.doCursor(position: smallint);
begin
   inherited doCursor(position);
end;

procedure TShopModeBox.DrawText;
const lOrigin: TsgPoint = (x: 4; y: 2);
var
  I, j, color: Integer;
begin
   GFontEngine.drawText(FParsedText[0], lOrigin.x, lOrigin.y, 1);
   for I := 1 to FParsedText.count - 1 do
   begin
      j := i + 1;
      if FOptionEnabled[i - 1] then
         color := 1
      else color := 4;
      GFontEngine.drawText(FParsedText[i], lOrigin.x + (j mod FColumns) * (columnWidth + SEPARATOR),
               (j div 2) * 15 + lOrigin.y, color);
   end;
end;

procedure TShopModeBox.return;
begin
   if TShopMenuPage(FOwner).FTransactionComplete then
      GMenuEngine.menuInt := 1
   else GMenuEngine.menuInt := 0;
   TShopMenuPage(FOwner).FOngoing := false;
   TShopMenuPage(FOwner).FInventoryBox.inventory := nil;
   inherited return;
end;

procedure TShopModeBox.doSetup(value: integer);
var
   which, i: integer;
begin
   inherited doSetup(value);
   FParsedText.Clear;
   which := TShopMenuPage(FOwner).FFormat;
   if not FAccessed then
      FParsedText.Add(GDatabase.VocabNum(V_SHOP_NUM_GREET, which))
   else FParsedText.Add(GDatabase.VocabNum(V_SHOP_NUM_CONTINUE, which));
   FParsedText.Add(GDatabase.VocabNum(V_SHOP_NUM_BUY, which));
   FParsedText.Add(GDatabase.VocabNum(V_SHOP_NUM_SELL, which));
   FParsedText.Add('Equipment');
   FParsedText.Add(GDatabase.VocabNum(V_SHOP_NUM_LEAVE, which));
   self.placeCursor(0);
   FAccessed := true;
   for I := 0 to 3 do
      FOptionEnabled[i] := true;
   case TShopMenuPage(FOwner).FShopStyle of
      0: ;
      1: FOptionEnabled[1] := false;
      2: FOptionEnabled[0] := false;
      else raise Exception.Create('Bad shop style!');
   end;
end;

{ TStockMenu }

constructor TStockMenu.Create(parent: TMenuSpriteEngine; coords: TRect;
   main: TMenuEngine; owner: TMenuPage);
begin
   inherited Create(parent, coords, main, owner);
   FColumns := 1;
   FDisplayCapacity := (coords.bottom div 16) - 1;
end;

procedure TStockMenu.doButton(const input: TButtonCode);
var owner: TShopMenuPage;
begin
   inherited doButton(input);
   owner := TShopMenuPage(FOwner);
   if input = btn_cancel then
   begin
      owner.state := ss_selling;
      owner.FDescBox.text := '';
   end
   else if (input = btn_enter) and (FOptionEnabled[FCursorPosition]) then
   begin
      owner.FTransactionBox.FState := ts_buying;
      owner.FTransactionBox.item := TRpgItem.newItem(NativeInt(FParsedText.Objects[FCursorPosition]), 1);
      owner.state := ss_transaction;
      self.focusMenu('Transaction', 0);
   end;
end;

procedure TStockMenu.doCursor(position: smallint);
var
   item: TRpgItem;
begin
   inherited doCursor(position);
   if position < FParsedText.count then
   begin
      item := TRpgItem.newItem(NativeInt(FParsedText.Objects[position]), 1);
      TShopMenuPage(FOwner).FCompat.item := item;
      TShopMenuPage(FOwner).FDescBox.text := item.desc;
   end;
end;

procedure TStockMenu.doSetup(value: integer);
var
   inventory: TStoreInventory;
   item: integer;
begin
   inherited doSetup(value);
   inventory := (FOwner as TShopMenuPage).inventory;
   FParsedText.Clear;
   for item in inventory do
      FParsedText.AddObject(GDatabase.findItem(item).name, TObject(Item));
   setLength(FOptionEnabled, FParsedText.count);
   self.update(GEnvironment.Party.money);
   if self.focused then
      self.placeCursor(value);
   TShopMenuPage(FOwner).FPromptBox.text :=
     GDatabase.VocabNum(V_SHOP_NUM_BUY_WHAT, TShopMenuPage(FOwner).FFormat);
end;

procedure TStockMenu.drawItem(id, x, y, color: integer);
var
   dummy: TItemTemplate;
begin
   dummy := GDatabase.findItem(NativeInt(FParsedText.Objects[id]));
   GFontEngine.drawText(dummy.name, x, y, color);
   GFontEngine.drawTextRightAligned(intToStr(dummy.cost), FBounds.Right - 22, y, color);
end;

procedure TStockMenu.update(cash: integer);
var
   item, i: integer;
begin
   for I := 0 to FParsedText.Count - 1 do
   begin
      item := nativeInt(FParsedText.Objects[i]);
      FOptionEnabled[i] := (GEnvironment.Party.money >= GDatabase.findItem(item).cost)
                           and (GEnvironment.Party.inventory.quantityOf(item) < MAXITEMS);
   end;
end;

{ TTransactionMenu }

procedure TTransactionMenu.DrawText;
var
   align: TsgFloatPoint;
begin
   if not assigned(FItem) then
      Exit;
   if FBlank then
      Exit;

   GFontEngine.drawText(FItem.name, 8, 42, 1);
   GFontEngine.drawText('x', 136, 42, 1);
   GFontEngine.drawTextRightAligned(intToStr(FExistingQuantity), 168, 42, 1);

   align := GFontEngine.drawTextRightAligned(GDatabase.vocab[V_MONEY_NAME], self.getRightSide, 74, 2);
   if FState = ts_buying then
      GFontEngine.drawTextRightAligned(intToStr(FItem.cost * FExistingQuantity), align.x - 8, 74, 1)
   else GFontEngine.drawTextRightAligned(intToStr(trunc(FItem.cost * FExistingQuantity * SELLBACK_RATIO)), align.x - 8, 74, 1);
end;

procedure TTransactionMenu.setItem(const Value: TRpgItem);
begin
   FItem := Value;
   FExistingQuantity := 1;
end;

procedure TTransactionMenu.doButton(const input: TButtonCode);
var
   owner: TShopMenuPage;
   dummy: integer;
   maximum, current: byte;
begin
   inherited doButton(input);
   owner := TShopMenuPage(FOwner);
   maximum := 0; //to suppress a compiler warning
   case FState of
      ts_off: assert(false);
      ts_buying:
      begin
         dummy := GEnvironment.Party.inventory.indexOf(FItem.id);
         if dummy <> -1 then
            maximum := min(MAXITEMS - (GEnvironment.Party.inventory[dummy] as TRpgItem).quantity,
                           GEnvironment.money div FItem.cost)
         else maximum := MAXITEMS;
      end;
      ts_selling: maximum := FItem.quantity;
   end;

   current := FExistingQuantity;
   case input of
      btn_cancel: ;
      btn_enter:
         if FExistingQuantity > 0 then
         begin
            playSound(sfxAccept);
            FBlank := true;
            case FState of
               ts_off: assert(false);
               ts_buying:
               begin
                  owner.FPromptBox.text := GDatabase.vocabNum(V_SHOP_NUM_BOUGHT, owner.FFormat);
                  sleep(750);
                  GEnvironment.Party.inventory.Add(FItem.id, FExistingQuantity);
                  assert(GEnvironment.Party.money >= FExistingQuantity * FItem.cost);
                  GEnvironment.Party.money := GEnvironment.Party.money - FExistingQuantity * FItem.cost;
               end;
               ts_selling:
               begin
                  owner.FPromptBox.text := GDatabase.vocabNum(V_SHOP_NUM_SOLD, owner.FFormat);
                  sleep(750);
                  if FExistingQuantity < FItem.quantity then
                     FItem.quantity := FItem.quantity - FExistingQuantity
                  else GEnvironment.Party.inventory.Remove(FItem.id, FItem.quantity);
                  GEnvironment.Party.money := GEnvironment.Party.money + trunc(FExistingQuantity * FItem.cost * SELLBACK_RATIO);
                  owner.FPromptBox.text := GDatabase.vocabNum(V_SHOP_NUM_SELL_WHAT, owner.FFormat);
               end;
            end;
            owner.FTransactionComplete := true;
         end;
      btn_up: FExistingQuantity := min(FExistingQuantity + 10, maximum);
      btn_down: FExistingQuantity := max(FExistingQuantity - 10, 0);
      btn_right:
         if FExistingQuantity < maximum then
            inc(FExistingQuantity);
      btn_left:
         if FExistingQuantity > 0 then
            dec(FExistingQuantity);
      else assert(false);
   end;
   if FExistingQuantity <> current then
   begin
      playSound(sfxCursor);
      InvalidateText;
   end;

   if input in [btn_cancel, btn_enter] then
   begin
      if FState = ts_selling then
         owner.state := ss_selling
      else if FState = ts_buying then
         owner.state := ss_buying
      else assert(false);
      self.state := ts_off;
      if input = btn_enter then
         self.return;
      owner.currentMenu.setup(CURSOR_UNCHANGED);
   end;
end;

procedure TTransactionMenu.doCursor(position: smallint);
var
   coords: TRect;
begin
   if self.focused then
   begin
      coords := rect(FBounds.left + 158, FBounds.Top + 46, 22, 20);
      GMenuEngine.cursor.visible := true;
      GMenuEngine.cursor.layout(SdlRectToTRect(coords));
   end;
end;

procedure TTransactionMenu.doSetup(value: integer);
begin
   inherited;
   case FState of
      ts_off: ;
      ts_buying: TShopMenuPage(FOwner).FPromptBox.text :=
        GDatabase.VocabNum(V_SHOP_NUM_HOW_MANY, TShopMenuPage(FOwner).FFormat);
      ts_selling: TShopMenuPage(FOwner).FPromptBox.text :=
        GDatabase.VocabNum(V_SHOP_NUM_SELL_QUANT, TShopMenuPage(FOwner).FFormat);
   end;
   FBlank := false;
end;

{ TCompatSprite }

constructor TCompatSprite.Create(const AParent: TSpriteEngine; const template: TRpgHero);
begin
   inherited Create(AParent);
   FTemplate := template;
   self.ImageName := template.template.mapSprite;
   self.SetSpecialRender;
end;

procedure TCompatSprite.DrawGrayscale;
var
   handle, current: integer;
   gla: TGlArrayF4;
   shaders: TdmShaders;
begin
   shaders := GGameEngine.CurrentMap.ShaderEngine;
   glGetIntegerv(gl_current_program, @current);
   try
      handle := shaders.ShaderProgram('default', 'tint', 'shift');
      shaders.UseShaderProgram(handle);
      shaders.SetUniformValue(handle, 'hShift', 0);
      shaders.SetUniformValue(handle, 'valMult', 1.0);
      gla[0] := 1;
      gla[1] := 1;
      gla[2] := 1;
      gla[3] := 1;
      shaders.SetUniformValue(handle, 'rgbValues', gla);
      shaders.SetUniformValue(handle, 'satMult', 0.0);
      inherited Draw;
   finally
      glUseProgram(current);
   end;
end;

procedure TCompatSprite.Draw;
var
   frame: integer;
   tSize: TSgPoint;
begin
   FHeartbeat := not FHeartbeat;
   if FHeartbeat then
      inc(FTickCount);
   if FTickCount = 48 then
      FTickCount := 0;
   frame := 7;
   tSize := FImage.textureSize;
   FImage.textureSize := FImage.textureSize * sgPoint(1, 2);
   try
      if FItem.usableByHypothetical(FTemplate.template.id) then
      begin
         case FTickCount div 12 of
            0: dec(frame);
            1, 3: ;
            2: inc(frame);
         end;
         imageIndex := frame;
         inherited Draw;
      end
      else begin
         imageIndex := frame;
         DrawGrayscale;
      end;
   finally
      FImage.textureSize := tSize;
   end;
end;

{ TShopCompatBox }

procedure TShopCompatBox.doSetup(value: integer);
var
   i: byte;
begin
   inherited doSetup(value);
   for I := 1 to MAXPARTYSIZE do
   begin
      FParty[i].Free;
      FParty[i] := nil;
      if GEnvironment.Party[i] <> GEnvironment.heroes[0] then
      begin
         FParty[i] := TCompatSprite.Create(self.engine, GEnvironment.Party[i]);
         FParty[i].X := 8 + ((i - 1) * 32);
         FParty[i].Y := 0;
         FParty[i].item := self.FItem;
      end;
   end;
end;

procedure TShopCompatBox.DrawText;
const ONE_PIXEL: TRect = (Right: 1; Bottom: 1);
var
   i: integer;
   first: boolean;
begin
   if not assigned(FItem) then
      Exit;

   first := false;
   for I := 1 to MAXPARTYSIZE do
      if assigned(FParty[i]) then
      begin
         //make SDL set the RGB shader
         if not first then
         begin
            first := true;
            FParty[i].Image.DrawRectTo(ONE_PIXEL, ONE_PIXEL);
         end;
         FParty[i].Draw;
      end;
   DelayExec(procedure begin InvalidateText; end); //redraw this every frame
end;

procedure TShopCompatBox.setItem(const Value: TRpgItem);
var i: byte;
begin
   FItem.Free;

   FItem := value;
   for I := 1 to MAXPARTYSIZE do
      if assigned(FParty[i]) then
         FParty[i].item := value;
   TShopMenuPage(FOwner).FQuantities.item := Value;
end;

{ TShopQuantityBox }

procedure TShopQuantityBox.DrawText;
begin
   if not assigned(FItem) then
      Exit;

   GFontEngine.drawText(GDatabase.vocab[V_ITEMS_OWNED], 2, 2, 2);
   GFontEngine.drawTextRightAligned(intToStr(GEnvironment.heldItems(FItem.id, false)), self.getRightside, 2, 1);
   GFontEngine.drawText(GDatabase.vocab[V_ITEMS_EQUIPPED], 2, 18, 2);
   GFontEngine.drawTextRightAligned(intToStr(GEnvironment.heldItems(FItem.id, true)), self.getRightside, 18, 1);
end;

{ TShopMenuPage }

function TShopMenuPage.topRect(input: TRect): TRect;
begin
   result := rect(input.Left, input.Top, input.Right, 32);
   result := SdlRectToTRect(result);
end;

function TShopMenuPage.midLeftRect(input: TRect): TRect;
begin
   result := rect(input.left, input.Top + 32, 184, 128);
   result := SdlRectToTRect(result);
end;

function TShopMenuPage.midRect(input: TRect): TRect;
begin
   result := rect(input.Left, input.Top + 32, input.Right, 128);
   result := SdlRectToTRect(result);
end;

function TShopMenuPage.midRightBRect(input: TRect): TRect;
begin
   result := rect(input.Left + 184, input.Top + 128, 136, 32);
   result := SdlRectToTRect(result);
end;

function TShopMenuPage.midRightMRect(input: TRect): TRect;
begin
   result := rect(input.Left + 184, input.Top + 80, 136, 48);
   result := SdlRectToTRect(result);
end;

function TShopMenuPage.midRightTRect(input: TRect): TRect;
begin
   result := rect(input.Left + 184, input.Top + 32, 136, 48);
   result := SdlRectToTRect(result);
end;

function TShopMenuPage.bottomOverlapRect(input: TRect): TRect;
begin
   result := rect(input.Left + 4, input.Top + 164, input.right - 8, 32);
   result := SdlRectToTRect(result);
end;

function TShopMenuPage.bottomRect(input: TRect): TRect;
begin
   result := rect(input.Left, input.Top + 160, input.right, 80);
   result := SdlRectToTRect(result);
end;

procedure TShopMenuPage.setState(const Value: TShopState);
begin
   FState := Value;
   SetVisible(true);
end;

procedure TShopMenuPage.setup(value: integer);
begin
   if not FOngoing then
      FTransactionComplete := false;
   FOngoing := true;
   FMainBox.FAccessed := false;
   inherited setup(value);
   SetVisible(true);
end;

procedure TShopMenuPage.setupEx(const data: TObject);
var
   shopData: TShopData;
begin
   inherited setupEx(data);
   shopData := data as TShopData;
   FInventory := shopData.inventory;
   FStyle := shopData.shopType;
   FFormat := shopData.messageStyle;
   self.state := ss_selling;
end;

procedure TShopMenuPage.setVisible(const value: boolean);
begin
   inherited setVisible(false);
   if value then
   begin
      FVisible := true;
      FMainBox.Visible := true;
      FDescBox.Visible := true;
      if FState = ss_selling then
         FInventoryBox.Visible := true
      else begin
         FCompat.Visible := true;
         FQuantities.Visible := true;
         FCash.Visible := true;
         if FState = ss_buying then
            FStockBox.Visible := true
         else FTransactionBox.Visible := true;
      end;
      FPromptBox.visible := FCurrentMenu <> FMainBox;
   end;
end;

constructor TShopMenuPage.Create(parent: TMenuSpriteEngine; coords: TRect; main: TMenuEngine;
  const layout: string);
begin
   inherited Create(parent, coords, main, layout);
   FMainBox := TShopModeBox.Create(parent, bottomRect(coords), main, self);
   registerComponent('Main', FMainBox);

   FDescBox := TOnelineLabelBox.Create(parent, topRect(coords), main, self);
   registerComponent('Desc', FDescBox);

   FPromptBox := TOnelineLabelBox.Create(parent, bottomOverlapRect(coords), main, self);
   registerComponent('Prompt', FPromptBox);

   FInventoryBox := TShopItemMenu.Create(parent, midRect(coords), main, self);
   registerComponent('Inventory', FInventoryBox);

   FStockBox := TStockMenu.Create(parent, midLeftRect(coords), main, self);
   registerComponent('Stock', FStockBox);

   FCompat := TShopCompatBox.Create(parent, midRightTRect(coords), main, self);
   registerComponent('Compat', FCompat);

   FQuantities := TShopQuantityBox.Create(parent, midRightMRect(coords), main, self);
   registerComponent('Quantities', FQuantities);

   FCash := TGameCashMenu.Create(parent, midRightBRect(coords), main, self);
   registerComponent('Cash', FCash);

   FTransactionBox := TTransactionMenu.Create(parent, midLeftRect(coords), main, self);
   registerComponent('Transaction', FTransactionBox);

   self.visible := false;
end;

procedure TShopMenuPage.focusMenu(referrer, which: TGameMenuBox; unchanged: boolean = false);
begin
   inherited focusMenu(referrer, which, unchanged);
   SetVisible(true);
end;

{ TShopItemMenu }

constructor TShopItemMenu.Create(parent: TMenuSpriteEngine; coords: TRect;
  main: TMenuEngine; owner: TMenuPage);
begin
   inherited Create(parent, coords, main, owner);
   self.onButton := shopButton;
   self.onCursor := shopCursor;
   self.onSetup := shopSetup;
end;

procedure TShopItemMenu.ShopButton(which: TButtonCode; theMenu: TGameMenuBox;
  theOwner: TMenuPage);
var
   owner: TShopMenuPage;
begin
   owner := TShopMenuPage(theOwner);

   if (inventory.count = 0) and (which <> btn_cancel) then Exit;

   if (which = btn_enter) and (optionEnabled[cursorPosition]) then
   begin
      owner.setState(ss_transaction);
      owner.transactionBox.state := ts_selling;
      owner.transactionBox.item := inventory[cursorPosition];
      focusMenu('Transaction', CURSOR_UNCHANGED);
   end else if which = btn_cancel then
   begin
      inventory := nil;
      owner.descBox.text := '';
   end;
end;

procedure TShopItemMenu.shopCursor(position: smallint; theMenu: TGameMenuBox;
  theOwner: TMenuPage);
var
   owner: TShopMenuPage;
begin
   owner := TShopMenuPage(theOwner);
   if assigned(inventory) and (inventory.Count > 0) then
      owner.descBox.text := inventory[position].desc;
end;

procedure TShopItemMenu.shopSetup(position: integer; theMenu: TGameMenuBox;
  theOwner: TMenuPage);
var
   i: integer;
begin
   if inventory = nil then
     Exit;
   for i := 0 to inventory.Count - 1 do
      optionEnabled[i] := inventory[i].cost > 0;
end;

initialization
   TMenuEngine.RegisterMenuPageEx(TShopMenuPage, 'Shop', '[]'); //layout is done in constructor
end.
