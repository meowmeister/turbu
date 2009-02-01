unit rm2x_shop_menu;
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
   types,
   chipset_data, shop_data, frames, menu_basis, rm2x_menu_components, item_data,
   script_interface, script_backend, charset_data, tiles, rpg_list,
   {asphyreSprite} SDL_sprite;

type
   TShopState = (ss_selling, ss_buying, ss_transaction);

   TShopModeBox = class(TRm2kGameMenuBox)
   private
      FAccessed: boolean;
      procedure return;
   public
      constructor Create(parent: TSpriteEngine; coords: TRect; main: TMenuEngine; owner: TMenuPage);
      procedure doSetup(value: integer); override;
      procedure Draw; override;
      procedure doButton(const input: TButtonCode); override;
   end;

   TStockMenu = class(TCustomScrollBox)
   private
      procedure update(cash: integer);
   protected
      procedure drawItem(id, x, y: word; color: byte); override;
   public
      constructor Create(parent: TSpriteEngine; coords: TRect; main: TMenuEngine; owner: TMenuPage);
      procedure doSetup(value: integer); override;
      procedure doButton(const input: TButtonCode); override;
      procedure doCursor(position: smallint); override;
   end;

   TTransactionState = (ts_off, ts_buying, ts_selling);

   TTransactionMenu = class(TRm2kGameMenuBox)
   private
      FState: TTransactionState;
      FItem: TRpgItem;
      FExistingQuantity: byte;

      procedure setItem(const Value: TRpgItem);
   public
      procedure Draw; override;
      procedure doCursor(position: smallint); override;
      procedure doButton(const input: TButtonCode); override;
      procedure doSetup(value: integer); override;

      property state: TTransactionState read FState write FState;
      property item: TRpgItem read FItem write setItem;
   end;

   TCompatSprite = class(TSystemMiniTile)
   private
      FTemplate: TRpgHero;
      FItem: TRpgItem;
      FTickCount: byte;
      FHeartbeat: boolean;
   public
      constructor Create(const AParent: TSpriteEngine; const template: TRpgHero); reintroduce;
      procedure Draw; override;

      property item: TRpgItem write FITem;
   end;

   TShopCompatBox = class(TRm2kGameMenuBox)
   private
      FItem: TRpgItem;
      FParty: array [1..MAXPARTYSIZE] of TCompatSprite;

      procedure setItem(const Value: TRpgItem);
   public
      procedure Draw; override;
      procedure doSetup(value: integer); override;
      procedure moveTo(coords: TRect); override;

      property item: TRpgItem write setItem;
   end;

   TShopQuantityBox = class(TSystemFrame)
   private
      FItem: TRpgItem;
   public
      procedure Draw; override;
      property item: TRpgItem write FItem;
   end;

   TShopMenuPage = class(TMenuPage)
   private
      FMainBox: TShopModeBox;
      FDescBox: TOnelineLabelBox;
      FPromptBox: TOnelineLabelBox;
      FStockBox: TStockMenu;
      FInventoryBox: TGameItemMenu;
      FTransactionBox: TTransactionMenu;
      FCompat: TShopCompatBox;
      FQuantities: TShopQuantityBox;
      FCash: TGameCashMenu;
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
      constructor Create(parent: TSpriteEngine; coords: TRect; main: TMenuEngine); override;
      procedure setup(value: integer); override;
      procedure focusMenu(referrer, which: TGameMenuBox; unchanged: boolean = false); override;

      property descBox: TOnelineLabelBox read FDescBox;
      property itemMenu: TGameItemMenu read FInventoryBox;
      property mainBox: TShopModeBox read FMainBox;
      property transactionBox: TTransactionMenu read FTransactionBox;
      property state: TShopState read FState write setState;
      property inventory: TStoreInventory read FInventory;
   end;

const SELLBACK_RATIO = 0.5;

implementation

uses
   classes, sysUtils,
   commons, LDB, chipset_graphics, text_graphics, rm2x_menu_engine,
   script_engine, hero_data, rs_system;

{ TShopModeBox }

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
            self.focusMenu(owner.FStockBox);
            owner.FStockBox.setup(0);
         end;
         1: //sell
         begin
            owner.FInventoryBox.inventory := GParty.inventory;
            self.focusMenu(owner.FInventoryBox);
            owner.FInventoryBox.setup(0);
            owner.FPromptBox.text := GDatabase.shopVocab[owner.FFormat, shp_sellWhat];
         end;
         2: //equip
         begin
            self.focusPage(GEquipmentPage);
            GEquipmentPage.setup(GParty[1].template.id);
         end;
         3: self.return; //leave
      end;
   end
   else if input = btn_cancel then
      self.return;
   //end IF
end;

constructor TShopModeBox.Create(parent: TSpriteEngine; coords: TRect; main: TMenuEngine; owner: TMenuPage);
var
   I: Integer;
const
   MENUTEXT: string = ' ' + #3 + 'Buy' + #3 + 'Sell' + #3 + 'Equip' + #3 + 'Leave';
begin
   inherited Create(parent, coords, main, owner);
   self.text := MENUTEXT;
   setLength(FOptionEnabled, 4);
   for I := 0 to 3 do
      FOptionEnabled[i] := true;
   FColumns := 2;
   FPromptLines := 1;
end;

procedure TShopModeBox.Draw;
var
  I: Integer;
  j, color: byte;
  origin: tpoint;
begin
   inherited Draw;
   origin := point(round(FCorners[topLeft].x - engine.WorldX), round(fcorners[topLeft].Y - engine.WorldY));
   drawText(FParsedText[0], 13, origin.y + 12, 0);
   for I := 1 to FParsedText.count - 1 do
   begin
      j := i + 1;
      if FOptionEnabled[i - 1] then
         color := 0
      else color := 3;
      drawText(FParsedText[i], 13 + (j mod FColumns) * (columnWidth + SEPARATOR),
               ((j div 2)) * 15 + origin.y + 12, color);
   end;
end;

procedure TShopModeBox.return;
begin
   if TShopMenuPage(FOwner).FTransactionComplete then
      TGameMap(Engine).menuInt := 1
   else TGameMap(Engine).menuInt := 0;
   TShopMenuPage(FOwner).FOngoing := false;
   TShopMenuPage(FOwner).FInventoryBox.inventory := nil;
   inherited return;
end;

procedure TShopModeBox.doSetup(value: integer);
var
   which, i: byte;
begin
   inherited doSetup(value);
   which := TShopMenuPage(FOwner).FFormat;
   with GDatabase do
   begin
      if not FAccessed then
         FParsedText[0] := shopVocab[which, shp_greet]
      else FParsedText[0] := shopVocab[which, shp_regreet];
      FParsedText[1] := shopVocab[which, shp_buy];
      FParsedText[2] := shopVocab[which, shp_sell];
//      FParsedText[3] := vocabulary[equipment]
      FParsedText[4] := shopVocab[which, shp_leave];
   end;
   self.placeCursor(0);
   FAccessed := true;
   for I := 0 to 3 do
      FOptionEnabled[i] := true;
   case TShopMenuPage(FOwner).FShopStyle of
      0: ;
      1: FOptionEnabled[1] := false;
      2: FOptionEnabled[0] := false;
      else raise EFatalError.create('Bad shop style!');
   end;
end;

{ TStockMenu }

constructor TStockMenu.Create(parent: TSpriteEngine; coords: TRect; main: TMenuEngine; owner: TMenuPage);
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
      owner.FTransactionBox.item := TRpgItem.newItem(TWordWrap(FParsedText.Objects[FCursorPosition]).getData, 1);
      owner.state := ss_transaction;
      self.focusMenu(owner.FTransactionBox);
      owner.FTransactionBox.setup(0);
   end;
end;

procedure TStockMenu.doCursor(position: smallint);
var dummy: TRpgItem;
begin
   inherited doCursor(position);
   dummy := TRpgItem.newItem(TWordWrap(FParsedText.Objects[position]).getData, 1);
   TShopMenuPage(FOwner).FCompat.item := dummy;
   TShopMenuPage(FOwner).FDescBox.text := dummy.desc;
end;

procedure TStockMenu.doSetup(value: integer);
var
   inventory: TStoreInventory;
   i: smallint;
begin
   inherited doSetup(value);
   inventory := (FOwner as TShopMenuPage).inventory;
   FParsedText.Clear;
   for i := 0 to inventory.length - 1 do
      FParsedText.AddObject(GDatabase.item[inventory[i]].name, tWordWrap.create(inventory[i]));
   //end FOR
   setLength(FOptionEnabled, FParsedText.count);
   self.update(GParty.money);
   if self.focused then
      self.placeCursor(value);
   TShopMenuPage(FOwner).FPromptBox.text := GDatabase.shopVocab[TShopMenuPage(FOwner).FFormat, shp_buyWhat];
end;

procedure TStockMenu.drawItem(id, x, y: word; color: byte);
var dummy: TItem;
begin
   dummy := GDatabase.item[TWordWrap(FParsedText.Objects[id]).getData];
   drawText(dummy.name, x, y, color);
   drawTextTo(intToStr(dummy.cost), FBounds.Right - 10, y, color);
end;

procedure TStockMenu.update(cash: integer);
var
   dummy: word;
   i: smallint;
begin
   for I := 0 to FParsedText.Count - 1 do
   begin
      dummy := TWordWrap(FParsedText.Objects[i]).getData;
      FOptionEnabled[i] := (GParty.money >= GDatabase.item[dummy].cost)
                           and (GParty.inventory.quantityOf(dummy) < MAXITEMS);
   end;
end;

{ TTransactionMenu }

procedure TTransactionMenu.Draw;
var dummy: word;
begin
   inherited Draw;
   if not assigned(FItem) then
      Exit;
   if FBlank then
      Exit;

   drawText(FItem.name, origin.X + 8, origin.Y + 42, 0);
   drawText('x', origin.X + 136, origin.Y + 42, 0);
   drawTextTo(intToStr(FExistingQuantity), origin.X + 168, origin.Y + 42, 0);

   dummy := drawTextTo(GDatabase.vocabulary[moneyUnit], self.rightside, origin.Y + 74, 1);
   if FState = ts_buying then
      drawTextTo(intToStr(FItem.cost * FExistingQuantity), dummy - 8, origin.Y + 74, 0)
   else drawTextTo(intToStr(trunc(FItem.cost * FExistingQuantity * SELLBACK_RATIO)), dummy - 8, origin.Y + 74, 0);
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
   case fstate of
      ts_off: assert(false);
      ts_buying:
      begin
         dummy := GParty.inventory.indexOf(FItem.id);
         if dummy <> -1 then
            maximum := MAXITEMS - (GParty.inventory[dummy] as TRpgItem).quantity
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
            GCurrentEngine.mediaPlayer.playSystemSound(sfxAccept);
            FBlank := true;
            case FState of
               ts_off: assert(false);
               ts_buying:
               begin
                  owner.FPromptBox.text := GDatabase.shopVocab[owner.FFormat, shp_bought];
                  sleep(750);
                  FItem.quantity := FExistingQuantity;
                  GParty.inventory.Add(FItem);
                  assert(GParty.money >= FItem.quantity * FItem.cost);
                  GParty.money := GParty.money - FItem.quantity * FItem.cost;
               end;
               ts_selling:
               begin
                  owner.FPromptBox.text := GDatabase.shopVocab[owner.FFormat, shp_sold];
                  sleep(750);
                  if FExistingQuantity < FItem.quantity then
                     FItem.quantity := FItem.quantity - FExistingQuantity
                  else GParty.inventory.Remove(FItem.id, FItem.quantity);
                  GParty.money := GParty.money + trunc(FExistingQuantity * FItem.cost * SELLBACK_RATIO);
                  owner.FPromptBox.text := GDatabase.shopVocab[owner.FFormat, shp_sellWhat];
               end;
            end;
            owner.FTransactionComplete := true;
         end;
      btn_up:
         if FExistingQuantity + 10 <= maximum then
            inc(FExistingQuantity, 10);
         //end if
      btn_down:
         if FExistingQuantity >= 10 then
            dec(FExistingQuantity, 10);
         //end if
      btn_right:
         if FExistingQuantity < maximum then
            inc(FExistingQuantity);
         //end if
      btn_left:
         if FExistingQuantity > 0 then
            dec(FExistingQuantity);
         //end if
      else assert(false);
   end;
   if FExistingQuantity <> current then
      GCurrentEngine.mediaPlayer.playSystemSound(sfxCursor);

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
      coords := rect(origin.X + 150, origin.y + 38, 22, 20);
      GGameEngine.cursor.visible := true;
      GGameEngine.cursor.layout(coords);
   end;
end;

procedure TTransactionMenu.doSetup(value: integer);
begin
   inherited;
   case FState of
      ts_off: ;
      ts_buying: TShopMenuPage(FOwner).FPromptBox.text := GDatabase.shopVocab[TShopMenuPage(FOwner).FFormat, shp_buyQty];
      ts_selling: TShopMenuPage(FOwner).FPromptBox.text := GDatabase.shopVocab[TShopMenuPage(FOwner).FFormat, shp_sellQty];
   end;
   FBlank := false;
end;

{ TCompatSprite }

constructor TCompatSprite.Create(const AParent: TSpriteEngine; const template: TRpgHero);
var
   dummy: string;
begin
   inherited Create(AParent, nil);
   FTemplate := template;
   dummy := template.template.sprite;
   GGameEngine.loadShopCharset(dummy);
   self.ImageName := 'shop ' + dummy;
   self.imageIndex := 1 + (template.template.spriteIndex * 3);
end;

procedure TCompatSprite.Draw;
var
   i: word;
begin
   FHeartbeat := not FHeartbeat;
   if FHeartbeat then
      inc(FTickCount);
   if FTickCount = 16 then
      FTickCount := 0;
   if FItem.template.usableBy[FTemplate.template.id] then
   begin
      i := FTemplate.template.spriteIndex * 3;
      case FTickCount div 4 of
         0,2: inc(i);
         1: ;
         3: inc(i, 2);
      end;
      imageIndex := i;
   end
   else imageIndex := FTemplate.template.spriteIndex + 24;
   inherited Draw;
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
      if GParty[i] <> GCurrentEngine.hero[0] then
      begin
         FParty[i] := TCompatSprite.Create(self.engine, GParty[i]);
         FParty[i].X := FBounds.Left + 8 + ((i - 1) * 32);
         FParty[i].Y := FBounds.Top + 8;
         FParty[i].item := self.FItem;
      end;
   end;
end;

procedure TShopCompatBox.Draw;
var
  i: byte;
begin
   inherited Draw;
   if not assigned(FItem) then
      Exit;

   for I := 1 to MAXPARTYSIZE do
      if assigned(FParty[i]) then
         FParty[i].Draw;
      //end IF
   //end FOR
end;

procedure TShopCompatBox.moveTo(coords: TRect);
var i: byte;
begin
   inherited moveTo(coords);
   for I := 1 to MAXPARTYSIZE do
   begin
      if assigned(FParty[i]) then
      begin
         FParty[i].X := FBounds.Left + 8 + ((i - 1) * 32);
         FParty[i].Y := FBounds.Top + 8;
      end;
   end;
end;

procedure TShopCompatBox.setItem(const Value: TRpgItem);
var i: byte;
begin
   FItem.Free;

   FItem := value;
   for I := 1 to MAXPARTYSIZE do
      if assigned(FParty[i]) then
         FParty[i].item := value;
      //end IF
   //end FOR
   TShopMenuPage(FOwner).FQuantities.item := Value;
end;

{ TShopQuantityBox }

procedure TShopQuantityBox.Draw;
begin
   inherited Draw;
   if not assigned(FItem) then
      Exit;

   drawText(GDatabase.vocabulary[ownedItems], origin.x + 10, origin.y + 10, 1);
   drawTextTo(intToStr(rs_system.heldItems(FItem.id, false)), self.rightside, origin.Y + 10, 0);
   drawText(GDatabase.vocabulary[equippedItems], origin.X + 10, origin.Y + 26, 1);
   drawTextTo(intToStr(rs_system.heldItems(FItem.id, true)), self.rightside, origin.Y + 26, 0);
end;

{ TShopMenuPage }

function TShopMenuPage.topRect(input: TRect): TRect;
begin
   result := rect(input.Left, input.Top, input.Right, 32);
end;

function TShopMenuPage.midLeftRect(input: TRect): TRect;
begin
   result := rect(input.left, input.Top + 32, 184, 128);
end;

function TShopMenuPage.midRect(input: TRect): TRect;
begin
   result := rect(input.Left, input.Top + 32, input.Right, 128);
end;

function TShopMenuPage.midRightBRect(input: TRect): TRect;
begin
   result := rect(input.Left + 184, input.Top + 128, 136, 32);
end;

function TShopMenuPage.midRightMRect(input: TRect): TRect;
begin
   result := rect(input.Left + 184, input.Top + 80, 136, 48);
end;

function TShopMenuPage.midRightTRect(input: TRect): TRect;
begin
   result := rect(input.Left + 184, input.Top + 32, 136, 48);
end;

function TShopMenuPage.bottomRect(input: TRect): TRect;
begin
   result := rect(input.Left, input.Top + 160, input.right, 80);
end;

function TShopMenuPage.bottomOverlapRect(input: TRect): TRect;
begin
   result := rect(input.Left + 4, input.Top + 164, input.right - 8, 32);
end;

procedure TShopMenuPage.setState(const Value: TShopState);
var
   i: byte;
begin
   FState := Value;
   for I := 2 to high(FComponent) do
      FComponent[i].Visible := false;
   if value = ss_selling then
      FInventoryBox.Visible := true
   else begin
      FCompat.Visible := true;
      FQuantities.Visible := true;
      FCash.Visible := true;
      if value = ss_buying then
         FStockBox.Visible := true
      else FTransactionBox.Visible := true;
   end;
   FPromptBox.visible := FCurrentMenu <> FMainBox;
end;

procedure TShopMenuPage.setup(value: integer);
var info: integer;
begin
   FInventory := GCurrentEngine.currentShop;
   if not FOngoing then
      FTransactionComplete := false;
   FOngoing := true;
   FPromptBox.Visible := false;
   FMainBox.FAccessed := false;
   info := GGameEngine.menuInt;
   FFormat := (info shr 4) and $FF;
   FShopStyle := info and $F;
   inherited setup(value);
end;

procedure TShopMenuPage.setVisible(const value: boolean);
begin
   FMainBox.Visible := value;
   FDescBox.Visible := value;
end;

constructor TShopMenuPage.Create(parent: TSpriteEngine; coords: TRect; main: TMenuEngine);
begin
   inherited Create(parent, coords, main);
   FMainBox := TShopModeBox.Create(parent, bottomRect(coords), main, self);
   registerComponent(FMainBox);

   FDescBox := TOnelineLabelBox.Create(parent, topRect(coords));
   registerComponent(FDescBox);

   FPromptBox := TOnelineLabelBox.Create(parent, bottomOverlapRect(coords));
   registerComponent(FPromptBox);

   FInventoryBox := TGameItemMenu.Create(parent, midRect(coords), main, self);
   FInventoryBox.onSetup := TSetupFunc(GScriptExec.GetProcAsMethodN('shop_item_setup'));
   FInventoryBox.onCursor := TCursorFunc(GScriptExec.GetProcAsMethodN('shop_item_cursor'));
   FInventoryBox.onButton := TButtonFunc(GScriptExec.GetProcAsMethodN('shop_item_button'));
   registerComponent(FInventoryBox);

   FStockBox := TStockMenu.Create(parent, midLeftRect(coords), main, self);
   FStockBox.Visible := false;
   registerComponent(FStockBox);

   FCompat := TShopCompatBox.Create(parent, midRightTRect(coords), main, self);
   FCompat.Visible := false;
   registerComponent(FCompat);

   FQuantities := TShopQuantityBox.Create(parent, midRightMRect(coords));
   FQuantities.Visible := false;
   registerComponent(FQuantities);

   FCash := TGameCashMenu.Create(parent, midRightBRect(coords));
   FCash.Visible := false;
   registerComponent(FCash);

   FTransactionBox := TTransactionMenu.Create(parent, midLeftRect(coords), main, self);
   FTransactionBox.Visible := false;
   registerComponent(FTransactionBox);

   self.state := ss_selling;
end;

procedure TShopMenuPage.focusMenu(referrer, which: TGameMenuBox; unchanged: boolean = false);
begin
   inherited focusMenu(referrer, which, unchanged);
   FPromptBox.Visible := which <> FMainBox;
end;

end.
