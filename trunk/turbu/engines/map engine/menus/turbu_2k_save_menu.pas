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
unit turbu_2k_save_menu;

interface
uses
   Types,
   turbu_constants, turbu_defs, timing,
   turbu_2k_frames, turbu_2k_menu_basis, turbu_2k_menu_components,
   sdl_sprite;

type
   TPortraitID = record
      name: string;
      index: integer;
      constructor Create(const name: string; index: integer);
   end;

   TSaveData = class
   private
      FName: string;
      FLevel: integer;
      FHp: integer;
      FPortraits: TArray<TPortraitID>;
   public
      constructor Create(const name: string; level, hp: integer; const portraits: TArray<TPortraitID>);
      property name: string read FName;
      property level: integer read FLevel;
      property hp: integer read FHp;
      property portraits: TArray<TPortraitID> read FPortraits;
   end;

   TSaveBox = class(TGameMenuBox)
   private
      FPortraits: TArray<TSprite>;
      FIndex: integer;
   protected
      procedure DrawText; override;
      procedure doCursor(position: smallint); override;
      procedure doButton(const input: TButtonCode); override;
      procedure doSetup(value: integer); override;
   end;

   TSaveMenuPage = class(TMenuPage)
   private
      FSlots: array[0..2] of TSaveBox;
      FTitle: TOnelineLabelBox;
      FSaveData: array[0..MAX_SAVE_SLOTS - 1] of TSaveData;
      FCursorPosition: integer;
      FTop: integer;
      FButtonLock: TRpgTimestamp;
      function readSaveData(index: integer): TSaveData;
      procedure ResetSlots;
      procedure MoveSlot(input: TButtonCode);
   public
      constructor Create(parent: TMenuSpriteEngine; coords: TRect; main: TMenuEngine;
        const layout: string); override;
      destructor Destroy; override;
      function saveData(index: integer): TSaveData;
      procedure button(input: TButtonCode); override;
      procedure setup(value: integer); override;
   end;

implementation
uses
   SysUtils, IOUtils,
   commons, project_folder,
   turbu_database, turbu_text_utils,
   turbu_2k_savegames, turbu_2k_sprite_engine, turbu_2k_map_engine,
   rs_media,
   sg_utils,
   dwsJSON;

{ TPortraitID }

constructor TPortraitID.Create(const name: string; index: integer);
begin
   self.name := name;
   self.index := index;
end;

{ TSaveData }

constructor TSaveData.Create(const name: string; level, hp: integer;
  const portraits: TArray<TPortraitID>);
begin
   FName := name;
   FLevel := level;
   FHp := hp;
   FPortraits := portraits;
end;

{ TSaveBox }

procedure TSaveBox.doButton(const input: TButtonCode);
var
   filename: string;
begin
   inherited doButton(input);
   if input = btn_enter then
   begin
      filename := TPath.Combine(GProjectFolder, format('save%.2d.tsg', [FIndex]));
      if FSetupValue = 0 then
         SaveTo(filename, GSpriteEngine.mapObj.id, true)
      else begin
         GGameEngine.Load(filename);
         FMenuEngine.leave(false);
      end;
      self.return;
   end;
end;

procedure TSaveBox.doCursor(position: smallint);
var
   coords: TRect;
begin
   if self.focused then
   begin
      coords := rect(FBounds.left + 8, FBounds.Top + 8, 52, 20);
      GMenuEngine.cursor.visible := true;
      GMenuEngine.cursor.layout(SdlRectToTRect(coords));
   end;
end;

procedure TSaveBox.doSetup(value: integer);
begin
   inherited doSetup(value);
   InvalidateText;
end;

procedure TSaveBox.DrawText;
var
   data: TSaveData;
   color: integer;
   portrait: TSprite;
   i: integer;
begin
   for portrait in FPortraits do
      portrait.Free;
   data := TSaveMenuPage(FOwner).SaveData(FIndex);
   if assigned(data) then
      color := 1
   else color := 4;
   GFontEngine.drawText(format('File %d', [FIndex]), 4, 4, color);
   if assigned(data) then
   begin
      GFontEngine.drawText(data.name, 4, 22, 1);
      GFontEngine.drawText('L', 4, 40, 2);
      GFontEngine.drawTextRightAligned(IntToStr(data.level), 22, 40, 1);
      GFontEngine.drawText('HP', 40, 40, 2);
      GFontEngine.drawTextRightAligned(IntToStr(data.hp), 70, 40, 1);
      setLength(FPortraits, length(data.portraits));
      for i := 0 to High(data.portraits) do
      begin
         FPortraits[i] := loadPortrait(data.portraits[i].name, data.portraits[i].index);
         FPortraits[i].x := 90 + (56 * i);
         FPortraits[i].y := 4;
         FPortraits[i].Draw;
      end;
   end
   else SetLength(FPortraits, 0);
end;

{ TSaveMenuPage }

constructor TSaveMenuPage.Create(parent: TMenuSpriteEngine; coords: TRect;
  main: TMenuEngine; const layout: string);
const SIZE: TPoint = (x: 320; y: 68);
var
   i: Integer;
   boxCoords: TRect;
begin
   inherited Create(parent, coords, main, layout);
   boxCoords.BottomRight := SIZE;
   boxCoords.Left := 0;
   for i := Low(FSlots) to High(FSlots) do
   begin
      boxCoords.Top := 32 + (68 * i);
      FSlots[i] := TSaveBox.Create(parent, SdlRectToTRect(boxCoords), main, self);
      FSlots[i].FIndex := i + 1;
      registerComponent(format('Slot%d', [i]), FSlots[i]);
   end;
   FTitle := TOnelineLabelBox.Create(parent, rect(0, 0, 320, 32), main, self);
   registerComponent('Title', FTitle);
end;

destructor TSaveMenuPage.Destroy;
begin
   FButtonLock.Free;
   inherited Destroy;
end;

procedure TSaveMenuPage.MoveSlot(input: TButtonCode);
begin
   case input of
      btn_down:
      begin
         if FCursorPosition = high(FSlots) then
         begin
            if FTop + high(FSlots) < MAX_SAVE_SLOTS then
            begin
               inc(FTop);
               ResetSlots;
            end;
         end
         else begin
            inc(FCursorPosition);
            focusMenu(nil, FSlots[FCursorPosition], true);
         end;
      end;
      btn_up:
      begin
         if FCursorPosition = 0 then
         begin
            if FTop > 1 then
            begin
               dec(FTop);
               ResetSlots;
            end;
         end
         else begin
            dec(FCursorPosition);
            focusMenu(nil, FSlots[FCursorPosition], true);
         end;
      end;
   end;
end;

procedure TSaveMenuPage.button(input: TButtonCode);
var
   oldSlot: integer;
begin
   if (input in [btn_up, btn_down]) and assigned(FButtonLock) and (FButtonLock.timeRemaining > 0) then
      Exit;
   FreeAndNil(FButtonLock);
   oldSlot := FTop + FCursorPosition;
   case input of
      btn_up, btn_down: MoveSlot(input);
      btn_enter, btn_cancel: inherited button(input);
   end;
   if oldSlot <> FTop + FCursorPosition then
   begin
      playSystemSound(sfxCursor);
      FButtonLock := TRpgTimestamp.Create(180);
   end;
end;

procedure TSaveMenuPage.ResetSlots;
var
   i: integer;
begin
   for i := 0 to High(FSlots) do
   begin
      FSlots[i].FIndex := FTop + i;
      FSlots[i].InvalidateText;
   end;
end;

function TSaveMenuPage.readSaveData(index: integer): TSaveData;
var
   filename, leader, portrait: string;
   obj, heroObj: TdwsJSONObject;
   party, heroes: TdwsJSONArray;
   i, hero, HP, LV, portraitID: integer;
   portraits: TArray<TPortraitID>;
begin
   filename := TPath.Combine(GProjectFolder, format('save%.2d.tsg', [index]));
   if not FileExists(filename) then
      exit(nil);
   obj := TdwsJSONObject.ParseFile(filename) as TdwsJSONObject;
   if obj = nil then
      exit(nil);
   try
      party := obj.Items['Environment'].Items['Party'].Items['Heroes'] as TdwsJSONArray;
      heroes := obj.Items['Environment'].Items['Heroes'] as TdwsJSONArray;
      for i := 0 to party.ElementCount - 1 do
      begin
         if party.Elements[i].IsNull then
            continue;
         hero := party.Elements[i].AsInteger;
         heroObj := heroes.Elements[hero - 1] as TdwsJSONObject;
         if leader = '' then
         begin
            if assigned(heroObj.Items['Name']) then
               leader := heroObj.Items['Name'].AsString
            else leader := GDatabase.hero[hero].name;
            HP := heroObj.Items['HitPoints'].AsInteger;
            if assigned(heroObj.Items['Level']) then
               LV := heroObj.Items['Level'].AsInteger
            else LV := GDatabase.hero[hero].minLevel;
         end;
         if assigned(heroObj.Items['FaceName']) then
            portrait := heroObj.Items['FaceName'].AsString
         else portrait := GDatabase.hero[hero].portrait;
         if assigned(heroObj.Items['FaceNum']) then
            portraitID := heroObj.Items['FaceNum'].AsInteger
         else portraitID := GDatabase.hero[hero].portraitIndex;
         SetLength(portraits, length(portraits) + 1);
         portraits[high(portraits)] := TPortraitID.Create(portrait, portraitID);
      end;
      result := TSaveData.Create(leader, LV, HP, portraits);
   finally
      obj.Free;
   end;
end;

function TSaveMenuPage.saveData(index: integer): TSaveData;
begin
   if clamp(index, low(FSaveData), high(FSaveData)) <> index then
      Exit(nil);
   if FSaveData[index] = nil then
      FSaveData[index] := ReadSaveData(index);
   result := FSaveData[index];
end;

procedure TSaveMenuPage.setup(value: integer);
begin
   inherited setup(value);
   if value = 0 then
      FTitle.text := GDatabase.vocab[V_SAVE_WHERE]
   else FTitle.text := GDatabase.vocab[V_LOAD_WHERE];
end;

initialization
   TMenuEngine.RegisterMenuPageEx(TSaveMenuPage, 'Save', '[]'); //layout is done in constructor
end.
