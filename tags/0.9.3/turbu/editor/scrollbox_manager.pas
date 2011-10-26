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

unit scrollbox_manager;

interface
uses
   SysUtils, ExtCtrls, StdCtrls, Graphics,
   sg_Defs, sdl_frame;

type
   TScrollboxManager = class
   private
      imgBackground: TPaintBox;
      imgMap: TSdlFrame;
      sbHoriz: TScrollBar;
      sbVert: TScrollBar;
   private
      FGetZoom: TFunc<single>;
      FGetTileSize: TFunc<integer>;
      FGetPosition: TFunc<TSgPoint>;
      procedure configureScrollBars(const size, position: TSgPoint);
   public
      function SetMapSize(const size: TSgPoint): TSgPoint;
      constructor Create(background: TPaintBox; map: TSdlFrame; sbH, sbV: TScrollBar;
         getZoom: TFunc<single>; getTileSize: TFunc<integer>; getPosition: TFunc<TSgPoint>);
   end;

   procedure GetColorsForDate(var bg, fg: TColor);

implementation
uses
   Controls, Math;

constructor TScrollboxManager.Create(background: TPaintBox; map: TSdlFrame; sbH,
  sbV: TScrollBar; getZoom: TFunc<single>; getTileSize: TFunc<integer>;
  getPosition: TFunc<TSgPoint>);
begin
   inherited Create;
   imgBackground := background;
   imgMap := map;
   sbHoriz := sbH;
   sbVert := sbV;
   FGetZoom := getZoom;
   FGetTileSize := getTileSize;
   FGetPosition := getPosition;
end;

procedure TScrollboxManager.configureScrollBars(const size, position: TSgPoint);

   procedure configureScrollBar(scrollbar: TScrollBar; size, pageSize, position: integer);
   begin
      scrollBar.PageSize := 1;
      scrollBar.Max := size;
      scrollBar.PageSize := pageSize;
      scrollBar.LargeChange := scrollBar.PageSize - FGetTileSize();
      scrollBar.Position := min(position, scrollBar.Max - scrollBar.PageSize);
   end;

begin
   configureScrollBar(sbHoriz, size.x, min(imgMap.LogicalWidth, size.x), position.x);
   configureScrollBar(sbVert, size.y, min(imgMap.LogicalHeight, size.y), position.y);
end;

function TScrollboxManager.SetMapSize(const size: TSgPoint): TSgPoint;
var
   pSize: TSgPoint;
begin
   pSize := size * FGetZoom();
   if (pSize.x >= imgBackground.ClientWidth) and (pSize.y >= imgBackground.ClientHeight) then
   begin
      imgMap.Align := alClient;
   end
   else begin
      imgMap.Align := alNone;
      imgMap.Width := min(imgBackground.ClientWidth, pSize.x);
      imgMap.Height := min(imgBackground.ClientHeight, pSize.y);
      imgMap.Left := (imgBackground.ClientWidth - imgMap.width) div 2;
      imgMap.Top := (imgBackground.ClientHeight - imgMap.height) div 2;
   end;
   if (imgMap.Width < imgBackground.ClientWidth) and (imgMap.Height < imgBackground.ClientHeight) then
      result := size
   else result := sgPoint(imgMap.Width, imgMap.Height) / FGetZoom();
   imgMap.LogicalSize := result;
   configureScrollBars(size, FGetPosition());
end;

procedure GetColorsForDate(var bg, fg: TColor);
const clOrange = $0045C9;
var
   year, month, day: word;
begin
   DecodeDate(date, year, month, day);
   if (month = 10) and (day = 31) then
   begin
      bg := clBlack;
      fg := clOrange;
   end
   else if (month = 12) and (day in [24, 25]) then
   begin
      bg := clGreen;
      fg := clRed;
   end
   else if (month = 7) and (day = 4) then
   begin
      bg := clRed;
      fg := clBlue;
   end
   else begin
      bg := clGray;
      fg := clWhite;
   end;
end;

end.
