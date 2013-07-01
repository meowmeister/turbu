unit text_graphics;
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

{uses
   asphyreFonts, asphyreSprite, asphyreDef;}

procedure drawText(text: string; x, y: word; color: byte);
function drawTextTo(text: string; x, y: word; color: byte): word;
procedure drawTextCentered(text: string; x, y: word; color: byte; width: word);

var
   whichFont: byte;

implementation
uses
   chipset_graphics, script_engine;

procedure drawText(text: string; x, y: word; color: byte);
begin
//fixme
{   with TGameMap(GCurrentEngine.parent) do
   begin
      fontEngine[whichFont].TextOut(text, x, y, systemGraphic.color[color], fxBlend);
   end;}
end;

function drawTextTo(text: string; x, y: word; color: byte): word;
begin
//   result := x - trunc(TGameMap(GCurrentEngine.parent).fontEngine[0].textWidth(text));
   drawText(text, result, y, color);
end;

procedure drawTextCentered(text: string; x, y: word; color: byte; width: word);
var
   midpoint, textWidth: word;
begin
   midpoint := x + (width div 2);
//   textWidth := trunc(TGameMap(GCurrentEngine.parent).fontEngine[0].textWidth(text));
   drawText(text, midpoint - (textWidth div 2), y, color);
end;

end.
