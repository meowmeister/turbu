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

unit EB_Maps;

interface
uses
   EventBuilder, EB_Expressions, turbu_defs;

type
   [UsesUnit('Maps')]
   TEBMapObject = class(TEBObject)
   protected
      function MapName(id: integer): string;
      function TransitionName(id: integer): string;
      function TransitionTypeName(id: integer): string;
      function RGB32(value: integer): integer;
      function FacingName(id: integer): string;
   end;

   TEBTransition = class(TEBMapObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBTeleport = class(TEBMapObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBMemorizeLocation = class(TEBMapObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBMemoTeleport = class(TEBMapObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBRideVehicle = class(TEBMapObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBTeleportVehicle = class(TEBMapObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBTeleportMapObj = class(TEBMapObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBSwapObjects = class(TEBMapObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBTerrainID = class(TEBMapObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBMapObjID = class(TEBMapObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBEraseScreen = class(TEBMapObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBShowScreen = class(TEBMapObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBTintScreen = class(TEBMapObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBFlashScreen = class(TEBMapObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBEndFlash = class(TEBMapObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBShakeScreen = class(TEBMapObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBPanScreen = class(TEBMapObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBWeather = class(TEBMapObject)
   private
      function WeatherName(id: integer): string;
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBImageBlock = class(TEBMapObject)
   protected
      function GetScriptEnd(indent: integer): string; virtual;
   public
      function GetScript(indent: integer): string; override;
   end;

   TEBNewImage = class(TEBImageBlock)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBImageColor = class(TEBMapObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBImageEffect = class(TEBMapObject)
   private
      function EffectName(id: integer): string;
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBImageMove = class(TEBImageBlock)
   protected
      function GetScriptEnd(indent: integer): string; override;
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBImageErase = class(TEBMapObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBShowAnim = class(TEBMapObject)
   private
      function AnimName(id: integer): string;
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBFlashObj = class(TEBMapObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBMoveMapObj = class(TEBMapObject)
   private
      function MoveString: string;
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBWaitMove = class(TEBMapObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBStopMove = class(TEBMapObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBChangeTileset = class(TEBMapObject)
   private
      function TilesetName(id: integer): string;
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBChangeBG = class(TEBMapObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBEncounterRate = class(TEBMapObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBTileSub = class(TEBMapObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBTeleLoc = class(TEBMapObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBTeleEnable = class(TEBMapObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBEscapeLoc = class(TEBMapObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBEscapeEnable = class(TEBMapObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBSaveEnable = class(TEBMapObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

implementation
uses
   Classes, SysUtils, TypInfo,
   EB_RPGScript;

{ TEBMapObject }

function TEBMapObject.FacingName(id: integer): string;
begin
   result := GetEnumName(TypeInfo(TFacing), id);
end;

function TEBMapObject.MapName(id: integer): string;
begin
   result := GetLookup(id, 'metadata');
end;

function TEBMapObject.RGB32(value: integer): integer;
begin
   result := round(value * (255 / 31));
end;

function TEBMapObject.TransitionName(id: integer): string;
begin
   result := GetEnumName(TypeInfo(TTransitions), id);
end;

function TEBMapObject.TransitionTypeName(id: integer): string;
begin
   result := GetEnumName(TypeInfo(TTransitionTypes), id);
end;

{ TEBTransition }

function TEBTransition.GetNodeText: string;
const LINE = 'Change Transition %s: %s';
begin
   result := format(LINE, [CleanEnum(TransitionTypeName(Values[0])),
                           CleanEnum(TransitionName(Values[1]))]);

end;

function TEBTransition.GetScriptText: string;
const LINE = 'SetTransition(%s, %s);';
begin
   result := format(LINE, [TransitionTypeName(Values[0]),
                           TransitionName(Values[1])]);
end;

{ TEBTeleport }

function TEBTeleport.GetNodeText: string;
const
   LINE = 'Teleport: %s, (%.3d,%.3d), %s';
   FACING: array[0..4] of string = ('Retain Facing', 'Up', 'Right', 'Down', 'Left');
begin
   result := format(LINE, [MapName(Values[0]), Values[1], Values[2], FACING[Values[3]]]);
end;

function TEBTeleport.GetScriptText: string;
begin
   result := format('Teleport(%d, %d, %d, %d);', [Values[0], Values[1], Values[2], Values[3]]);
end;

{ TEBMemorizeLocation }

function TEBMemorizeLocation.GetNodeText: string;
const LINE = 'Memorize Location: Ints[%s], Ints[%s], Ints[%s]';
begin
   result := format(LINE, [IntName(Values[0]), IntName(Values[1]), IntName(Values[2])]);
end;

function TEBMemorizeLocation.GetScriptText: string;
const LINE = 'MemorizeLocation(Ints[%d], Ints[%d], Ints[%d]);';
begin
   result := format(LINE, [Values[0], Values[1], Values[2]]);
end;

{ TEBMemoTeleport }

function TEBMemoTeleport.GetNodeText: string;
const LINE = 'Teleport to Memorized Location: Ints[%s], (Ints[%s], Ints[%s])';
begin
   result := format(LINE, [IntName(Values[0]), IntName(Values[1]), IntName(Values[2])]);
end;

function TEBMemoTeleport.GetScriptText: string;
begin
   result := format('Teleport(Ints[%d], Ints[%d], Ints[%d], 0);', [Values[0], Values[1], Values[2]]);
end;

{ TEBRideVehicle }

function TEBRideVehicle.GetNodeText: string;
begin
   result := 'Ride Vehicle';
end;

function TEBRideVehicle.GetScriptText: string;
begin
   result := 'RideVehicle;';
end;

{ TEBTeleportVehicle }

function TEBTeleportVehicle.GetNodeText: string;
const
   LINE = 'Teleport Vehicle %s: %s, (%.3d,%.3d));';
   LINE2 = 'Teleport Vehicle %s: Ints[%s], (Ints[%s], Ints[%s]));';
var
   vname: string;
begin
   vname := VehicleName(values[0]);
   if boolean(Values[1]) then
      result := format(LINE2, [vname, IntName(Values[2]), IntName(Values[3]), IntName(Values[4])])
   else result := format(LINE, [vname, MapName(Values[2]), Values[3], Values[4]]);
end;

function TEBTeleportVehicle.GetScriptText: string;
const
   LINE = 'TeleportVehicle(%d, %d, %d, %d);';
   LINE2 = 'TeleportVehicle(%d, ints[%d], ints[%d], ints[%d]);';
begin
   if boolean(Values[1]) then
      result := LINE2
   else result := LINE;
   result := format(result, [Values[0], Values[2], Values[3], Values[4]]);
end;

{ TEBTeleportMapObj }

function TEBTeleportMapObj.GetNodeText: string;
const LINE = 'Teleport Map Object %s: (%.3d,%.3d)';
begin
   if boolean(Values[0]) then
   begin
      result := stringReplace(LINE, '%.3d', 'Ints[%s]', [rfReplaceAll]);
      result := format(result, [ChildNode[0], IntName(Values[1]), IntName(Values[2])]);
   end
   else result := format(LINE, [ChildNode[0], Values[1], Values[2]]);
end;

function TEBTeleportMapObj.GetScriptText: string;
const LINE = 'TeleportMapObject(%s, %d, %d);';
begin
   if boolean(Values[0]) then
      result := stringReplace(LINE, '%d', 'Ints[%d]', [rfReplaceAll])
   else result := LINE;
   result := format(result, [ChildScript[0], Values[1], Values[2]]);
end;

{ TEBSwapObjects }

function TEBSwapObjects.GetNodeText: string;
const LINE = 'Swap Map Objects: %s, %s';
begin
   result := format(LINE, [ChildNode[0], ChildNode[1]]);
end;

function TEBSwapObjects.GetScriptText: string;
const LINE = 'SwapMapObjects(%s, %s);';
begin
   result := format(LINE, [ChildScript[0], ChildScript[1]]);
end;

{ TEBTerrainID }

function TEBTerrainID.GetNodeText: string;
const LINE = 'Get Terrain ID: (%.3d,%.3d)';
begin
   if boolean(Values[0]) then
   begin
      result := stringReplace(LINE, '%.3d', 'Ints[%s]', [rfReplaceAll]);
      result := format(result, [IntName(Values[1]), IntName(Values[2])]);
   end
   else begin
      result := LINE;
      result := format(result, [Values[1], Values[2]]);
   end;
   result := format('%s, Ints[%s]', [result, IntName(Values[3])]);
end;

function TEBTerrainID.GetScriptText: string;
const LINE = 'GetTerrainID(%d, %d);';
begin
   if boolean(Values[0]) then
      result := stringReplace(LINE, '%d', 'Ints[%d]', [rfReplaceAll])
   else result := LINE;
   result := format(result, [Values[1], Values[2]]);
   result := format('Ints[%d] := %s', [Values[3], result]);
end;

{ TEBMapObjID }

function TEBMapObjID.GetNodeText: string;
const LINE = 'Get Map Object ID At: (%.3d,%.3d)';
begin
   if boolean(Values[0]) then
   begin
      result := stringReplace(LINE, '%.3d', 'Ints[%s]', [rfReplaceAll]);
      result := format(result, [IntName(Values[1]), IntName(Values[2])]);
   end
   else begin
      result := LINE;
      result := format(result, [Values[1], Values[2]]);
   end;
   result := format('%s, Ints[%d]', [result, Values[3]]);
end;

function TEBMapObjID.GetScriptText: string;
const LINE = 'ObjIDAt(%d, %d);';
begin
   if boolean(Values[0]) then
      result := stringReplace(LINE, '%d', 'Ints[%d]', [rfReplaceAll])
   else result := LINE;
   result := format(result, [Values[1], Values[2]]);
   result := format('Ints[%d] := %s', [Values[3], result]);
end;

{ TEBEraseScreen }

function TEBEraseScreen.GetNodeText: string;
begin
   result := format('Erase Screen: %s', [CleanEnum(TransitionName(Values[0]))]);
end;

function TEBEraseScreen.GetScriptText: string;
begin
   result := format('EraseScreen(%s);', [TransitionName(Values[0])]);
end;

{ TEBShowScreen }

function TEBShowScreen.GetNodeText: string;
begin
   result := format('Show Screen: %s', [CleanEnum(TransitionName(Values[0]))]);
end;

function TEBShowScreen.GetScriptText: string;
begin
   result := format('ShowScreen(%s);', [TransitionName(Values[0])]);
end;

{ TEBTintScreen }

function TEBTintScreen.GetNodeText: string;
const LINE = 'Tint Screen: RGB(%.3d,%.3d,%.3d), Sat:(%.3d), %s';
begin
   result := format(LINE, [Values[0], Values[1], Values[2], Values[3], SecondFraction(Values[4])]);
   if boolean(Values[5]) then
      result := result + ' (Wait)';
end;

function TEBTintScreen.GetScriptText: string;
const LINE = 'TintScreen(%d, %d, %d, %d, %d, %s);';
begin
   result := format(LINE, [Values[0], Values[1], Values[2], Values[3], Values[4], BOOL_STR[Values[5]]]);
end;

{ TEBFlashScreen }

function TEBFlashScreen.GetNodeText: string;
const LINE = 'Flash Screen: RGB(%d,%d,%d), Str: %d, %s';
begin
   result := format(LINE, [Values[0], Values[1], Values[2], Values[3], SecondFraction(Values[4])]);
   if boolean(Values[5]) then
      result := result + ' (Wait)';
   if boolean(Values[6]) then
      result := result + ' (Continual)';
end;

function TEBFlashScreen.GetScriptText: string;
const LINE = 'FlashScreen(%d, %d, %d, %d, %d, %s, %s);';
begin
   result := format(LINE, [RGB32(Values[0]), RGB32(Values[1]), RGB32(Values[2]),
                           RGB32(Values[3]), Values[4], BOOL_STR[Values[5]], BOOL_STR[Values[6]]]);
end;

{ TEBEndFlash }

function TEBEndFlash.GetNodeText: string;
begin
   result := 'End Flash Screen';
end;

function TEBEndFlash.GetScriptText: string;
begin
   result := 'EndFlashScreen';
end;

{ TEBShakeScreen }

function TEBShakeScreen.GetNodeText: string;
const LINE = 'Shake Screen: Power: %d, Speed: %d, %s';
begin
   result := format(LINE, [Values[0], Values[1], SecondFraction(Values[2])]);
   if boolean(Values[3]) then
      result := result + ' (Wait)';
end;

function TEBShakeScreen.GetScriptText: string;
const LINE = 'ShakeScreen(%d, %d, %d, %s);';
begin
   result := format(LINE, [Values[0], Values[1], Values[2], BOOL_STR[Values[3]]]);
end;

{ TEBPanScreen }

function TEBPanScreen.GetNodeText: string;
begin
   result := 'Pan Screen: ';
   case Values[0] of
      0: result := result + 'Fixed';
      1: result := result + 'Cancel Fixed';
      2: result := result + format('%s: %d Tile(s), ',
                                   [CleanEnum(FacingName(Values[1])), Values[2]]);
      3: result := result + 'Return To Hero ';
   end;
   if Values[0] >= 2 then
   begin
      result := result + 'Speed: ' + intToStr(Values[3]);
      if boolean(Values[4]) then
         result := result + ' (Wait)';
   end;
end;

function TEBPanScreen.GetScriptText: string;
const
   PANLINE = 'PanScreen(%s, %d, %d, %s);';
   RETLINE = 'ReturnScreen(%d, %s);';
begin
   case Values[0] of
      0: result := 'LockScreen;';
      1: result := 'UnlockScreen;';
      2: result := format(PANLINE, [FacingName(Values[1]), Values[2], Values[3], BOOL_STR[Values[4]]]);
      3: result := format(RETLINE, [Values[3], BOOL_STR[Values[4]]]);
   end; //end of CASE block
end;

{ TEBWeather }

function TEBWeather.WeatherName(id: integer): string;
begin
   result := GetEnumName(TypeInfo(TWeatherEffects), id);
end;

function TEBWeather.GetNodeText: string;
begin
   result := format('Set Weather: %s', [CleanEnum(WeatherName(Values[0]))]);
   if Values[0] <> 0 then
      result := result + format(', Severity %d', [Values[1]]);
end;

function TEBWeather.GetScriptText: string;
const LINE = 'SetWeather(%s, %d);';
begin
   result := format(LINE, [WeatherName(Values[0]), Values[1] + 1]);
end;

{ TEBImageBlock }

function TEBImageBlock.GetScript(indent: integer): string;
var
   list: TStringList;
   obj: TEBObject;
   tail: string;
   i: Integer;
begin
   list := TStringList.Create;
   try
      list.Add(inherited GetScript(indent));
      for obj in self do
         list.Add(obj.GetScript(indent));
      tail := self.GetScriptEnd(indent);
      if tail <> '' then
         list.Add(tail);
      if list.Count > 1 then
      begin
         for i := 0 to list.Count - 1 do
            list[i] := '  ' + list[i];
         list.insert(0, indentString(indent) + 'begin');
         list.add(indentString(indent) + 'end;');
      end;
      result := TrimRight(list.Text);
   finally
      list.free;
   end;
end;

function TEBImageBlock.GetScriptEnd(indent: integer): string;
begin
   result := '';
end;

{ TEBNewImage }

function TEBNewImage.GetNodeText: string;
const LINE = 'New Image: #%d, %s, (%.3d,%.3d)';
begin
   if boolean(Values[1]) then
   begin
      result := stringReplace(LINE, '%.3d', 'Ints[%s]', [rfReplaceAll]);
      result := format(result, [Values[0], Text, IntName(Values[2]), IntName(Values[3])]);
   end
   else begin
      result := LINE;
      result := format(result, [Values[0], Text, Values[2], Values[3]]);
   end;
end;

function TEBNewImage.GetScriptText: string;
const CALL = 'image[%d] := NewImage(%s,  %d,  %d,  %d, %d, %s, %s);';
begin
   result := CALL;
   if boolean(Values[1]) then //replace only the first two
      result := StringReplace(result, '  %d,', ' Ints[%d],', [rfReplaceAll]);
   result := format(result, [Values[0], QuotedStr(Text), Values[2], Values[3],
                             Values[4], Values[5], BOOL_STR[Values[6]], BOOL_STR[Values[7]]]);
end;

{ TEBImageColor }

function TEBImageColor.GetNodeText: string;
const LINE = 'Apply Image Colors: #%d, RGB(%.3d,%.3d,%.3d), Sat:(%.3d)';
begin
   result := format(LINE, [Values[0], Values[1], Values[2], Values[3], Values[4]]);
end;

function TEBImageColor.GetScriptText: string;
const LINE = 'image[%d].applyImageColors(%d, %d, %d, %d);';
begin
   result := format(LINE, [Values[0], Values[1], Values[2], Values[3], Values[4]]);
end;

{ TEBImageEffect }

function TEBImageEffect.EffectName(id: integer): string;
begin
   result := GetEnumName(TypeInfo(TImageEffects), id);
end;

function TEBImageEffect.GetNodeText: string;
const LINE = 'Apply Image Effect: #%d, %s';
begin
   result := format(LINE, [Values[0], CleanEnum(EffectName(Values[1]))]);
   if Values[1] <> 0 then
      result := result + format(', %d', [Values[2]]);
end;

function TEBImageEffect.GetScriptText: string;
const LINE = 'image[%d].applyImageEffect(%s, %d);';
begin
   result := format(LINE, [Values[0], EffectName(Values[1]), Values[2]]);
end;

{ TEBImageMove }

function TEBImageMove.GetNodeText: string;
const LINE = 'Move Image: #%d, (%.3d,%.3d), %s';
begin
   if boolean(Values[1]) then
   begin
      result := stringReplace(LINE, '%.3d', 'Ints[%s]', [rfReplaceAll]);
      result := format(result, [Values[0], IntName(Values[2]), IntName(Values[3]), SecondFraction(Values[6])]);
   end
   else begin
      result := LINE;
      result := format(result, [Values[0], Values[2], Values[3], SecondFraction(Values[6])]);
   end;
   if boolean(values[7]) then
      result := result + ' (Wait)';
end;

function TEBImageMove.GetScriptEnd(indent: integer): string;
begin
   if boolean(values[7]) then
      result := IndentString(indent) + format('Image[%d].WaitFor;', [Values[0]])
   else result := '';
end;

function TEBImageMove.GetScriptText: string;
const LINE = 'image[%d].MoveTo(%d,  %d,  %d, %d, %d);';
begin
   result := LINE;
   if boolean(Values[1]) then //replace only the first two
      result := StringReplace(result, '%d,  ', 'Ints[%d], ', [rfReplaceAll]);
   result := format(result, [Values[0], Values[2], Values[3], Values[4], Values[5], Values[6]]);
end;

{ TEBImageErase }

function TEBImageErase.GetNodeText: string;
begin
   result := format('Erase Image: #%d', [Values[0]]);
end;

function TEBImageErase.GetScriptText: string;
begin
   result := format('image[%d].Erase;', [Values[0]]);
end;

{ TEBShowAnim }

function TEBShowAnim.AnimName(id: integer): string;
begin
   result := GetLookup(id, 'animations');
end;

function TEBShowAnim.GetNodeText: string;
const LINE = 'Show Battle Anim: #%s, %s ';
begin
   result := format(LINE, [AnimName(Values[0]), ChildScript[0]]);
   if boolean(Values[1]) then
      result := result + ' (Wait)';
end;

function TEBShowAnim.GetScriptText: string;
const LINE = 'ShowBattleAnim(%d, %s, %s);';
begin
   result := format(LINE, [Values[0], ChildScript[0], BOOL_STR[Values[1]]]);
end;

{ TEBFlashObj }

function TEBFlashObj.GetNodeText: string;
const LINE = 'Flash Sprite: %s, RGB(%d,%d,%d), Str: %d, %s';
begin
   result := format(LINE, [ChildNode[0], Values[0], Values[1], Values[2],
                           Values[3], SecondFraction(Values[4])]);
   if boolean(Values[5]) then
      result := result + ' (Wait)';
end;

function TEBFlashObj.GetScriptText: string;
const LINE = '%s.Flash(%d, %d, %d, %d, %d, %s)';
begin
   result := format(LINE, [ChildScript[0], RGB32(Values[0]), RGB32(Values[1]),
                           RGB32(Values[2]), RGB32(Values[3]), Values[4], BOOL_STR[Values[5]]]);
end;

{ TEBMoveMapObj }

function TEBMoveMapObj.MoveString: string;
var
   list: TStringList;
   i: integer;
begin
   if self.text = '' then
      Exit('');

   list := TStringList.Create;
   try
      list.StrictDelimiter := true;
      list.CommaText := stringReplace(self.Text, ';', ',', [rfReplaceAll]);
      if list[list.count - 1] = '' then
         list.delete(list.Count - 1); //remove the empty final string
      for i := 0 to List.Count - 1 do
         if list[i] <> '' then
            list[i] := CleanEnum(list[i]);
      result := TrimLeft(list.CommaText);
   finally
      list.free;
   end;
end;

function TEBMoveMapObj.GetNodeText: string;
const LINE = 'Move map Object: %s, (%s)';
begin
   result := format(LINE, [ChildNode[0], MoveString]);
end;

function TEBMoveMapObj.GetScriptText: string;
const LINE = '%s.move(%d, %s, %s, %s);';
begin
   result := format(LINE, [ChildScript[0], Values[0], BOOL_STR[Values[1]],
                           BOOL_STR[Values[2]], QuotedStr(Text)]);
end;

{ TEBWaitMove }

function TEBWaitMove.GetNodeText: string;
begin
   result := 'Wait On All Move Scripts';
end;

function TEBWaitMove.GetScriptText: string;
begin
   result := 'WaitUntilMoved;';
end;

{ TEBStopMove }

function TEBStopMove.GetNodeText: string;
begin
   result := 'Cancel All Move Scripts';
end;

function TEBStopMove.GetScriptText: string;
begin
   result := 'StopMoveScripts;';
end;

{ TEBChangeTileset }

function TEBChangeTileset.TilesetName(id: integer): string;
begin
   result := self.GetLookup(id, 'tilesets');
end;

function TEBChangeTileset.GetNodeText: string;
begin
   result := 'Change Current Tileset: ' + CleanEnum(tilesetName(Values[0]));
end;

function TEBChangeTileset.GetScriptText: string;
begin
   result := format('ChangeTileset(%d);', [Values[0]]);
end;

{ TEBChangeBG }

function TEBChangeBG.GetNodeText: string;
begin
   result := 'Change BG Image: ' + Text;
end;

function TEBChangeBG.GetScriptText: string;
const LINE = 'SetBGImage(%s, %d, %d, %s, %s);';
var
   x, y: integer;
begin
   if values[0] = 1 then
      x := values[3]
   else x := 0;
   if values[1] = 1 then
      y := values[5]
   else y := 0;
   result := format(LINE, [QuotedStr(Text), x, y,
                           BOOL_STR[Values[2]], BOOL_STR[Values[4]]]);
end;

{ TEBEncounterRate }

function TEBEncounterRate.GetNodeText: string;
const LINE = 'Set Encounter Rate: Between %d and %d Steps';
begin
   result := format(LINE, [Values[0], Values[1]]);
end;

function TEBEncounterRate.GetScriptText: string;
const LINE = 'SetEncounterRate(%d, %d);';
begin
   result := format(LINE, [Values[0], Values[1]]);
end;

{ TEBTileSub }

function TEBTileSub.GetNodeText: string;
const LINE = 'Substitute Tiles: Layer %d, Change %.3d to %.3d';
begin
   result := format(LINE, [Values[0] + 1, Values[1], Values[2]]);
end;

function TEBTileSub.GetScriptText: string;
const LINE = 'SubstituteTiles(%d, %d, %d);';
begin
   result := format(LINE, [Values[0] + 1, Values[1], Values[2]]);
end;

{ TEBTeleLoc }

function TEBTeleLoc.GetNodeText: string;
begin
   result := 'Teleport Position: ';
   if boolean(Values[0]) then
      result := result + 'Disable'
   else result := result + format('Set to: %s, (%.3d, %.3d)', [mapName(Values[1]), Values[2], Values[3]]);
end;

function TEBTeleLoc.GetScriptText: string;
begin
   if boolean(Values[0]) then
      result := result + 'DisableTeleport;'
   else result := format('SetTeleport(%d, %d, %d);', [Values[1], Values[2], Values[3]]);
end;

{ TEBTeleEnable }

function TEBTeleEnable.GetNodeText: string;
begin
   if boolean(Values[0]) then
      result := 'Enable Teleport on Current Map'
   else result := 'Disable Teleport on Current Map';
end;

function TEBTeleEnable.GetScriptText: string;
begin
   result := format('EnableTeleport(%s);', [BOOL_STR[Values[0]]]);
end;

{ TEBEscapeLoc }

function TEBEscapeLoc.GetNodeText: string;
begin
   result := 'Escape Position: ';
   if boolean(Values[0]) then
      result := result + 'Disable'
   else result := result + format('Set to: %s, (%.3d, %.3d)', [mapName(Values[1]), Values[2], Values[3]]);
end;

function TEBEscapeLoc.GetScriptText: string;
begin
   if boolean(Values[0]) then
      result := result + 'DisableEscape;'
   else result := format('SetEscape(%d, %d, %d);', [Values[1], Values[2], Values[3]]);
end;

{ TEBEscapeEnable }

function TEBEscapeEnable.GetNodeText: string;
begin
   if boolean(Values[0]) then
      result := 'Enable Escape on Current Map'
   else result := 'Disable Escape on Current Map';
end;

function TEBEscapeEnable.GetScriptText: string;
begin
   result := format('EnableEscape(%s);', [BOOL_STR[Values[0]]]);
end;

{ TEBSaveEnable }

function TEBSaveEnable.GetNodeText: string;
begin
   if boolean(Values[0]) then
      result := 'Enable Saving on Current Map'
   else result := 'Disable Saving on Current Map';
end;

function TEBSaveEnable.GetScriptText: string;
begin
   result := format('EnableSave(%s);', [BOOL_STR[Values[0]]]);
end;

initialization
   TEBObject.RegisterClasses([TEBTransition, TEBTeleport, TEBMemorizeLocation, TEBMemoTeleport,
                    TEBRideVehicle, TEBTeleportVehicle, TEBSwapObjects, TEBTerrainID,
                    TEBMapObjID, TEBEraseScreen, TEBShowScreen, TEBTintScreen, TEBFlashScreen,
                    TEBShakeScreen, TEBPanScreen, TEBWeather, TEBNewImage, TEBImageColor,
                    TEBImageEffect, TEBImageMove, TEBImageErase, TEBShowAnim, TEBFlashObj,
                    TEBMoveMapObj, TEBWaitMove, TEBStopMove, TEBChangeTileset,
                    TEBChangeBG, TEBEncounterRate, TEBTileSub, TEBTeleLoc, TEBTeleEnable,
                    TEBEscapeLoc, TEBEscapeEnable, TEBSaveEnable, TEBTeleportMapObj,
                    TEBEndFlash]);
end.
