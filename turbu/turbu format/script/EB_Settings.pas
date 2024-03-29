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

unit EB_Settings;

interface
uses
   EventBuilder, EB_RpgScript;

type
   TEBVehicleBGM = class(TEBObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   [UsesUnit('Messages')]
   TEBSysSkin = class(TEBObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

implementation
uses
   SysUtils, TypInfo, Classes,
   turbu_defs, EB_Expressions;

{ TEBVehicleBGM }

function TEBVehicleBGM.GetNodeText: string;
const LINE = 'Set Vehicle Music: %s, %s';
begin
   result := format(LINE, [Self.GetLookup(values[0], 'Vehicles'), Text]);
end;

function TEBVehicleBGM.GetScriptText: string;
const
   LINE = 'Vehicle[%d].SetMusic(%s, %d, %d, %d, %d);';
begin
   result := format(LINE, [Values[0], QuotedStr(Text),
                           Values[1], Values[2], Values[3], Values[4]]);
end;

{ TEBSysSkin }

function TEBSysSkin.GetNodeText: string;
begin
   result := format('Change System Skin: %s', [Text]);
end;

function TEBSysSkin.GetScriptText: string;
begin
   result := format('SetSkin(%s, %s);', [QuotedStr(Text), BOOL_STR[values[0]]]);
end;

initialization
   TEBObject.RegisterClasses([TEBSysSkin, TEBVehicleBGM]);
end.
