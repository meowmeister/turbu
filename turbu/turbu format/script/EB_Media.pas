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

unit EB_Media;

interface
uses
   EventBuilder;

type
   [UsesUnit('Media')]
   TEBMediaObject = class(TEBObject);

   TEBPlayBGM = class(TEBMediaObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBFadeBGM = class(TEBMediaObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBMemBGM = class(TEBMediaObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBPlayMemBGM = class(TEBMediaObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBPlaySFX = class(TEBMediaObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBPlayMovie = class(TEBMediaObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBSysSFX = class(TEBMediaObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBSysBGM = class(TEBMediaObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

implementation
uses
   SysUtils, Classes, TypInfo,
   EB_RpgScript, EB_ObjectHelper, EB_Expressions, turbu_defs;

{ TEBPlayBGM }

function TEBPlayBGM.GetNodeText: string;
begin
   result := 'Play BGM: ' + Text;
end;

function TEBPlayBGM.GetScriptText: string;
const LINE = 'PlayMusic(%s, %d, %d, %d, %d);';
begin
   result := format(LINE, [QuotedStr(Text), Values[0], Values[1], Values[2], Values[3]]);
end;

{ TEBFadeBGM }

function TEBFadeBGM.GetNodeText: string;
begin
   result := 'Fade Out BGM: ' + SecondFraction(Values[0]);
end;

function TEBFadeBGM.GetScriptText: string;
begin
   result := format('FadeOutMusic(%d);', [Values[0]]);
end;

{ TEBMemBGM }

function TEBMemBGM.GetNodeText: string;
begin
   result := 'Memorize BGM';
end;

function TEBMemBGM.GetScriptText: string;
begin
   result := 'MemorizeBGM;';
end;

{ TEBPlayMemBGM }

function TEBPlayMemBGM.GetNodeText: string;
begin
   result := 'Play Memorized BGM';
end;

function TEBPlayMemBGM.GetScriptText: string;
begin
   result := 'PlayMemorizedBGM;';
end;

{ TEBPlaySFX }

function TEBPlaySFX.GetNodeText: string;
begin
   result := 'Play SFX: ' + Text;
end;

function TEBPlaySFX.GetScriptText: string;
const LINE = 'PlaySound(%s, %d, %d, %d);';
begin
   result := format(LINE, [QuotedStr(Text), Values[0], Values[1], Values[2]]);
end;

{ TEBPlayMovie }

function TEBPlayMovie.GetNodeText: string;
const LINE = 'Play Movie: %s, Position: (%.3d, %.3d), Size: (%dx%d)';
begin
   if boolean(Values[0]) then
   begin
      result := StringReplace(LINE, '%.3d', 'Ints[%s]', [rfReplaceAll]);
      result := format(result, [Text, IntName(Values[1]), IntName(Values[2]), Values[3], Values[4]]);
   end
   else result := LINE;
   result := format(result, [Text, Values[1], Values[2], Values[3], Values[4]]);
end;

function TEBPlayMovie.GetScriptText: string;
const LINE = 'PlayMovie(%s,  %d,  %d, %d, %d);';
begin
   if boolean(Values[0]) then //only replace the first two
      result := StringReplace(LINE, '  %d', ' Ints[%d]', [rfReplaceAll])
   else result := LINE;
   result := format(result, [Text, Values[1], Values[2], Values[3], Values[4]]);
end;

{ TEBSysSFX }

function TEBSysSFX.GetNodeText: string;
const LINE = 'Set System SFX: %s, %s';
begin
   result := format(LINE, [CleanEnum(GetEnumName(TypeInfo(TSfxTypes), Values[0])), Text]);
end;

function TEBSysSFX.GetScriptText: string;
const LINE = 'SetSystemSound(%s, %s, %d, %d, %d);';
begin
   result := format(LINE, [GetEnumName(TypeInfo(TSfxTypes), Values[0]), QuotedStr(Text),
                           Values[1], Values[2], Values[3]]);
end;

{ TEBSysBGM }

function TEBSysBGM.GetNodeText: string;
const LINE = 'Set System Music: %s, %s';
begin
   result := format(LINE, [CleanEnum(GetEnumName(TypeInfo(TBgmTypes), Values[0])), Text]);
end;

function TEBSysBGM.GetScriptText: string;
const
   LINE = 'SetSystemMusic(%s, %s, %d, %d, %d, %d);';
begin
   result := format(LINE, [GetEnumName(TypeInfo(TBgmTypes), Values[0]), QuotedStr(Text),
                           Values[1], Values[2], Values[3], Values[4]]);
end;

initialization
   TEBObject.RegisterClasses([TEBPlayBGM, TEBFadeBGM, TEBMemBGM, TEBPlayMemBGM,
                    TEBPlaySFX, TEBPlayMovie, TEBSysBGM, TEBSysSFX]);
end.
