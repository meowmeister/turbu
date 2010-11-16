unit EB_Settings;

interface
uses
   EventBuilder, EB_RpgScript;

type
   [UsesUnit('Settings')]
   TEBSettingsObject = class(TEBObject);

   TEBSysBGM = class(TEBObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBSysSFX = class(TEBObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBSysSkin = class(TEBObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

implementation
uses
   SysUtils, TypInfo, Classes,
   turbu_defs;

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

{ TEBSysSkin }

function TEBSysSkin.GetNodeText: string;
begin
   result := format('Change System Skin: %s', [Text]);
end;

function TEBSysSkin.GetScriptText: string;
begin
   result := format('SetSkin(%s);', [QuotedStr(Text)]);
end;

initialization
   RegisterClasses([TEBSysBGM, TEBSysSFX, TEBSysSkin]);
end.
