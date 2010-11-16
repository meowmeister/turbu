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

implementation
uses
   SysUtils, Classes,
   EB_RpgScript;

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
const LINE = 'PlaySound(%s, %d, %d, %d)';
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

initialization
   RegisterClasses([TEBPlayBGM, TEBFadeBGM, TEBMemBGM, TEBPlayMemBGM,
                    TEBPlaySFX, TEBPlayMovie]);
end.
