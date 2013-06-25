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

unit EB_Messages;

interface
uses
   Classes,
   EventBuilder, EB_RpgScript;

type
   [UsesUnit('Messages')]
   TEBMessageObject = class(TEBObject);

   TEBShowMessage = class(TEBMessageObject)
   protected
      function MultilineText(indent, overhang: integer): string;
   public
      function GetScript(indent: integer): string; override;
      function GetNodeText: string; override;
      function GetNode: TEBNode; override;
   end;

   TEBMessageOptions = class(TEBMessageObject)
   public
      function GetScriptText: string; override;
      function GetNodeText: string; override;
   end;

   TEBPortrait = class(TEBMessageObject)
   public
      function GetScriptText: string; override;
      function GetNodeText: string; override;
   end;

   [UsesUnit('Messages')]
   TEBChoiceExpr = class(TEBExpression)
   public
      function GetScriptText: string; override;
      function GetNodeText: string; override;
   end;

   [UsesUnit('Messages')]
   TEBChoiceMessage = class(TEBCase)
   public
      function GetNodeText: string; override;
      procedure Loaded; override;
   end;

   TEBInputNumber = class(TEBMessageObject)
   public
      function GetScriptText: string; override;
      function GetNodeText: string; override;
   end;

   TEBInputHeroName = class(TEBMessageObject)
   public
      function GetScriptText: string; override;
      function GetNodeText: string; override;
   end;

   TEBSave = class(TEBMessageObject)
   public
      function GetScriptText: string; override;
      function GetNodeText: string; override;
   end;

   TEBMenu = class(TEBMessageObject)
   public
      function GetScriptText: string; override;
      function GetNodeText: string; override;
   end;

   TEBMenuEnable = class(TEBMessageObject)
   public
      function GetScriptText: string; override;
      function GetNodeText: string; override;
   end;

   [UsesUnit('Messages')]
   TEBShop = class(TEBMaybeIf)
   public
      function GetScriptBase: string; override;
      function GetNodeText: string; override;
   end;

   [UsesUnit('Messages')]
   TEBInn = class(TEBMaybeIf)
   public
      function GetScriptBase: string; override;
      function GetNodeText: string; override;
   end;

implementation
uses
   SysUtils, TypInfo,
   turbu_defs, EB_Expressions,
   EB_ObjectHelper;

{ TEBShowMessage }

function TEBShowMessage.GetNodeText: string;
begin
   result := 'Message: ' + self.Text;
end;

function TEBShowMessage.GetNode: TEBNode;
var
   subobj: TEBObject;
begin
   result := inherited GetNode;
   for subobj in self do
   begin
      assert(subobj is TEBExtension);
      result.Add(subobj.GetNode);
   end;
end;

function TEBShowMessage.GetScript(indent: integer): string;
begin
   result := IndentString(indent) + format('ShowMessage(%s);', [self.MultilineText(indent, 12)]);
end;

function TEBShowMessage.MultilineText(indent, overhang: integer): string;
var
   wrap: string;
   child: TEBObject;
begin
   result := QuotedStr(self.Text);
   wrap := ' + CRLF +' + CRLF + IndentString(indent) + StringOfChar(' ', overhang);
   for child in self do
      result := result + wrap + QuotedStr(child.Text);
end;

{ TEBMessageOptions }

function TEBMessageOptions.GetNodeText: string;
begin
   result := 'Set Message Options: ';
   case Values[0] of
      0: result := result + 'Opaque, ';
      1: result := result + 'Transparent, ';
      2: result := result + 'Translucent, ';
   end;
   case Values[1] of
      0: result := result + 'Top, ';
      1: result := result + 'Middle, ';
      2: result := result + 'Bottom, ';
   end;
   case boolean(Values[2]) of
      false: result := result + 'Fixed Position, ';
      true: result := result + 'Don''t Hide Hero, ';
   end;
   case boolean(Values[3]) of
      false: result := result + 'Continue Events';
      true: result := result + 'Wait Until Done';
   end;
end;

function TEBMessageOptions.GetScriptText: string;
const
   CALL = 'messageOptions(%s, %s, %s, %s);';
   MBOX: array[0..2] of string = ('mb_top', 'mb_middle', 'mb_bottom');
begin
   result := format(CALL, [BOOL_STR[Values[0]], MBOX[values[1]],
                           BOOL_STR[values[2]], BOOL_STR[Values[3]]]);
end;

{ TEBPortrait }

function TEBPortrait.GetNodeText: string;
begin
   result := 'Set Message Portrait: ';
   if Text= '' then
      result := result + 'None'
   else
   begin
      result := result + Text + ' ' + intToStr(Values[0] + 1) + ', ';
      case boolean(Values[1]) of
         false: result := result + 'Left';
         true: result := result + 'Right';
      end;
      if Values[2] = 1 then
         result := result + ', Flipped';
   end;
end;

function TEBPortrait.GetScriptText: string;
const
   CALL = 'setPortrait(%s, %d, %s, %s);';
begin
   if text = '' then
      result := 'clearPortrait;'
   else
      result := format(CALL, [quotedStr(text), Values[0], BOOL_STR[Values[1]],
                              BOOL_STR[Values[2]]]);
end;

{ TEBInputNumber }

function TEBInputNumber.GetNodeText: string;
const
   LINE = 'Input Number: %d Digits, Var[%d]';
begin
   result := format(LINE, [Values[0], Values[1]]);
end;

function TEBInputNumber.GetScriptText: string;
const
   LINE = 'Ints[%d] := inputNumber(%d);';
begin
   result := format(LINE, [Values[1], Values[0]]);
end;

{ TEBChoiceExpr }

function TEBChoiceExpr.GetNodeText: string;
begin
   result := 'Show Choice: ' + self.Text;
end;

function TEBChoiceExpr.GetScriptText: string;
const LINE = 'ShowChoice(%s, %d)';
begin
   result := format(LINE, [QuotedStr(Text), Values[0]]);
end;

{ TEBChoiceMessage }

function TEBChoiceMessage.GetNodeText: string;
begin
   result := self.ChildNode[0];
end;

procedure TEBChoiceMessage.Loaded;
begin
   if (Self.ChildCount = 0) or not (self.children[0] is TEBExpression) then
   begin
      self.Insert(0, TEBChoiceExpr.Create(nil));
      self.Children[0].Text := self.Text;
      self.Text := '';
      self.children[0].Values.add(self.Values[0]);
      self.Values.Delete(0);
   end;
end;

{ TEBInputHeroName }

function TEBInputHeroName.GetNodeText: string;
begin
   result := 'Enter Hero Name: ' + HeroName(Values[0]);
end;

function TEBInputHeroName.GetScriptText: string;
const LINE = '%s := inputText(%s, %d);';
var
   heroname: string;
begin
   heroname := format('hero[%d].name', [values[0]]);
   if boolean(values[1]) then
      result := format(LINE, [heroname, heroname, values[0]])
   else result := format(LINE, [heroname, QuotedStr(''), values[0]]);
end;

{ TEBSave }

function TEBSave.GetNodeText: string;
begin
   result := 'Call Save Menu';
end;

function TEBSave.GetScriptText: string;
begin
   result := 'SaveMenu;';
end;

{ TEBMenu }

function TEBMenu.GetNodeText: string;
begin
   result := 'Open Menu';
end;

function TEBMenu.GetScriptText: string;
begin
   result := 'OpenMenu;';
end;

{ TEBMenuEnable }

function TEBMenuEnable.GetNodeText: string;
begin
   if boolean(Values[0]) then
      result := 'Enable Menu'
   else result := 'Disable Menu';
end;

function TEBMenuEnable.GetScriptText: string;
const LINE = 'MenuEnabled := %s;';
begin
   result := format(LINE, [BOOL_STR[Values[0]]]);
end;

{ TEBShop }

function TEBShop.GetNodeText: string;
const LINE = 'Open Shop: %s, (%s)';
var
   shoptype: string;
   list: TStringList;
   i: integer;
begin
   shoptype := CleanEnum(GetEnumName(TypeInfo(TShopTypes), values[0]));
   list := TStringList.Create;
   try
      for i := 2 to self.Values.Count - 1 do
         list.Add(GetLookup(Values[i], 'Items'));
      result := format(LINE, [shoptype, list.CommaText]);
   finally
      list.Free;
   end;
end;

function TEBShop.GetScriptBase: string;
const LINE = 'Shop(%s, %d, [%s])';
var
   list: TStringList;
   i: integer;
begin
   list := TStringList.Create;
   try
      for i := 2 to self.Values.Count - 1 do
         list.Add(IntToStr(Values[i]));
      result := format(LINE, [GetEnumName(TypeInfo(TShopTypes), values[0]), Values[1], list.CommaText]);
   finally
      list.Free;
   end;
end;

{ TEBInn }

function TEBInn.GetNodeText: string;
const LINE = 'Show Inn, Price: %d';
begin
   result := format(LINE, [Values[1]]);
end;

function TEBInn.GetScriptBase: string;
const LINE = 'Inn(%d, %d)';
begin
   result := format(LINE, [values[0], Values[1]]);
end;

initialization
   TEBObject.RegisterClasses([TEBShowMessage, TEBMessageOptions, TEBPortrait, TEBChoiceMessage,
                    TEBInputNumber, TEBInputHeroName, TEBSave, TEBMenu, TEBMenuEnable,
                    TEBShop, TEBInn, TEBChoiceExpr]);
end.
