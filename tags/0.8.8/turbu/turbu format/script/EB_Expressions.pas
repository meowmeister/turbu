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

unit EB_Expressions;

interface
uses
   EventBuilder, turbu_defs;

type
   TEBVariableValue = class(TEBExpression)
   private
      FIsGlobal: boolean;
   public
      constructor Create(name: string); reintroduce; overload;
      constructor Create(name: string; subscript: integer); reintroduce; overload;
      constructor Create(name: string; subscript: TEBExpression); reintroduce; overload;
      function GetNodeText: string; override;
   published
      property Global: boolean read FIsGlobal write FIsGlobal stored FIsGlobal;
   end;

   TEBNilValue = class(TEBVariableValue)
   public
      constructor Create(parent: TEBObject);
   end;

   TEBSwitchesValue = class(TEBVariableValue)
   public
      constructor Create(subscript: integer); overload;
      constructor Create(subscript: TEBExpression); overload;
      function GetNodeText: string; override;
   end;

   TEBIntsValue = class(TEBVariableValue)
   public
      constructor Create(subscript: integer); overload;
      constructor Create(subscript: TEBExpression); overload;
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBBooleanValue = class(TEBExpression)
   public
      constructor Create(value: boolean); reintroduce;
      function GetNodeText: string; override;
   end;

   TEBIntegerValue = class(TEBExpression)
   public
      constructor Create(value: integer); reintroduce;
      function GetNodeText: string; override;
   end;

   TEBStringValue = class(TEBExpression)
   public
      constructor Create(value: string); reintroduce;
      function GetNodeText: string; override;
   end;

   TEBEnumValue = class(TEBExpression)
   public
      constructor Create(value: string); reintroduce;
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBLookupValue = class(TEBIntegerValue)
   private
      FLookup: string;
   public
      constructor Create(value: integer; name: string);
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   published
      property lookup: string read FLookup write FLookup;
   end;

   TEBComparison = class(TEBExpression)
   public
      constructor Create(left, right: TEBExpression; comparison: TComparisonOp); reintroduce;
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBChainable = class(TEBExpression)
   private
      FHint: integer;
   protected
      function GetChain: TEBChainable; virtual;
      procedure SetChain(const Value: TEBChainable); virtual;
      function GetLink: string; virtual;
   public
      constructor Create(value: string; next: TEBChainable = nil); reintroduce;
      function GetNodeText: string; override;
      property chain: TEBChainable read GetChain write SetChain;
      function GetScriptText: string; override;
   published
      property hint: integer read FHint write FHint;
   end;

   TEBObjExpr = class(TEBChainable);

   TEBObjArrayValue = class(TEBObjExpr)
   protected
      function GetChain: TEBChainable; override;
      procedure SetChain(const Value: TEBChainable); override;
      function GetLink: string; override;
   public
      constructor Create(value: string; subscript: integer; next: TEBChainable = nil); overload;
      constructor Create(value: string; subscript: TEBExpression; next: TEBChainable = nil); overload;
   end;

   TEBLookupObjExpr = class(TEBObjArrayValue)
   private
      FLookup: string;
   public
      constructor Create(value: string; subscript: integer; name: string; next: TEBChainable = nil); overload;
      constructor Create(value: string; subscript: TEBExpression; name: string; next: TEBChainable = nil); overload;
      function GetLink: string; override;
   published
      property lookup: string read FLookup write FLookup;
   end;

   TEBPropExpr = class(TEBChainable)
   end;

   TEBCall = class(TEBChainable)
   protected
      function GetChain: TEBChainable; override;
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBMethodCall = class(TEBCall)
   public
      function GetScriptText: string; override;
   end;

   TEBCast = class(TEBExpression)
   public
      constructor Create(value: string; base: TEBExpression); reintroduce;
      function GetNodeText: string; override;
   end;

   TEBBinaryOp = class(TEBExpression)
   private
      FOp: TBinaryOp;
   public
      constructor Create(left, right: TEBExpression; op: TBinaryOp); reintroduce;
      function GetNodeText: string; override;
   published
      property op: TBinaryOp read FOp write FOp;
   end;

   TEBIntArray = class(TEBExpression)
   private
      FLookup: string;
   published
      function GetNodeText: string; override;
      function GetScriptText: string; override;
      property lookup: string read FLookup write FLookup;
   end;

   TEBParenExpr = class(TEBExpression)
   public
      constructor Create(subExpr: TEBExpression); reintroduce;
      function GetNodeText: string; override;
   end;

function CreateSubscript(mode, data: integer): TEBExpression;

const
   COMPARISONS: array[TComparisonOp] of string = ('=', '>=', '<=', '>', '<', '<>');
   HINTS: array[0..3] of string = ('', '''s', ' is', ' has');

implementation
uses
   SysUtils, Classes, Variants, Math,
   EB_RPGScript;

function CreateSubscript(mode, data: integer): TEBExpression;
begin
   case mode of
      0: result := TEBIntegerValue.Create(data);
      1: result := TEBVariableValue.Create('Num');
      2: result := TEBIntsValue.Create(data);
      else raise ERPGScriptError.CreateFmt('Unknown subscript mode value: %d!', [mode]);
   end;
end;

{ TEBVariableValue }

constructor TEBVariableValue.Create(name: string);
begin
   inherited Create(nil);
   self.Text := name;
end;

constructor TEBVariableValue.Create(name: string; subscript: integer);
begin
   self.Create(name + '[]');
   Values.Add(subscript);
end;

constructor TEBVariableValue.Create(name: string; subscript: TEBExpression);
begin
   self.Create(name + '[]');
   self.Add(subscript);
end;

function TEBVariableValue.GetNodeText: string;
begin
   if ComponentCount > 0 then
      result := StringReplace(self.Text, '[]', format('[%s]', [(components[0] as TEBExpression).GetNodeText]), [])
   else if values.Count > 0 then
      result := StringReplace(self.Text, '[]', format('[%d]', [Values[0]]), [])
   else result := self.Text;
end;

{ TEBComparison }

constructor TEBComparison.Create(left, right: TEBExpression;
  comparison: TComparisonOp);
begin
   inherited Create(nil);
   self.Add(left);
   self.Add(right);
   Values.Add(ord(comparison));
end;

function TEBComparison.GetNodeText: string;
const
   LINE = '%s %s %s';
begin
   assert(self.ComponentCount = 2);
   assert(self.Values.Count = 1);
   result := format(LINE, [(components[0] as TEBExpression).GetNodeText,
                           COMPARISONS[TComparisonOp(Values[0])],
                           (components[1] as TEBExpression).GetNodeText]);
end;

function TEBComparison.GetScriptText: string;
const
   LINE = '%s %s %s';
begin
   assert(self.ComponentCount = 2);
   assert(self.Values.Count = 1);
   result := format(LINE, [ChildScript[0], COMPARISONS[TComparisonOp(Values[0])],
                           ChildScript[1]]);
end;

{ TEBBooleanValue }

constructor TEBBooleanValue.Create(value: boolean);
begin
   inherited Create(nil);
   Values.Add(ord(value));
end;

function TEBBooleanValue.GetNodeText: string;
begin
   result := BOOL_STR[Values[0]];
end;

{ TEBSwitchesValue }

constructor TEBSwitchesValue.Create(subscript: integer);
begin
   inherited Create('Switch', subscript);
end;

constructor TEBSwitchesValue.Create(subscript: TEBExpression);
begin
   inherited Create('Switch', subscript);
end;

function TEBSwitchesValue.GetNodeText: string;
begin
   if values.count > 0 then
      result := format('Switches[%s]', [SwitchName(values[0])])
   else result := inherited GetNodeText;
end;

{ TEBIntsValue }

constructor TEBIntsValue.Create(subscript: integer);
begin
   inherited Create('Ints', subscript);
end;

constructor TEBIntsValue.Create(subscript: TEBExpression);
begin
   inherited Create('Ints', subscript);
end;

function TEBIntsValue.GetNodeText: string;
begin
   if values.count > 0 then
      result := format('Ints[%s]', [IntName(values[0])])
   else result := inherited GetNodeText;
end;

function TEBIntsValue.GetScriptText: string;
begin
   if values.count > 0 then
      result := format('Ints[%d]', [values[0]])
   else result := inherited GetScriptText;
end;

{ TEBIntegerValue }

constructor TEBIntegerValue.Create(value: integer);
begin
   inherited Create(nil);
   Values.Add(value);
end;

function TEBIntegerValue.GetNodeText: string;
begin
   result := IntToStr(Values[0]);
end;

{ TEBChainable }

constructor TEBChainable.Create(value: string; next: TEBChainable);
begin
   inherited Create(nil);
   self.text := value;
   if assigned(next) then
      Add(next);
end;

function TEBChainable.GetChain: TEBChainable;
begin
   if ComponentCount > 0 then
      result := self.Components[0] as TEBChainable
   else result := nil;
end;

function TEBChainable.GetNodeText: string;
const LINE = '%s%s %s';
begin
   result := GetLink;
   if assigned(self.chain) then
      result := format(LINE, [result, HINTS[chain.hint], chain.GetNodeText]);
end;

function TEBChainable.GetScriptText: string;
const LINE = '%s.%s';
begin
   result := GetLink;
   if assigned(self.chain) then
      result := format(LINE, [result, chain.GetScript(0)]);
end;

function TEBChainable.GetLink: string;
begin
   result := self.Text;
end;

procedure TEBChainable.SetChain(const Value: TEBChainable);
begin
   assert(self.ComponentCount = 0);
   self.Add(value);
end;

{ TEBLookupValue }

constructor TEBLookupValue.Create(value: integer; name: string);
begin
   inherited Create(value);
   FLookup := name;
end;

function TEBLookupValue.GetNodeText: string;
begin
   result := GetLookup(values[0], FLookup);
end;

function TEBLookupValue.GetScriptText: string;
begin
   result := IntToStr(values[0]);
end;

{ TEBLookupObjExpr }

constructor TEBLookupObjExpr.Create(value: string; subscript: integer;
  name: string; next: TEBChainable);
begin
   inherited Create(value, subscript, next);
   FLookup := name;
end;

constructor TEBLookupObjExpr.Create(value: string; subscript: TEBExpression;
  name: string; next: TEBChainable);
begin
   inherited Create(value, subscript, next);
   FLookup := name;
end;

function TEBLookupObjExpr.GetLink: string;
begin
   if Values.Count > 0 then
      result := GetLookup(values[0], FLookup)
   else result := inherited GetLink;
end;

{ TEBStringValue }

constructor TEBStringValue.Create(value: string);
begin
   inherited Create(nil);
   self.text := value;
end;

function TEBStringValue.GetNodeText: string;
begin
   result := quotedStr(self.Text);
end;

{ TEBEnumValue }

constructor TEBEnumValue.Create(value: string);
begin
   inherited Create(nil);
   self.text := value;
end;

function TEBEnumValue.GetNodeText: string;
begin
   result := CleanEnum(Text);
end;

function TEBEnumValue.GetScriptText: string;
begin
   result := text;
end;

{ TEBObjArrayValue }

constructor TEBObjArrayValue.Create(value: string; subscript: integer;
  next: TEBChainable);
begin
   inherited Create(value, next);
   Values.Add(subscript);
end;

constructor TEBObjArrayValue.Create(value: string; subscript: TEBExpression;
  next: TEBChainable);
begin
   inherited Create(value, next);
   Add(subscript);
end;

function TEBObjArrayValue.GetChain: TEBChainable;
begin
   if (ComponentCount > 0) and (Components[0] is TEBChainable) then
      result := inherited GetChain
   else result := nil;
end;

function TEBObjArrayValue.GetLink: string;
var
   subscript: TEBExpression;
begin
   if Values.Count > 0 then
      result := format('%s[%d]', [Text, Values[0]])
   else begin
      if Components[0] is TEBChainable then
         subscript := components[1] as TEBExpression
      else subscript := components[0] as TEBExpression;
      result := format('%s[%s]', [Text, subscript.GetNodeText]);
   end;
end;

procedure TEBObjArrayValue.SetChain(const Value: TEBChainable);
var
   subscript: TEBExpression;
begin
   if ComponentCount > 0 then
   begin
      subscript := Components[0] as TEBExpression;
      RemoveComponent(subscript);
   end
   else subscript := nil;
   inherited SetChain(value);
   if assigned(subscript) then
      Add(Subscript);
end;

{ TEBCall }

function TEBCall.GetChain: TEBChainable;
begin
   if (self.ComponentCount > 0) and (Components[0] is TEBChainable) then
      result := TEBChainable(Components[0])
   else result := nil;
end;

function TEBCall.GetNodeText: string;
const
   LINE = '%s(%s)';
var
   list: TStringList;
   child: TEBObject;
   expr: TEBExpression;
begin
   result := self.CleanEnum(self.Text);
   if self.ComponentCount = 0 then
      Exit;

   list := TStringList.Create;
   try
      list.StrictDelimiter := true;
      for child in self do
      begin
         expr := child as TEBExpression;
         if not expr.Silent then
            list.add(' ' + expr.GetNodeText);
      end;
      result := format(LINE, [result, TrimLeft(list.commaList)]);
   finally
      list.free;
   end;
end;

function TEBCall.GetScriptText: string;
const
   LINE = '%s(%s)';
var
   list: TStringList;
   child: TEBObject;
   expr: TEBExpression;
begin
   result := self.Text;
   if self.ComponentCount = 0 then
      Exit;

   list := TStringList.Create;
   try
      list.StrictDelimiter := true;
      for child in self do
      begin
         expr := child as TEBExpression;
         list.add(' ' + expr.GetScriptText);
      end;
      result := format(LINE, [result, TrimLeft(list.commaList)]);
   finally
      list.free;
   end;
end;

{ TEBBinaryOp }

constructor TEBBinaryOp.Create(left, right: TEBExpression; op: TBinaryOp);
begin
   inherited Create(nil);
   Add(left);
   Add(right);
   FOp := op;
end;

function TEBBinaryOp.GetNodeText: string;
const
   OPS: array[TBinaryOp] of string = ('+', '-', '*', 'div', 'mod', '=');
   LINE = '%s %s %s';
begin
   result := format(LINE, [(Components[0] as TEBExpression).GetNodeText, OPS[FOp], (Components[1] as TEBExpression).GetNodeText]);
end;

{ TEBIntArray }

function TEBIntArray.GetNodeText: string;
var
   list: TStringList;
   val: integer;
begin
   list := TStringList.Create;
   list.StrictDelimiter := true;
   try
      for val in values do
         list.add(GetLookup(val, FLookup));
      result := StringReplace(list.CommaList, ',', ', ', [rfReplaceAll]);
      result := format('[%s]', [result]);
   finally
      list.free;
   end;
end;

function TEBIntArray.GetScriptText: string;
var
   list: TStringList;
   val: integer;
begin
   list := TStringList.Create;
   try
      for val in values do
         list.add(intToStr(val));
      result := format('[%s]', [list.CommaList]);
   finally
      list.free;
   end;
end;

{ TEBNilValue }

constructor TEBNilValue.Create(parent: TEBObject);
begin
   inherited Create(parent);
   self.Text := 'nil';
end;

{ TEBMethodCall }

function TEBMethodCall.GetScriptText: string;
const
   LINE = '%s.%s(%s)';
var
   list: TStringList;
   i: integer;
   selfptr: string;
begin
   result := self.Text;
   if self.ComponentCount = 0 then
      Exit;

   list := TStringList.Create;
   try
      list.StrictDelimiter := true;
      selfptr := (Components[0] as TEBExpression).GetScriptText;
      for i := 1 to self.ComponentCount - 1 do
         list.add(' ' + (Components[i] as TEBExpression).GetScriptText);
      result := format(LINE, [result, TrimLeft(list.commaList)]);
      if AnsiSameText(selfptr, 'self') then
         Delete(result, 1, 5);
   finally
      list.free;
   end;
end;

{ TEBParenExpr }

constructor TEBParenExpr.Create(subExpr: TEBExpression);
begin
   inherited Create(nil);
   Add(subExpr);
end;

function TEBParenExpr.GetNodeText: string;
begin
   result := format('(%s)', [ChildNode[0]]);
end;

{ TEBCast }

constructor TEBCast.Create(value: string; base: TEBExpression);
begin
   inherited Create(nil);
   self.Text := value;
   Add(base);
end;

function TEBCast.GetNodeText: string;
const LINE = '%s(%s)';
begin
   result := format(LINE, [self.Text, self.ChildScript[0]]);
end;

initialization
   RegisterClasses([TEBVariableValue, TEBSwitchesValue, TEBIntsValue,
                    TEBBooleanValue, TEBIntegerValue, TEBStringValue, TEBEnumValue,
                    TEBLookupValue, TEBComparison, TEBObjExpr, TEBObjArrayValue,
                    TEBLookupObjExpr, TEBPropExpr, TEBCall, TEBBinaryOp,
                    TEBIntArray, TEBNilValue, TEBParenExpr]);
end.
