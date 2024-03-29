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
   Classes,
   EventBuilder, turbu_operators;

type
   TEBVariableValue = class(TEBExpression)
   protected
      FIsGlobal: boolean;
      procedure SerializeProps(list: TStringList; depth: integer); override;
      procedure AssignProperty(const key, value: string); override;
   public
      constructor Create(name: string); reintroduce; overload;
      constructor Create(name: string; subscript: integer); reintroduce; overload;
      constructor Create(name: string; subscript: TEBExpression); reintroduce; overload;
      function GetNodeText: string; override;
      function GetScriptText: string; override;
      property Global: boolean read FIsGlobal write FIsGlobal stored FIsGlobal;
   end;

   TEBNilValue = class(TEBVariableValue)
   public
      constructor Create(parent: TEBObject); override;
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
   protected
      procedure SerializeProps(list: TStringList; depth: integer); override;
      procedure AssignProperty(const key, value: string); override;
   public
      constructor Create(value: integer; name: string);
      function GetNodeText: string; override;
      function GetScriptText: string; override;
      property lookup: string read FLookup write FLookup;
   end;

   TEBComparison = class(TEBExpression)
   public
      constructor Create(left, right: TEBExpression; comparison: TComparisonOp); reintroduce;
      function GetNodeText: string; override;
      function GetScriptText: string; override;
      procedure negate;
   end;

   TEBComparisonBool = class(TEBExpression)
   public
      constructor Create(expr: TEBExpression; value: boolean); reintroduce;
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
      function GetScriptLink: string; virtual;
      procedure SerializeProps(list: TStringList; depth: integer); override;
      procedure AssignProperty(const key, value: string); override;
   public
      constructor Create(value: string; next: TEBChainable = nil); reintroduce;
      function GetNodeText: string; override;
      property chain: TEBChainable read GetChain write SetChain;
      function GetScriptText: string; override;
      property hint: integer read FHint write FHint;
   end;

   TEBObjExpr = class(TEBChainable);

   TEBObjArrayValue = class(TEBObjExpr)
   protected
      function GetChain: TEBChainable; override;
      procedure SetChain(const Value: TEBChainable); override;
      function GetLink: string; override;
      function GetScriptLink: string; override;
   public
      constructor Create(value: string; subscript: integer; next: TEBChainable = nil); overload;
      constructor Create(value: string; subscript: TEBExpression; next: TEBChainable = nil); overload;
   end;

   TEBLookupObjExpr = class(TEBObjArrayValue)
   private
      FLookup: string;
   protected
      procedure SerializeProps(list: TStringList; depth: integer); override;
      procedure AssignProperty(const key, value: string); override;
      function GetLink: string; override;
      function GetScriptLink: string; override;
   public
      constructor Create(value: string; subscript: integer; name: string; next: TEBChainable = nil); overload;
      constructor Create(value: string; subscript: TEBExpression; name: string; next: TEBChainable = nil); overload;
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
      const
         OPS: array[TBinaryOp] of string = ('+', '-', '*', 'div', 'mod', '=');
         LINE = '%s %s %s';
   protected
      procedure SerializeProps(list: TStringList; depth: integer); override;
      procedure AssignProperty(const key, value: string); override;
   public
      constructor Create(left, right: TEBExpression; op: TBinaryOp); reintroduce;
      function GetScriptText: string; override;
      function GetNodeText: string; override;
      property op: TBinaryOp read FOp write FOp;
   end;

   TEBIntArray = class(TEBExpression)
   private
      FLookup: string;
   protected
      procedure SerializeProps(list: TStringList; depth: integer); override;
      procedure AssignProperty(const key, value: string); override;
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
      property lookup: string read FLookup write FLookup;
   end;

   TEBParenExpr = class(TEBExpression)
   public
      constructor Create(subExpr: TEBExpression); reintroduce;
      function GetNodeText: string; override;
   end;

   TEBBooleanBinList = class(TEBExpression)
   protected
      function opName: string; virtual; abstract;
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBAndList = class(TEBBooleanBinList)
   protected
      function opName: string; override;
   end;

   TEBOrList = class(TEBBooleanBinList)
   protected
      function opName: string; override;
   end;

   TEBNotExpr = class(TEBExpression)
   public
      constructor Create(AParent: TEBObject; base: TEBExpression); reintroduce;
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   function CleanEnum(const name: string): string;

const
   COMPARISONS: array[TComparisonOp] of string = ('=', '>=', '<=', '>', '<', '<>');
   NEG_COMPARISONS: array[TComparisonOp] of TComparisonOp = (co_notEquals, co_lt, co_gt, co_ltE, co_gtE, co_equals);
   HINTS: array[0..3] of string = ('', '''s', ' is', ' has');

implementation
uses
   SysUtils, Variants, Math, TypInfo,
   EB_RPGScript;

function CleanEnum(const name: string): string;

   function Joseph_Styons_UnCamelCase(const camel: string) : string;
     function IsUppercase(c: char): boolean; inline;
     begin
       Result := (c >= 'A') and (c <= 'Z');
     end;
   const
     c_Delim = #32;
   var
     i,offset: integer;
   begin
     if Length(camel) > 2 then
     begin
       //initialize with a big empty string
       result := StringOfChar(' ', length(Camel) * 2);

       //offset will contain the # of spaces we've added
       offset := 0;

       //first char never changes, just copy it over
       Result[1] := camel[1];

       for i := 2 to Length(camel) do
       begin
         //go ahead and copy the current char
         Result[i+offset] := camel[i];

         //we only do anything interesting when the *next* char is uppercase
         if (i < length(camel)) and IsUppercase(camel[i+1]) then
         begin
           //special case: XXx should become X-Xx, so look two ahead
           if IsUppercase(camel[i]) then
           begin
             if (i < Length(camel)-1) and not(IsUppercase(camel[i+2])) then
             begin
               //if we match the special case, then add a space
               Inc(offset);
               Result[i+offset] := c_Delim;
             end;
           end
           else begin
             //if we are lowercase, followed by uppercase, add a space
             Inc(offset);
             Result[i+offset] := c_Delim;
           end;
         end;
       end;
       //cut out extra spaces
       SetLength(Result,Length(camel)+offset);
     end
     else Result := camel;  //no change if < 2 chars
   end;

var
   index: integer;
begin
   index := pos('_', name);
   result := copy(name, index + 1, MAXINT);
   result[1] := UpCase(result[1]);
   result := Joseph_Styons_UnCamelCase(result);
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

procedure TEBVariableValue.AssignProperty(const key, value: string);
begin
   if (key = 'Global') and (value = 'True') then
      Global := true
   else inherited;
end;

constructor TEBVariableValue.Create(name: string; subscript: TEBExpression);
begin
   self.Create(name + '[]');
   self.Add(subscript);
end;

function TEBVariableValue.GetNodeText: string;
begin
   if ChildCount > 0 then
      result := StringReplace(self.Text, '[]', format('[%s]', [ChildNode[0]]), [])
   else if values.Count > 0 then
      result := StringReplace(self.Text, '[]', format('[%d]', [Values[0]]), [])
   else result := self.Text;
end;

function TEBVariableValue.GetScriptText: string;
begin
   if ChildCount > 0 then
      result := StringReplace(self.Text, '[]', format('[%s]', [ChildScript[0]]), [])
   else if values.Count > 0 then
      result := StringReplace(self.Text, '[]', format('[%d]', [Values[0]]), [])
   else result := self.Text;
end;

procedure TEBVariableValue.SerializeProps(list: TStringList; depth: integer);
begin
   inherited;
   if Global then
      List.Add(IndentString(depth) + 'Global = True');
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
   assert(self.ChildCount = 2);
   assert(self.Values.Count = 1);
   result := format(LINE, [ChildNode[0], COMPARISONS[TComparisonOp(Values[0])],
                           ChildNode[1]]);
end;

function TEBComparison.GetScriptText: string;
const
   LINE = '%s %s %s';
begin
   assert(self.ChildCount = 2);
   assert(self.Values.Count = 1);
   result := format(LINE, [ChildScript[0], COMPARISONS[TComparisonOp(Values[0])],
                           ChildScript[1]]);
end;

procedure TEBComparison.negate;
begin
   if self.children[1].classtype = TEBBooleanValue then //switch true and false
      self.children[1].Values[0] := self.children[1].Values[0] xor 1
   else Values[0] := ord(NEG_COMPARISONS[TComparisonOp(Values[0])]);
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
   if ChildCount > 0 then
      result := self.Children[0] as TEBChainable
   else result := nil;
end;

function TEBChainable.GetNodeText: string;
const LINE = '%s%s %s';
begin
   result := GetLink;
   if assigned(self.chain) then
      result := format(LINE, [result, HINTS[chain.hint], chain.GetNodeText]);
end;

function TEBChainable.GetScriptLink: string;
begin
   result := StringReplace(GetLink, ' ', '', [rfReplaceAll]);
end;

function TEBChainable.GetScriptText: string;
const LINE = '%s.%s';
begin
   result := GetScriptLink;
   if assigned(self.chain) then
      result := format(LINE, [result, chain.GetScript(0)]);
end;

function TEBChainable.GetLink: string;
begin
   result := self.Text;
end;

procedure TEBChainable.SerializeProps(list: TStringList; depth: integer);
begin
   inherited;
   list.Add(IndentString(depth) + 'Hint = ' + IntToStr(hint));
end;

procedure TEBChainable.AssignProperty(const key, value: string);
begin
   if key = 'Hint' then
      self.hint := StrToInt(value)
   else inherited;
end;

procedure TEBChainable.SetChain(const Value: TEBChainable);
begin
   assert(self.ChildCount = 0);
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

procedure TEBLookupValue.AssignProperty(const key, value: string);
begin
   if key = 'Lookup' then
      Lookup := value
   else inherited;
end;

procedure TEBLookupValue.SerializeProps(list: TStringList; depth: integer);
begin
   inherited;
   list.Add(IndentString(depth) + 'Lookup = ' + FLookup);
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

function TEBLookupObjExpr.GetScriptLink: string;
const
   BASE = '%s[%d]';
   LINE = '%s.%s';
begin
   if Values.Count > 0 then
      result := format(BASE, [self.Text, values[0]])
   else result := inherited GetScriptLink;
end;

procedure TEBLookupObjExpr.SerializeProps(list: TStringList; depth: integer);
begin
   inherited;
   List.Add(IndentString(depth) + 'Lookup = ' + lookup);
end;

procedure TEBLookupObjExpr.AssignProperty(const key, value: string);
begin
   if key = 'Lookup' then
      Lookup := value
   else inherited;
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
   if (ChildCount > 0) and (Children[0] is TEBChainable) then
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
      if Children[0] is TEBChainable then
         subscript := Children[1] as TEBExpression
      else subscript := Children[0] as TEBExpression;
      result := format('%s[%s]', [Text, subscript.GetNodeText]);
   end;
end;

function TEBObjArrayValue.GetScriptLink: string;
var
   subscript: TEBExpression;
begin
   if Values.Count > 0 then
      result := format('%s[%d]', [Text, Values[0]])
   else begin
      if Children[0] is TEBChainable then
         subscript := Children[1] as TEBExpression
      else subscript := Children[0] as TEBExpression;
      result := format('%s[%s]', [Text, subscript.GetScriptText]);
   end;
end;

procedure TEBObjArrayValue.SetChain(const Value: TEBChainable);
var
   subscript: TEBExpression;
begin
   if ChildCount > 0 then
   begin
      subscript := Children[0] as TEBExpression;
      Children.Extract(subscript);
   end
   else subscript := nil;
   inherited SetChain(value);
   if assigned(subscript) then
      Add(Subscript);
end;

{ TEBCall }

function TEBCall.GetChain: TEBChainable;
begin
   if (self.ChildCount > 0) and (Children[0] is TEBChainable) then
      result := TEBChainable(Children[0])
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
   result := CleanEnum(self.Text);
   if self.ChildCount = 0 then
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
   if self.ChildCount = 0 then
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
begin
   result := format(LINE, [ChildNode[0], OPS[FOp], ChildNode[1]]);
end;

function TEBBinaryOp.GetScriptText: string;
begin
   result := format(LINE, [ChildScript[0], OPS[FOp], ChildScript[1]]);
end;

procedure TEBBinaryOp.SerializeProps(list: TStringList; depth: integer);
begin
   inherited;
   list.Add(IndentString(depth) + 'Op = ' + GetEnumName(TypeInfo(TBinaryOp), ord(op)));
end;

procedure TEBBinaryOp.AssignProperty(const key, value: string);
begin
   if key = 'Op' then
      Op := TBinaryOp(GetEnumValue(TypeInfo(TBinaryOp), value))
   else inherited;
end;

{ TEBIntArray }

procedure TEBIntArray.AssignProperty(const key, value: string);
begin
   if key = 'Lookup' then
      Lookup := value
   else inherited;
end;

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

procedure TEBIntArray.SerializeProps(list: TStringList; depth: integer);
begin
   inherited;
   list.Add(IndentString(depth) + 'Lookup = ' + lookup);
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
   if self.ChildCount = 0 then
      Exit;

   list := TStringList.Create;
   try
      list.StrictDelimiter := true;
      selfptr := (Children[0] as TEBExpression).GetScriptText;
      for i := 1 to self.ChildCount - 1 do
         list.add(' ' + (Children[i] as TEBExpression).GetScriptText);
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

{ TEBComparisonBool }

constructor TEBComparisonBool.Create(expr: TEBExpression; value: boolean);
begin
   inherited Create(nil);
   self.Add(expr);
   Values.Add(ord(value));
end;

function TEBComparisonBool.GetNodeText: string;
const
   LINE = '%s = %s';
begin
   assert(self.ChildCount = 1);
   assert(self.Values.Count = 1);
   result := format(LINE, [ChildNode[0], BOOL_STR[Values[0]]]);
end;

function TEBComparisonBool.GetScriptText: string;
begin
   assert(self.ChildCount = 1);
   assert(self.Values.Count = 1);
   result := ChildScript[0];
   if Values[0] = 0 then
      result := 'not ' + result;
end;

{ TEBBooleanBinList }

function TEBBooleanBinList.GetNodeText: string;
var
   i, count: integer;
begin
   count := self.children.Count;
   assert(count > 0);
   if count = 1 then
      result := self.ChildNode[0]
   else begin
      result := '';
      for i := 0 to count - 1 do
      begin
         result := result + format('(%s)', [self.ChildNode[i]]);
         if i < count - 1 then
            result := result + ' ' + opName + ' ';
      end;
   end;
end;

function TEBBooleanBinList.GetScriptText: string;
var
   i, count: integer;
begin
   count := self.children.Count;
   assert(count > 0);
   if count = 1 then
      result := self.ChildScript[0]
   else begin
      result := '';
      for i := 0 to count - 1 do
      begin
         result := result + format('(%s)', [self.ChildScript[i]]);
         if i < count - 1 then
            result := result + ' ' + opName + ' ';
      end;
   end;
end;

{ TEBNotExpr }

constructor TEBNotExpr.Create(AParent: TEBObject; base: TEBExpression);
begin
   inherited Create(AParent);
   Add(base);
end;

function TEBNotExpr.GetNodeText: string;
begin
   result := 'not [' + ChildNode[0] + ']';
end;

function TEBNotExpr.GetScriptText: string;
begin
   result := 'not (' + ChildScript[0] + ')';
end;

{ TEBOrList }

function TEBOrList.opName: string;
begin
   result := 'or';
end;

{ TEBAndList }

function TEBAndList.opName: string;
begin
   result := 'and';
end;

initialization
   TEBObject.RegisterClasses([TEBVariableValue,
                    TEBBooleanValue, TEBIntegerValue, TEBStringValue, TEBEnumValue,
                    TEBLookupValue, TEBComparison, TEBObjExpr, TEBObjArrayValue,
                    TEBLookupObjExpr, TEBPropExpr, TEBCall, TEBBinaryOp,
                    TEBIntArray, TEBNilValue, TEBParenExpr, TEBComparisonBool,
                    TEBAndList, TEBOrList, TEBNotExpr]);
end.
