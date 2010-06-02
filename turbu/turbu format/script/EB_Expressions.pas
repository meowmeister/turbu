unit EB_Expressions;

interface
uses
   EventBuilder, turbu_defs, turbu_database_interface;

type
   TEBExpression = class(TEBObject)
   protected
      class var FDatastore: IRpgDatastore;
      class function GetLookup(id: integer; const name: string): string;
   public
      function GetScript(indent: integer): string; override;
      function GetNode: TEBNode; override;
      class property Datastore: IRpgDatastore read FDatastore write FDatastore;
   end;

   TEBVariableValue = class(TEBExpression)
   public
      constructor Create(name: string); reintroduce; overload;
      constructor Create(name: string; subscript: integer); reintroduce; overload;
      constructor Create(name: string; subscript: TEBExpression); reintroduce; overload;
      function GetNodeText: string; override;
   end;

   TEBSwitchesValue = class(TEBVariableValue)
   public
      constructor Create(subscript: integer); overload;
      constructor Create(subscript: TEBExpression); overload;
   end;

   TEBIntsValue = class(TEBVariableValue)
   public
      constructor Create(subscript: integer); overload;
      constructor Create(subscript: TEBExpression); overload;
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
   end;

   TEBLookupValue = class(TEBIntegerValue)
   private
      FLookup: string;
   public
      constructor Create(value: integer; name: string);
      function GetNodeText: string; override;
   published
      property lookup: string read FLookup write FLookup;
   end;

   TEBComparison = class(TEBExpression)
   public
      constructor Create(left, right: TEBExpression; comparison: TComparisonOp); reintroduce;
      function GetNodeText: string; override;
   end;

   TEBChainable = class(TEBExpression)
   private
      FHint: integer;
      function GetChain: TEBChainable;
      procedure SetChain(const Value: TEBChainable);
   protected
      function GetLink: string; virtual;
   public
      constructor Create(value: string; next: TEBChainable = nil); reintroduce;
      function GetNodeText: string; override;
      property chain: TEBChainable read GetChain write SetChain;
   published
      property hint: integer read FHint write FHint;
   end;

   TEBObjExpr = class(TEBChainable);

   TEBObjArrayValue = class(TEBObjExpr)
   public
      constructor Create(value: string; subscript: integer; next: TEBChainable = nil);
   end;

   TEBLookupObjExpr = class(TEBObjArrayValue)
   private
      FLookup: string;
   public
      constructor Create(value: string; subscript: integer; name: string; next: TEBChainable = nil);
      function GetNodeText: string; override;
   published
      property lookup: string read FLookup write FLookup;
   end;

   TEBPropExpr = class(TEBChainable)
   end;

   TEBCall = class(TEBChainable)
   public
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

const
   COMPARISONS: array[TComparisonOp] of string = ('=', '>=', '<=', '>', '<', '<>');

{ HINTS:
   1: 's
   2: is
   3: has
}
implementation
uses
   SysUtils, Classes, DB, Variants,
   EB_RPGScript;

{ TEBExpression }

function TEBExpression.GetNode: TEBNode;
begin
   raise ERPGScriptError.Create('Expressions don''t get their own tree nodes!');
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

{ TEBExpression }

function TEBExpression .GetScript(indent: integer): string;
begin
   result := GetNodeText;
end;

class function TEBExpression.GetLookup(id: integer; const name: string): string;
var
   dataset: TDataset;
   lResult: variant;
begin
   if assigned(FDatastore) then
      result := FDatastore.NameLookup(name, id)
   else result := '';
   if result = '' then
      result := format('??%s #%d??', [name, id]);
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

{ TEBIntsValue }

constructor TEBIntsValue.Create(subscript: integer);
begin
   inherited Create('Ints', subscript);
end;

constructor TEBIntsValue.Create(subscript: TEBExpression);
begin
   inherited Create('Ints', subscript);
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
   result := self.Components[0] as TEBChainable;
end;

function TEBChainable.GetNodeText: string;
begin
   result := GetLink;
   if assigned(self.chain) then
      result := result + '.' + chain.GetLink;
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

{ TEBLookupObjExpr }

constructor TEBLookupObjExpr.Create(value: string; subscript: integer;
  name: string; next: TEBChainable);
begin
   inherited Create(value, subscript, next);
   FLookup := name;
end;

function TEBLookupObjExpr.GetNodeText: string;
begin
   result := GetLookup(values[0], FLookup);
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
   result := self.text;
end;

{ TEBObjArrayValue }

constructor TEBObjArrayValue.Create(value: string; subscript: integer;
  next: TEBChainable);
begin
   inherited Create(value, next);
   Values.Add(subscript);
end;

{ TEBCall }

function TEBCall.GetNodeText: string;
const
   LINE = '%s(%s)';
var
   list: TStringList;
   child: TEBObject;
   expr: TEBExpression;
begin
   if self.ComponentCount = 0 then
      Exit(self.Text);

   list := TStringList.Create;
   try
      list.StrictDelimiter := true;
      for child in self do
      begin
         expr := child as TEBExpression;
         list.add(' ' + expr.GetNodeText);
      end;
      result := format(LINE, [self.Text, TrimLeft(list.commaText)]);
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

initialization
   RegisterClasses([TEBVariableValue, TEBSwitchesValue, TEBIntsValue,
                    TEBBooleanValue, TEBIntegerValue, TEBStringValue, TEBEnumValue,
                    TEBLookupValue, TEBComparison, TEBObjExpr, TEBObjArrayValue,
                    TEBLookupObjExpr, TEBPropExpr, TEBCall, TEBBinaryOp]);
end.
