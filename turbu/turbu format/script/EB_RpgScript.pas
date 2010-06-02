unit EB_RpgScript;

interface
uses
   SysUtils,
   turbu_defs,
   EventBuilder, EB_Expressions;

type
   TEBUntranslated = class(TEBObject)
   private
      FOpcode: integer;
   public
      function GetScript(indent: integer): string; override;
      function GetNode: TEBNode; override;
      function GetNodeText: string; override;
   published
      property opcode: integer read FOpcode write FOpcode;
   end;

   TEBBlock = class(TEBObject)
   private
      function MustBlock: boolean;
   protected
      function AlwaysBlock: boolean; virtual;
      function AlwaysEndBlock: boolean; virtual;
   public
      function GetScript(indent: integer): string; override;
      function GetNode: TEBNode; override;
      function GetNodeText: string; override;
   end;

   TEBProcedure = class(TEBBlock)
   protected
      function ParamList: string;
      function varBlock: string;
      function constBlock: string;
      function HasVar: boolean;
      function HasConst: boolean;
      function AlwaysBlock: boolean; override;
   public
      function GetScript(indent: integer): string; override;
   end;

   TEBExtension = class(TEBObject)
   public
      function GetScript(indent: integer): string; override;
      function GetNodeText: string; override;
   end;

   TEBCase = class(TEBBlock)
   protected
      function AlwaysEndBlock: boolean; override;
   public
      constructor Create(parent: TEBObject; expr: TEBExpression); reintroduce;
      function GetScript(indent: integer): string; override;
      function GetNodeText: string; override;
   end;

   TEBCodeBlock = class(TEBBlock)
   public
      function GetScript(indent: integer): string; override;
   end;

   TEBCaseBlock = class(TEBCodeBlock)
   public
      function GetScript(indent: integer): string; override;
      function GetNodeText: string; override;
   end;

   TEBEnumCaseBlock = class(TEBCodeBlock)
   public
      function GetScript(indent: integer): string; override;
      function GetNodeText: string; override;
   end;

   TEBElseBlock = class(TEBCodeBlock)
   public
      function GetScript(indent: integer): string; override;
      function GetNodeText: string; override;
   end;

   TEBEndCase = class(TEBObject)
   public
      function GetScript(indent: integer): string; override;
      function GetNodeText: string; override;
   end;

   TEBIf = class(TEBObject)
   private
      FElseSet: boolean;
   public
      constructor Create(parent: TEBObject; left, right: TEBExpression; op: TComparisonOp); reintroduce;
      procedure Add(aObject: TEBObject); override;
      procedure SetElse;
      function GetScript(indent: integer): string; override;
      function GetNode: TEBNode; override;
      function GetNodeText: string; override;
   end;

   TEBFunctionCall = class(TEBObject)
   public
      constructor Create(parent: TEBObject; call: TEBCall); reintroduce;
      function GetScript(indent: integer): string; override;
      function GetNodeText: string; override;
   end;

   TEBExit = class(TEBObject)
   public
      function GetNodeText: string; override;
      function GetScript(indent: integer): string; override;
   end;

   TEBForLoop = class(TEBBlock)
   public
      function GetScript(indent: integer): string; override;
      function GetNodeText: string; override;
   end;

   ERPGScriptError = class(Exception);

const
   BOOL_STR: array[0..1] of string = ('false', 'true');

implementation
uses
   Classes;

{ TEBUntranslated }

function TEBUntranslated.GetNodeText: string;
const LINE = 'Untranslated: Opcode: %d, Text: %s, Values: %s';
begin
   result := format(LINE, [FOpcode, QuotedStr(self.Text), QuotedStr(self.ArgList)]);
end;

function TEBUntranslated.GetNode: TEBNode;
var
   obj: TEBObject;
begin
   result := inherited GetNode;
   for obj in self do
      result.Add(obj.GetNode);
end;

function TEBUntranslated.GetScript(indent: integer): string;
var
   obj: TEBObject;
begin
   result := indentString(indent) + '// ' + GetNodeText;
   for obj in self do
      result := result + CRLF + obj.GetScript(indent + 1);
end;

{ TEBBlock }

function TEBBlock.AlwaysBlock: boolean;
begin
   result := false;
end;

function TEBBlock.AlwaysEndBlock: boolean;
begin
   result := true;
end;

function TEBBlock.GetNodeText: string;
begin
   result := '';
end;

function TEBBlock.GetNode: TEBNode;
var
  child: TEBObject;
begin
   result := inherited GetNode;
   for child in self do
      if not (child is TEBExpression) then
         result.Add(child.GetNode);
   if AlwaysEndBlock then
      result.Add(TEBNode.Create(self, '<>'))
end;

function TEBBlock.GetScript(indent: integer): string;
var
   list: TStringList;
   element: TEBObject;
begin
   list := TStringList.Create;
   try
      if MustBlock then
         list.add(indentString(indent) + 'begin');
      inc(indent);
      for element in self do
         list.Add(element.GetScript(indent));
      dec(indent);
      if MustBlock then
         list.Add(indentString(indent) + 'end;');
      result := list.Text;
   finally
      list.free;
   end;
end;

function TEBBlock.MustBlock: boolean;
begin
   result := (self.ComponentCount > 1) or AlwaysBlock;
end;

{ TEBProcedure }

function TEBProcedure.GetScript(indent: integer): string;
const HEADER = 'procedure %s%s;' + CRLF;
begin
   assert(indent = 0);
   result := format(HEADER, [self.name, self.paramList]) + inherited GetScript(0);
end;

function TEBProcedure.ParamList: string;
begin
   result := ''; //TODO: implement this
end;

function TEBProcedure.HasVar: boolean;
begin
   result := false;
end;

function TEBProcedure.HasConst: boolean;
begin
   result := false;
end;

function TEBProcedure.varBlock: string;
begin
   result := '';
end;

function TEBProcedure.AlwaysBlock: boolean;
begin
   result := true;
end;

function TEBProcedure.constBlock: string;
begin
   result := '';
end;

{ TEBExtension }

function TEBExtension.GetNodeText: string;
begin
   result := self.Text;
end;

function TEBExtension.GetScript(indent: integer): string;
begin
   result := IndentString(indent) + self.Text;
end;

{ TEBCase }

function TEBCase.AlwaysEndBlock: boolean;
begin
   result := false;
end;

constructor TEBCase.Create(parent: TEBObject; expr: TEBExpression);
begin
   inherited Create(parent);
   self.add(expr);
end;

function TEBCase.GetNodeText: string;
begin
   result := (components[0] as TEBExpression).GetScript(0)
end;

function TEBCase.GetScript(indent: integer): string;
const LINE = 'case %s of';
var
   list: TStringList;
   obj: TEBObject;
begin
   list := TStringList.Create;
   try
      list.Add(IndentString(indent) + format(LINE, [(components[0] as TEBExpression).GetScript(0)]));
      for obj in self do
      begin
         if obj is TEBExpression then
            Continue
         else if obj is TEBCodeBlock then
            list.Add(obj.GetScript(indent))
         else list.Add(obj.GetScript(indent + 1));
      end;
      result := list.Text;
   finally
      list.Free;
   end;
end;

{ TEBElseBlock }

function TEBElseBlock.GetNodeText: string;
begin
   result := 'Cancel Case';
end;

function TEBElseBlock.GetScript(indent: integer): string;
begin
   result := IndentString(indent) + 'else ' + inherited GetScript(indent);
end;

{ TEBCaseBlock }

function TEBCaseBlock.GetNodeText: string;
begin
   result := '[' + Text + '] Case';
end;

function TEBCaseBlock.GetScript(indent: integer): string;
begin
   result := indentString(indent) + intToStr(values[0]) + ':' + inherited GetScript(indent);
   if pos(CRLF, result) <> 0 then
      result := CRLF + result;
end;

{ TEBEnumCaseBlock }

function TEBEnumCaseBlock.GetNodeText: string;
var
   index: integer;
begin
   index := pos('_', text);
   result := Copy(Text, index + 1, MAXINT);
   result[1] := UpCase(result[1]);
   result := result + ' Case:';
end;

function TEBEnumCaseBlock.GetScript(indent: integer): string;
begin
   result := indentString(indent) + Text + ':' + inherited GetScript(indent);
   if pos(CRLF, result) <> 0 then
      result := CRLF + result;
end;

{ TEBCodeBlock }

function TEBCodeBlock.GetScript(indent: integer): string;
begin
   result := inherited GetScript(indent);
   if result = '' then
      result := ';';
end;

{ TEBEndCase }

function TEBEndCase.GetNodeText: string;
begin
   result := 'End Case';
end;

function TEBEndCase.GetScript(indent: integer): string;
begin
   result := IndentString(indent) + 'end;';
end;

{ TEBIf }

procedure TEBIf.Add(aObject: TEBObject);
var
   block: TEBCodeBlock;
begin
   if ComponentCount = 0 then
   begin
      inherited Add(aObject);
      Exit;
   end;
   if FElseSet then
   begin
      if ComponentCount = 2 then
         block := TEBCodeBlock.Create(self)
      else block := self.Components[2] as TEBCodeBlock;
   end else
      if ComponentCount = 1 then
         block := TEBCodeBlock.Create(self)
      else block := self.Components[1] as TEBCodeBlock;
   block.Add(aObject);
end;

constructor TEBIf.Create(parent: TEBObject; left, right: TEBExpression; op: TComparisonOp);
begin
   inherited Create(parent);
   Add(TEBComparison.Create(left, right, op));
end;

function TEBIf.GetNode: TEBNode;
const LINE = 'IF %s ';
var
   I: Integer;
   node: TEBNode;
begin
   assert(self.ComponentCount in [1..3]);
   result := inherited GetNode;
   if ComponentCount > 1 then
   begin
      result.add((Components[1] as TEBCodeBlock).GetNode);
      if ComponentCount = 3 then
      begin
         node := TEBNode.Create(self, 'else');
         node.add((Components[2] as TEBCodeBlock).GetNode);
         result.add(node);
      end;
   end;
   for I := 1 to self.ComponentCount - 1 do
      result.Add((Components[i] as TEBCodeBlock).GetNode);
end;

function TEBIf.GetNodeText: string;
begin
   result := 'IF ' + (self.Components[0] as TEBComparison).GetNodeText;
end;

function TEBIf.GetScript(indent: integer): string;
const LINE = 'if %s then ';
var
   list: TStringList;
begin
   assert(self.ComponentCount in [1..3]);
   list := TStringList.Create;
   try
      list.add(indentString(indent) + format(LINE, [(self.Components[0] as TEBExpression).GetScript(indent)]));
      if ComponentCount = 1 then
         list.add(indentString(indent + 1) + ';')
      else begin
         list.add((Components[1] as TEBCodeBlock).GetScript(indent + 1));
         if ComponentCount = 3 then
         begin
            list.add(IndentString(indent) + 'else');
            list.add((Components[2] as TEBCodeBlock).GetScript(indent + 1));
         end;
      end;
      result := list.Text;
   finally
      list.free;
   end;
end;

procedure TEBIf.SetElse;
begin
   assert(not FElseSet);
   FElseSet := true;
end;

{ TEBFunctionCall }

constructor TEBFunctionCall.Create(parent: TEBObject; call: TEBCall);
begin
   inherited Create(parent);
   self.Add(call);
end;

function TEBFunctionCall.GetNodeText: string;
begin
   result := self.GetScript(0);
end;

function TEBFunctionCall.GetScript(indent: integer): string;
begin
   result := IndentString(indent) + (components[0] as TEBCall).GetScript(0);
end;

{ TEBExit }

function TEBExit.GetNodeText: string;
begin
   result := 'End Script';
end;

function TEBExit.GetScript(indent: integer): string;
begin
   result := IndentString(indent) + 'Exit;';
end;

{ TEBForLoop }

function TEBForLoop.GetNodeText: string;
const LINE = 'For each %s from %d to %d:';
begin
   result := format(LINE, [Text, Values[0], Values[1]]);
end;

function TEBForLoop.GetScript(indent: integer): string;
const
   LINE = 'for %s := %d to %d do';
   DOWNLINE = 'for %s := %d downto %d do';
begin
   if values[0] <= values[1] then
      result := format(LINE, [Text, Values[0], Values[1]])
   else format(DOWNLINE, [Text, Values[0], Values[1]]);
   result := IndentString(indent) + result + inherited GetScript(indent);
end;

initialization
   RegisterClasses([TEBUntranslated, TEBBlock, TEBProcedure, TEBExtension,
                    TEBCase, TEBCaseBlock, TEBElseBlock, TEBEndCase, TEBIf,
                    TEBCodeBlock, TEBEnumCaseBlock, TEBForLoop]);
end.
