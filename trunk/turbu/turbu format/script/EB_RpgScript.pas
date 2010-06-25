unit EB_RpgScript;

interface
uses
   SysUtils, Classes,
   turbu_defs,
   EventBuilder, EB_Expressions;

type
   TEBUntranslated = class(TEBObject)
   private
      FOpcode: integer;
   public
      function GetScript(indent: integer): string; override;
      function GetScriptText: string; override;
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
      function GetScriptText: string; override;
   end;

   TEBProgram = class(TEBBlock)
   public
      function GetScript(indent: integer): string; override;
      function GetScriptText: string; override;
   end;

   TEBUnit = class(TEBBlock)
   public
      function GetScript(indent: integer): string; override;
      function GetScriptText: string; override;
   end;

   TEBMap = class(TEBProgram);

   TEBExtension = class(TEBObject)
   public
      function GetScriptText: string; override;
      function GetNodeText: string; override;
   end;

   TEBCase = class(TEBBlock)
   protected
      function AlwaysEndBlock: boolean; override;
   public
      constructor Create(parent: TEBObject; expr: TEBExpression); reintroduce;
      function GetScript(indent: integer): string; override;
      function GetScriptText: string; override;
      function GetNodeText: string; override;
   end;

   TEBCodeBlock = class(TEBBlock)
   public
      function GetScriptText: string; override;
   end;

   TEBCaseBlock = class(TEBCodeBlock)
   public
      function GetScriptText: string; override;
      function GetNodeText: string; override;
   end;

   TEBEnumCaseBlock = class(TEBCodeBlock)
   public
      function GetScriptText: string; override;
      function GetNodeText: string; override;
   end;

   TEBElseBlock = class(TEBCodeBlock)
   public
      function GetScriptText: string; override;
      function GetNodeText: string; override;
   end;

   TEBEndCase = class(TEBObject)
   public
      function GetScriptText: string; override;
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
      function GetScriptText: string; override;
      function GetNode: TEBNode; override;
      function GetNodeText: string; override;
   end;

   TEBFunctionCall = class(TEBObject)
   public
      constructor Create(parent: TEBObject; call: TEBCall); reintroduce;
      function GetScriptText: string; override;
      function GetNodeText: string; override;
   end;

   TEBExit = class(TEBObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBLoop = class(TEBBlock)
   protected
      function AlwaysEndBlock: boolean; override;
   public
      function GetScript(indent: integer): string; override;
   end;

   TEBForLoop = class(TEBBlock)
   public
      function GetScriptText: string; override;
      function GetNodeText: string; override;
   end;

   TEBWhileLoop = class(TEBBlock)
   public
      function GetScriptText: string; override;
      function GetNodeText: string; override;
   end;

   TEBLabel = class(TEBObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBBreak = class(TEBObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBGoto = class(TEBObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBComment = class(TEBObject)
   protected
      function MultilineText(indent, overhang: integer): string;
   public
      function GetNodeText: string; override;
      function GetScript(indent: integer): string; override;
   end;

   ERPGScriptError = class(Exception);

   TEBObjectHelper = class helper for TEBObject
   public
      function HeroName(id: integer): string;
      function CleanEnum(const name: string): string;
      function SecondFraction(count: integer): string;
   end;

   TStringsHelper = class helper for TStrings
   public
      function CommaList: string;
   end;

const
   BOOL_STR: array[0..1] of string = ('false', 'true');

implementation

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
   list: TStringList;
   obj: TEBObject;
begin
   list := TStringList.Create;
   try
      list.Add(IndentString(indent) + GetScriptText);
      for obj in self do
         list.add(obj.GetScript(indent + 1));
      result := TrimRight(list.Text);
   finally
      list.Free;
   end;
end;

function TEBUntranslated.GetScriptText: string;
begin
   result := '// ' + GetNodeText;
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
      for element in self do
         list.Add(element.GetScript(indent + 1));
      if MustBlock then
         list.Add(indentString(indent) + 'end;');
      result := TrimRight(list.Text);
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
begin
   assert(indent = 0);
   result := GetScriptText + inherited GetScript(0);
end;

function TEBProcedure.GetScriptText: string;
const HEADER = 'procedure %s%s;' + CRLF;
begin
   result := format(HEADER, [self.name, self.paramList]);
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

function TEBExtension.GetScriptText: string;
begin
   result := self.Text;
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
   result := ChildScript[0];
end;

function TEBCase.GetScript(indent: integer): string;
var
   list: TStringList;
   obj: TEBObject;
begin
   list := TStringList.Create;
   try
      list.Add(IndentString(indent) + GetScriptText);
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

function TEBCase.GetScriptText: string;
const LINE = 'case %s of';
begin
   result := format(LINE, [ChildScript[0]]);
end;

{ TEBElseBlock }

function TEBElseBlock.GetNodeText: string;
begin
   result := 'Cancel Case';
end;

function TEBElseBlock.GetScriptText: string;
begin
   result := 'else ' + inherited GetScriptText;
end;

{ TEBCaseBlock }

function TEBCaseBlock.GetNodeText: string;
begin
   result := '[' + Text + '] Case';
end;

function TEBCaseBlock.GetScriptText: string;
begin
   result := intToStr(values[0]) + ':' + inherited GetScriptText;
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

function TEBEnumCaseBlock.GetScriptText: string;
begin
   result := Text + ':' + inherited GetScriptText;
   if pos(CRLF, result) <> 0 then
      result := CRLF + result;
end;

{ TEBCodeBlock }

function TEBCodeBlock.GetScriptText: string;
begin
   result := inherited GetScriptText;
   if result = '' then
      result := ';';
end;

{ TEBEndCase }

function TEBEndCase.GetNodeText: string;
begin
   result := 'End Case';
end;

function TEBEndCase.GetScriptText: string;
begin
   result := 'end;';
end;

{ TEBIf }

procedure TEBIf.Add(aObject: TEBObject);
var
   block: TEBCodeBlock;
begin
   if (ComponentCount = 0) or (aObject is TEBCodeBlock) then
   begin
      inherited Add(aObject);
      Exit;
   end;
   if FElseSet then
      block := self.Components[2] as TEBCodeBlock
   else
      block := self.Components[1] as TEBCodeBlock;
   block.Add(aObject);
end;

constructor TEBIf.Create(parent: TEBObject; left, right: TEBExpression; op: TComparisonOp);
begin
   inherited Create(parent);
   Add(TEBComparison.Create(left, right, op));
   TEBCodeBlock.Create(self);
end;

function TEBIf.GetNode: TEBNode;
const LINE = 'IF %s ';
var
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
   end
   else result.Add(TEBNode.Create(self, '<>'));
end;

function TEBIf.GetNodeText: string;
begin
   result := 'IF ' + (self.Components[0] as TEBComparison).GetNodeText;
end;

function TEBIf.GetScript(indent: integer): string;
var
   list: TStringList;
   i: integer;
begin
   assert(self.ComponentCount in [1..3]);
   list := TStringList.Create;
   try
      list.add(IndentString(indent) + GetScriptText);
      if ComponentCount = 1 then
         list.add(indentString(indent + 1) + ';')
      else begin
         list.add((Components[1] as TEBCodeBlock).GetScript(indent));
         if ComponentCount = 3 then
         begin
            list.add(IndentString(indent) + 'else');
            list.add((Components[2] as TEBCodeBlock).GetScript(indent));
         end;
      end;
      for I := List.Count - 1 downto 0 do
         if list[i] = '' then
            list.Delete(i);
      result := TrimRight(list.Text);
   finally
      list.free;
   end;

end;

function TEBIf.GetScriptText: string;
begin
  result := format('if %s then ', [ChildScript[0]]);
end;

procedure TEBIf.SetElse;
begin
   assert(not FElseSet);
   FElseSet := true;
   TEBCodeBlock.Create(self);
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

function TEBFunctionCall.GetScriptText: string;
begin
   result := (components[0] as TEBCall).GetScript(0);
end;

{ TEBExit }

function TEBExit.GetNodeText: string;
begin
   result := 'End Script';
end;

function TEBExit.GetScriptText: string;
begin
   result := 'Exit;';
end;

{ TEBLoop }

function TEBLoop.AlwaysEndBlock: boolean;
begin
   result := false;
end;

function TEBLoop.GetScript(indent: integer): string;
begin
   result := indentString(indent) + GetScriptText + CRLF + inherited GetScript(indent);
end;

{ TEBForLoop }

function TEBForLoop.GetNodeText: string;
const LINE = 'For each %s from %d to %d:';
begin
   result := format(LINE, [Text, Values[0], Values[1]]);
end;

function TEBForLoop.GetScriptText: string;
const
   LINE = 'for %s := %d to %d do';
   DOWNLINE = 'for %s := %d downto %d do';
begin
   if values[0] <= values[1] then
      result := format(LINE, [Text, Values[0], Values[1]])
   else format(DOWNLINE, [Text, Values[0], Values[1]]);
end;

{ TEBObjectHelper }

function TEBObjectHelper.CleanEnum(const name: string): string;

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

function TEBObjectHelper.HeroName(id: integer): string;
begin
   result := self.GetLookup(id, 'heroes');
end;

function TEBObjectHelper.SecondFraction(count: integer): string;
begin
   result := formatFloat('###.#', count / 10) + ' sec';
end;

{ TStringsHelper }

function TStringsHelper.CommaList: string;
begin
  Delimiter := ',';
  QuoteChar := #129;
  Result := StringReplace(self.DelimitedText, #129, '', []);
end;

{ TEBLabel }

function TEBLabel.GetNodeText: string;
begin
   result := format('Label #%d', [Values[0]]);
end;

function TEBLabel.GetScriptText: string;
begin
   result := format('L%d:', [Values[0]]);
end;

{ TEBGoto }

function TEBGoto.GetNodeText: string;
begin
   result := format('Goto Label #%d', [Values[0]]);
end;

function TEBGoto.GetScriptText: string;
begin
   result := format('Goto L%d;', [Values[0]]);
end;

{ TEBWhileLoop }

function TEBWhileLoop.GetNodeText: string;
begin
   if (ComponentCount > 0) and (Components[0] is TEBExpression) then
      result := format('Loop While (%s):', [ChildNode[0]])
   else result := 'Loop Indefinitely:';
end;

function TEBWhileLoop.GetScriptText: string;
begin
   if (ComponentCount > 0) and (Components[0] is TEBExpression) then
      result := format('while %s do', [ChildScript[0]])
   else result := 'while true do';
end;

{ TEBBreak }

function TEBBreak.GetNodeText: string;
begin
   result := 'Break Loop';
end;

function TEBBreak.GetScriptText: string;
begin
   result := 'Break;';
end;

{ TEBComment }

function TEBComment.GetNodeText: string;
begin
   result := 'Comment: ' + Text;
end;

function TEBComment.GetScript(indent: integer): string;
begin
   if ComponentCount = 0 then
      result := '// ' + Text
   else result := '{' + MultilineText(indent, 3) + ')';
   result := IndentString(indent) + result;
end;

function TEBComment.MultilineText(indent, overhang: integer): string;
var
   wrap: string;
   child: TEBObject;
begin
   result := self.Text;
   for child in self do
      result := result + #13#10 + child.Text;
   wrap := '+ CRLF +' + #13#10 + IndentString(indent) + StringOfChar(' ', overhang);
   result := StringReplace(self.Text, #13#10, wrap, [rfReplaceAll]);
end;

{ TEBProgram }

function TEBProgram.GetScript(indent: integer): string;
begin
   result := GetScriptText + CRLF + inherited GetScript(0) + CRLF + 'end.'
end;

function TEBProgram.GetScriptText: string;
begin
   result := format('program %s;', [self.name]);
end;

{ TEBUnit }

function TEBUnit.GetScript(indent: integer): string;
var
   list: TStringList;
   child: TEBObject;
begin
   list := TStringList.Create;
   try
      list.add(GetScriptText);
      list.add('interface');
      list.add('');
      for child in self do
         list.add(child.GetScriptText);
      list.add('');
      list.add('implementation');
      for child in self do
         list.add(child.GetScript(0));
      list.add('');
      list.add('end.');
      result := list.Text;
   finally
      list.free;
   end;
end;

function TEBUnit.GetScriptText: string;
begin
   result := format('unit %s;', [self.name]);
end;

initialization
   RegisterClasses([TEBUntranslated, TEBBlock, TEBProcedure, TEBExtension,
                    TEBCase, TEBCaseBlock, TEBElseBlock, TEBEndCase, TEBIf,
                    TEBCodeBlock, TEBEnumCaseBlock, TEBForLoop, TEBLabel, TEBGoto,
                    TEBWhileLoop, TEBBreak, TEBComment, TEBProgram, TEBMap,
                    TEBFunctionCall, TEBUnit, TEBExit]);
end.
