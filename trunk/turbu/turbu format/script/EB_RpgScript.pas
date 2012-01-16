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
   protected
      procedure SerializeProps(list: TStringList; depth: integer); override;
   public
      function GetScript(indent: integer): string; override;
      function GetScriptText: string; override;
      function GetNode: TEBNode; override;
      function GetNodeText: string; override;
      property opcode: integer read FOpcode write FOpcode;
   end;

   TEBProcedure = class(TEBRoutine)
   private
      FVarBlock: TStringList;
      FLabelBlock: TStringList;
      procedure ScanHeader;
   protected
      function ParamList: string;
      function varBlock: string;
      function constBlock: string;
      function labelBlock: string;
      function HasVar: boolean;
      function HasConst: boolean;
      function HasLabels: boolean;
      function AlwaysBlock: boolean; override;
   public
      constructor Create(AOwner: TEBObject); override;
      destructor Destroy; override;
      function AddParam(const name, &type: string): TEBParam;
      function GetScript(indent: integer): string; override;
      function GetScriptText: string; override;
      function GetVarBlock: TStringList; override;
   end;

   TEBProgram = class(TEBBlock)
   public
      function GetScript(indent: integer): string; override;
      function GetScriptText: string; override;
      function MustBlock: boolean; override;
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

   TEBElseBlock = class;

   TEBCase = class(TEBBlock)
   protected
      function AlwaysEndBlock: boolean; override;
   public
      constructor Create(parent: TEBObject; expr: TEBExpression); reintroduce; overload;
      function GetScript(indent: integer): string; override;
      function GetScriptText: string; override;
      function GetNodeText: string; override;
      function GetElseBlock: TEbElseBlock;
   end;

   TEBMaybeCase = class(TEBCase)
   private
      FCaseBlock: boolean;
   protected
      procedure SerializeProps(list: TStringList; depth: integer); override;
      procedure AssignProperty(const key, value: string); override;
   public
      function GetScript(indent: integer): string; override;
      function GetScriptText: string; override; final;
      function GetScriptBase: string; virtual; abstract;

      property CaseBlock: boolean read FCaseBlock write FCaseBlock;
   end;

   TEBCodeBlock = class(TEBBlock)
   public
      function GetScriptText: string; override;
   end;

   TEBCaseBlock = class(TEBCodeBlock)
   public
      function GetScript(indent: integer): string; override;
      function GetScriptText: string; override;
      function GetNodeText: string; override;
   end;

   TEBEnumCaseBlock = class(TEBCaseBlock)
   public
      function GetScriptText: string; override;
      function GetNodeText: string; override;
   end;

   TEBElseBlock = class(TEBCodeBlock)
   public
      function GetScript(indent: integer): string; override;
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
      function StripTrailingSem(const value: string): string;
      function StripTrailingSemCommented(const value: string): string;
   protected
      procedure Loaded; override;
   public
      constructor Create(parent: TEBObject; left, right: TEBExpression; op: TComparisonOp); reintroduce; overload;
      procedure Add(aObject: TEBObject); override;
      procedure SetElse;
      function ClearElse: TEBCodeBlock;
      function GetScript(indent: integer): string; override;
      function GetScriptText: string; override;
      function GetNode: TEBNode; override;
      function GetNodeText: string; override;

      property ElseSet: boolean read FElseSet;
   end;

   TEBMaybeIf = class(TEBIf)
   private
      FIfBlock: boolean;
   protected
      procedure SerializeProps(list: TStringList; depth: integer); override;
      procedure AssignProperty(const key, value: string); override;
   public
      procedure Setup(const blockName: string = '');
      function GetScript(indent: integer): string; override;
      function GetScriptText: string; override; final;
      function GetScriptBase: string; virtual; abstract;
      function GetNode: TEBNode; override;

      property IfBlock: boolean read FIfBlock write FIfBlock;
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
   private
      FTempList: TStringList;
      procedure MergeEqual(const value: string; const data1, data2: TObject);
      procedure AddSingle(const value: string; const data: TObject);
   protected
      procedure NeededVariables(list: TStringList); override;
   public
      function GetScript(indent: integer): string; override;
      function GetScriptText: string; override;
      function GetNodeText: string; override;
      function NeededVariableType: THeaderItems; override;
   end;

   TEBWhileLoop = class(TEBLoop)
   public
      function GetScriptText: string; override;
      function GetNodeText: string; override;
   end;

   TEBLabel = class(TEBObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
      procedure NeededVariables(list: TStringList); override;
      function NeededVariableType: THeaderItems; override;
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
      procedure NeededVariables(list: TStringList); override;
      function NeededVariableType: THeaderItems; override;
   end;

   TEBComment = class(TEBObject)
   protected
      function MultilineText(indent, overhang: integer): string;
   public
      function GetNodeText: string; override;
      function GetScript(indent: integer): string; override;
   end;

   TEbAssignment = class(TEBObject)
   public
      function GetNodeText: string; override;
      function GetScriptText: string; override;
   end;

   TEBObjectHelper = class helper for TEBObject
   private
      class function VarName(id: integer; const group: string): string; static;
   public
      function HeroName(id: integer): string;
      function VehicleName(id: integer): string;
      function CleanEnum(const name: string): string;
      function SecondFraction(count: integer): string;
      class function IntName(id: integer): string;
      function SwitchName(id: integer): string;
   end;

   TStringsHelper = class helper for TStrings
   public
      function CommaList: string;
   end;

const
   BOOL_STR: array[0..1] of string = ('false', 'true');
   ADDREM: array[0..1] of string = ('Add', 'Remove');

implementation
uses
   StrUtils,
   StringListComp;

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

procedure TEBUntranslated.SerializeProps(list: TStringList; depth: integer);
begin
   assert(false);
end;

{ TEBProcedure }

constructor TEBProcedure.Create(AOwner: TEBObject);
begin
   inherited Create(AOwner);
   FVarBlock := TStringList.Create;
   FLabelBlock := TStringList.Create;
end;

destructor TEBProcedure.Destroy;
begin
   FVarBlock.Free;
   FLabelBlock.Free;
   inherited Destroy;
end;

function TEBProcedure.GetScript(indent: integer): string;
var
   list: TStringList;
begin
   assert(indent = 0);
   list := TStringList.Create;
   try
      try
         list.add(GetScriptText);
         ScanHeader;
         if HasLabels then
         begin
            list.Add('label');
            list.Add(Self.labelBlock);
         end;
         if HasVar then
         begin
            list.Add('var');
            list.Add(Self.varBlock);
         end;
         list.Add(inherited GetScript(0));
         result := list.Text;
      except
         on E: ERpgScriptError do
         begin
            E.Message := format('Error building script %s: %s', [self.Name, E.Message]);
            raise;
         end;
      end;
   finally
      list.Free;
   end;
end;

function TEBProcedure.GetScriptText: string;
const HEADER = 'procedure %s%s;';
begin
   result := format(HEADER, [self.name, self.paramList]);
end;

function TEBProcedure.GetVarBlock: TStringList;
begin
   result := FVarBlock;
end;

procedure TEBProcedure.ScanHeader;
var
   header: TEBHeader;
   child: TEBObject;
   counter: integer;
   inbuilt: TArray<TEBVariable>;
   variable: TEBVariable;
   reqs: TStringList;
   i: integer;
begin
   header := EnsureHeader;
   SetLength(inbuilt, header.ChildCount);
   counter := 0;
   for child in header do
      if (child is TEBVariable) and not (child is TEBParam) then
      begin
         inbuilt[counter] := TEBVariable(child);
         inc(counter);
      end;
   FVarBlock.Clear;
   FLabelBlock.Clear;
   reqs := RequiredVariables;
   try
      for variable in inbuilt do
         if reqs.IndexOfName(variable.Text) <> -1 then
            reqs.Delete(reqs.IndexOfName(variable.Text))
         else reqs.Values[variable.text] := variable.VarType;
      for i := 0 to reqs.Count - 1 do
      begin
         case (reqs.Objects[i] as TEBObject).NeededVariableType of
            hi_none, hi_const: assert(false);
            hi_var: FVarBlock.Add('  ' + stringReplace(reqs[i], '=', ': ', []) + ';');
            hi_label: FLabelBlock.Add(reqs[i]);
         end;
      end;
   finally
      reqs.Free;
   end;
end;

function TEBProcedure.ParamList: string;
var
   header: TEBHeader;
begin
   header := self.EnsureHeader;
   if header.ChildCount = 0 then
      Exit('');
   result := header.GetScriptText;
end;

function TEBProcedure.HasVar: boolean;
begin
   result := FVarBlock.Count > 0;
end;

function TEBProcedure.labelBlock: string;
begin
   result := '  ' + FLabelBlock.CommaText + ';';
end;

function TEBProcedure.HasConst: boolean;
begin
   result := false;
end;

function TEBProcedure.HasLabels: boolean;
begin
   result := FLabelBlock.Count > 0;
end;

function TEBProcedure.varBlock: string;
begin
   result := FVarBlock.Text;
   if AnsiEndsText(#13#10, result) then
      result := copy(result, 1, length(result) - 2);
end;

function TEBProcedure.AddParam(const name, &type: string): TEBParam;
begin
   result := TEBParam.Create(Self.EnsureHeader);
   result.Text := name;
   result.VarType := &type;
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
         if (obj is TEBExpression) then
            Continue
         else if obj is TEBBlock then
            list.Add(obj.GetScript(indent + 1))
         else list.Add(obj.GetScript(indent));
      result := TrimRight(list.Text);
   finally
      list.Free;
   end;
end;

function TEBCase.GetScriptText: string;
const LINE = 'case %s of';
begin
   result := format(LINE, [ChildScript[0]]);
end;

function TEBCase.GetElseBlock: TEbElseBlock;
begin
   if (self.ChildCount > 0) and  (self.Children[self.ChildCount - 1] is TEbElseBlock) then
      result := TEbElseBlock(self.Children[self.ChildCount - 1])
   else result := nil;
end;

{ TEBElseBlock }

function TEBElseBlock.GetNodeText: string;
begin
   result := 'Cancel Case';
end;

function TEBElseBlock.GetScript(indent: integer): string;
begin
   result := IndentString(indent) + 'else ' + CRLF + inherited GetScript(indent);
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

function TEBCaseBlock.GetScript(indent: integer): string;
var
   subscript: string;
begin
   subscript := inherited GetScript(indent);
   result := IndentString(indent) + GetScriptText;
   if subscript <> '' then
   begin
      if pos(CRLF, subscript) <> 0 then
         subscript := CRLF + subscript
      else subscript := ' ' + TrimLeft(subscript);
      result := result + subscript;
   end
   else result := result + ';';
end;

function TEBCaseBlock.GetScriptText: string;
begin
   result := intToStr(values[0]) + ':';
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
   result := Text + ':';
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
constructor TEBIf.Create(parent: TEBObject; left, right: TEBExpression; op: TComparisonOp);
begin
   inherited Create(parent);
   Add(TEBComparison.Create(left, right, op));
   TEBCodeBlock.Create(self);
end;

procedure TEBIf.Add(aObject: TEBObject);
var
   block: TEBCodeBlock;
begin
   if (ChildCount = 0) or (aObject is TEBCodeBlock) then
   begin
      inherited Add(aObject);
      Exit;
   end;
   if FElseSet then
      block := self.Children[2] as TEBCodeBlock
   else block := self.Children[1] as TEBCodeBlock;
   block.Add(aObject);
end;

function TEBIf.ClearElse: TEBCodeBlock;
begin
   assert(FElseSet);
   FElseSet := false;
   result := self.children.Extract(self.children[2]) as TEBCodeBlock;
end;

function TEBIf.GetNode: TEBNode;
const LINE = 'IF %s ';
var
   node: TEBNode;
begin
   assert(self.ChildCount in [1..3]);
   result := inherited GetNode;
   if ChildCount > 1 then
   begin
      result.add((Children[1] as TEBCodeBlock).GetNode);
      if ChildCount = 3 then
      begin
         node := TEBNode.Create(self, 'else');
         node.add((Children[2] as TEBCodeBlock).GetNode);
         result.add(node);
      end;
   end
   else result.Add(TEBNode.Create(self, '<>'));
end;

function TEBIf.GetNodeText: string;
begin
   result := 'IF ' + ChildNode[0];
end;

function TEBIf.GetScript(indent: integer): string;
var
   list: TStringList;
   i: integer;
   ifScript, elseScript: string;
begin
   assert(self.ChildCount in [1..3]);
   list := TStringList.Create;
   try
      list.add(IndentString(indent) + GetScriptText);
      if ChildCount = 1 then
         list.add(indentString(indent + 1) + ';')
      else begin
         ifScript := (Children[1] as TEBCodeBlock).GetScript(indent);
         if (children[1].ChildCount = 1) and (children[1].children[0] is TEBIf) then
            ifScript := IndentString(indent) + 'begin' + CRLF + ifScript + CRLF + indentString(indent) + 'end;';
         list.add(ifScript);
         if ChildCount = 3 then
         begin
            list[list.Count - 1] := StripTrailingSem(list[list.Count - 1]);
            list.add(IndentString(indent) + 'else');
            elseScript := (Children[2] as TEBCodeBlock).GetScript(indent);
            if elseScript = '' then
               elseScript := indentString(indent + 1) + ';';
            list.add(elseScript);
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

procedure TEBIf.Loaded;
begin
   FElseSet := ChildCount > 2;
end;

procedure TEBIf.SetElse;
begin
   assert(not FElseSet);
   FElseSet := true;
   TEBCodeBlock.Create(self);
end;

function TEBIf.StripTrailingSemCommented(const value: string): string;
var
   list: TStringList;
   line: string;
   i: Integer;
   inComment: boolean;
begin
   //find the last line that's not a comment and strip a sem from it
   //this relies on EventBuilder not mixing code and comments in the same line
   list := TStringList.Create;
   try
      list.text := value;
      inComment := false;
      for i := list.count - 1 downto 0 do
      begin
         line := list[i];
         if InComment then
         begin
            if AnsiStartsStr('{', TrimLeft(line)) then
               InComment := false;
            Continue;
         end;
         if line[length(line)] = '}' then
         begin
            InComment := true;
            Continue;
         end;
         if AnsiStartsStr('//', TrimLeft(line)) then
            Continue;
         assert(line[length(line)] = ';');
         list[i] := Copy(line, 1, length(line) - 1);
         break;
      end;
      result := list.Text;
   finally
      list.free;
   end;
end;

function TEBIf.StripTrailingSem(const value: string): string;
begin
   if value = '' then
      exit('');
   if value[length(value)] <> ';' then
      result := stripTrailingSemCommented(value)
   else result := Copy(value, 1, length(value) - 1);
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
   result := (Children[0] as TEBCall).GetScript(0);
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

function TEBForLoop.GetScript(indent: integer): string;
begin
   result := IndentString(indent) + GetScriptText + CRLF + inherited GetScript(indent);
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

procedure TEBForLoop.MergeEqual(const value: string; const data1, data2: TObject);
var
   index: integer;
begin
   if (data1 = nil) and (data2 <> nil) then
   begin
      index := FTempList.IndexOfName(copy(value, 1, pos('=', value) - 1));
      assert(index >= 0);
      assert (FTempList.Objects[index] = nil);
      FTempList.Objects[index] := data2;
   end;
end;

procedure TEBForLoop.AddSingle(const value: string; const data: TObject);
begin
   FTempList.AddObject(value, data);
end;

procedure TEBForLoop.NeededVariables(list: TStringList);
var
   sublist: TStringList;
   index: integer;
begin
   sublist := TStringList.Create;
   try
      inherited NeededVariables(sublist);
      index := sublist.IndexOfName(self.text);
      if index = -1 then
      begin
         sublist.Values[self.Text] := 'integer';
         index := sublist.IndexOfName(self.text);
      end;

      if sublist.Objects[index] <> nil then
         raise ERPGScriptError.CreateFmt('Index variable %s cannot be used for nested loops', [self.text])
      else sublist.Objects[index] := self;
      if sublist.ValueFromIndex[index] <> 'integer' then
         raise ERpgScriptError.CreateFmt('Type of index variable %s must be integer, not %s', [self.text, sublist.ValueFromIndex[index]]);
      FTempList := list;
      StringListCompare(list, sublist, self.MergeEqual, nil, AddSingle);
      FTempList := nil;
   finally
      sublist.free;
   end;
end;

function TEBForLoop.NeededVariableType: THeaderItems;
begin
   result := hi_var;
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

function TEBObjectHelper.VehicleName(id: integer): string;
begin
   result := self.GetLookup(id, 'vehicles');
end;

class function TEBObjectHelper.VarName(id: integer; const group: string): string;
var
   name: string;
begin
   name := GetLookup(id, group);
   if name = '' then
      result := intToStr(id)
   else result := format('%d: %s', [id, name]);
end;

class function TEBObjectHelper.IntName(id: integer): string;
begin
   result := VarName(id, 'Variables');
end;

function TEBObjectHelper.SecondFraction(count: integer): string;
begin
   result := formatFloat('###.#', count / 10) + ' sec';
end;

function TEBObjectHelper.SwitchName(id: integer): string;
begin
   result := VarName(id, 'Switches');
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

procedure TEBLabel.NeededVariables(list: TStringList);
var
   idx: integer;
   labeltext: string;
begin
   labeltext := 'L' + intToStr(values[0]);
   idx := list.IndexOf(labeltext);
   if idx = -1 then
      list.AddObject(labeltext, self)
   else begin
      if list.objects[idx] is TEBLabel then
         raise ERPGScriptError.CreateFmt('Label %d declared multiple times', [Values[0]])
      else list.Objects[idx] := self;
   end;
end;

function TEBLabel.NeededVariableType: THeaderItems;
begin
   result := hi_label;
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

function TEBGoto.NeededVariableType: THeaderItems;
begin
   result := hi_label;
end;

procedure TEBGoto.NeededVariables(list: TStringList);
var
   labeltext: string;
begin
   labeltext := 'L' + intToStr(values[0]);
   if list.IndexOf(labeltext) = -1 then
      list.AddObject(labeltext, self);
end;

{ TEBWhileLoop }

function TEBWhileLoop.GetNodeText: string;
begin
   if (ChildCount > 0) and (Children[0] is TEBExpression) then
      result := format('Loop While (%s):', [ChildNode[0]])
   else result := 'Loop Indefinitely:';
end;

function TEBWhileLoop.GetScriptText: string;
begin
   if (ChildCount > 0) and (Children[0] is TEBExpression) then
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
   if ChildCount = 0 then
      result := '// ' + Text
   else result := '{' + MultilineText(indent, 3) + '}';
   result := IndentString(indent) + result;
end;

function TEBComment.MultilineText(indent, overhang: integer): string;
var
   wrap: string;
   child: TEBObject;
begin
   result := self.Text;
   wrap := #13#10 + IndentString(indent) + StringOfChar(' ', overhang);
   for child in self do
      result := result + wrap + child.Text;
end;

{ TEBProgram }

function TEBProgram.GetScript(indent: integer): string;
var
   list, uList: TStringList;
begin
   list := TStringList.Create;
   try
      list.Add(GetScriptText);
      list.Add('');
      uList := self.UsesList;
      try
         if uList.Count > 0 then
         begin
            list.Add('uses');
               list.Add(format('  %s;', [uList.CommaText]));
            list.Add('');
         end;
      finally
         uList.Free;
      end;
      list.Add(inherited GetScript(-1));
      list.Add('');
      list.Add('begin');
      list.Add('');
      list.Add('end.');
      result := list.Text;
   finally
      list.Free;
   end;
end;

function TEBProgram.GetScriptText: string;
begin
   result := format('program %s;', [self.name]);
end;

function TEBProgram.MustBlock: boolean;
begin
   result := false;
end;

{ TEBUnit }

function TEBUnit.GetScript(indent: integer): string;
var
   list: TStringList;
   uList: TStringList;
   child: TEBObject;
begin
   list := TStringList.Create;
   try
      list.add(GetScriptText);
      list.add('interface');
      uList := self.UsesList;
      try
         if uList.Count > 0 then
         begin
            list.Add('uses');
               list.Add(format('  %s;', [uList.CommaText]));
         end;
      finally
         uList.Free;
      end;
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

{ TEbAssignment }

function TEbAssignment.GetNodeText: string;
begin
   raise ERPGScriptError.Create('Not Implemented');
end;

function TEbAssignment.GetScriptText: string;
const LINE = '%s := %s;';
begin
   result := format(LINE, [ChildScript[0], ChildScript[1]]);
end;

{ TEBMaybeCase }

function TEBMaybeCase.GetScript(indent: integer): string;
begin
   if FCaseBlock then
      result := inherited GetScript(indent) + CRLF + indentString(indent) + 'end;'
   else result := IndentString(indent) + GetScriptText;
end;

function TEBMaybeCase.GetScriptText: string;
begin
   result := GetScriptBase;
   if FCaseBlock then
      result := format('case %s of', [result])
   else result := result + ';';
end;

procedure TEBMaybeCase.AssignProperty(const key, value: string);
begin
   if (key = 'CaseBlock') and (value = 'True') then
      FCaseBlock := true
   else inherited;
end;

procedure TEBMaybeCase.SerializeProps(list: TStringList; depth: integer);
begin
   inherited;
   if FCaseBlock then
      list.Add('CaseBlock = True');
end;

{ TEBMaybeIf }

function TEBMaybeIf.GetNode: TEBNode;
begin
   if IfBlock then
      result := inherited GetNode
   else result := TEBNode.Create(self, GetNodeText);
end;

function TEBMaybeIf.GetScript(indent: integer): string;
begin
   if FIfBlock then
      result := inherited GetScript(indent)
   else result := IndentString(indent) + GetScriptText;
end;

function TEBMaybeIf.GetScriptText: string;
begin
   result := GetScriptBase;
   if FIfBlock then
      result := format('if %s then', [result])
   else result := result + ';';
end;

procedure TEBMaybeIf.AssignProperty(const key, value: string);
begin
   if (key = 'IfBlock') and (value = 'True') then
      FIfBlock := true
   else inherited;
end;

procedure TEBMaybeIf.SerializeProps(list: TStringList; depth: integer);
begin
   inherited;
   if FIfBlock then
      list.Add('IfBlock = True');
end;

procedure TEBMaybeIf.Setup(const blockName: string);
begin
   self.add(TEBBooleanValue.Create(true));
   TEBCodeBlock.Create(self).Name := blockName;
end;

initialization
   TEBObject.RegisterClasses([TEBUntranslated, TEBProcedure, TEBExtension,
                    TEBCase, TEBCaseBlock, TEBElseBlock, TEBEndCase, TEBIf,
                    TEBCodeBlock, TEBEnumCaseBlock, TEBForLoop, TEBLabel, TEBGoto,
                    TEBWhileLoop, TEBBreak, TEBComment, TEBProgram, TEBMap,
                    TEBFunctionCall, TEBUnit, TEBExit]);
end.
