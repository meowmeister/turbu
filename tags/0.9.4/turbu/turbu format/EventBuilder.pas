unit EventBuilder;

interface
uses
   Classes, Generics.Collections, SysUtils, TypInfo,
   turbu_containers, turbu_database_interface;

type
   UsesUnitAttribute = class(TCustomAttribute)
   private
    FName: string;
   public
      constructor Create(const name: string);
      property name: string read FName;
   end;

   TEBObject = class;
   TEBObjectClass = class of TEBObject;

   TEBEnumerator = class
   private
      FObject: TEBObject;
      FIndex: integer;
   public
      constructor Create(obj: TEBObject);
      function MoveNext: Boolean;
      function GetCurrent: TEBObject;
      property Current: TEBObject read GetCurrent;
   end;

   TEBNodeData = class
   private
      FObject: TEBObject;
      FLine: string;
      FParent: TEBNodeData;
      function GetParent: TEBNodeData;
   public
      constructor Create(obj: TEBObject; line: string);
      property obj: TEBObject read FObject;
      property line: string read FLine;
      property parent: TEBNodeData read FParent write FParent;
      property FindParent: TEBNodeData read GetParent;
   end;

   TEBNode = class(THierarchyTreeNode<TEBNodeData>)
   private
      procedure CheckTree(node: TEBNode);
   public
      constructor Create(obj: TEBObject; line: string);
      procedure Add(node: THierarchyTreeNode<TEBNodeData>); override;
   end;

   TEBVariable = class;
   THeaderItems = (hi_none, hi_var, hi_const, hi_label);

   TEBObject = class
   private
      class var Registry: TDictionary<string, TEBObjectClass>;
   private
      FName: string;
      FText: string;
      FValues: TList<integer>;
      FChildren: TObjectList<TEBObject>;
      FNameDic: TStringList;
      FOwner: TEBObject;
      FDeleting: boolean;

      function HasText: Boolean;
      function GetArgList: string;
      procedure SetArgList(const Value: string);
      function GetUnit: string;
      function GetChildText(index: integer): string;
      function GetChildNode(index: integer): string;
      procedure EnsureNameDic;
      class function CreateNew(const line: string; parent: TEBObject): TEBObject;
      procedure LoadProperties(list: TStringList; var index: integer);
      procedure ValidateAssignment(const line: string; out name, value: string);
      procedure SetText(const line: string);
      procedure SetValues(const line: string);
      procedure ParseAssignment(const line: string);
      function GetChildCount: integer;
      function LookupName(const name: string): TEBObject;
   protected
      type TVarDesc = record
         name: string;
         vType: integer;
         write: boolean;
      end;

      class function Deserialize(list: TStringList; var index: integer; parent: TEBObject): TEBObject;
      procedure SerializeTo(list: TStringList; depth: integer);
      procedure SerializeProps(list: TStringList; depth: integer); virtual;
      function Empty: boolean; virtual;
      procedure AddNamed(aObject: TEBObject); virtual;
      procedure AssignProperty(const key, value: string); virtual;

      class var FDatastore: IRpgDatastore;

      class function GetLookup(id: integer; const name: string): string;
      class function GetFKLookup(key, id: integer; const name: string): string;
      function IndentString(level: integer): string;
      procedure NeededVariables(list: TStringList); virtual;
      procedure ScanUsesList(list: TStringList); virtual;
      property ChildScript[index: integer]: string read GetChildText;
      property ChildNode[index: integer]: string read GetChildNode;
      property ArgList: string read GetArgList write SetArgList;
   public
      constructor Create(AParent: TEBObject); virtual;
      class function Load(const Value: string): TEBObject;
      class function LoadFromStream(stream: TStream): TEBObject;
      destructor Destroy; override;
      class procedure RegisterClasses(classes: array of TEBObjectClass);

      function GetScriptText: string; virtual;
      function GetNodeText: string; virtual; abstract;
      function GetNode: TEBNode; virtual;
      function GetScript(indent: integer): string; virtual;
      function GetEnumerator: TEBEnumerator;
      function Serialize: string;
      procedure Add(aObject: TEBObject); virtual;
      procedure Insert(index: integer; aObject: TEBObject);
      procedure RemoveChild(aObject: TEBObject);
      procedure SaveScript;
      function RequiredVariables: TStringList;
      function UsesList: TStringList;
      function Clone: TEBObject;
      procedure Clear;
      procedure Extract;
      function NeededVariableType: THeaderItems; virtual;
      procedure FreeChild(const name: string);
      procedure Loaded; virtual;

      property Values: TList<integer> read FValues;
      property InUnit: string read GetUnit;
      property ChildCount: integer read GetChildCount;
      property children: TObjectList<TEBObject> read FChildren;
      property ChildByName[const name: string]: TEBObject read LookupName;
      property Owner: TEBObject read FOwner;
      class property Datastore: IRpgDatastore read FDatastore write FDatastore;

      property Name: string read FName write FName;
      property Text: string read FText write FText stored HasText;
   end;

   TEBClass = class of TEBObject;

   TEBBlock = class(TEBObject)
   protected
      function AlwaysBlock: boolean; virtual;
      function AlwaysEndBlock: boolean; virtual;
      function MustBlock: boolean; virtual;
      function ExprNodeTree: boolean; virtual;
   public
      function GetScript(indent: integer): string; override;
      function GetNode: TEBNode; override;
      function GetNodeText: string; override;
   end;

   TEBExpression = class(TEBObject)
   private
      FSilent: boolean;
   protected
      procedure SerializeProps(list: TStringList; depth: integer); override;
      procedure AssignProperty(const key, value: string); override;
   public
      function GetScriptText: string; override;
      function GetNode: TEBNode; override;
      property Silent: boolean read FSilent write FSilent stored FSilent;
   end;

   TEBVariable = class(TEBExpression)
   private
      FType: string;
   protected
      procedure SerializeProps(list: TStringList; depth: integer); override;
      procedure AssignProperty(const key, value: string); override;
   public
      function GetNodeText: string; override;
      property VarType: string read FType write FType;
   end;

   TEBParam = class(TEBVariable)
   private
      FFlags: TParamFlags;
      function HasFlags: Boolean;
      function FlagsName: string;
   protected
      procedure SerializeProps(list: TStringList; depth: integer); override;
      procedure AssignProperty(const key, value: string); override;
   public
      property Flags: TParamFlags read FFlags write FFlags stored HasFlags;
   end;

   TEBHeader = class(TEBExpression)
   public
      function GetScriptText: string; override;
   end;

   TEBRoutine = class(TEBBlock)
   private
      FHeader: TEBHeader;
      procedure CreateHeader;
      function GetParams: TArray<TEBParam>;
   protected
      function EnsureHeader: TEBHeader;
      procedure SerializeProps(list: TStringList; depth: integer); override;
      procedure AddNamed(obj: TEBObject); override;
   public
      destructor Destroy; override;
      procedure RemoveParam(const name: string);
      function GetVarBlock: TStringList; virtual; abstract;
      property ParamList: TArray<TEBParam> read GetParams;
   end;

   ERPGScriptError = class(Exception);

const
   CRLF = #13#10;
   BAD_LOOKUP = #1;

implementation
uses
   RTTI,
   RTTIHelper;

const INDENT_SIZE = 2;

procedure TEBObject.Add(aObject: TEBObject);
begin
   if aObject = self then
      raise ERpgScriptError.Create('Can''t add an object as a child of itself');
   if aObject.FOwner <> nil then
      AObject.FOwner.children.Extract(aObject);
   aObject.FOwner := self;
   FChildren.Add(aObject);
   if aObject.name <> '' then
   begin
      EnsureNameDic;
      FNameDic.AddObject(aObject.Name, aObject);
   end;
end;

procedure TEBObject.Insert(index: integer; aObject: TEBObject);
var
   idx: integer;
begin
   if aObject = self then
      raise ERpgScriptError.Create('Can''t insert an object as a child of itself');
   if aObject.FOwner <> nil then
   begin
      if aObject.FOwner = self then
      begin
         idx := children.IndexOf(aObject);
         if (idx > 0) and (idx < index) then
            dec(index);
      end;
      AObject.FOwner.children.Extract(aObject);
   end;
   aObject.FOwner := self;
   FChildren.Insert(index, aObject);
   if aObject.name <> '' then
   begin
      EnsureNameDic;
      FNameDic.AddObject(aObject.Name, aObject);
   end;
end;

procedure TEBObject.RemoveChild(aObject: TEBObject);
var
   obj: TEBObject;
begin
   if FDeleting then
      Exit;
   obj := FChildren.Extract(aObject);
   if (obj = aObject) and (obj.Name <> '') and assigned(FNameDic) then
      FNameDic.Delete(FNameDic.IndexOf(obj.Name));
end;

procedure TEBObject.AddNamed(aObject: TEBObject);
begin
   Add(aObject);
end;

procedure TEBObject.AssignProperty(const key, value: string);
begin
   raise ERPGScriptError.CreateFmt('Invalid property %s, value %s for object of type %s', [key, value, self.ClassName]);
end;

procedure TEBObject.Clear;
begin
   values.Clear;
   FChildren.Clear;
end;

function TEBObject.Clone: TEBObject;
begin
   result := TEBObject.Load(self.Serialize);
end;

constructor TEBObject.Create(AParent: TEBObject);
begin
   inherited Create;
   if assigned(AParent) then
      AParent.Add(self);
   FValues := TList<integer>.Create;
   FChildren := TObjectList<TEBObject>.Create;
end;

destructor TEBObject.Destroy;
begin
   FDeleting := true;
   if assigned(FOwner) then
      FOwner.RemoveChild(self);
   FNameDic.Free;
   FChildren.Free;
   FValues.Free;
   inherited Destroy;
end;

class function TEBObject.CreateNew(const line: string; parent: TEBObject): TEBObject;
var
   name, clsName: string;
   newclass: TEBObjectClass;
   colonPos: integer;
begin
   colonPos := pos(':', line);
   if colonPos = 0 then
   begin
      name := '';
      clsName := copy(line, 8, MAXINT);
   end
   else begin
      name := copy(line, 8, colonPos - 8);
      clsName := copy(line, colonPos + 1, MAXINT);
   end;
   name := trim(name);
   clsName := trim(clsName);
   if not Registry.TryGetValue(clsName, newClass) then
      raise ERPGScriptError.CreateFmt('Class %s is not registered', [clsName]);
   result := newclass.Create(nil);
   if name = '' then
   begin
      if assigned(parent) then
         parent.Add(result)
   end
   else begin
      result.Name := name;
      if assigned(parent) then
         parent.addNamed(result);
   end;
end;

procedure TEBObject.ValidateAssignment(const line: string; out name, value: string);
var
   eqPos: integer;
begin
   eqPos := pos('=', line);
   if eqPos = 0 then
      raise ERPGScriptError.CreateFmt('Expected property assignment but found "%s"', [line]);
   name := trim(Copy(line, 1, eqPos - 1));
   value := trim(copy(line, eqPos + 1, MAXINT));
end;

procedure TEBObject.SetText(const line: string);
var
   dummy, lText: string;
begin
   ValidateAssignment(line, dummy, lText);
   self.Text := AnsiDequotedStr(lText, '''');
end;

procedure TEBObject.SetValues(const line: string);
var
   dummy, lValues: string;
begin
   ValidateAssignment(line, dummy, lValues);
   if not (lValues[1] = '(') and(lValues[length(lValues)] = ')') then
      raise ERPGScriptError.CreateFmt('Invalid Values list "%s"', [lValues]);
   self.SetArgList(copy(lValues, 2, length(lValues) - 2));
end;

function TEBObject.UsesList: TStringList;
var
   selfidx: integer;
begin
   result := TStringList.Create;
   try
      result.Duplicates := dupIgnore;
      result.Sorted := true;
      self.ScanUsesList(result);
      selfidx := result.IndexOf(self.Name);
      if selfidx >= 0 then
         result.Delete(selfidx);
   except
      result.Free;
      raise;
   end;
end;

procedure TEBObject.ScanUsesList(list: TStringList);
var
   child: TEBObject;
   unitName: string;
begin
   for child in self do
      if not (child is TEBExpression) then
         child.ScanUsesList(list);
   unitName := self.InUnit;
   if unitName <> '' then
      list.Add(unitName);
end;

procedure TEBObject.ParseAssignment(const line: string);
var
   key, value: string;
begin
   ValidateAssignment(line, key, value);
   self.AssignProperty(key, value);
end;

procedure TEBObject.LoadProperties(list: TStringList; var index: integer);
var
   line, token: string;
   spacePos: integer;
begin
   while true do
   begin
      inc(index);
      if index >= list.Count then
         raise ERPGScriptError.Create('Unexpected end of file');
      line := TrimLeft(list[index]);
      spacePos := pos(' ', line);
      if spacePos = 0 then
      begin
         if line = 'end' then
            Break;
         raise ERPGScriptError.CreateFmt('Unknown token "%s"', [line]);
      end;
      token := copy(line, 1, spacePos - 1);
      if Token = 'object' then
         Deserialize(list, index, self)
      else if Token = 'Values' then
         SetValues(line)
      else if token = 'Text' then
         SetText(line)
      else begin
         ParseAssignment(line);
      end;
   end;
end;

function TEBObject.LookupName(const name: string): TEBObject;
var
   idx: integer;
begin
   if assigned(fNameDic) then
      idx := FNameDic.IndexOf(name)
   else idx := -1;
   if idx = -1 then
      result := nil
   else result := FNameDic.Objects[idx] as TEBObject;
end;

procedure TEBObject.FreeChild(const name: string);
var
   idx: integer;
   child: TEBObject;
begin
   idx := FNameDic.IndexOf(name);
   if idx <> -1 then
   begin
      child := FNameDic.Objects[idx] as TEBObject;
      FNameDic.Delete(idx);
      FChildren.Remove(child);
   end;
end;

class function TEBObject.Deserialize(list: TStringList;
  var index: integer; parent: TEBObject): TEBObject;
var
   line: string;
   sem: boolean;
begin
   if index >= list.Count then
      raise ERPGScriptError.Create('Unexpected end of file');
   line := TrimLeft(list[index]);
   if Pos('object ', line) = 0 then
      raise ERPGScriptError.CreateFmt('Object token expected but found "%s"', [line]);
   sem := line[length(line)] = ';';
   if sem then
      delete(line, length(line), 1);
   result := CreateNew(line, parent);
   if not sem then
   try
      result.LoadProperties(list, index);
   except
      result.Free;
      raise;
   end;
   result.Loaded;
end;

function TEBObject.Empty: boolean;
begin
   result := (FText = '') and (FValues.Count = 0) and (ChildCount = 0);
end;

procedure TEBObject.EnsureNameDic;
begin
   if FNameDic = nil then
   begin
      FNameDic := TStringList.Create;
      FNameDic.Sorted := true;
      FNameDic.Duplicates := dupError;
   end;
end;

procedure TEBObject.Extract;
begin
   assert(FOwner.children.Extract(self) = self);
   FOwner := nil;
end;

function TEBObject.HasText: Boolean;
begin
   result := FText <> '';
end;

function TEBObject.IndentString(level: integer): string;
begin
   result := StringOfChar(' ', level * INDENT_SIZE);
end;

class function TEBObject.LoadFromStream(stream: TStream): TEBObject;
var
   size: int64;
   value: utf8String;
begin
   size := stream.Size - stream.Position;
   setLength(value, size);
   stream.Read(value[1], size);
   result := Load(string(value));
end;

class function TEBObject.Load(const Value: string): TEBObject;
var
   list: TStringList;
   i: integer;
begin
   list := TStringList.Create;
   try
      list.Text := value;
      i := 0;
      result := TEBObject.Deserialize(list, i, nil);
   finally
      list.Free;
   end;
//   assert(result.Serialize = value);
end;

procedure TEBObject.Loaded;
begin
   //this virtual method intentionally left blank
end;

procedure TEBObject.NeededVariables(list: TStringList);
var
   child: TEBObject;
begin
   for child in self do
      if not (child is TEBExpression) then
         child.NeededVariables(list);
end;

function TEBObject.NeededVariableType: THeaderItems;
begin
   result := hi_none;
end;

procedure TEBObject.SaveScript;
var
   list: TStringList;
   filename: string;
begin
   list := TStringList.Create;
   try
      list.text := self.GetScript(0);
      filename := IncludeTrailingPathDelimiter(ExtractFilePath(paramStr(0))) + 'script_debug.txt';
      list.SaveToFile(filename);
   finally
      list.free;
   end;
end;

function TEBObject.Serialize: string;
var
   list: TStringList;
begin
   list := TStringList.Create;
   try
      self.SerializeTo(list, 0);
      result := list.Text;
   finally
      list.Free;
   end;
end;

procedure TEBObject.SerializeProps(list: TStringList; depth: integer);
begin
   //intentionally left blank;
end;

procedure TEBObject.SerializeTo(list: TStringList; depth: integer);
var
   nameline: string;
   isEmpty: boolean;
   indent, indent2: string;
   child: TEBObject;
begin
   if self.Name = '' then
      nameline := format('object %s', [self.ClassName])
   else nameline := format('object %s: %s', [self.Name, self.ClassName]);
   indent := IndentString(depth);
   isEmpty := self.Empty;
   if isEmpty then
      nameline := nameline + ';';
   list.add(indent + nameline);
   if isEmpty then
      Exit;
   indent2 := indent + '  ';
   if HasText then
      list.Add(indent2 + format('Text = %s', [QuotedStr(Text)]));
   if Values.Count > 0 then
      list.Add(indent2 + format('Values = (%s)', [GetArgList]));
   SerializeProps(list, depth + 1);
   for child in FChildren do
      child.SerializeTo(list, depth + 1);
   list.Add(indent + 'end');
end;

procedure TEBObject.SetArgList(const Value: string);
var
   data: string;
   parser: TStringList;
begin
   FValues.Clear;
   parser := TStringList.Create;
   try
      parser.CommaText := Value;
      for data in parser do
         FValues.Add(StrToInt(data));
   finally
      parser.Free;
   end;
end;

function TEBObject.GetArgList: string;
var
   list: TStringList;
   Values: integer;
begin
   list := TStringList.Create;
   try
      for Values in FValues do
         list.Add(IntToStr(Values));
      result := list.CommaText;
   finally
      list.Free;
   end;
end;

function TEBObject.GetChildCount: integer;
begin
   result := FChildren.Count;
end;

function TEBObject.GetChildNode(index: integer): string;
begin
   result := FChildren[index].GetNodeText;
end;

function TEBObject.GetChildText(index: integer): string;
begin
   result := FChildren[index].GetScriptText;
end;

function TEBObject.GetEnumerator: TEBEnumerator;
begin
   result := TEBEnumerator.Create(self);
end;

class function TEBObject.GetFKLookup(key, id: integer;
  const name: string): string;
begin
   if assigned(FDatastore) then
      result := FDatastore.NameLookup(name, key, id)
   else result := BAD_LOOKUP;
   if result = BAD_LOOKUP then
      result := format('??%s #%d??', [name, id]);
end;

class function TEBObject.GetLookup(id: integer; const name: string): string;
begin
   if assigned(FDatastore) then
      result := FDatastore.NameLookup(name, id)
   else result := BAD_LOOKUP;
   if result = BAD_LOOKUP then
      result := format('??%s #%d??', [name, id]);
end;

function TEBObject.GetNode: TEBNode;
begin
   result := TEBNode.Create(self, GetNodeText);
end;

function TEBObject.GetScript(indent: integer): string;
begin
   result := IndentString(indent) + GetScriptText;
end;

function TEBObject.GetScriptText: string;
begin
   AbstractErrorProc; //for some reason this doesn't link right as an abstract
end;

function TEBObject.GetUnit: string;
var
   att: UsesUnitAttribute;
begin
   att :=  UsesUnitAttribute(TRttiContext.Create.GetType(self.ClassInfo).GetAttribute(UsesUnitAttribute));
   if assigned(att) then
      result := att.name
   else result := '';
end;

class procedure TEBObject.RegisterClasses(classes: array of TEBObjectClass);
var
   cls: TEBObjectClass;
begin
   for cls in classes do
      Registry.Add(cls.ClassName, cls);
end;

function TEBObject.RequiredVariables: TStringList;
begin
   result := TStringList.Create;
   result.Sorted := true;
   result.Duplicates := dupError;
   try
      NeededVariables(result);
   except
      result.Free;
      raise;
   end;
end;

{ UsesUnitAttribute }

constructor UsesUnitAttribute.Create(const name: string);
begin
   FName := name;
end;

{ TEBEnumerator }

constructor TEBEnumerator.Create(obj: TEBObject);
begin
   FObject := obj;
   FIndex := -1;
end;

function TEBEnumerator.GetCurrent: TEBObject;
begin
   result := FObject.FChildren[FIndex];
end;

function TEBEnumerator.MoveNext: Boolean;
begin
   inc(FIndex);
   result := FIndex < FObject.ChildCount;
end;

{ TEBNodeData }

constructor TEBNodeData.Create(obj: TEBObject; line: string);
begin
   FObject := obj;
   FLine := line;
end;

function TEBNodeData.GetParent: TEBNodeData;
begin
   result := FParent;
   while (result.line = '') and assigned(result.FParent) do
      result := result.FParent;
end;

{ TEBNode }

procedure TEBNode.CheckTree(node: TEBNode);
var
   subnode, broken: THierarchyTreeNode<TEBNodeData>;
   ebNode: TEBNode absolute subnode;
   obj: TEBObject;
   index: integer;
   mustcheck: boolean;
begin
   obj := node.Data.obj;
   broken := nil;
   for subnode in node.Right do
      if ebNode.Data.obj = obj then
      begin
         broken := subnode;
         break;
      end;
   if broken <> nil then
   begin
      mustcheck := false;
      index := node.Right.IndexOf(broken) + 1;
      assert(broken.Parent = node);
      while index < node.Right.Count do
         if node.Right[index].Parent <> broken then //prevent infinite recursion
         begin
            mustcheck := true;
            node.Right[index].Parent := broken;
         end
         else inc(index); //prevent infinite loop caused by preventing infinite recursion
      if assigned(broken.Right) and mustcheck then
         CheckTree(broken as TEBNode);
   end;
end;

constructor TEBNode.Create(obj: TEBObject; line: string);
begin
   inherited Create(TEBNodeData.Create(obj, line));
end;

procedure TEBNode.Add(node: THierarchyTreeNode<TEBNodeData>);
begin
   inherited Add(node);
   (node as TEBNode).Data.parent := self.Data;
   if assigned(node.Right) then
      CheckTree(node as TEBNode);
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

function TEBBlock.ExprNodeTree: boolean;
begin
   result := false;
end;

function TEBBlock.GetNodeText: string;
begin
   result := self.name;
end;

function TEBBlock.GetNode: TEBNode;
var
   child: TEBObject;
   exprNodes: boolean;
begin
   result := inherited GetNode;
   exprNodes := self.ExprNodeTree;
   for child in self do
      if exprNodes or not (child is TEBExpression) then
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
         if not (element is TEBExpression) then
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
   result := (self.ChildCount > 1) or AlwaysBlock;
end;

{ TEBExpression }

function TEBExpression.GetScriptText: string;
begin
   result := GetNodeText;
end;

procedure TEBExpression.SerializeProps(list: TStringList; depth: integer);
begin
   inherited;
   if FSilent then
      list.Add(IndentString(depth) + 'Silent = True');
end;

procedure TEBExpression.AssignProperty(const key, value: string);
begin
   if (key = 'Silent') and (value = 'True') then
      FSilent := true
   else inherited;
end;

function TEBExpression.GetNode: TEBNode;
begin
   result := inherited;
//   raise ERPGScriptError.Create('Expressions don''t get their own tree nodes!');
end;

{ TEBRoutine }

destructor TEBRoutine.Destroy;
begin
   FHeader.Free;
   inherited Destroy;
end;

procedure TEBRoutine.AddNamed(obj: TEBObject);
begin
   if (obj.Name <> 'Header') or assigned(FHeader) or not (obj is TEBHeader) then
      raise ERPGScriptError.Create('Invalid header assignment');
   FHeader := TEBHeader(obj);
end;

{$WARN CONSTRUCTING_ABSTRACT OFF}
procedure TEBRoutine.CreateHeader;
begin
   FHeader := TEBHeader.Create(nil);
   FHeader.Name := 'Header';
end;
{$WARN CONSTRUCTING_ABSTRACT ON}

function TEBRoutine.EnsureHeader: TEbHeader;
begin
   if FHeader = nil then
      CreateHeader;
   result := FHeader;
end;

function TEBRoutine.GetParams: TArray<TEBParam>;
var
   header: TEBHeader;
   child: TEBObject;
   counter: integer;
begin
   header := EnsureHeader;
   SetLength(result, header.ChildCount);
   counter := 0;
   for child in header do
      if child is TEBParam then
      begin
         result[counter] := TEBParam(child);
         inc(counter);
      end;
   SetLength(result, counter);
end;

procedure TEBRoutine.RemoveParam(const name: string);
var
   header: TEBHeader;
   obj: TEBObject;
begin
   header := EnsureHeader;
   for obj in header do
      if (obj is TEBParam) and (obj.text = name) then
      begin
         obj.free;
         Break;
      end;
end;

procedure TEBRoutine.SerializeProps(list: TStringList; depth: integer);
begin
   inherited;
   if assigned(FHeader) and not FHeader.Empty then
      FHeader.SerializeTo(list, depth);
end;

{ TEBHeader }

function TEBHeader.GetScriptText: string;
var
   child: TEBObject;
   childParam: TEBParam absolute child;
   lastType: string;
   lastParams: TParamFlags;
   fragment: TStringList;

   procedure flush;
   const PARAM = '%s %s: %s';
   begin
      if fragment.count = 0 then
         Exit;
      if result <> '' then
         result := result + '; ';
      result := result + TrimLeft(format(PARAM, [childParam.FlagsName, fragment.CommaText, lastType]));
      fragment.Clear;
   end;

   procedure CheckFlush;
   begin
      if (childParam.FFlags <> lastParams) or (childParam.VarType <> lastType) then
      begin
         flush;
         lastType := childParam.VarType;
         lastParams := childParam.FFlags;
      end;
   end;

begin
   if ChildCount = 0 then
      Exit('');
   fragment := TStringList.Create;
   try
      for child in self do
      begin
         CheckFlush;
         assert(child is TEBParam);
         fragment.Add(child.Text);
      end;
      Flush;
   finally
      fragment.Free;
   end;
   result := format('(%s)', [result]);
end;

{ TEBParam }

procedure TEBParam.AssignProperty(const key, value: string);
begin
   if key = 'Flags' then
      byte(FFlags) := StringToSet(PTypeInfo(TypeInfo(TParamFlags)), value)
   else inherited;
end;

function TEBParam.FlagsName: string;
begin
   result := TypInfo.SetToString(PTypeInfo(TypeInfo(TParamFlags)), byte(FFlags * [pfVar, pfConst, pfOut]));
   result := stringReplace(result, 'pf', '', [rfReplaceAll]);
end;

function TEBParam.HasFlags: Boolean;
begin
   result := FFlags <> [];
end;

procedure TEBParam.SerializeProps(list: TStringList; depth: integer);
begin
   inherited;
   if FFlags <> [] then
      List.Add(IndentString(depth) + 'Flags = ' + self.FlagsName);
end;

{ TEBVariable }

procedure TEBVariable.AssignProperty(const key, value: string);
begin
   if key = 'Vartype' then
      FType := value
   else inherited;
end;

function TEBVariable.GetNodeText: string;
begin
   result := FText;
end;

procedure TEBVariable.SerializeProps(list: TStringList; depth: integer);
begin
   inherited;
   list.Add(IndentString(depth) + 'Vartype = ' + FType);
end;

initialization
   TEBObject.Registry := TDictionary<string, TEBObjectClass>.Create;
   TEBObject.RegisterClasses([TEBObject, TEBHeader, TEBParam]);
finalization
   TEBObject.Registry.Free;
end.
