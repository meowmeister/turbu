unit EventBuilder;

interface
uses
   Classes, Generics.Collections, SysUtils,
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

   TEBObject = class(TComponent)
   private
      FText: string;
      FValues: TList<integer>;
      procedure ReadValueList(Reader: TReader);
      procedure WriteValueList(Writer: TWriter);
      function HasText: Boolean;
      function GetArgList: string;
      procedure SetArgList(const Value: string);
      function GetUnit: string;
      function GetChildText(index: integer): string;
      function GetChildNode(index: integer): string;
   protected
      type TVarDesc = record
         name: string;
         vType: integer;
         write: boolean;
      end;

      class var FDatastore: IRpgDatastore;
      procedure DefineProperties(Filer: TFiler); override;
      function GetChildOwner: TComponent; override;
      procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
      procedure Notification(AComponent: TComponent;
        Operation: TOperation); override;

      class function GetLookup(id: integer; const name: string): string;
      function IndentString(level: integer): string;
      procedure NeededVariables(list: TStringList); virtual;
      property ChildScript[index: integer]: string read GetChildText;
      property ChildNode[index: integer]: string read GetChildNode;
      property ArgList: string read GetArgList write SetArgList;
   public
      constructor Create(AParent: TComponent); override;
      class function Load(const Value: string): TEBObject;
      class function LoadFromStream(stream: TStream): TEBObject;
      destructor Destroy; override;
      function GetScriptText: string; virtual;
      function GetNodeText: string; virtual; abstract;
      function GetNode: TEBNode; virtual;
      function GetScript(indent: integer): string; virtual;
      function GetEnumerator: TEBEnumerator;
      function Serialize: string;
      procedure Add(aObject: TEBObject); virtual;
      procedure SaveScript;
      function RequiredVariables: TStringList;

      property Values: TList<integer> read FValues write FValues;
      property InUnit: string read GetUnit;
      class property Datastore: IRpgDatastore read FDatastore write FDatastore;
   published
      property Text: string read FText write FText stored HasText;
   end;

   TEBClass = class of TEBObject;

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

   TEBExpression = class(TEBObject)
   private
      FSilent: boolean;
   public
      function GetScriptText: string; override;
      function GetNode: TEBNode; override;
   published
      property Silent: boolean read FSilent write FSilent stored FSilent;
   end;

   TEBVariable = class(TEBExpression)
   private
      FType: string;
   published
      property VarType: string read FType write FType;
   end;

   TEBParam = class(TEBVariable);

   TEBHeader = class(TEBExpression)
   public
      function GetScriptText: string; override;
   end;

   TEBRoutine = class(TEBBlock)
   private
      procedure CreateHeader;
      function GetParams: TArray<TEBParam>;
   protected
      function EnsureHeader: TEBHeader;
   public
      procedure RemoveParam(const name: string);
      function GetVarBlock: TStringList; virtual; abstract;
      property ParamList: TArray<TEBParam> read GetParams;
   end;

   ERPGScriptError = class(Exception);

const
   CRLF = #13#10;

implementation
uses
   RTTI,
   RTTIHelper;

const INDENT_SIZE = 2;

procedure TEBObject.Add(aObject: TEBObject);
begin
   self.InsertComponent(aObject);
end;

constructor TEBObject.Create(AParent: TComponent);
begin
   inherited Create(nil);
   include(FComponentStyle, csSubComponent);
   if assigned(AParent) then
      (AParent as TEBObject).Add(self);
   FValues := TList<integer>.Create;
end;

destructor TEBObject.Destroy;
begin
   FValues.Free;
   inherited Destroy;
end;

procedure TEBObject.DefineProperties(Filer: TFiler);
begin
   inherited DefineProperties(Filer);
   Filer.DefineProperty('Values', ReadValueList, WriteValueList, FValues.Count > 0);
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
   list: TStringList;
begin
   list := TStringList.Create;
   try
      list.LoadFromStream(stream);
      result := Load(list.text);
   finally
      list.Free;
   end;
end;

class function TEBObject.Load(const Value: string): TEBObject;
var
   StrStream:TStringStream;
   BinStream: TMemoryStream;
begin
   result := nil;
   StrStream := TStringStream.Create(Value);
   BinStream := TMemoryStream.Create;
   try
      ObjectTextToBinary(StrStream, BinStream);
      BinStream.Seek(0, soFromBeginning);
      try
         Result:= BinStream.ReadComponent(nil) as TEBObject;
      except
         result.Free;
         raise;
      end;
   finally
      BinStream.Free;
      StrStream.Free;
   end;
//   assert(result.Serialize = value);
end;

procedure TEBObject.NeededVariables(list: TStringList);
var
   child: TEBObject;
begin
   for child in self do
      if not (child is TEBExpression) then
         child.NeededVariables(list);
end;

procedure TEBObject.Notification(AComponent: TComponent; Operation: TOperation);
begin
   //suppress this, because it's very slow and not needed for Event Builder
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
   BinStream:TMemoryStream;
   StrStream: TStringStream;
begin
   BinStream := TMemoryStream.Create;
   StrStream := TStringStream.Create;
   try
      BinStream.WriteComponent(self);
      BinStream.Seek(0, soFromBeginning);
      ObjectBinaryToText(BinStream, StrStream);
      StrStream.Seek(0, soFromBeginning);
      Result:= StrStream.DataString;
   finally
      StrStream.Free;
      BinStream.Free
   end;
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

function TEBObject.GetChildOwner: TComponent;
begin
   result := self;
end;

procedure TEBObject.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
   enumerator: TEBObject;
begin
   for enumerator in self do
      proc(enumerator);
end;

function TEBObject.GetChildNode(index: integer): string;
begin
   result := (Components[index] as TEBObject).GetNodeText;
end;

function TEBObject.GetChildText(index: integer): string;
begin
   result := (Components[index] as TEBObject).GetScriptText;
end;

function TEBObject.GetEnumerator: TEBEnumerator;
begin
   result := TEBEnumerator.Create(self);
end;

class function TEBObject.GetLookup(id: integer; const name: string): string;
begin
   if assigned(FDatastore) then
      result := FDatastore.NameLookup(name, id)
   else result := '';
   if result = '' then
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

procedure TEBObject.WriteValueList(Writer: TWriter);
begin
   Writer.WriteString(self.ArgList);
end;

procedure TEBObject.ReadValueList(Reader: TReader);
begin
   self.ArgList := reader.ReadString;
end;

function TEBObject.RequiredVariables: TStringList;
begin
   result := TStringList.Create;
   NeededVariables(result);
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
   result := FObject.Components[FIndex] as TEBObject;
end;

function TEBEnumerator.MoveNext: Boolean;
begin
   inc(FIndex);
   result := FIndex < FObject.ComponentCount;
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
   result := (self.ComponentCount > 1) or AlwaysBlock;
end;

{ TEBExpression }

function TEBExpression.GetScriptText: string;
begin
   result := GetNodeText;
end;

function TEBExpression.GetNode: TEBNode;
begin
   raise ERPGScriptError.Create('Expressions don''t get their own tree nodes!');
end;

{ TEBRoutine }

{$WARN CONSTRUCTING_ABSTRACT OFF}
procedure TEBRoutine.CreateHeader;
begin
   TEBHeader.Create(self).ComponentIndex := 0;
end;
{$WARN CONSTRUCTING_ABSTRACT ON}

function TEBRoutine.EnsureHeader: TEbHeader;
begin
   if (self.ComponentCount = 0) or not (self.Components[0] is TEBHeader) then
      CreateHeader;
   result := self.components[0] as TEBHeader;
end;

function TEBRoutine.GetParams: TArray<TEBParam>;
var
   header: TEBHeader;
   comp: TComponent;
   counter: integer;
begin
   header := EnsureHeader;
   SetLength(result, header.ComponentCount);
   counter := 0;
   for comp in header do
      if comp is TEBParam then
      begin
         result[counter] := TEBParam(comp);
         inc(counter);
      end;
   SetLength(result, counter);
end;

procedure TEBRoutine.RemoveParam(const name: string);
var
   header: TEBHeader;
   comp: TComponent;
begin
   header := EnsureHeader;
   for comp in header do
      if (comp is TEBParam) and (TEbObject(comp).text = name) then
      begin
         comp.free;
         Break;
      end;
end;

{ TEBHeader }

function TEBHeader.GetScriptText: string;
begin
   result := ''; //implement this
end;

initialization
   RegisterClass(TEBObject);
end.
