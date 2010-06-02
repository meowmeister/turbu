unit EventBuilder;

interface
uses
   Classes, Generics.Collections,
   turbu_containers;

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
   protected
      type TVarDesc = record
         name: string;
         vType: integer;
         write: boolean;
      end;

      procedure DefineProperties(Filer: TFiler); override;
      property ArgList: string read GetArgList write SetArgList;
      function IndentString(level: integer): string;
      function GetChildOwner: TComponent; override;
      procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
   public
      constructor Create(AParent: TComponent); override;
      class function Load(const Value: string): TEBObject;
      destructor Destroy; override;
      function GetScript(indent: integer): string; virtual; abstract;
      function GetNodeText: string; virtual; abstract;
      function GetNode: TEBNode; virtual;
      function GetEnumerator: TEBEnumerator;
      function Serialize: string;
      procedure Add(aObject: TEBObject); virtual;
      procedure SaveScript;

      property Values: TList<integer> read FValues write FValues;
      property InUnit: string read GetUnit;
   published
      property Text: string read FText write FText stored HasText;
   end;

   TEBClass = class of TEBObject;

const
   CRLF = #13#10;

implementation
uses
   SysUtils, RTTI,
   RTTIHelper;

const INDENT_SIZE = 2;

procedure TEBObject.Add(aObject: TEBObject);
begin
   self.InsertComponent(aObject);
end;

constructor TEBObject.Create(AParent: TComponent);
begin
   inherited Create(AParent);
   include(FComponentStyle, csSubComponent);
   if assigned(AParent) then
   begin
      assert(AParent is TEBObject);
      AParent.InsertComponent(self);
   end;
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

function TEBObject.GetEnumerator: TEBEnumerator;
begin
   result := TEBEnumerator.Create(self);
end;

function TEBObject.GetNode: TEBNode;
begin
   result := TEBNode.Create(self, GetNodeText);
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
      index := node.Right.IndexOf(broken);
      assert(broken.Parent = node);
      while index < node.Right.Count do
         node.Right[index].Parent := broken;
      if assigned(broken.Right) then
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

initialization
   RegisterClass(TEBObject);
end.
