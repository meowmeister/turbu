unit turbu_containers;

interface
uses
   SysUtils, generics.collections,
   turbu_functional;

type
   TRpgObjectList<T: class> = class(TObjectList<T>)
   private
      function getHigh: integer;
   public
      procedure map(work: TMapper<T>);
      function mapF(work: TMapperFunc<T, T>): TRpgObjectList<T>; overload;
      function mapF<U: class>(work: TMapperFunc<T, U>): TRpgObjectList<U>; overload;
      function mapFP<U>(work: TMapperFunc<T, U>): TList<U>;
      function reduce<U>(work: TReducer<T, U>): U;
      function where(filter: TPredicate<T>): TRpgObjectList<T>;
      function firstWhere(filter: TPredicate<T>): T;
      function countWhere(filter: TPredicate<T>): integer;
      property High: integer read getHigh;
   end;

   TListNode<T: class> = class
   private
      FValue: T;
      FNext: TListNode<T>;
   public
      constructor Create(value: T);
      function append(value: TListNode<T>): TListNode<T>;
   end;

   THierarchyTreeList<T: class> = class;

   THierarchyTreeNode<T: class> = class
   private type

      TEnumerator = class(TEnumerator<T>)
      private
         FTopNode: THierarchyTreeNode<T>;
         FList: TListNode<T>;
      protected
         function DoGetCurrent: T; override;
         function DoMoveNext: Boolean; override;
         function ToList: TListNode<T>;
      public
         constructor Create(value: THierarchyTreeNode<T>);
         destructor Destroy; override;
         function GetCurrent: T;
         function MoveNext: boolean;
         property Current: T read GetCurrent;
      end;

      TNodeList = THierarchyTreeList<T>;

   private
      FData: T;
      FRight: TNodeList;
      FList: TNodeList;
      function GetParent: THierarchyTreeNode<T>;
      function GetLevel: integer;
      procedure SetParent(const Value: THierarchyTreeNode<T>);
   public
      constructor Create(value: T);
      destructor Destroy; override;
      procedure Add(node: THierarchyTreeNode<T>); overload;
      procedure Add(value: T); overload;
      function GetEnumerator: TEnumerator;

      property List: TNodeList read FList;
      property Right: TNodeList read FRight;
      property Parent: THierarchyTreeNode<T> read GetParent write SetParent;
      property Level: integer read GetLevel;
      property Data: T read FData write FData;
   end;

   THierarchyTreeList<T: class> = class(TRpgObjectList<THierarchyTreeNode<T>>)
   private
      FParent: THierarchyTreeNode<T>;
      FLevel: integer;
      procedure SetParent(Value: THierarchyTreeNode<T>);
   public
      constructor Create(parent: THierarchyTreeNode<T>);
      function Add(value: THierarchyTreeNode<T>): integer;
      function Remove(value: THierarchyTreeNode<T>): integer;
      function ToLinkedList(out tail: TListNode<T>): TListNode<T>;
      property Parent: THierarchyTreeNode<T> read FParent write SetParent;
      property Level: integer read FLevel;
   end;

implementation

{ TRpgObjectList<T> }

function TRpgObjectList<T>.countWhere(filter: TPredicate<T>): integer;
var
   output: integer;
begin
   output := 0;
   TFunctional.map<T>(self,
      procedure(const input: T)
      begin
         if filter(input) then
            inc(output);
      end);
   result := output;
end;

function TRpgObjectList<T>.firstWhere(filter: TPredicate<T>): T;
var
   enumerator: T;
begin
   for enumerator in self do
      if filter(enumerator) then
         Exit(enumerator);
   result := nil;
end;

function TRpgObjectList<T>.getHigh: integer;
begin
   result := Count - 1;
end;

procedure TRpgObjectList<T>.map(work: TMapper<T>);
begin
   TFunctional.map<T>(self, work);
end;

function TRpgObjectList<T>.mapF(work: TMapperFunc<T, T>): TRpgObjectList<T>;
begin
   result := self.mapF<T>(work);
end;

function TRpgObjectList<T>.mapFP<U>(work: TMapperFunc<T, U>): TList<U>;
var
   output: TList<U>;
begin
   output := TList<U>.Create;
   TFunctional.map<T>(self,
      procedure(const input: T)
      begin
         output.Add(work(input));
      end);
   result := output;
end;

function TRpgObjectList<T>.reduce<U>(work: TReducer<T, U>): U;
begin
   result := TFunctional.reduce2<T, U>(self, work);
end;

function TRpgObjectList<T>.mapF<U>(work: TMapperFunc<T, U>): TRpgObjectList<U>;
var
   output: TRpgObjectList<U>;
begin
   output := TRpgObjectList<U>.Create(false);
   TFunctional.map<T>(self,
      procedure(const input: T)
      begin
         output.Add(work(input));
      end);
   result := output;
end;

function TRpgObjectList<T>.where(filter: TPredicate<T>): TRpgObjectList<T>;
var
   output: TRpgObjectList<T>;
begin
   output := TRpgObjectList<T>.Create(false);
   TFunctional.map<T>(self,
      procedure(const input: T)
      begin
         if filter(input) then
            output.Add(input);
      end);
   result := output;
end;

{ THierarchyTreeNode<T> }

constructor THierarchyTreeNode<T>.Create(value: T);
begin
   inherited Create;
   FData := value;
end;

destructor THierarchyTreeNode<T>.Destroy;
begin
   FRight.Free;
   FData.Free;
   inherited Destroy;
end;

function THierarchyTreeNode<T>.GetEnumerator: TEnumerator;
begin
   result := TEnumerator.Create(self);
end;

function THierarchyTreeNode<T>.GetLevel: integer;
begin
   if assigned(FList) then
      result := FList.Level
   else result := 0;
end;

function THierarchyTreeNode<T>.GetParent: THierarchyTreeNode<T>;
begin
   result := FList.FParent;
end;

procedure THierarchyTreeNode<T>.SetParent(const Value: THierarchyTreeNode<T>);
begin
   FList.Extract(self);
   value.Add(self);
end;

procedure THierarchyTreeNode<T>.Add(node: THierarchyTreeNode<T>);
begin
   if not assigned(FRight) then
      FRight := TNodeList.Create(self);
   FRight.Add(node);
end;

procedure THierarchyTreeNode<T>.Add(value: T);
begin
   self.Add(THierarchyTreeNode<T>.Create(value));
end;

{ THierarchyTreeList<T> }

function THierarchyTreeList<T>.Add(value: THierarchyTreeNode<T>): integer;
begin
   result := inherited Add(value);
   value.FList := self;
end;

constructor THierarchyTreeList<T>.Create(parent: THierarchyTreeNode<T>);
begin
   inherited Create(true);
   SetParent(parent);
end;

function THierarchyTreeList<T>.Remove(value: THierarchyTreeNode<T>): integer;
begin
   result := inherited Remove(value);
   if count = 0 then
   begin
      assert(FParent.FRight = self);
      FreeAndNil(FParent.FRight);
   end;
end;

procedure THierarchyTreeList<T>.SetParent(Value: THierarchyTreeNode<T>);
begin
   FParent := value;
   FLevel := parent.level + 1;
end;

function THierarchyTreeList<T>.ToLinkedList(out tail: TListNode<T>): TListNode<T>;
var
   enumerator: THierarchyTreeNode<T>;
   looping: boolean;
   newtail: TListNode<T>;
begin
   looping := false;
   result := TListNode<T>.Create(self.First.Data);
   tail := result;
   for enumerator in self do
   begin
      if looping then
         tail := tail.append(TListNode<T>.Create(enumerator.data))
      else looping := true;
      if assigned(enumerator.FRight) then
      begin
         tail.append(enumerator.FRight.ToLinkedList(newtail));
         tail := newtail;
      end;
   end;
end;

{ THierarchyTreeNode<T>.TEnumerator }

constructor THierarchyTreeNode<T>.TEnumerator.Create(
  value: THierarchyTreeNode<T>);
begin
   FTopNode := value;
   FList := TListNode<T>.Create(nil);
   FList.append(self.ToList);
end;

destructor THierarchyTreeNode<T>.TEnumerator.Destroy;
begin
   while assigned(FList) do
      MoveNext;
   inherited Destroy;
end;

function THierarchyTreeNode<T>.TEnumerator.DoGetCurrent: T;
begin
   result := self.GetCurrent;
end;

function THierarchyTreeNode<T>.TEnumerator.DoMoveNext: Boolean;
begin
   result := self.MoveNext;
end;

function THierarchyTreeNode<T>.TEnumerator.GetCurrent: T;
begin
   result := FList.FValue;
end;

function THierarchyTreeNode<T>.TEnumerator.MoveNext: boolean;
var
   nextnode: TListNode<T>;
begin
   nextnode := FList.FNext;
   result := assigned(nextnode);
   FList.Free;
   FList := nextnode;
end;

function THierarchyTreeNode<T>.TEnumerator.ToList: TListNode<T>;
var
   node: TListNode<T>;
begin
   node := TListNode<T>.Create(FTopNode.FData);
   result := node;
   if assigned(FTopNode.Right) then
      result.append(FTopNode.Right.ToLinkedList(node));
end;

{ THierarchyTreeNode<T>.TListNode }

constructor TListNode<T>.Create(value: T);
begin
   FValue := value;
end;

function TListNode<T>.append(value: TListNode<T>): TListNode<T>;
begin
   assert(FNext = nil);
   FNext := value;
   result := value;
end;

end.
