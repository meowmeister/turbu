unit EBListView;

interface
uses
   ComCtrls,
   EventBuilder, EB_RpgScript;

type
   TEBTreeView = class(TTreeView)
   private
      FProc: TEBProcedure;
      procedure SetProc(const Value: TEBProcedure);
      procedure EndLoading;
   public
      destructor Destroy; override;
      property proc: TEBProcedure read FProc write SetProc;
   end;

procedure Register;

implementation
uses
   classes, Generics.Collections;

procedure Register;
begin
   RegisterComponents('TURBU', [TEBTreeView])
end;

{ TEBTreeView }

destructor TEBTreeView.Destroy;
begin
   FProc.Free;
   inherited;
end;

procedure TEBTreeView.EndLoading;
var
   item: TTreeNode;
begin
   for item in self.items do
      item.Expand(true);
   self.SelectFirst;
   items.GetFirstNode.MakeVisible;
end;

procedure TEBTreeView.SetProc(const Value: TEBProcedure);
var
   tree: TEBNode;
   node, last: TEBNodeData;
   stack: TStack<TEBNodeData>;
   dict: TDictionary<TEBNodeData, TTreeNode>;
begin
   FProc.Free;
   Items.Clear;
   FProc := Value;
   tree := FProc.GetNode;
   stack := TStack<TEBNodeData>.Create;
   dict := TDictionary<TEBNodeData, TTreeNode>.Create;
   try
      last := tree.Data;
      stack.push(tree.Data);
      dict.add(last, nil);
      for node in tree do
      begin
         if (node = last) or (node.line = '') then
            Continue;
         if (last <> stack.Peek) then
            if (node.FindParent = last) then
               stack.Push(last)
            else while node.FindParent <> stack.Peek do
               stack.Pop;
         dict.Add(node, items.AddChildObject(dict[stack.Peek], node.line, node.obj));
         last := node;
      end;
      EndLoading;
   finally
      stack.free;
      dict.Free;
      tree.free;
   end;
end;

end.
