unit EBListView;

interface
uses
   ComCtrls, messages,
   EventBuilder, EB_RpgScript;

type
   TEBTreeView = class(TTreeView)
   private
      FProc: TEBProcedure;
      FDblClickExpand: boolean;
      procedure SetProc(const Value: TEBProcedure);
      procedure EndLoading;
      procedure ReconcilePostEdit(obj: TEbObject; node: TTreeNode);
      procedure InsertObject;
   protected
      procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
      procedure DblClick; override;
   public
      property proc: TEBProcedure read FProc write SetProc;
   published
      property DoubleClickExpand: boolean read FDblClickExpand write FDblClickExpand stored FDblClickExpand;
      property ReadOnly default true;
   end;

procedure Register;

implementation
uses
   controls, classes, Generics.Collections,
   EbEdit, EbSelector;

procedure Register;
begin
   RegisterComponents('TURBU', [TEBTreeView])
end;

{ TEBTreeView }

procedure TEBTreeView.DblClick;
var
   node: TTreeNode;
   obj: TEbObject;
begin
   node := self.Selected;
   if assigned(node) then
   begin
      if node.Text = '<>' then
         InsertObject
      else begin
         obj := TObject(node.Data) as TEbObject;
         if assigned(obj) then
            if EbEdit.EditEbObject(obj) and ((obj.ComponentCount > 0) or (node.HasChildren)) then
               self.ReconcilePostEdit(obj, node);
      end;
   end;
   inherited DblClick;
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

{$WARN CONSTRUCTING_ABSTRACT OFF}
procedure TEBTreeView.InsertObject;
var
   selector: TfrmEBSelector;
   editor: TfrmEBEditBase;
   obj: TEbObject;
begin
   selector := TfrmEBSelector.Create(nil);
   try
      if (selector.ShowModal = mrOK) and assigned(selector.Current) then
      begin
         editor := selector.Current.Create(nil);
         try
            obj := editor.NewObj;
         finally
            editor.free;
         end;
         if assigned(obj) then
            ;// do something
      end;
   finally
      selector.Free;
   end;
end;
{$WARN CONSTRUCTING_ABSTRACT ON}

procedure TEBTreeView.SetProc(const Value: TEBProcedure);
var
   tree: TEBNode;
   node, last: TEBNodeData;
   stack: TStack<TEBNodeData>;
   dict: TDictionary<TEBNodeData, TTreeNode>;
begin
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

procedure TEBTreeView.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
   //basically copied from TControl.WMLButtonDblClk, but making the call to
   //inherited that dispatches messages to close/open the tree node optional
   SendCancelMode(Self);
   if FDblClickExpand then
      inherited;
   if csCaptureMouse in ControlStyle then
      MouseCapture := True;
   if csClickEvents in ControlStyle then
      DblClick;
   MouseDown(mbLeft, [ssLeft, ssDouble], message.XPos, message.YPos);
end;

procedure TEBTreeView.ReconcilePostEdit(obj: TEbObject; node: TTreeNode);

   function FindSubnode(nodelist: TObjectList<TTreeNode>; subObj: TEbObject): TTreeNode;
   begin
      for result in nodelist do
         if result.Data = subObj then
            Exit;
      result := nil;
   end;

var
   subObj: TEbObject;
   subnode: TTreeNode;
   nodelist: TObjectList<TTreeNode>;
begin
   nodelist := nil;
   Items.BeginUpdate;
   try
      nodelist := TObjectList<TTreeNode>.Create;
      subnode := node.getFirstChild;
      while assigned(subnode) do
      begin
         nodelist.add(subnode);
         subnode := node.GetNextChild(subnode);
      end;
      for subObj in obj do
      begin
         subnode := findSubnode(nodelist, subObj);
         if assigned(subnode) then
            nodelist.Extract(subnode)
         else subnode := Items.AddChildObject(node, subObj.GetNodeText, subObj);
         subnode.MoveTo(node, naAddChild);
      end;
   finally
      nodelist.free;
      Items.EndUpdate;
   end;
end;

end.
