unit EBListView;

interface
uses
   ComCtrls, Messages, Classes, DB, DBCtrls,
   EventBuilder;

type
   TOrphanEvent = procedure(sender: TObject; obj: TEbObject) of object;

   TEBTreeView = class(TTreeView)
   private
      FProc: TEBRoutine;
      FDblClickExpand: boolean;
      FOnOrphan: TOrphanEvent;
      FDataLink: TFieldDataLink;
      procedure SetProc(const Value: TEBRoutine);
      procedure EndLoading;
      procedure ReconcilePostEdit(obj: TEbObject; node: TTreeNode);
      procedure InsertObject;
      procedure Orphan(obj: TEbObject);
      procedure SetupContext;
      procedure AddObjectContext(context: TDataset; const base: string; const name, vartype: string);
      procedure AddVariable(context: TDataset; const name, vartype: string);
      procedure AddToContext(context: TDataset; const name, vartype: string);
   private
      function GetDataSet: TDataSet;
      procedure SetDataSet(const Value: TDataSet);
   protected
      procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
      procedure DblClick; override;
   public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      property proc: TEBRoutine read FProc write SetProc;
   published
      property DoubleClickExpand: boolean read FDblClickExpand write FDblClickExpand stored FDblClickExpand;
      property OnOrphan: TOrphanEvent read FOnOrphan write FOnOrphan;
      property ReadOnly default true;
      property ShowLines default false;
      property RowSelect default true;
      property Context: TDataSet read GetDataSet write SetDataSet;
   end;

procedure Register;

implementation
uses
   SysUtils, Controls, Generics.Collections, DBClient{$IFNDEF COMPONENT},
   EbEdit, EbSelector, turbu_vartypes, turbu_constants, turbu_script_interface,
   turbu_database, uPSCompiler, array_editor{$ENDIF};

procedure Register;
begin
   RegisterComponents('TURBU', [TEBTreeView])
end;

{ TEBTreeView }

constructor TEBTreeView.Create(AOwner: TComponent);
begin
   inherited Create(AOwner);
   ReadOnly := true;
   ShowLines := false;
   RowSelect := true;
   FDatalink := TFieldDataLink.Create;
   FDatalink.DataSource := TDataSource.Create(self);
end;

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
         {$IFNDEF COMPONENT}
         if assigned(obj) then
            if EbEdit.EditEbObject(obj) and ((obj.ComponentCount > 0) or (node.HasChildren)) then
               self.ReconcilePostEdit(obj, node);
         {$ENDIF}
      end;
   end;
   inherited DblClick;
end;

destructor TEBTreeView.Destroy;
begin
   FreeAndNil(FDatalink);
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

function TEBTreeView.GetDataSet: TDataSet;
begin
   Result := FDataLink.DataSource.Dataset;
end;

procedure TEBTreeView.SetDataSet(const Value: TDataSet);
begin
{$IFNDEF COMPONENT}
   if assigned(FDatalink.DataSource) and (TfrmArrayEdit.VariableContext = FDatalink.DataSource.Dataset) then
      TfrmArrayEdit.VariableContext := nil;
   FDataLink.DataSource.Dataset := Value;
   if Value <> nil then
   begin
     Value.FreeNotification(Self);
     TfrmArrayEdit.VariableContext := Value;
   end;
{$ELSE}
   FDataLink.DataSource.dataset := Value;
   if Value <> nil then Value.FreeNotification(Self);
{$ENDIF}
end;

{$WARN CONSTRUCTING_ABSTRACT OFF}
procedure TEBTreeView.InsertObject;
{$IFNDEF COMPONENT}
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
{$ELSE}
begin
end;
{$ENDIF}
{$WARN CONSTRUCTING_ABSTRACT ON}

procedure TEBTreeView.SetProc(const Value: TEBRoutine);
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
      SetupContext;
   finally
      stack.free;
      dict.Free;
      tree.free;
   end;
end;

procedure TEBTreeView.AddVariable(context: TDataset; const name, vartype: string);
begin
   context.Append;
   context.FieldByName('name').AsString := name;
   context.FieldByName('DisplayName').AsString := name;
   context.FieldByName('type').AsString:= vartype;
   context.Post;
end;

procedure TEBTreeView.AddObjectContext(context: TDataset; const base: string; const name, vartype: string);
{$IFNDEF COMPONENT}
var
   objtype: TPSCompileTimeClass;
   prop: TPSDelphiClassItemProperty;
   i: integer;
begin
   objtype := (GScriptEngine.compiler.FindType(AnsiString(base)) as TPSClassType).CL;
   if objtype = nil then
      raise ERPGScriptError.CreateFmt('Variable type %s not found in script engine.', [VarType]);
   for i := 0 to objtype.Count do
      if objtype.Items[i] is TPSDelphiClassItemProperty then
      begin
         prop := TPSDelphiClassItemProperty(objtype.Items[i]);
         asm int 3 end; //figure out how to get type information once I encounter this
         prop.decl.ParamCount;
      end;
{$ELSE}
begin
{$ENDIF}
end;

procedure TEBTreeView.AddToContext(context: TDataset; const name, vartype: string);
var
   varInt: integer;
begin
{$IFNDEF COMPONENT}
   varInt := lookupType(VarType);
   if varInt = -1 then
      raise ERPGScriptError.CreateFmt('Unregistered variable type %s.', [VarType]);
   if varInt >= VT_OBJECT then
      AddObjectContext(context, '', name, vartype)
   else AddVariable(context, name, vartype);
{$ENDIF}
end;

procedure TEBTreeView.SetupContext;
var
   ctx: TClientDataset;
   i: integer;
   param: TEBParam;
   locals: TStringList;
begin
   if (self.Context = nil) then
      Exit;
   ctx := Context as TClientDataset;
   ctx.Active := false;
   ctx.CreateDataset;
   for param in proc.paramList do
      AddToContext(ctx, param.text, param.vartype);
   locals := proc.GetVarBlock;
   try
      for i := 0 to locals.count - 1 do
         AddToContext(ctx, locals.Names[i], locals.ValueFromIndex[i]);
   finally
      locals.free;
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

procedure TEBTreeView.Orphan(obj: TEbObject);
begin
   if not assigned(FOnOrphan) then
      obj.Free
   else FOnOrphan(self, obj);
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
      for subnode in nodelist do
         self.Orphan(TObject(subnode.Data) as TEbObject);
   finally
      nodelist.free;
      Items.EndUpdate;
   end;
end;

end.
