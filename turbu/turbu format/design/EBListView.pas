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

unit EBListView;

interface
uses
   ComCtrls, Messages, Classes, DB, DBCtrls, Generics.Collections, DBClient,
   EventBuilder{$IFNDEF COMPONENT}, turbu_map_interface{$ENDIF};

type
   TOrphanEvent = procedure(sender: TObject; obj: TEbObject) of object;
   TIsObjectEvent = function(value: integer): boolean of object;

   TEBTreeView = class(TTreeView)
   private
      FProc: TEBRoutine;
      FDblClickExpand: boolean;
      FOnOrphan: TOrphanEvent;
      FOnIsObjectType: TIsObjectEvent;
      FDataset: TCustomClientDataset;
      FGlobals: TCustomClientDataset;
      {$IFNDEF COMPONENT}
      FMap: IRpgMap;
      FMapObject: IRpgMapObject;
      {$ENDIF}
      FReadOnly: boolean;
      procedure SetProc(const Value: TEBRoutine);
      procedure EndLoading;
      procedure ReconcilePostEdit(obj: TEbObject; node: TTreeNode; list: TList<TEBObject>);
      procedure InsertObject;
      procedure Orphan(obj: TEbObject);
      procedure SetupContext;
      procedure AddObjectContext(context: TCustomClientDataSet; const base: string; const name, vartype: string);
      procedure AddVariable(context: TCustomClientDataSet; const name, vartype: string);
      procedure AddToContext(context: TCustomClientDataSet; const name, vartype: string); overload;
      procedure AddToContext(context: TCustomClientDataSet; const decl: string); overload;
      function GetEBChildren(obj: TEBObject): TList<TEBObject>;
      function ValidateList(obj: TEbObject; list: TList<TEBObject>): boolean;
      function BuildTree(base: TEBObject; parent: TTreeNode; isNew: boolean): TTreeNode;
      function IsObjectType(value: integer): boolean;
   private
      function GetDataSet: TCustomClientDataSet;
      procedure SetDataSet(const Value: TCustomClientDataSet);
      procedure insertNewObject(obj: TEBObject);
   private
      FCategory: string;
      FSuffix: string;
      procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
      procedure WMChar(var Message: TWMChar); message WM_CHAR;
      procedure WMGetDlgCode(var msg: TMessage); message WM_GETDLGCODE;
   protected
      FDeleteLock: boolean;
      procedure DblClick; override;
      procedure KeyDown(var Key: Word; Shift: TShiftState); override;
      procedure KeyUp(var Key: Word; Shift: TShiftState); override;
      procedure Notification(AComponent: TComponent; Operation: TOperation); override;
   public
      constructor Create(AOwner: TComponent); override;
      property proc: TEBRoutine read FProc write SetProc;
      {$IFNDEF COMPONENT}
      property map: IRpgMap read FMap write FMap;
      property mapObj: IRpgMapObject read FMapObject write FMapObject;
      {$ENDIF}
   published
      property DoubleClickExpand: boolean read FDblClickExpand write FDblClickExpand stored FDblClickExpand;
      property OnOrphan: TOrphanEvent read FOnOrphan write FOnOrphan;
      property OnIsObjectType: TIsObjectEvent read FOnIsObjectType write FOnIsObjectType;
      property ReadOnly: boolean read FReadOnly write FReadOnly default false;
      property ShowLines default false;
      property RowSelect default true;
      property Context: TCustomClientDataset read GetDataSet write SetDataSet;
      property Globals: TCustomClientDataset read FGlobals write FGlobals;
      property EditorCategory: string read FCategory write FCategory;
      property EditorSuffix: string read FSuffix write FSuffix;
   end;

procedure Register;

implementation
uses
   Windows, SysUtils, Controls, Forms {$IFNDEF COMPONENT},
   EbEdit, EbSelector, turbu_vartypes, EB_RpgScript,
   array_editor{$ENDIF};

procedure Register;
begin
   RegisterComponents('TURBU', [TEBTreeView])
end;

{ TEBTreeView }

constructor TEBTreeView.Create(AOwner: TComponent);
begin
   inherited Create(AOwner);
   inherited ReadOnly := true;
   ShowLines := false;
   RowSelect := true;
end;

function TEBTreeView.GetEBChildren(obj: TEBObject): TList<TEBObject>;
var
   subObj: TEBObject;
begin
   if obj.ChildCount = 0 then
      Exit(nil);
   result := TList<TEBObject>.Create;
   for subobj in obj do
      if (FSuffix <> '') or not (subObj is TEBExpression) then
         result.add(subObj);
   if result.Count = 0 then
      FreeAndNil(result);
end;

procedure TEBTreeView.DblClick;
var
   node: TTreeNode;
   obj: TEbObject;
   list: TList<TEBObject>;
begin
   if self.ReadOnly then
      Exit;
   node := self.Selected;
   if assigned(node) then
   begin
      if node.Text = '<>' then
         InsertObject
      else begin
         obj := TObject(node.Data) as TEbObject;
         {$IFNDEF COMPONENT}
         if assigned(obj) then
         begin
            if node.HasChildren then
               list := GetEBChildren(obj)
            else list := nil;
            try
               if EbEdit.EditEbObject(obj, map, mapObj, FCategory, FSuffix, self.Context, FGlobals) and
                 ((obj.ChildCount > 0) or (node.HasChildren) or assigned(list)) then
               begin
                  if assigned(node.Parent) and (node.Parent.Data = obj) then
                     node := node.Parent;
                  self.ReconcilePostEdit(obj, node, list)
               end
               else node.Text := obj.GetNodeText;
            finally
               list.Free;
            end;
         end;
         {$ENDIF}
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

function TEBTreeView.GetDataSet: TCustomClientDataSet;
begin
   Result := FDataset;
end;

procedure TEBTreeView.SetDataSet(const Value: TCustomClientDataSet);
begin
   FDataset := Value;
   if Value <> nil then
      Value.FreeNotification(Self);
end;

procedure TEBTreeView.insertNewObject(obj: TEBObject);
var
   parent, current: TTreeNode;
   index: integer;
   parentObj: TEBObject;
begin
{$IFNDEF COMPONENT}
   parent := self.Selected.Parent;
   current := BuildTree(obj, parent, false);
   current.MoveTo(self.Selected, naInsert);
   current.Expand(true);
   if assigned(parent) then
      parentObj := TObject(parent.Data) as TEBObject
   else parentObj := FProc;
   if assigned(selected.Data) then
   begin
      index := parentObj.children.IndexOf(TObject(selected.Data) as TEBObject);
      if TObject(selected.Data).ClassType = TEBCodeBlock then
      begin
         parentObj := selected.data;
         index := parentObj.ChildCount;
      end
      else if index = -1 then
         index := parentObj.ChildCount;
   end
   else begin
      index := 0;
      parentObj := parent.data;
   end;
   parentObj.Children.Insert(index, obj);
   SetupContext;
{$ENDIF}
end;

procedure TEBTreeView.InsertObject;
{$IFNDEF COMPONENT}
var
   obj: TEbObject;
begin
   obj := TfrmEBSelector.Select(FCategory, FSuffix, FMap, FMapObject, FDataset, FGlobals);
   if assigned(obj) then
      InsertNewObject(obj);
end;

{$ELSE}
begin
end;
{$ENDIF}

function TEBTreeView.IsObjectType(value: integer): boolean;
begin
   result := assigned(FOnIsObjectType) and FOnIsObjectType(value);
end;

procedure TEBTreeView.KeyDown(var Key: Word; Shift: TShiftState);
var
   node: TTreeNode;
   obj, sub: TEBObject;
   list: TList<TEBObject>;
begin
   if (key = VK_DELETE) and (shift = []) then
   begin
      if not FDeleteLock then
      begin
         FDeleteLock := true;
         node := self.Selected;
         if assigned(node) and (node.Text <> '') and (node.Text <> '<>')then
         begin
            obj := TObject(node.Data) as TEbObject;
            list := GetEBChildren(obj);
            if assigned(list) then
               try
                  for sub in list do
                     Orphan(sub);
               finally
                  list.Free;
               end;
            obj.Free;
            node.Delete;
         end;
      end;
      key := 0;
   end
   else if (key = VK_INSERT) and (shift = []) then
      InsertObject
   else inherited KeyDown(key, shift);
end;

procedure TEBTreeView.KeyUp(var Key: Word; Shift: TShiftState);
begin
   if (key = VK_DELETE) and (shift = []) then
   begin
      FDeleteLock := false;
      key := 0;
   end
   else inherited KeyUp(key, shift);
end;

procedure TEBTreeView.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
   if (Operation = opRemove) and ((AComponent = FGlobals) or (AComponent = FDataset)) then
   begin
      if AComponent = FGlobals then
         FGlobals := nil
      else FDataset := nil;
   end
   else inherited Notification(AComponent, operation);
end;

function TEBTreeView.BuildTree(base: TEBObject; parent: TTreeNode; isNew: boolean): TTreeNode;
var
   node, last: TEBNodeData;
   tree: TEBNode;
   stack: TStack<TEBNodeData>;
   dict: TDictionary<TEBNodeData, TTreeNode>;
begin
   tree := base.GetNode;
   stack := TStack<TEBNodeData>.Create;
   dict := TDictionary<TEBNodeData, TTreeNode>.Create;
   Items.BeginUpdate;
   try
      if isNew then
         last := tree.Data
      else last := tree.Data.parent;
      stack.push(last);
      dict.add(last, parent);
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
      if IsNew then
         SetupContext;
      result := dict[tree.Data];
   finally
      items.EndUpdate;
      stack.free;
      dict.Free;
      tree.free;
   end;
   if not isNew then
      assert(assigned(result));
end;

procedure TEBTreeView.SetProc(const Value: TEBRoutine);
begin
   //prevent a corner case where saved data is restored from FMemStream,
   //invalidating the Items.Clear call
   self.HandleNeeded;
   Items.Clear;
   FProc := Value;
   BuildTree(value, nil, true);
end;

procedure TEBTreeView.AddVariable(context: TCustomClientDataSet; const name, vartype: string);
begin
   context.Append;
   context.FieldByName('Name').AsString := name;
   context.FieldByName('type').AsString:= vartype;
   context.Post;
end;

procedure TEBTreeView.AddObjectContext(context: TCustomClientDataSet; const base: string; const name, vartype: string);
{$IFNDEF COMPONENT}
{var
   objtype: TPSCompileTimeClass;
   prop: TPSDelphiClassItemProperty;
   i: integer;}
begin
{   objtype := (GScriptEngine.compiler.FindType(AnsiString(base)) as TPSClassType).CL;
   if objtype = nil then
      raise ERPGScriptError.CreateFmt('Variable type %s not found in script engine.', [VarType]);
   for i := 0 to objtype.Count do
      if objtype.Items[i] is TPSDelphiClassItemProperty then
      begin
         prop := TPSDelphiClassItemProperty(objtype.Items[i]);
         asm int 3 end; //figure out how to get type information once I encounter this
         prop.decl.ParamCount;
      end; }
{$ELSE}
begin
{$ENDIF}
end;

procedure TEBTreeView.AddToContext(context: TCustomClientDataSet; const name,
  vartype: string);
var
   varInt: integer;
begin
{$IFNDEF COMPONENT}
   varInt := lookupType(VarType);
   if varInt = -1 then
      raise ERPGScriptError.CreateFmt('Unregistered variable type %s.', [VarType]);
   if IsObjectType(varInt) then
      AddObjectContext(context, '', name, vartype)
   else AddVariable(context, name, vartype);
{$ENDIF}
end;

procedure TEBTreeView.AddToContext(context: TCustomClientDataSet; const decl: string);
var
   name, vartype: string;
   colonPos: integer;
begin
   colonPos := pos(':', decl);
   name := trim(copy(decl, 1, colonPos - 1));
   vartype := trim(copy(decl, colonPos + 1));
   if vartype[length(vartype)] = ';' then
      SetLength(vartype, length(vartype) - 1);

   AddToContext(context, name, vartype);
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
   for i := 0 to locals.count - 1 do
      AddToContext(ctx, locals[i]);
end;

procedure TEBTreeView.WMChar(var Message: TWMChar);
begin
   case message.CharCode of
      VK_RETURN: self.DblClick;
      VK_SPACE, VK_INSERT: self.InsertObject;
      else inherited;
   end;
end;

procedure TEBTreeView.WMGetDlgCode(var msg: TMessage);
begin
   msg.Result := msg.Result or DLGC_WANTALLKEYS or DLGC_WANTARROWS;
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

function TEBTreeView.ValidateList(obj: TEbObject; list: TList<TEBObject>): boolean;
var
   i, j: integer;
begin
   result := true;
   j := 0;
   for i := 0 to obj.children.Count - 1 do
   begin
      if obj.children[i] is TEBExpression then
         Continue;
      if (j >= list.Count) or (list[j] <> obj.children[i]) then
      begin
         result := false;
         break;
      end;
      inc(j);
   end;
   result := result and (j = list.count);
end;

procedure TEBTreeView.ReconcilePostEdit(obj: TEbObject; node: TTreeNode; list: TList<TEBObject>);
var
   subObj: TEbObject;
   subnode, newnode: TTreeNode;
   valid: boolean;
begin
   Items.BeginUpdate;
   try
      if assigned(list) then
         valid := validateList(obj, list)
      else valid := false;
      if valid then
      begin
         subnode := node.getFirstChild;
         while assigned(subnode) do
         begin
            if subnode.Data <> obj then
               subnode.Text := (TObject(subnode.Data) as TEBObject).GetNodeText;
            subnode := node.GetNextChild(subnode);
         end;
         node.Text := obj.GetNodeText;
      end
      else begin
         if assigned(list) then
            for subObj in list do
               if not obj.children.Contains(subObj) then
                  Orphan(subObj);
         newnode := BuildTree(obj, node.Parent, false);
         newnode.MoveTo(node, naInsert);
         newnode.Expand(true);
         node.Free;
         SetupContext;
      end;
   finally
      Items.EndUpdate;
   end;
end;

end.
