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

unit EbSelector;

interface

uses
   Controls, Forms, ComCtrls, StdCtrls, Classes, ExtCtrls,
   Collections.Base, Collections.MultiMaps,
   EbEdit;

type
   TTreeData = record //because generic KV pairs are broken
      key: EditorCategoryAttribute;
      value: TEbEditorClass;
      constructor Create(k: EditorCategoryAttribute; v: TEbEditorClass);
   end;

   TfrmEBSelector = class(TfrmEbEditBase)
      trvList: TTreeView;
      procedure FormCreate(Sender: TObject);
      procedure trvListClick(Sender: TObject);
    procedure trvListDblClick(Sender: TObject);
   private
      type
         TTreeTemplate = class(TDoubleSortedMultiMap<string, TTreeData>);
      class var
         FMap: TTreeTemplate;
         FMapCount: integer;
      procedure BuildMap;
      function GetCurrent: TEbEditorClass;
   protected
      class constructor Create;
      class destructor Destroy;
   public
      property Current: TEbEditorClass read GetCurrent;
   end;

implementation
uses
   Math, SysUtils, Generics.Defaults, Generics.Collections,
   EventBuilder;

{$R *.dfm}

{ TfrmEBSelector }

procedure TfrmEBSelector.BuildMap;
var
   cls: TEbEditorClass;
   cat: EditorCategoryAttribute;
begin
   if AllEditors.Count = FMapCount then
      Exit;
   FMap.Clear;
   for cls in AllEditors.Values do
   begin
      cat := cls.Category;
      if assigned(cat) then
      begin
         if (not FMap.ContainsKey(cat.category)) or
            (FMap[cat.category].Where(
            function(data: TTreeData): boolean
            begin
               result := data.value = cls;
            end).Count = 0) then
            FMap.Add(cat.category, TTreeData.Create(cat, cls))
      end;
   end;
   FMapCount := AllEditors.Count;
end;

type
   TTreeDataType = class(TCustomComparer<TTreeData>)
      function Compare(const Left, Right: TTreeData): Integer; override;
      function Equals(const Left, Right: TTreeData): Boolean; overload; override;
      function GetHashCode(const Value: TTreeData): Integer; overload; override;
   end;

function TTreeDataType.Compare(const Left, Right: TTreeData): Integer;
begin
   result := StrComp(PChar(Left.Key.name), PChar(Right.Key.name));
end;

function TTreeDataType.Equals(const Left, Right: TTreeData): Boolean;
begin
  result := (left.key = right.key) and (left.value = right.value);
end;

function TTreeDataType.GetHashCode(const Value: TTreeData): Integer;
begin
  result := BobJenkinsHash(Value, SizeOf(Value), 0);
end;

var
   treeData: TTreeDataType;

class constructor TfrmEBSelector.Create;
begin
   treeData := TTreeDataType.Create;
   FMap := TTreeTemplate.Create(TRules<string>.Default,
      TRules<TTreeData>.Custom(treeData));
end;

class destructor TfrmEBSelector.Destroy;
begin
   FMap.free;
   treeData.Free;
end;

procedure TfrmEBSelector.FormCreate(Sender: TObject);
var
   category: TTreeNode;
   pair: TPair<string, IOperableCollection<TTreeData>>;
   data: TTreeData;
begin
   inherited;
   BuildMap;
   for pair in FMap.Dictionary do
   begin
      category := trvList.Items.AddChild(nil, pair.key);
      for data in pair.Value do
         trvList.Items.AddChildObject(category, data.key.name, data.Value);
   end;
end;

procedure TfrmEBSelector.trvListClick(Sender: TObject);
begin
   btnOK.Enabled := assigned(trvList.selected) and assigned(trvList.selected.Data);
end;

procedure TfrmEBSelector.trvListDblClick(Sender: TObject);
begin
   if GetCurrent <> nil then
      btnOK.Click;
end;

function TfrmEBSelector.GetCurrent: TEbEditorClass;
begin
   result := trvList.selected.Data;
   if assigned(result) then
      assert(TClass(result).InheritsFrom(TfrmEBEditBase));
end;

{ TTreeData }

constructor TTreeData.Create(k: EditorCategoryAttribute; v: TEbEditorClass);
begin
   key := k;
   value := v;
end;

end.
