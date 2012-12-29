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
   Controls, Forms, ComCtrls, StdCtrls, Classes, ExtCtrls, Generics.Collections,
   turbu_MultiMaps, EbEdit;

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
      procedure FormDestroy(Sender: TObject);
   private
      type
         TTreeTemplate = class(TMultiMap<string, TTreeData>);
      class var
         FMap: TDictionary<string, TTreeTemplate>;
         FMapCount: integer;
      var
         FCategories: TStringList;
         FCatStr: string;
         FSuffix: string;
      procedure BuildMap;
      function GetCurrent: TEbEditorClass;
      procedure AddEditor(cls: TEBEditorClass; lMap: TTreeTemplate);
   protected
      class constructor Create;
      class destructor Destroy;
   public
      constructor Create(const cat, suffix: string); reintroduce;
      property Current: TEbEditorClass read GetCurrent;
   end;

implementation
uses
Windows,
   Math, SysUtils, Generics.Defaults,
   EventBuilder, turbu_functional;

{$R *.dfm}

{ TfrmEBSelector }

procedure TfrmEBSelector.AddEditor(cls: TEBEditorClass; lMap: TTreeTemplate);
var
   cat: EditorCategoryAttribute;
   ctx: EditorContextAttribute;
begin
   ctx := cls.Context;
   if ctx = nil then
   begin
      OutputDebugString(PChar(Format('No context for editor class %s', [cls.ClassName])));
      Exit;
   end
   else if (FCategories.IndexOf(ctx.name) = -1) then
      Exit;
   cat := cls.Category;
   if assigned(cat) then
   begin
      if (not lMap.ContainsKey(cat.category)) or
         (TFunctional.countWhere<TTreeData>
            (lMap[cat.category],
            function(data: TTreeData): boolean
            begin result := data.value = cls; end) = 0) then
            lMap.Add(cat.category, TTreeData.Create(cat, cls))
   end;
end;

procedure TfrmEBSelector.BuildMap;
var
   cls: TEbEditorClass;
   catStr: string;
   lMap: TTreeTemplate;
begin
   catStr := FCategories.CommaText;
   FMap.TryGetValue(catStr, lMap);
   if (assigned(lMap)) and (AllEditors.Count = FMapCount) then
      Exit;
   if lMap = nil then
   begin
      lMap := TTreeTemplate.Create;
      FMap.Add(catStr, lMap)
   end
   else FMap.Clear;
   for cls in AllEditors.Values do
      addEditor(cls, lMap);
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
   FMap := TObjectDictionary<string, TTreeTemplate>.Create([doOwnsValues]);
end;

constructor TfrmEBSelector.Create(const cat, suffix: string);
var
   i: integer;
begin
   FCategories := TStringList.Create;
   FCategories.StrictDelimiter := true;
   FCategories.CommaText := cat;
   FCategories.Add('Universal');
   if suffix <> '' then
      for i := 0 to FCategories.count - 1 do
         FCategories[i] := FCategories[i] + suffix;
   FCatStr := cat;
   FSuffix := suffix;
   inherited Create(nil);
end;

class destructor TfrmEBSelector.Destroy;
begin
   FMap.free;
   treeData.Free;
end;

procedure TfrmEBSelector.FormCreate(Sender: TObject);
var
   category: TTreeNode;
   data: TTreeData;
   keys: TArray<string>;
   key: string;
   list: TArray<TTreeData>;
   lMap: TTreeTemplate;
begin
   inherited;
   BuildMap;
   lMap := FMap[FCategories.CommaText];
   keys := lMap.Keys.ToArray;
   TArray.Sort<string>(keys);
   for key in keys do
   begin
      category := trvList.Items.AddChild(nil, key);
      list := lMap[key].ToArray;
      TArray.Sort<TTreeData>(list, TreeData);
      for data in list do
         trvList.Items.AddChildObject(category, data.key.name, data.Value);
   end;
end;

procedure TfrmEBSelector.FormDestroy(Sender: TObject);
begin
   FCategories.Free;
   inherited;
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
