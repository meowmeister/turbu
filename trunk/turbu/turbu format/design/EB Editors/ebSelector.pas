unit EbSelector;

interface

uses
   Controls, Forms, ComCtrls, StdCtrls, Classes, ExtCtrls,
   DeHL.Collections.DoubleSortedMultiMap, DeHL.Types,
   EbEdit;

type
   TTreeData = record //because generic KV pairs are broken
      key: EditorCategoryAttribute;
      value: TEbEditorClass;
      constructor Create(k: EditorCategoryAttribute; v: TEbEditorClass);
   end;

   TTreeDataType = class(TType<TTreeData>)
   public
      { Comparator }
      function Compare(const AValue1, AValue2: TTreeData): Integer; override;
   end;

   TfrmEBSelector = class(TfrmEbEditBase)
      trvList: TTreeView;
      procedure FormCreate(Sender: TObject);
      procedure trvListClick(Sender: TObject);
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
   Math,
   DeHL.Collections.Base, DeHL.KeyValuePair,
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

class constructor TfrmEBSelector.Create;
begin
   TType<TTreeData>.Register(TTreeDataType);
   FMap := TTreeTemplate.Create(TStringType.Default, TTreeDataType.Default);
end;

class destructor TfrmEBSelector.Destroy;
begin
   FMap.free;
   TTreeDataType.UnRegister;
end;

procedure TfrmEBSelector.FormCreate(Sender: TObject);
var
   category: TTreeNode;
   pair: TKeyValuePair<string, IList<TTreeData>>;
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

function TfrmEBSelector.GetCurrent: TEbEditorClass;
begin
   result := trvList.selected.Data;
   if assigned(result) then
      assert(TClass(result).InheritsFrom(TfrmEBEditBase));
end;

{ TTreeDataType }

function TTreeDataType.Compare(const AValue1, AValue2: TTreeData): Integer;
begin
   result := AValue1.Key.order - AValue2.Key.order;
end;

{ TTreeData }

constructor TTreeData.Create(k: EditorCategoryAttribute; v: TEbEditorClass);
begin
   key := k;
   value := v;
end;

end.