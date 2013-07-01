unit DBTabControl;

interface

uses
  Classes, Windows, SysUtils, Messages, Controls, ComCtrls, DB, DBCtrls;

type
  TCustomDBTabControl = class(TCustomTabControl)
  private
    FDataLink: TFieldDataLink;
    FPrevTabIndex: Integer;
    FShowInsertTab: Boolean;
    procedure ActiveChanged(Sender: TObject);
    procedure DataChanged(Sender: TObject);
    function GetDataField: String;
    function GetDataSource: TDataSource;
    function GetField: TField;
    procedure RebuildTabs;
    procedure SetDataField(const Value: String);
    procedure SetDataSource(Value: TDataSource);
    procedure SetShowInsertTab(Value: Boolean);
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    function CanChange: Boolean; override;
    procedure Change; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    procedure Loaded; override;
    property DataField: String read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property Field: TField read GetField;
    property ShowInsertTab: Boolean read FShowInsertTab write SetShowInsertTab
      default False;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
  end;

  TDBTabControl = class(TCustomDBTabControl)
  public
    property DisplayRect;
    property Field;
  published
    property Align;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DockSite;
    property DataField;
    property DataSource;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HotTrack;
    property Images;
    property MultiLine;
    property OwnerDraw;
    property ParentBiDiMode;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property RaggedRight;
    property ScrollOpposite;
    property ShowHint;
    property ShowInsertTab;
    property Style;
    property TabHeight;
    property TabOrder;
    property TabPosition;
    property TabStop;
    property TabWidth;
    property Visible;
    property OnChange;
    property OnChanging;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawTab;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetImageIndex;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

implementation

{ TCustomDBTabControl }

procedure TCustomDBTabControl.ActiveChanged(Sender: TObject);
begin
  RebuildTabs;
end;

function TCustomDBTabControl.CanChange: Boolean;
begin
  FPrevTabIndex := TabIndex;
  Result := (inherited CanChange) and (DataSource <> nil) and
    (DataSource.State in [dsBrowse, dsEdit, dsInsert]);
end;

procedure TCustomDBTabControl.Change;
begin
  try
    if DataSource <> nil then
    begin
      if FShowInsertTab and (TabIndex = Tabs.Count - 1) then
        DataSource.DataSet.Append
      else if DataSource.State = dsInsert then
        DataSource.DataSet.CheckBrowseMode
      else
        DataSource.DataSet.MoveBy(TabIndex - FPrevTabIndex);
    end;
    inherited Change;
  except
    TabIndex := FPrevTabIndex;
    raise;
  end;
end;

procedure TCustomDBTabControl.CMExit(var Message: TCMExit);
begin
  try
    FDataLink.UpdateRecord;
  except
    SetFocus;
    raise;
  end;
  inherited;
end;

procedure TCustomDBTabControl.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(FDataLink);
end;

constructor TCustomDBTabControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnActiveChange := ActiveChanged;
  FDataLink.OnDataChange := DataChanged;
end;

procedure TCustomDBTabControl.DataChanged(Sender: TObject);
const
  StarCount: array[Boolean] of Integer = (0, 1);
var
  NewTabIndex: Integer;
begin
  if (DataSource <> nil) and (FDataLink.Active) then
    with DataSource do
    begin
      if DataSet.RecordCount <> Tabs.Count - StarCount[FShowInsertTab] then
        RebuildTabs;
      if (State = dsInsert) and FShowInsertTab then
        TabIndex := Tabs.Count - 1
      else if Tabs.Count > 0 then
      begin
        NewTabIndex := Tabs.IndexOfObject(TObject(DataSet.RecNo));
        if (TabIndex = NewTabIndex) and not (State = dsInsert) and
            (Field <> nil) and (Field.AsString <> Tabs[TabIndex]) then
          Tabs[TabIndex] := Field.AsString;
        TabIndex := NewTabIndex;
      end;
    end;
end;

destructor TCustomDBTabControl.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

function TCustomDBTabControl.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or FDataLink.ExecuteAction(Action);
end;

function TCustomDBTabControl.GetDataField: String;
begin
  Result := FDataLink.FieldName;
end;

function TCustomDBTabControl.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TCustomDBTabControl.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TCustomDBTabControl.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (DataSource <> nil) and (DataSource.State = dsInsert) and
    (Key = VK_ESCAPE) then
  begin
    DataSource.DataSet.Cancel;
    Change;
  end;
  inherited keyDown(Key, Shift);
end;

procedure TCustomDBTabControl.Loaded;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then
    RebuildTabs;
end;

procedure TCustomDBTabControl.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
      (AComponent = DataSource) then
    DataSource := nil;
end;

procedure TCustomDBTabControl.RebuildTabs;
var
  ar, i, rc: integer;
begin
//  Exit;
  if (DataSource <> nil) and (DataSource.State = dsBrowse) then
  begin
    Tabs.BeginUpdate;
    ar := FDataLink.ActiveRecord;
    try
      Tabs.Clear;
      rc := DataSource.dataset.RecordCount;
      if dataSource.DataSet.State = dsInsert then
         dec(rc);
      FDataLink.BufferCount := rc;
      for i := 0 to rc - 1 do
      begin
        FDatalink.ActiveRecord := i;
        if (Field = nil) or Field.IsNull then
          Tabs.AddObject(IntToStr(DataSource.DataSet.RecNo), TObject(DataSource.DataSet.RecNo))
        else
          Tabs.AddObject(Field.AsString, TObject(DataSource.DataSet.RecNo));
      end;
      if FShowInsertTab then
        Tabs.AddObject('*', TObject(-1));
    finally
      FDataLink.ActiveRecord := ar;
      Tabs.EndUpdate;
    end;
  end
  else
    Tabs.Clear;
end;

procedure TCustomDBTabControl.SetDataField(const Value: String);
begin
  FDataLink.FieldName := Value;
  RebuildTabs;
end;

procedure TCustomDBTabControl.SetDataSource(Value: TDataSource);
begin
  FDataLink.DataSource := Value;
  if DataSource <> nil then
    DataSource.FreeNotification(Self);
  if not (csLoading in ComponentState) then
    RebuildTabs;
end;

procedure TCustomDBTabControl.SetShowInsertTab(Value: Boolean);
begin
  if FShowInsertTab <> Value then
  begin
    FShowInsertTab := Value;
    RebuildTabs;
  end;
end;

function TCustomDBTabControl.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or FDataLink.UpdateAction(Action);
end;

end.

