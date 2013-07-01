unit dbCheckListBox;

interface
uses
   Classes, Messages, Controls, CheckLst, DB, DBCtrls;

type
   TdbCheckListBox = class(TCheckListBox)
   private
      FDataLink: TFieldDataLink;
      procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
      function GetField: TField;
      function GetDataField: WideString;
      function GetDataSource: TDataSource;
      procedure SetDataField(const Value: WideString);
      procedure SetDataSource(const Value: TDataSource);
      procedure DataChange(Sender: TObject);
      procedure UpdateData(Sender: TObject);
   private
      FSettingEdit: boolean;
      function GetMask: integer;
   protected
      procedure Notification(AComponent: TComponent; Operation: TOperation); override;
      procedure ClickCheck; override;
   public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      function ExecuteAction(Action: TBasicAction): Boolean; override;
      function UpdateAction(Action: TBasicAction): Boolean; override;
      property Field: TField read GetField;
   published
      property DataField: WideString read GetDataField write SetDataField;
      property DataSource: TDataSource read GetDataSource write SetDataSource;
   end;

implementation
uses
   StdCtrls;

{ TdbCheckListBox }

constructor TdbCheckListBox.Create(AOwner: TComponent);
begin
   inherited Create(AOwner);
   FDataLink := TFieldDataLink.Create;
   FDataLink.Control := self;
   FDataLink.OnDataChange := DataChange;
   FDataLink.OnUpdateData := UpdateData;
   FDataLink.OnActiveChange := DataChange;
end;

procedure TdbCheckListBox.DataChange(Sender: TObject);
var
   mask: integer;
   maskSet: set of 0..31 absolute mask;
   i: integer;
begin
   if FSettingEdit then
      Exit;
   if assigned(FDataLink.Field) and (FDatalink.Field is TNumericField) then
   begin
      mask := FDatalink.Field.AsInteger;
      for i := 0 to self.Count - 1 do
         self.Checked[i] := i in maskSet;
   end
   else begin
      self.CheckAll(cbUnchecked, false, false);
   end;
   self.ClearSelection;
end;

destructor TdbCheckListBox.Destroy;
begin
   FDataLink.Free;
   inherited Destroy;
end;

procedure TdbCheckListBox.ClickCheck;
begin
   FSettingEdit := true;
   try
      FDatalink.Edit;
      FDataLink.Modified;
   finally
      FSettingEdit := false;
   end;
   inherited ClickCheck;
end;

procedure TdbCheckListBox.CMGetDataLink(var Message: TMessage);
begin
   Message.Result := Integer(FDataLink);
end;

function TdbCheckListBox.GetDataField: WideString;
begin
   Result := FDataLink.FieldName;
end;

procedure TdbCheckListBox.SetDataField(const Value: WideString);
begin
   FDataLink.FieldName := Value;
end;

function TdbCheckListBox.GetDataSource: TDataSource;
begin
   Result := FDataLink.DataSource;
end;

procedure TdbCheckListBox.SetDataSource(const Value: TDataSource);
begin
   FDataLink.DataSource := Value;
   if Value <> nil then Value.FreeNotification(Self);
end;

function TdbCheckListBox.GetField: TField;
begin
   Result := FDataLink.Field;
end;

function TdbCheckListBox.GetMask: integer;
var
   mask: set of 0..31 absolute result;
   i: integer;
begin
   mask := [];
   for i := 0 to self.Count - 1 do
      if self.Checked[i] then
         include(mask, i);
end;

procedure TdbCheckListBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
   inherited Notification(AComponent, Operation);
   if (Operation = opRemove) and (FDataLink <> nil) and
      (AComponent = DataSource) then DataSource := nil;
end;

function TdbCheckListBox.ExecuteAction(Action: TBasicAction): Boolean;
begin
   Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
      FDataLink.ExecuteAction(Action);
end;

function TdbCheckListBox.UpdateAction(Action: TBasicAction): Boolean;
begin
   Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
      FDataLink.UpdateAction(Action);
end;

procedure TdbCheckListBox.UpdateData(Sender: TObject);
begin
   FDataLink.Field.AsInteger := GetMask;
end;

end.
