unit DBIndexComboBox;

interface
uses
   StdCtrls, DBCtrls, Classes, RTTI;

type
   TDBIndexComboBox = class(TDBComboBox)
   private
      class var
         FContext: TRttiContext;
         FDatalinkField: TRttiField;
      class constructor Create;
   protected
      class destructor Destroy; //FIXME: This shouldn't have to be protected
   private
      FDataLink: TFieldDataLink;
      procedure SetComboText(const Value: string);
      function GetComboText: string;

      procedure UpdateData(Sender: TObject);
      procedure DataChange(Sender: TObject);
   public
      constructor Create(AOwner: TComponent); override;
   published
      property Style default csDropDownList;
   end;

procedure Register;

implementation
uses
   DB, Messages, Windows;

procedure Register;
begin
   RegisterComponents('TURBU', [TDBIndexComboBox]);
end;

{ TDBIndexComboBox }

constructor TDBIndexComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FDataLink := FDatalinkField.GetValue(self).AsObject as TFieldDataLink;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnUpdateData := UpdateData;
  self.Style := csDropDownList;
end;

class constructor TDBIndexComboBox.Create;
var
   classType: TRttiType;
begin
  FContext := TRttiContext.Create;
  classType := FContext.GetType(ClassParent.ClassInfo);
  assert(assigned(classType));
  FDatalinkField := classType.GetField('FDataLink');

  assert(assigned(FDatalinkField));
end;

class destructor TDBIndexComboBox.Destroy;
begin
   FContext.Free;
end;

function TDBIndexComboBox.GetComboText: string;
var
  I: Integer;
begin
  if Style in [csDropDown, csSimple] then Result := Text else
  begin
    I := ItemIndex;
    if I < 0 then Result := '' else Result := Items[I];
  end;
end;

procedure TDBIndexComboBox.SetComboText(const Value: string);
var
  I: Integer;
  Redraw: Boolean;
begin
  if Value <> GetComboText then
  begin
    if Style <> csDropDown then
    begin
      Redraw := (Style <> csSimple) and HandleAllocated;
      if Redraw then SendMessage(Handle, WM_SETREDRAW, 0, 0);
      try
        if Value = '' then I := -1 else I := Items.IndexOf(Value);
        ItemIndex := I;
      finally
        if Redraw then
        begin
          SendMessage(Handle, WM_SETREDRAW, 1, 0);
          Invalidate;
        end;
      end;
      if I >= 0 then Exit;
    end;
    if Style in [csDropDown, csSimple] then Text := Value;
  end;
end;

procedure TDBIndexComboBox.DataChange(Sender: TObject);
begin
  if not (Style = csSimple) and DroppedDown then Exit;
  if (FDataLink.Field <> nil) and (FDataLink.Field is TNumericField) then
    self.ItemIndex := FDataLink.Field.AsInteger
  else
    if csDesigning in ComponentState then
      SetComboText(Name)
    else
      SetComboText('');
end;

procedure TDBIndexComboBox.UpdateData(Sender: TObject);
begin
  if FDatalink.Editing then
     FDataLink.Field.AsInteger := self.ItemIndex;
end;

end.
