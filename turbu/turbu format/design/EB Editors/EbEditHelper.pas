unit EbEditHelper;

interface
uses
   StdCtrls,
   JvSpin,
   EBEdit, variable_selector, EventBuilder, IDLookupCombo;

   procedure UploadValuePtrSelection(v1, v2: integer; r1, r2: TRadioButton;
     valueBox: TJvSpinEdit; ptrBox: TIntSelector); overload;
   procedure UploadValuePtrSelection(expr: TEBExpression; r1, r2: TRadioButton;
     valueBox: TJvSpinEdit; ptrBox: TIntSelector); overload;
   procedure UploadLookupPtrSelection(expr: TEBExpression; r1, r2: TRadioButton;
     valueBox: TIDLookupCombo; ptrBox: TIntSelector); overload;
   procedure UploadLookupPtrSelection(v1, v2: integer; r1, r2: TRadioButton;
     valueBox: TIDLookupCombo; ptrBox: TIntSelector); overload;
   function DownloadValuePtrSelection(r1, r2: TRadioButton;
     valueBox: TJvSpinEdit; ptrBox: TIntSelector): TIntPair;
   function DownloadLookupPtrSelection(r1, r2: TRadioButton;
     valueBox: TIDLookupCombo; ptrBox: TIntSelector; const lookupName: string): TEBExpression;
   function DownloadLookupPtrSelectionInts(r1, r2: TRadioButton;
     valueBox: TIDLookupCombo; ptrBox: TIntSelector): TIntPair;


implementation
uses
   EB_Expressions, EB_Expressions_RM;

procedure UploadLookupPtrSelection(expr: TEBExpression;
  r1, r2: TRadioButton; valueBox: TIDLookupCombo; ptrBox: TIntSelector);
begin
   if expr is TEBLookupValue then
   begin
      r1.Checked := true;
      valueBox.ID := expr.Values[0];
   end
   else begin
      assert(expr is TEBIntsValue);
      r2.Checked := true;
      ptrBox.ID := expr.Values[0];
   end;
end;

procedure UploadLookupPtrSelection(v1, v2: integer; r1,
  r2: TRadioButton; valueBox: TIDLookupCombo; ptrBox: TIntSelector);
begin
   if v1 = 0 then
   begin
      r1.Checked := true;
      valueBox.ID := v2;
   end
   else begin
      r2.Checked := true;
      ptrBox.ID := v2;
   end;
end;

function DownloadLookupPtrSelection(r1, r2: TRadioButton;
  valueBox: TIDLookupCombo; ptrBox: TIntSelector; const lookupName: string): TEBExpression;
begin
   assert(r1.Checked or r2.Checked);
   if r1.Checked then
      result := TEBLookupValue.Create(valueBox.id, lookupName)
   else result := TEBIntsValue.Create(ptrBox.id);
end;

function DownloadLookupPtrSelectionInts(r1, r2: TRadioButton;
  valueBox: TIDLookupCombo; ptrBox: TIntSelector): TIntPair;
begin
   assert(r1.Checked or r2.Checked);
   if r1.Checked then
   begin
      result[1] := 0;
      result[2] := valueBox.id;
   end
   else begin
      result [1] := 1;
      result [2] := ptrBox.ID;
   end;
end;

procedure UploadValuePtrSelection(v1, v2: integer; r1,
  r2: TRadioButton; valueBox: TJvSpinEdit; ptrBox: TIntSelector);
begin
   if v1 = 0 then
   begin
      r1.Checked := true;
      valueBox.AsInteger := v2;
   end
   else begin
      r2.Checked := true;
      ptrBox.ID := v2;
   end;
end;

procedure UploadValuePtrSelection(expr: TEBExpression; r1,
  r2: TRadioButton; valueBox: TJvSpinEdit; ptrBox: TIntSelector);
var
   v1: integer;
begin
   assert(expr.values.count > 0);
   v1 := ord(expr.ClassType = TEBIntsValue);
   UploadValuePtrSelection(v1, expr.Values[0], r1, r2, valueBox, ptrBox);
end;

function DownloadValuePtrSelection(r1, r2: TRadioButton;
  valueBox: TJvSpinEdit; ptrBox: TIntSelector): TIntPair;
begin
   assert(r1.Checked or r2.Checked);
   if r1.Checked then
   begin
      result[1] := 0;
      result[2] := valueBox.AsInteger;
   end
   else begin
      result [1] := 1;
      result [2] := ptrBox.ID;
   end;
end;

end.
