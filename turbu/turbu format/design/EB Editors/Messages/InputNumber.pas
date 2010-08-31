unit InputNumber;

interface

uses
  SysUtils, Classes, Controls, Forms, StdCtrls, ExtCtrls,
  EbEdit, EventBuilder, Mask, JvExMask, JvSpin, variable_selector;

type
   [EditorCategory('Messages', 'Input Number', 4)]
   TfrmInputNumber = class(TfrmEbEditBase)
      GroupBox1: TGroupBox;
      spnDigits: TJvSpinEdit;
      GroupBox2: TGroupBox;
      selInteger: TIntSelector;
   protected
      procedure UploadObject(obj: TEbObject); override;
      procedure DownloadObject(obj: TEbObject); override;
      function NewClassType: TEbClass; override;
   end;

implementation
uses
   EB_Messages;

{$R *.dfm}

{ TfrmInputNumber }

procedure TfrmInputNumber.DownloadObject(obj: TEbObject);
begin
   assert(obj is TEBInputNumber);
   obj.Values.Clear;
   obj.Values.Add(spnDigits.AsInteger);
   obj.Values.Add(selInteger.ID);
end;

function TfrmInputNumber.NewClassType: TEbClass;
begin
   result := TEBInputNumber;
end;

procedure TfrmInputNumber.UploadObject(obj: TEbObject);
begin
   assert(obj is TEBInputNumber);
   spnDigits.AsInteger := obj.Values[0];
   selInteger.ID := obj.Values[1];
end;

initialization
   RegisterEbEditor(TEBInputNumber, TfrmInputNumber);
end.
