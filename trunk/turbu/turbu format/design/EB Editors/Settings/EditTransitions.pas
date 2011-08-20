unit EditTransitions;

interface

uses
  Forms, StdCtrls, Classes, Controls, ExtCtrls,
  EventBuilder, EbEdit;

type
   [EditorCategory('Settings', 'Change System Skin')]
   TfrmEditTransitions = class(TfrmEbEditBase)
      GroupBox1: TGroupBox;
      cboEvent: TComboBox;
      cboTransition: TComboBox;
   protected
      procedure UploadObject(obj: TEbObject); override;
      procedure DownloadObject(obj: TEbObject); override;
      function NewClassType: TEbClass; override;
   end;

implementation
uses
   EB_Maps;

{$R *.dfm}

{ TfrmEditTransitions }

function TfrmEditTransitions.NewClassType: TEbClass;
begin
   result := TEBTransition;
end;

procedure TfrmEditTransitions.DownloadObject(obj: TEbObject);
begin
   obj.Clear;
   obj.Values.Add(cboEvent.ItemIndex);
   obj.Values.Add(cboTransition.ItemIndex + 1);
end;

procedure TfrmEditTransitions.UploadObject(obj: TEbObject);
begin
   cboEvent.ItemIndex := obj.Values[0];
   cboTransition.ItemIndex := obj.Values[1] - 1;
end;

initialization
   RegisterEbEditor(TEBTransition, TfrmEditTransitions);
finalization
   UnregisterEbEditor(TEBTransition);
end.
