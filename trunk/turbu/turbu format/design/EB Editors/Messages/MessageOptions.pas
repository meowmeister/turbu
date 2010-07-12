unit MessageOptions;

interface

uses
   StdCtrls, Classes, Controls, ExtCtrls,
   EbEdit, EventBuilder;

type
   [EditorCategory('Messages', 'Message Options', 1)]
   TfrmMessageOptions = class(TfrmEbEditBase)
      GroupBox3: TGroupBox;
      radVisibility: TRadioGroup;
      radPosition: TRadioGroup;
      chkNoHide: TCheckBox;
      chkBlock: TCheckBox;
   protected
      procedure UploadObject(obj: TEbObject); override;
      procedure DownloadObject(obj: TEbObject); override;
      function NewClassType: TEbClass; override;
   end;

implementation
uses
   EB_Messages;

{$R *.dfm}

{ TfrmMessageOptions }

procedure TfrmMessageOptions.UploadObject(obj: TEbObject);
begin
   assert(obj is TEBMessageOptions);
   radVisibility.ItemIndex := obj.Values[0];
   radPosition.ItemIndex := obj.Values[1];
   chkNoHide.Checked := boolean(obj.Values[2]);
   chkBlock.Checked := boolean(obj.Values[3]);
end;

procedure TfrmMessageOptions.DownloadObject(obj: TEbObject);
begin
   obj.Values.Clear;
   obj.Values.Add(radVisibility.ItemIndex);
   obj.Values.Add(radPosition.ItemIndex);
   obj.Values.Add(ord(chkNoHide.Checked));
   obj.Values.Add(ord(chkBlock.Checked));
end;

function TfrmMessageOptions.NewClassType: TEbClass;
begin
   result := TEbMessageOptions;
end;

initialization
   RegisterEbEditor(TEbMessageOptions, TfrmMessageOptions);

end.
