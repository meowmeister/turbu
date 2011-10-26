unit EditSkin;

interface

uses
   Forms, StdCtrls, ExtCtrls, Classes, Controls,
   EventBuilder, EbEdit, imageSelectorFrame;

type
   [EditorCategory('Settings', 'Change System Skin')]
   TfrmSkinSelector = class(TfrmEbEditBase)
      frameImageSelector: TframeImageSelector;
      Panel2: TPanel;
      radStyle: TRadioGroup;
      radFont: TRadioGroup;
      procedure FormCreate(Sender: TObject);
   protected
      procedure UploadObject(obj: TEbObject); override;
      procedure DownloadObject(obj: TEbObject); override;
      function NewClassType: TEbClass; override;
   end;

implementation
uses
   archiveInterface, EB_Settings;

{$R *.dfm}

procedure TfrmSkinSelector.FormCreate(Sender: TObject);
begin
   frameImageSelector.Setup(GArchives[IMAGE_ARCHIVE], 'System');
end;

function TfrmSkinSelector.NewClassType: TEbClass;
begin
   result := TEBSysSkin;
end;

procedure TfrmSkinSelector.UploadObject(obj: TEbObject);
begin
   frameImageSelector.Selection := obj.Text;
   radStyle.ItemIndex := obj.Values[0];
   radFont.ItemIndex := obj.Values[1];
end;

procedure TfrmSkinSelector.DownloadObject(obj: TEbObject);
begin
   obj.Clear;
   obj.Text := frameImageSelector.Selection;
   obj.Values.Add(radStyle.ItemIndex);
   obj.Values.Add(radFont.ItemIndex);
end;

initialization
   RegisterEbEditor(TEBSysSkin, TfrmSkinSelector);
finalization
   UnregisterEbEditor(TEBSysSkin);
end.
