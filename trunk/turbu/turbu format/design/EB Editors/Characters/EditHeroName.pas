unit EditHeroName;

interface

uses
  StdCtrls, DBCtrls, Classes, Controls, ExtCtrls, DB,
  ebEdit, EventBuilder, dm_Database, IDLookupCombo;

type
   [EditorCategory('Characters', 'Change Hero Name')]
   TfrmEBEditHeroName = class(TfrmEbEditBase)
      cboHero: TIDLookupCombo;
      lblNewName: TStaticText;
      srcHero: TDataSource;
      txtName: TEdit;
   protected
      procedure UploadObject(obj: TEbObject); override;
      procedure DownloadObject(obj: TEbObject); override;
      function NewClassType: TEbClass; override;
   end;

   [EditorCategory('Characters', 'Change Hero Title')]
   TFrmEBEditHeroTitle = class(TfrmEBEditHeroName)
      procedure FormShow(Sender: TObject);
   protected
      function NewClassType: TEbClass; override;
   end;

implementation
uses
   EB_Characters;

{$R *.dfm}

{ TfrmEBEditHeroName }

procedure TfrmEBEditHeroName.UploadObject(obj: TEbObject);
begin
   cboHero.id := obj.Values[0];
   txtName.Text := obj.Text;
end;

procedure TfrmEBEditHeroName.DownloadObject(obj: TEbObject);
begin
   obj.Clear;
   obj.Values[0] := cboHero.id;
   obj.Text := txtName.Text;
end;

function TfrmEBEditHeroName.NewClassType: TEbClass;
begin
   result := TEBHeroName;
end;

{ TFrmEBEditHeroTitle }

procedure TFrmEBEditHeroTitle.FormShow(Sender: TObject);
begin
   inherited FormShow(Sender);
   self.Caption := 'Change Hero Title';
   lblNewName.Caption := 'New Title';
end;

function TFrmEBEditHeroTitle.NewClassType: TEbClass;
begin
   result := TEBHeroTitle;
end;

initialization
   RegisterEbEditor(TEBHeroName, TfrmEBEditHeroName);
   RegisterEbEditor(TEBHeroTitle, TfrmEBEditHeroTitle);
finalization
   UnRegisterEbEditor(TEBHeroName);
   UnRegisterEbEditor(TEBHeroTitle);
end.
