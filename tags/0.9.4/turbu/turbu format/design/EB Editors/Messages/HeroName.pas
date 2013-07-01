unit HeroName;

interface

uses
   Classes, Controls, Forms, StdCtrls, DBCtrls, ExtCtrls,
   EventBuilder, EbEdit, IDLookupCombo, DB;

type
   [EditorCategory('Messages', 'Enter Hero Name')]
   [EditorContext('RM2K')]
   TfrmInputHeroName = class(TfrmEbEditBase)
      GroupBox1: TGroupBox;
      cboHeroID: TIDLookupCombo;
      chkShowName: TCheckBox;
      srcHeroes: TDataSource;
   protected
      procedure UploadObject(obj: TEbObject); override;
      procedure DownloadObject(obj: TEbObject); override;
      function NewClassType: TEbClass; override;
   end;

implementation
uses
   dm_database,
   EB_Messages;

{$R *.dfm}

{ TfrmInputHeroName }

function TfrmInputHeroName.NewClassType: TEbClass;
begin
   result := TEBInputHeroName;
end;

procedure TfrmInputHeroName.UploadObject(obj: TEbObject);
begin
   cboHeroID.id := obj.Values[0];
   chkShowName.Checked := boolean(obj.Values[1]);
end;

procedure TfrmInputHeroName.DownloadObject(obj: TEbObject);
begin
   obj.Clear;
   obj.Values.add(cboHeroID.id);
   obj.Values.add(ord(chkShowName.Checked));
end;

initialization
   RegisterEbEditor(TEBInputHeroName, TfrmInputHeroName);
finalization
   UnRegisterEbEditor(TEBInputHeroName);
end.
