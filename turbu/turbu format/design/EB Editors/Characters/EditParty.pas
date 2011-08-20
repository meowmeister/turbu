unit EditParty;

interface

uses
  DB, StdCtrls, DBCtrls, Classes, Controls, ExtCtrls,
  EventBuilder, dm_database, EbEdit, variable_selector, IDLookupCombo,
  button_edit;

type
   [EditorCategory('Characters', 'Change Party')]
   TfrmEBEditParty = class(TfrmEbEditBase)
      GroupBox2: TGroupBox;
      cboHeroID: TIDLookupCombo;
      radSpecificItem: TRadioButton;
      radItemPtr: TRadioButton;
      selItemID: TIntSelector;
      srcHeroes: TDataSource;
      grpOperation: TRadioGroup;
   protected
      procedure UploadObject(obj: TEbObject); override;
      procedure DownloadObject(obj: TEbObject); override;
      function NewClassType: TEbClass; override;
      procedure EnableControlsProperly; override;
   end;

implementation
uses
   EB_Characters;

{$R *.dfm}

{ TfrmEBEditParty }

procedure TfrmEBEditParty.EnableControlsProperly;
begin
   EnableControl(cboHeroID, radSpecificItem);
   EnableControl(selItemID, radItemPtr);
end;

function TfrmEBEditParty.NewClassType: TEbClass;
begin
   result := TEBChangeParty;
end;

procedure TfrmEBEditParty.DownloadObject(obj: TEbObject);
begin
   obj.Clear;
   obj.Values.Add(grpOperation.ItemIndex);
   obj.Add(DownloadLookupPtrSelection(radSpecificItem, radItemPtr, cboHeroID, selItemID, 'Heroes'));
end;

procedure TfrmEBEditParty.UploadObject(obj: TEbObject);
begin
   grpOperation.ItemIndex := obj.Values[0];
   UploadLookupPtrSelection(obj.Children[0] as TEBExpression, radSpecificItem,
     radItemPtr, cboHeroID, selItemID);
end;

initialization
   RegisterEbEditor(TEBChangeParty, TfrmEBEditParty);
finalization
   UnRegisterEbEditor(TEBChangeParty);
end.
