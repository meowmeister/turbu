unit EditExp;

interface

uses
   StdCtrls, Classes, Controls, ExtCtrls, DB, DBCtrls, Mask,
   JvExMask, JvSpin,
   EventBuilder, EbEdit, dm_database, variable_selector, IDLookupCombo;

type
   [EditorCategory('Characters', 'Change Experience', 3)]
   TfrmEBEditExp = class(TfrmEbEditBase)
      cboHeroID: TIDLookupCombo;
      radSpecificHero: TRadioButton;
      radHeroPtr: TRadioButton;
      selItemID: TIntSelector;
      srcHeroes: TDataSource;
      radAllParty: TRadioButton;
      grpOperation: TRadioGroup;
      grpItemCount: TGroupBox;
      radExactAmount: TRadioButton;
      radPointer: TRadioButton;
      spnExactValue: TJvSpinEdit;
      selValue: TIntSelector;
      chkLevelMssage: TCheckBox;
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

{ TfrmEBEditExp }

procedure TfrmEBEditExp.EnableControlsProperly;
begin
   EnableControl(cboHeroID, radSpecificHero);
   EnableControl(selItemID, radHeroPtr);
   EnableControl(spnExactValue, radExactAmount);
   EnableControl(selValue, radPointer);
   chkLevelMssage.Enabled := grpOperation.ItemIndex = 0;
end;

function TfrmEBEditExp.NewClassType: TEbClass;
begin
   result := TEBExperience;
end;

procedure TfrmEBEditExp.UploadObject(obj: TEbObject);
begin
   case obj.Values[0] of
      0: radAllParty.Checked := true;
      1:
      begin
         radSpecificHero.Checked := true;
         cboHeroID.id := obj.Values[1];
      end;
      2:
      begin
         radHeroPtr.Checked := true;
         selItemID.ID := obj.Values[1];
      end
      else assert(false);
   end;
   grpOperation.ItemIndex := obj.Values[2];
   UploadValuePtrSelection(obj.Values[3], obj.Values[4], radExactAmount, radPointer, spnExactValue, selValue);
   chkLevelMssage.Checked := boolean(obj.Values[5]);
   EnableControlsProperly;
end;

procedure TfrmEBEditExp.DownloadObject(obj: TEbObject);
var
   pair: TIntPair;
begin
   obj.Clear;
   if radAllParty.Checked then
      obj.Values.AddRange([0, 0])
   else if radSpecificHero.Checked then
   begin
      obj.Values.Add(1);
      obj.values.Add(cboHeroID.id);
   end
   else begin
      obj.Values.Add(2);
      obj.Values.Add(selItemID.ID);
   end;
   obj.Values.Add(grpOperation.ItemIndex);
   pair := DownloadValuePtrSelection(radExactAmount, radPointer, spnExactValue, selValue);
   obj.Values.AddRange(pair);
   obj.Values.Add(ord(chkLevelMssage.Checked));
end;

initialization
   RegisterEbEditor(TEBExperience, TfrmEBEditExp);
end.
