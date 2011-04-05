unit EditExp;

interface

uses
   StdCtrls, Classes, Controls, ExtCtrls, DB, DBCtrls, Mask,
   JvExMask, JvSpin,
   EventBuilder, EbEdit, dm_database, variable_selector, IDLookupCombo;

type
   [EditorCategory('Characters', 'Change Experience')]
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
      chkLevelMessage: TCheckBox;
   protected
      procedure UploadObject(obj: TEbObject); override;
      procedure DownloadObject(obj: TEbObject); override;
      function NewClassType: TEbClass; override;
      procedure EnableControlsProperly; override;
   end;

   [EditorCategory('Characters', 'Change Level')]
   TFrmEBEditLevel = class(TfrmEBEditExp)
      procedure FormShow(Sender: TObject);
   protected
      function NewClassType: TEbClass; override;
   end;

   [EditorCategory('Characters', 'Change HP')]
   TFrmEBEditHP = class(TfrmEBEditExp)
      procedure FormShow(Sender: TObject);
   protected
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
   chkLevelMessage.Enabled := grpOperation.ItemIndex = 0;
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
   chkLevelMessage.Checked := boolean(obj.Values[5]);
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
   obj.Values.Add(ord(chkLevelMessage.Checked));
end;

{ TFrmEBEditLevel }

procedure TFrmEBEditLevel.FormShow(Sender: TObject);
begin
   inherited FormShow(sender);
   self.Caption := 'Change Level';
   spnExactValue.MaxValue := 99;
   grpOperation.Items[0] := 'Increase Level';
   grpOperation.Items[1] := 'Decrease Level';
end;

function TFrmEBEditLevel.NewClassType: TEbClass;
begin
   result := TEBLevel;
end;

{ TFrmEBEditHP }

procedure TFrmEBEditHP.EnableControlsProperly;
begin
   inherited EnableControlsProperly;
   chkLevelMessage.Enabled := grpOperation.ItemIndex = 1;
end;

procedure TFrmEBEditHP.FormShow(Sender: TObject);
begin
   inherited FormShow(sender);
   self.Caption := 'Change HP';
   spnExactValue.MaxValue := 9999;
   chkLevelMessage.Caption := 'Hero can die from HP decrease';
   grpOperation.Items[0] := 'Increase HP';
   grpOperation.Items[1] := 'Decrease HP';
end;

function TFrmEBEditHP.NewClassType: TEbClass;
begin
   result := TEBChangeHP;
end;

initialization
   RegisterEbEditor(TEBExperience, TfrmEBEditExp);
   RegisterEbEditor(TEBLevel, TfrmEBEditLevel);
   RegisterEbEditor(TEBChangeHP, TfrmEBEditHP);
end.
