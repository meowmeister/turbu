unit ebPartyBase;

interface

uses
   Controls, Classes, ExtCtrls, DB, StdCtrls, DBCtrls,
   EventBuilder, EbEdit, variable_selector, IDLookupCombo;

type
   TfrmEBPartyBase = class(TfrmEbEditBase)
      grpCharacter: TGroupBox;
      cboHeroID: TIDLookupCombo;
      radSpecificHero: TRadioButton;
      radHeroPtr: TRadioButton;
      selItemID: TIntSelector;
      radAllParty: TRadioButton;
      srcHeroes: TDataSource;
   private
      function WhichRadioButton: integer;
   protected
      procedure UploadObject(obj: TEbObject); override;
      procedure DownloadObject(obj: TEbObject); override;
      procedure EnableControlsProperly; override;
  end;

implementation
uses
   EB_Expressions;

{$R *.dfm}

{ TfrmEBPartyBase }

function TfrmEBPartyBase.WhichRadioButton: integer;
begin
   if radAllParty.checked then
      result := 0
   else if radSpecificHero.Checked then
      result := 1
   else if radHeroPtr.Checked then
      result := 2
   else raise ERPGScriptError.Create('No valid selection for grpCharacter');
end;

procedure TfrmEBPartyBase.DownloadObject(obj: TEbObject);
var
   subscript: TEBChainable;
begin
   obj.Clear;
   subscript := nil;
   case WhichRadioButton of
      0: subscript := TEBObjArrayValue.Create('Party', TEBVariableValue.Create('Num'));
      1: subscript := TEBLookupObjExpr.Create('Hero', cboHeroID.id, 'heroes');
      2: subscript := TEBLookupObjExpr.Create('Hero', TEBIntsValue.Create(selItemID.ID), 'heroes');
      else assert(false);
   end;
   obj.Add(subscript);
end;

procedure TfrmEBPartyBase.EnableControlsProperly;
begin
   EnableControl(cboHeroID, radSpecificHero);
   EnableControl(selItemID, radHeroPtr);
end;

procedure TfrmEBPartyBase.UploadObject(obj: TEbObject);
var
   sub: TEBExpression;
begin
   sub := obj.Components[0] as TEBExpression;
   if sub.classtype = TEBObjArrayValue then
      radAllParty.checked := true
   else begin
      assert(sub is TEBLookupObjExpr);
      if sub.Values.Count > 0 then
      begin
         radSpecificHero.Checked := true;
         cboHeroID.id := sub.Values[0];
      end
      else begin
         sub := sub.Components[1] as TEBIntsValue;
         radHeroPtr.Checked := true;
         selItemID.ID := sub.Values[0];
      end;
   end;
end;

end.
