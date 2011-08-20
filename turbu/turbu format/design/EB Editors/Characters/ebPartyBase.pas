unit ebPartyBase;

interface

uses
   Controls, Classes, ExtCtrls, DB, StdCtrls, DBCtrls,
   EventBuilder, EbEdit, variable_selector, IDLookupCombo, button_edit;

type
   TfrmEBPartyBase = class(TfrmEbEditBase)
      grpCharacter: TGroupBox;
      cboHeroID: TIDLookupCombo;
      radSpecificHero: TRadioButton;
      radHeroPtr: TRadioButton;
      selHeroID: TIntSelector;
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
      2: subscript := TEBLookupObjExpr.Create('Hero', TEBIntsValue.Create(selHeroID.ID), 'heroes');
      else assert(false);
   end;
   obj.Add(subscript);
end;

procedure TfrmEBPartyBase.EnableControlsProperly;
begin
   EnableControl(cboHeroID, radSpecificHero);
   EnableControl(selHeroID, radHeroPtr);
end;

procedure TfrmEBPartyBase.UploadObject(obj: TEbObject);
var
   sub: TEBExpression;
   i: integer;
   found: boolean;
begin
   sub := obj.Children[0] as TEBExpression;
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
         found := false;
         for I := 0 to sub.ChildCount - 1 do
            if sub.Children[i] is TEBIntsValue then
            begin
               sub := TEBIntsValue(sub.Children[i]);
               radHeroPtr.Checked := true;
               selHeroID.ID := sub.Values[0];
               found := true;
               break;
            end;
         if not found then
            raise ERPGScriptError.Create('Subscript not found');
      end;
   end;
end;

end.
