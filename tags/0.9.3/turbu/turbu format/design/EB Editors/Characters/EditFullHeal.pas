unit EditFullHeal;

interface

uses
   DBClient, Classes, Controls, ExtCtrls, DB, StdCtrls, DBCtrls,
   EventBuilder, variable_selector, ebPartyBase, IDLookupCombo, EBEdit;

type
   [EditorCategory('Characters', 'Full Heal')]
   TfrmEBEditFullHeal = class(TfrmEBPartyBase)
   protected
      function NewClassType: TEbClass; override;
   end;

implementation
uses
   EB_Characters, EB_Expressions;

{$R *.dfm}

{ TfrmEBFullHeal }

function TfrmEBEditFullHeal.NewClassType: TEbClass;
begin
   result := TEBFullHeal;
end;

initialization
   RegisterEbEditor(TEBFullHeal, TfrmEBEditFullHeal);
finalization
   UnRegisterEbEditor(TEBFullHeal);
end.
