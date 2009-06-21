unit turbu_heroes;

interface
uses
   turbu_classes;

type
   //stub declaration that will be filled in later.
   TRpgHero = class(TRpgObject)
   protected
      class function templateClass: TDatafileClass; override;
   end;

   TRpgParty = class(TRpgObject)
   protected
      class function templateClass: TDatafileClass; override;
   end;

   TPartyEvent = procedure(hero: TRpgHero; party: TRpgParty) of object;

implementation
uses
   turbu_characters;

{ TRpgHero }

class function TRpgHero.templateClass: TDatafileClass;
begin
   result := TClassTemplate;
end;

{ TRpgParty }

class function TRpgParty.templateClass: TDatafileClass;
begin
   result := nil;
end;

end.
