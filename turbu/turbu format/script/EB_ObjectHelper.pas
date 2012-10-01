unit EB_ObjectHelper;

interface
uses
   EventBuilder;

type
   TEBObjectHelper = class helper for TEBObject
   private
      class function VarName(id: integer; const group: string): string; static;
   public
      function HeroName(id: integer): string;
      function VehicleName(id: integer): string;
      function SecondFraction(count: integer): string;
      class function IntName(id: integer): string;
      function SwitchName(id: integer): string;
   end;

implementation
uses
   SysUtils;

{ TEBObjectHelper }

function TEBObjectHelper.HeroName(id: integer): string;
begin
   result := self.GetLookup(id, 'heroes');
end;

function TEBObjectHelper.VehicleName(id: integer): string;
begin
   result := self.GetLookup(id, 'vehicles');
end;

class function TEBObjectHelper.VarName(id: integer; const group: string): string;
var
   name: string;
begin
   name := GetLookup(id, group);
   if name = '' then
      result := intToStr(id)
   else result := format('%d: %s', [id, name]);
end;

class function TEBObjectHelper.IntName(id: integer): string;
begin
   result := VarName(id, 'Variables');
end;

function TEBObjectHelper.SecondFraction(count: integer): string;
begin
   result := formatFloat('###.#', count / 10) + ' sec';
end;

function TEBObjectHelper.SwitchName(id: integer): string;
begin
   result := VarName(id, 'Switches');
end;

end.
