unit rm2_turbu_resists;
{*****************************************************************************
* The contents of this file are used with permission, subject to
* the Mozilla Public License Version 1.1 (the "License"); you may
* not use this file except in compliance with the License. You may
* obtain a copy of the License at
* http://www.mozilla.org/MPL/MPL-1.1.html
*
* Software distributed under the License is distributed on an
* "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
* implied. See the License for the specific language governing
* rights and limitations under the License.
*
*****************************************************************************
*
* This file was created by Mason Wheeler.  He can be reached for support at
* www.turbu-rpg.com.
*****************************************************************************}

interface
uses
   LDB, condition_data,
   turbu_resists;

type
   T2k2AttributeTemplate = class helper for TAttributeTemplate
   public
      constructor Convert(base: TAttribute; id: word);
   end;

   T2k2ConditionTemplate = class helper for TConditionTemplate
   public
      constructor Convert(base: TCondition; id: word);
   end;

implementation

{ T2k2AttributeTemplate }

constructor T2k2AttributeTemplate.Convert(base: TAttribute; id: word);
begin
   inherited Create;
   self.id := id;
   self.name := string(base.name);
   self.requiredForSkills := base.weaponRestrict;
   self.standard := base.rate[3];
end;

{ T2k2ConditionTemplate }

constructor T2k2ConditionTemplate.Convert(base: TCondition; id: word);
begin
   inherited Create;
   self.id := id;
   self.name := string(base.name);
end;

end.
