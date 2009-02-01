unit rm2_turbu_sounds;
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
   rm_sound, turbu_sounds;

type
   T2k2SoundTemplate = class helper for TSoundTemplate
   public
      constructor Convert(base: TRmMusic);
   end;

implementation

{ T2k2SoundTemplate }

constructor T2k2SoundTemplate.Convert(base: TRmMusic);
begin
   inherited Create;
   self.filename := unicodeString(base.filename);
   self.fadeIn := base.fadeIn;
   self.tempo := base.tempo;
   self.left := base.left;
   self.right := base.right;
   self.volume := base.volume;
end;

end.
