unit turbu_versioning;
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
type
   TVersion = packed record
      constructor Create(a, b: byte; c: word); overload;
      constructor Create(value: cardinal); overload;
      class operator LessThan(a: TVersion; b: TVersion): boolean; inline;
      class operator GreaterThan(a: TVersion; b: TVersion): boolean; inline;
      function name: string;

      case boolean of
         true:
            (build: word;
            minor,major: byte);
         false:
            (value: cardinal);
      end;

implementation
uses
   sysUtils;

{ TVersion }

constructor TVersion.Create(a, b: byte; c: word);
begin
   self.major := a;
   self.minor := b;
   self.build := c;
end;

constructor TVersion.Create(value: cardinal);
begin
   self.value := value;
end;

class operator TVersion.GreaterThan(a, b: TVersion): boolean;
begin
   result := a.value > b.value;
end;

class operator TVersion.LessThan(a, b: TVersion): boolean;
begin
   result := a.value < b.value;
end;

function TVersion.name: string;
begin
   result := intToStr(major) + '.' + intToStr(minor) + '.' + intToStr(build);
end;

end.
