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
unit turbu_script_engine;

interface
uses
   Generics.Collections,
   rsCompiler, rsExec, rsDefsBackend,
   turbu_map_interface, turbu_map_objects;

type
   TScriptEngine = class
   private
      FCompiler: TrsCompiler;
      FExec: TrsExec;
      FCurrentProgram: TrsProgram;
      FMapObjects: TList<TRpgMapObject>;
   public
      constructor Create;
      destructor Destroy; override;
      procedure LoadScript(const script: string);
      procedure LoadMap(const map: IRpgMap);
   end;

implementation
uses
   SysUtils, Classes;

{ TScriptEngine }

constructor TScriptEngine.Create;
begin
   FCompiler := TrsCompiler.Create;
   FExec := TrsExec.Create;
   FMapObjects := TList<TRpgMapObject>.Create;
end;

destructor TScriptEngine.Destroy;
begin
   FMapObjects.Free;
   FCompiler.Free;
   FCurrentProgram.Free;
   FExec.Free;
   inherited Destroy;
end;

procedure TScriptEngine.LoadMap(const map: IRpgMap);
var
   list: TStrings;
   i: integer;
begin
   FMapObjects.Clear;
   list := map.GetMapObjects;
   try
      FMapObjects.capacity := list.Count;
      for i := 0 to list.Count - 1 do
         FMapObjects.Add(list.Objects[i] as TRpgMapObject);
   finally
      list.Free;
   end;
end;

procedure TScriptEngine.LoadScript(const script: string);
begin
   FreeAndNil(FCurrentProgram);
   FCurrentProgram := FCompiler.Compile(script);
   FExec.Load(FCurrentProgram);
end;

end.
