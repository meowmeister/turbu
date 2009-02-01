unit turbu_plugin;
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
  SysUtils, Dialogs, Forms, Contnrs,
  JvPlugin,
  turbu_plugin_interface;

type
   TRpgPlugin = class(TJvPlugin, ITurbuPluginInterface)
   private
      { Private declarations }
   public
      { Public declarations }
      function listPlugins: TObjectList;
   end;

function RegisterPlugin: TRpgPlugin; stdcall;

exports
   RegisterPlugin;

implementation

{$R *.DFM}

// IMPORTANT NOTE: If you change the name of the Plugin container,
// you must set the type below to the same type. (Delphi changes
// the declaration, but not the procedure itself. Both the return
// type and the type created must be the same as the declared type above.

function RegisterPlugin: TRpgPlugin;
begin
   Result := TRpgPlugin.Create(nil);
end;

{ TRpgPlugin }

function TRpgPlugin.listPlugins: TObjectList;
begin
   result := TObjectList.Create;
end;

end.
