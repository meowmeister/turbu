unit openformThrowaway;
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
   Forms, Classes, dialogs, inifiles, //windows libs
   mapview; //other forms

type
  TopenForm = class(TForm)
    dlgOpen: TOpenDialog;
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
   openForm: TopenForm;

implementation
uses
   sysUtils, //delphi libs
   commons; //TURBU libs
{$R *.dfm}

procedure TopenForm.FormShow(Sender: TObject);
var
   ini: TIniFile;
begin
   ini := TIniFile.Create(GetRegistryValue('\Software\ASCII\RPG2000', 'ApplicationPath') + 'RPG2000.ini');
   try
      dlgOpen.InitialDir := ini.ReadString('RPG2000', 'ProjectBasePath', '');
{      assert(frmGameForm.fontDB.update);
      assert(frmGameForm.fontEngine.loadFromASDb(frmGameForm.fontDB));}
      if dlgOpen.Execute then
      begin
         if init(dlgOpen.FileName) then
         begin
            frmGameForm.show;
            self.release;
         end;
      end else Application.Terminate;
   finally
      ini.free;
   end; //end of 3rd TRY block
end;

end.
