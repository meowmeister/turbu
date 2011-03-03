unit console;
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
  Classes, Controls, Forms, StdCtrls, ComCtrls, Grids, ValEdit, //windows libs
  commons, script_engine; //turbu libs

type
   TConsoleEventThread = class(TEventThread)
   private
   public
      constructor Create(script: AnsiString);
   end;

   TfrmConsole = class(TForm)
      txtScriptLine: TEdit;
      btnOK: TButton;
      Label1: TLabel;
      pclDisplay: TPageControl;
      tcpSwitches: TTabSheet;
      tcpVariables: TTabSheet;
      vleSwitches: TValueListEditor;
      vleVariables: TValueListEditor;
      btnSwitchRescan: TButton;
      btnVariableRescan: TButton;
    lblMemAllocated: TLabel;
      procedure btnOKClick(Sender: TObject);
      procedure btnVariableRescanClick(Sender: TObject);
      procedure btnSwitchRescanClick(Sender: TObject);
      procedure FormShow(Sender: TObject);
   private
      { Private declarations }
      FConsoleScript: TConsoleEventThread;
   public
      property newScript: TConsoleEventThread read FConsoleScript write FConsoleScript;
   end;

var
  frmConsole: TfrmConsole;

implementation
uses
  sysutils, windows, //windows libs
  mapview, LDB; //turbu libs

{$R *.dfm}

procedure TfrmConsole.btnOKClick(Sender: TObject);
var
   script: string;
begin
   if txtScriptLine.Text <> '' then
   begin
      script := SCRIPT_HEADER + txtScriptLine.Text + SCRIPT_FOOTER;
      txtScriptLine.Text := '';
      try
         FConsoleScript := TConsoleEventThread.Create(script);
         GScriptEngine.registerConsoleThread(FConsoleScript);
      except
         on E: EThread do ;
      end;
   end;
   SetForegroundWindow(frmGameForm.Handle);
   sleep(50);
   btnSwitchRescanClick(self);
end;

procedure TfrmConsole.btnSwitchRescanClick(Sender: TObject);
var
   i: integer;
   dummy: string;
begin
   vleSwitches.Strings.Clear;
   for I := 1 to high(GSwitches) do
   begin
      dummy := intToStr(i) + ': ' + mapEngine.database.switches.name[i] + '=';
      case GSwitches[i] of
         true: dummy := dummy + 'true';
         false: dummy := dummy + 'false';
      end;
      vleSwitches.Strings.Add(dummy);
      vleSwitches.ItemProps[intToStr(i) + ': ' + mapEngine.database.switches.name[i]].ReadOnly := true;
   end;
//   lblMemAllocated.Caption := IntToStr(preloader.totalMemoryAllocated) + ' KB';
end;

procedure TfrmConsole.btnVariableRescanClick(Sender: TObject);
var
  I: Integer;
  dummy: string;
begin
   vleVariables.Strings.Clear;
   for I := 1 to high(GVariables) do
   begin
      dummy := intToStr(i) + ': ' + mapEngine.database.variables.name[i] + '='
            + intToStr(GVariables[i]);
      vleVariables.Strings.Add(dummy);
      vleVariables.ItemProps[intToStr(i) + ': ' + mapEngine.database.variables.name[i]].ReadOnly := true;
   end;
end;

procedure TfrmConsole.FormShow(Sender: TObject);
begin
   btnVariableRescanClick(self);
   btnSwitchRescanClick(self);
end;

{ TConsoleEventThread }

constructor TConsoleEventThread.Create(script: AnsiString);
begin
   inherited Create(GScriptEngine, nil, nil);
   if not GScriptEngine.compiler.Compile(script) then
   begin
   msgBox(GScriptEngine.compiler.Msg[0].MessageToString, 'Error');
      raise EFatalError.create('Could not compile event script!');
   end;
   GScriptEngine.compiler.getOutput(script);
   setupScript(script);
end;

end.
