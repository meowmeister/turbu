unit uPSI_item_menu;
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

{
This file has been generated by UnitParser v0.7, written by M. Knight
and updated by NP. v/d Spek and George Birbilis. 
Source Code from Carlo Kok has been used to implement various sections of
UnitParser. Components of ROPS are used in the construction of UnitParser,
code implementing the class wrapper is taken from Carlo Kok's conv utility

}
interface
 
uses
   SysUtils
  ,Classes
  ,uPSComponent
  ,uPSRuntime
  ,uPSCompiler
  ;
 
type 
(*----------------------------------------------------------------------------*)
  TPSImport_rm2x_item_menu = class(TPSPlugin)
  protected
    procedure CompileImport1(CompExec: TPSScript); override;
    procedure ExecImport1(CompExec: TPSScript; const ri: TPSRuntimeClassImporter); override;
  end;
 
 
{ compile-time registration functions }
procedure SIRegister_TItemMenuPage(CL: TPSPascalCompiler);
procedure SIRegister_rm2x_item_menu(CL: TPSPascalCompiler);

{ run-time registration functions }
procedure RIRegister_TItemMenuPage(CL: TPSRuntimeClassImporter);
procedure RIRegister_rm2x_item_menu(CL: TPSRuntimeClassImporter);

procedure Register;

implementation


uses
   windows
  ,chipset_data
  ,menu_basis
  ,rm2X_menu_components
  ,rpg_list
  ,rm2x_item_menu
  ;
 
 
procedure Register;
begin
  RegisterComponents('Pascal Script', [TPSImport_rm2x_item_menu]);
end;

(* === compile-time registration functions === *)
(*----------------------------------------------------------------------------*)
procedure SIRegister_TItemMenuPage(CL: TPSPascalCompiler);
begin
  //with RegClassS(CL,'TMenuPage', 'TItemMenuPage') do
  with CL.AddClassN(CL.FindClass('TMenuPage'),'TItemMenuPage') do
  begin
    RegisterProperty('descBox', 'TOnelineLabelBox', iptr);
    RegisterProperty('itemMenu', 'TGameItemMenu', iptr);
  end;
end;

(*----------------------------------------------------------------------------*)
procedure SIRegister_rm2x_item_menu(CL: TPSPascalCompiler);
begin
  SIRegister_TItemMenuPage(CL);
end;

(* === run-time registration functions === *)
(*----------------------------------------------------------------------------*)
procedure TItemMenuPageitemMenu_R(Self: TItemMenuPage; var T: TGameItemMenu);
begin T := Self.itemMenu; end;

(*----------------------------------------------------------------------------*)
procedure TItemMenuPagedescBox_R(Self: TItemMenuPage; var T: TOnelineLabelBox);
begin T := Self.descBox; end;

(*----------------------------------------------------------------------------*)
procedure RIRegister_TItemMenuPage(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TItemMenuPage) do
  begin
    RegisterPropertyHelper(@TItemMenuPagedescBox_R,nil,'descBox');
    RegisterPropertyHelper(@TItemMenuPageitemMenu_R,nil,'itemMenu');
  end;
end;

(*----------------------------------------------------------------------------*)
procedure RIRegister_rm2x_item_menu(CL: TPSRuntimeClassImporter);
begin
  RIRegister_TItemMenuPage(CL);
end;

 
 
{ TPSImport_rm2x_item_menu }
(*----------------------------------------------------------------------------*)
procedure TPSImport_rm2x_item_menu.CompileImport1(CompExec: TPSScript);
begin
  SIRegister_rm2x_item_menu(CompExec.Comp);
end;
(*----------------------------------------------------------------------------*)
procedure TPSImport_rm2x_item_menu.ExecImport1(CompExec: TPSScript; const ri: TPSRuntimeClassImporter);
begin
  RIRegister_rm2x_item_menu(ri);
end;
(*----------------------------------------------------------------------------*)
 
 
end.
