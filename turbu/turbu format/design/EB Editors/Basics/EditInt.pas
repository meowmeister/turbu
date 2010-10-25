unit EditInt;

interface

uses
   SysUtils, Controls, Forms, Classes, Dialogs, ExtCtrls, StdCtrls, Mask,
   JvExMask, JvSpin,
   EbEdit, variable_selector, turbu_classes;

type
   [EditorCategory('Basics', 'Set Integer', 1)]
   TfrmEBSetInteger = class(TfrmEbEditBase)
      grpOperand: TGroupBox;
      radSwitch: TRadioButton;
      radInt: TRadioButton;
      IntSelector1: TIntSelector;
      grpOperation: TRadioGroup;
      radNumber: TRadioButton;
      radValue: TRadioButton;
      radReference: TRadioButton;
      radRandom: TRadioButton;
      radItem: TRadioButton;
      spnNumber: TJvSpinEdit;
      selValue: TIntSelector;
      selIndex: TIntSelector;
      cbxItem: TComboBox;
      cbxItemAction: TComboBox;
      cbxFunctions: TComboBox;
      procedure FormCreate(Sender: TObject);
   private
      FFunctionList: TDeclList;
      procedure DisableControls;
   public
     { Public declarations }
     destructor Destroy; override;
   end;

implementation
uses
   turbu_database, turbu_script_interface, turbu_vartypes,
   EB_System;

{$R *.dfm}

destructor TfrmEBSetInteger.Destroy;
begin
   FFunctionList.Free;
   inherited Destroy;
end;

procedure TfrmEBSetInteger.DisableControls;
var
   i: integer;
begin
   for i := 0 to grpOperand.ControlCount - 1 do
      if not (grpOperand.Controls[i] is TRadioButton) then
         grpOperand.Controls[i].Enabled := false;
end;

procedure TfrmEBSetInteger.FormCreate(Sender: TObject);
var
   decl: TRpgDecl;
begin
   FFunctionList := (GScriptEngine as IDesignScriptEngine).GetFunctions(turbu_vartypes.lookupType('integer'));
   for decl in FFunctionList do
      cbxFunctions.AddItem(decl.designName, decl);
end;

{initialization
   RegisterEbEditor(TEBGlobalInt, TfrmEbSetInteger);}
end.
