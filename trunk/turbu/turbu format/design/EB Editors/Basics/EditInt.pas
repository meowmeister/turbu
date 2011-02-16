unit EditInt;

interface

uses
   SysUtils, Controls, Forms, Classes, Dialogs, ExtCtrls, StdCtrls, Mask,
   JvExMask, JvSpin,
   EventBuilder, EbEdit, EB_Expressions, variable_selector, turbu_classes;

type
   [EditorCategory('Basics', 'Set Integer', 1)]
   TfrmEBSetInteger = class(TfrmEbEditBase)
      grpOperand: TGroupBox;
      radVariable: TRadioButton;
      radReference: TRadioButton;
      selLhsValue: TIntSelector;
      grpOperation: TRadioGroup;
      radNumber: TRadioButton;
      radValue: TRadioButton;
      radRefValue: TRadioButton;
      radFunction: TRadioButton;
      radProperty: TRadioButton;
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
      procedure BuildFunctionList;
      procedure LoadFunction(call: TEBCall);
   protected
      procedure UploadObject(obj: TEbObject); override;
//      procedure DownloadObject(obj: TEbObject); override;
      function NewClassType: TEbClass; override;
   public
     { Public declarations }
     destructor Destroy; override;
   end;

implementation
uses
   turbu_database, turbu_script_interface, turbu_vartypes, turbu_defs, turbu_constants,
   EB_System;

{$R *.dfm}

procedure TfrmEBSetInteger.BuildFunctionList;
const FUNCTIONS: array[1..2] of string = ('random', 'heldItems');
var
   engine: IDesignScriptEngine;
   name: string;
   decl: TRpgDecl;
   param: TNameType;
begin
//   TODO: Implement this way eventually
//   FFunctionList := (GScriptEngine as IDesignScriptEngine).GetFunctions(turbu_vartypes.lookupType('integer'));
   engine := GScriptEngine as IDesignScriptEngine;
   FFunctionList := TDeclList.Create;
   for name in FUNCTIONS do
      FFunctionList.Add(engine.FindFunction(name).Clone);
   for decl in FFunctionList do
   begin
      for param in decl do //verification
         assert(param.typeVar in [vt_boolean, vt_integer]);
      cbxFunctions.AddItem(decl.designName, decl);
   end;
end;

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
begin
   BuildFunctionList;
end;

procedure TfrmEBSetInteger.LoadFunction(call: TEBCall);
var
   txt: string;
begin
   assert(cbxFunctions.Items.IndexOf(call.text) <> -1);
   txt := call.GetNodeText;
   txt := copy(txt, pos('(', txt) + 1, 9999);
   delete(txt, length(txt) - 1, 1);
end;

function TfrmEBSetInteger.NewClassType: TEbClass;
begin
   result := TEBGlobalInt;
end;

procedure TfrmEBSetInteger.UploadObject(obj: TEbObject);
var
   lValue, rValue: TEBExpression;
begin
   assert(obj is TEBGlobalInt);
   lValue := obj.components[0] as TEBExpression;
   if lValue is TEBIntegerValue then
   begin
      radVariable.Checked := true;
      selLhsValue.ID := lValue.Values[0];
   end
   else begin
      radReference.Checked := true;
      if lValue is TEBIntsValue then
         selLhsValue.ID := lValue.Values[0]
      else selLhsValue.ID := ContextLookup((lValue as TEBVariableValue).GetNodeText);
   end;
   rValue := obj.components[1] as TEBExpression;
   if rValue is TEBBinaryOp then
   begin
      assert(lValue.GetScriptText = (rValue.Components[1] as TEBExpression).GetScriptText);
      grpOperation.ItemIndex := ord(TEBBinaryOp(rValue).op) + 1;
      rValue := rValue.Components[1] as TEBExpression;
   end;
   if rValue is TEBIntegerValue then
   begin
      radNumber.Checked := true;
      spnNumber.AsInteger := rValue.Values[0];
   end
   else if rValue is TEBIntsValue then
   begin
      if rValue.ComponentCount = 0 then
      begin
         radValue.Checked := true;
         selValue.ID := rValue.Values[0];
      end
      else begin
         rValue := rValue.Components[0] as TEBIntsValue;
         radRefValue.Checked := true;
         selIndex.ID := rValue.Values[0];
      end;
   end
   else if rValue is TEBCall then
   begin
      radFunction.Checked := true;
      LoadFunction(TEBCall(rValue));
   end
   else if rValue is TEBLookupObjExpr then
   begin
      radProperty.Checked := true;
   end;
end;

{initialization
   RegisterEbEditor(TEBGlobalInt, TfrmEbSetInteger);}
end.
