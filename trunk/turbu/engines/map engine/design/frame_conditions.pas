unit frame_conditions;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, DB, DBClient, DBCtrls,
  dm_database, DBIndexComboBox, Mask, JvExMask, JvSpin, JvDBSpinEdit;

type
  TframeConditions = class(TFrame)
    gbxConditions: TGroupBox;
    lblSwitch1On: TLabel;
    chkSwitch1: TDbCheckBox;
    chkSwitch2: TDbCheckBox;
    chkVar1: TDbCheckBox;
    spnVar1: TJvDbSpinEdit;
    cboVarOp1: TDBIndexComboBox;
    chkItem: TDbCheckBox;
    chkHero: TDbCheckBox;
    chkVar2: TDbCheckBox;
    spnVar2: TJvDbSpinEdit;
    chkTimer1: TDbCheckBox;
    spnSeconds1: TJvDbSpinEdit;
    spnMinutes1: TJvDBSpinEdit;
    chkTimer2: TDbCheckBox;
    spnMinutes2: TJvDbSpinEdit;
    spnSeconds2: TJvDbSpinEdit;
    cboVarOp2: TDBIndexComboBox;
    cboItem: TDBLookupComboBox;
    cboHero: TDBLookupComboBox;
    cboTimerOp1: TDBIndexComboBox;
    cboTimerOp2: TDBIndexComboBox;
    DBLookupComboBox5: TDBLookupComboBox;

    dsConditions: TClientDataSet;
    dsConditionsMaster: TIntegerField;
    dsConditionsSwitch1: TIntegerField;
    dsConditionsSwitch2: TIntegerField;
    dsConditionsVariable1: TIntegerField;
    dsConditionsVariable2: TIntegerField;
    dsConditionsVar1Op: TByteField;
    dsConditionsScript: TWideStringField;
    dsConditionsbSwitch1: TBooleanField;
    srcConditions: TDataSource;
    btnSwitch1: TButton;
    txtSwitch1: TDBEdit;
    btnSwitch2: TButton;
    txtSwitch2: TDBEdit;
    btnVar1: TButton;
    txtVar1: TDBEdit;
    txtVar2: TDBEdit;
    btnVar2: TButton;
    procedure EnableControls(const controls: array of TControl; enabled: boolean);
    procedure chkSwitch1Click(Sender: TObject);
    procedure chkSwitch2Click(Sender: TObject);
    procedure chkVar1Click(Sender: TObject);
    procedure chkVar2Click(Sender: TObject);
    procedure chkItemClick(Sender: TObject);
    procedure chkHeroClick(Sender: TObject);
    procedure chkTimer1Click(Sender: TObject);
    procedure chkTimer2Click(Sender: TObject);
    function EditSwitches(id: integer): integer;
    function EditVars(id: integer): integer;
    procedure btnSwitch1Click(Sender: TObject);
    procedure btnSwitch2Click(Sender: TObject);
    procedure btnVar1Click(Sender: TObject);
    procedure btnVar2Click(Sender: TObject);
  private
    { Private declarations }
    procedure SetFieldValue(field: TIntegerField; value: integer);
  public
    { Public declarations }
  end;

implementation
uses
   RTTI,
   commons,
   array_editor;

{$R *.dfm}

function TframeConditions.EditSwitches(id: integer): integer;
begin
   TfrmArrayEdit.Edit('Switch', 'boolean', dmDatabase.Switches, id);
   result := id;
end;

function TframeConditions.EditVars(id: integer): integer;
begin
   TfrmArrayEdit.Edit('Variable', 'integer', dmDatabase.Variables, id);
   result := id;
end;

procedure TframeConditions.btnSwitch1Click(Sender: TObject);
begin
   SetFieldValue(dsConditionsSwitch1, EditSwitches(dsConditionsSwitch1.Value));
end;

procedure TframeConditions.btnSwitch2Click(Sender: TObject);
begin
   SetFieldValue(dsConditionsSwitch2, EditSwitches(dsConditionsSwitch2.Value));
end;

procedure TframeConditions.btnVar1Click(Sender: TObject);
begin
   SetFieldValue(dsConditionsVariable1, EditVars(dsConditionsVariable1.Value));
end;

procedure TframeConditions.btnVar2Click(Sender: TObject);
begin
   SetFieldValue(dsConditionsVariable2, EditVars(dsConditionsVariable2.Value));
end;

procedure TframeConditions.chkHeroClick(Sender: TObject);
begin
   EnableControls([cboHero], chkHero.checked);
end;

procedure TframeConditions.chkItemClick(Sender: TObject);
begin
   EnableControls([cboItem], chkItem.checked);
end;

procedure TframeConditions.chkSwitch1Click(Sender: TObject);
begin
   EnableControls([txtSwitch1, btnSwitch1], chkSwitch1.checked);
end;

procedure TframeConditions.chkSwitch2Click(Sender: TObject);
begin
   EnableControls([txtSwitch2, btnSwitch2], chkSwitch2.checked);
end;

procedure TframeConditions.chkTimer1Click(Sender: TObject);
begin
   EnableControls([cboTimerOp1, spnSeconds1, spnMinutes1], chkTimer1.checked);
end;

procedure TframeConditions.chkTimer2Click(Sender: TObject);
begin
   EnableControls([cboTimerOp2, spnSeconds2, spnMinutes2], chkTimer2.checked);
end;

procedure TframeConditions.chkVar1Click(Sender: TObject);
begin
   EnableControls([txtVar1, btnVar1, cboVarOp1, spnVar1], chkVar1.checked);
end;

procedure TframeConditions.chkVar2Click(Sender: TObject);
begin
   EnableControls([txtVar2, btnVar2, cboVarOp2, spnVar2], chkVar2.checked);
end;

procedure TframeConditions.EnableControls(const controls: array of TControl;
  enabled: boolean);
var
   control: TControl;
   context: TRttiContext;
   src: TRttiProperty;
begin
   commons.EnableControls(controls, enabled);
   for control in controls do
   begin
      src := context.GetType(control.ClassType).GetProperty('DataSource');
      if assigned(src) then
         if enabled then
            src.SetValue(control, TValue.From(self.srcConditions))
         else src.SetValue(control, TValue.Empty);
   end;
end;

procedure TframeConditions.SetFieldValue(field: TIntegerField; value: integer);
begin
   if value <> field.Value then
   begin
      field.Dataset.Edit;
      field.Value := value;
      field.Dataset.Post;
   end;
end;

end.
