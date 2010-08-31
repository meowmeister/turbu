unit variable_selector;

interface
uses
   Classes, StdCtrls, Controls, DBClient;

type
   TVarSelector = class(TCustomEdit)
   private
      FButton: TButton;
      FID: integer;
      FVarName: string;
      procedure ButtonClick(Sender: TObject);
      procedure UpdateText;
      procedure SetID(const Value: integer);
   protected
      procedure SetParent(AParent: TWinControl); override;
      function GetName: string; virtual; abstract;
      function GetDset: TClientDataset; virtual; abstract;
   public
      constructor Create(AOwner: TComponent); override;
      property ID: integer read FID write SetID;
      property varname: string read FVarName;
   end;

   TSwitchSelector = class(TVarSelector)
      function GetName: string; override;
      function GetDset: TClientDataset; override;
   end;

   TIntSelector = class(TVarSelector)
      function GetName: string; override;
      function GetDset: TClientDataset; override;
   end;

procedure Register;

implementation
{$IFNDEF COMPONENT}
uses
   array_editor, dm_database;
{$ENDIF}

procedure Register;
begin
   RegisterComponents('TURBU', [TIntSelector, TSwitchSelector]);
end;

{ TVarSelector }

constructor TVarSelector.Create(AOwner: TComponent);
begin
   inherited Create(AOwner);
   self.ReadOnly := true;
   self.ID := 0;
end;

procedure TVarSelector.SetID(const Value: integer);
begin
   FID := Value;
{$IFNDEF COMPONENT}
   UpdateText;
{$ELSE}
   self.Caption := '';
{$ENDIF}
end;

procedure TVarSelector.SetParent(AParent: TWinControl);
begin
   inherited SetParent(AParent);
   if not assigned(FButton) then
   begin
      FButton := TButton.Create(self);
      FButton.Parent := self;
      FButton.Width := 22;
      FButton.Height := 22;
      FButton.Align := alRight;
      FButton.Caption := '...';
      FButton.OnClick := self.ButtonClick;
   end;
end;

procedure TVarSelector.UpdateText;
begin
{$IFNDEF COMPONENT}
   if FID >= 0 then
      Self.caption := GetDset.Lookup('id', FID, 'DisplayName')
   else begin
      FVarname := TfrmArrayEdit.VariableContext.Lookup('id', -FID, 'name');
      Self.Caption := 'Local: ' + FVarname;
   end;
{$ENDIF}
end;

procedure TVarSelector.ButtonClick(Sender: TObject);
begin
{$IFNDEF COMPONENT}
   TfrmArrayEdit.Edit(GetName, 'integer', GetDset, FID);
   UpdateText;
{$ENDIF}
end;

{ TSwitchSelector }

function TSwitchSelector.GetDset: TClientDataset;
begin
{$IFNDEF COMPONENT}
   result := dmDatabase.Switches;
{$ENDIF}
end;

function TSwitchSelector.GetName: string;
begin
   result := 'Switch';
end;

{ TIntSelector }

function TIntSelector.GetDset: TClientDataset;
begin
{$IFNDEF COMPONENT}
   result := dmDatabase.Variables;
{$ENDIF}
end;

function TIntSelector.GetName: string;
begin
   result := 'Variable';
end;

end.
