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

unit variable_selector;

interface
uses
   Messages, Classes, StdCtrls, Controls, DBClient;

type
   TVarSelector = class(TCustomEdit)
   private
      FButton: TButton;
      FID: integer;
      FVarName: string;
      procedure ButtonClick(Sender: TObject);
      procedure UpdateText;
      procedure SetID(const Value: integer);
      procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
   protected
      procedure SetParent(AParent: TWinControl); override;
      function GetName: string; virtual; abstract;
      function GetDset: TCustomClientDataset; virtual; abstract;
   public
      constructor Create(AOwner: TComponent); override;
      property ID: integer read FID write SetID;
      property varname: string read FVarName;
   published
      property Enabled;
   end;

   TSwitchSelector = class(TVarSelector)
      function GetName: string; override;
      function GetDset: TCustomClientDataset; override;
   end;

   TIntSelector = class(TVarSelector)
      function GetName: string; override;
      function GetDset: TCustomClientDataset; override;
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

procedure TVarSelector.CMEnabledChanged(var Message: TMessage);
begin
   inherited;
   FButton.Enabled := self.Enabled;
end;

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

function TSwitchSelector.GetDset: TCustomClientDataset;
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

function TIntSelector.GetDset: TCustomClientDataset;
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
