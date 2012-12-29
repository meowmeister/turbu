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
   Messages, Classes, StdCtrls, Controls, DBClient,
   button_edit;

type
   TCustomVarSelector = class(TRpgCustomButtonEdit)
   private
      FID: integer;
      FVarName: string;
      FLocals: TCustomClientDataset;
      procedure UpdateText;
      procedure SetID(const Value: integer);
    procedure SetVarName(const Value: string);
   protected
      procedure ButtonClick(Sender: TObject); override;
      function GetName: string; virtual; abstract;
      function GetDset: TCustomClientDataset; virtual; abstract;
   public
      constructor Create(AOwner: TComponent); override;
      property ID: integer read FID write SetID;
      property varname: string read FVarName write SetVarName;
   published
      property LocalContext: TCustomClientDataset read FLocals write FLocals;
   end;

   TVarSelector = class(TCustomVarSelector)
   private
      FGlobals: TCustomClientDataset;
   protected
      function GetDset: TCustomClientDataset; override;
      function GetName: string; override;
   published
      property GlobalContext: TCustomClientDataset read FGlobals write FGlobals;
   end;

procedure Register;

implementation
uses
   variants
   {$IFNDEF COMPONENT}, array_editor{$ENDIF};

procedure Register;
begin
   RegisterComponents('TURBU', [TVarSelector]);
end;

{ TCustomVarSelector }

constructor TCustomVarSelector.Create(AOwner: TComponent);
begin
   inherited Create(AOwner);
   self.ID := 0;
end;

procedure TCustomVarSelector.SetID(const Value: integer);
begin
   FID := Value;
   UpdateText;
end;

procedure TCustomVarSelector.SetVarName(const Value: string);
var
   id: variant;
begin
   assert(assigned(FLocals));
   id := FLocals.Lookup('name', value, 'id');
   assert(not VarIsNull(id));
   FID := -id;
   UpdateText;
end;

procedure TCustomVarSelector.UpdateText;
begin
   {$IFNDEF COMPONENT}
   if FID >= 0 then
   begin
      if GetDset <> nil then
         Self.caption := GetDset.Lookup('id', FID, 'Name')
      else self.Caption := '';
   end
   else if assigned(FLocals) then
   begin
      FVarname := FLocals.Lookup('id', -FID, 'name');
      Self.Caption := 'Local: ' + FVarname;
   end
   else {$ENDIF} Self.Caption := '';
end;

procedure TCustomVarSelector.ButtonClick(Sender: TObject);
begin
{$IFNDEF COMPONENT}
   TfrmArrayEdit.Edit(GetName, 'integer', GetDset, FLocals, FID);
   UpdateText;
{$ENDIF}
end;

{ TVarSelector }

function TVarSelector.GetDset: TCustomClientDataset;
begin
   result := FGlobals;
end;

function TVarSelector.GetName: string;
begin
   if assigned(FGlobals) then
      result := FGlobals.Name
   else result := '';
end;

end.
