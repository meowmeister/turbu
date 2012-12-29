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

unit button_edit;

interface
uses
   Messages, Classes, Controls, StdCtrls;

type
   TRpgCustomButtonEdit = class(TCustomEdit)
   private
      FButton: TButton;
      procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
   protected
      procedure ButtonClick(Sender: TObject); virtual; abstract;
      procedure SetParent(AParent: TWinControl); override;
   public
      constructor Create(AOwner: TComponent); override;
   published
      property Enabled;
   end;

   TRpgButtonEdit = class(TRpgCustomButtonEdit)
   private
      FOnClick: TNotifyEvent;
   protected
      procedure ButtonClick(Sender: TObject); override;
   published
      property OnButtonClick: TNotifyEvent read FOnClick write FOnClick;
   end;

procedure Register;

implementation

procedure Register;
begin
   RegisterComponents('TURBU', [TRpgButtonEdit]);
end;

{ TRpgCustomButtonEdit }

procedure TRpgCustomButtonEdit.CMEnabledChanged(var Message: TMessage);
begin
   inherited;
   FButton.Enabled := self.Enabled;
end;

constructor TRpgCustomButtonEdit.Create(AOwner: TComponent);
begin
   inherited Create(AOwner);
   self.ReadOnly := true;
   self.Text := '';
end;

procedure TRpgCustomButtonEdit.SetParent(AParent: TWinControl);
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

{ TRpgButtonEdit }

procedure TRpgButtonEdit.ButtonClick(Sender: TObject);
begin
   if assigned(FOnClick) then
      FOnClick(sender);
end;

end.
