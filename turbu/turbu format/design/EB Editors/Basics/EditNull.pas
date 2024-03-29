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

unit EditNull;

interface

uses
   Forms, Messages, StdCtrls, ExtCtrls, Classes, Controls,
   EventBuilder, EbEdit;

const WM_HIDE = WM_USER + 1;

type
   //Ugly hack to create an "editor window" for types that don't need one.
   TfrmEBEditNull = class(TfrmEbEditBase)
   private
      procedure CMActivate(var message: TMessage); message CM_ACTIVATE;
      procedure WMHide(var message: TMessage); message WM_HIDE;
      procedure CMVisibleChanged(var Message: TMessage); message CM_VISIBLECHANGED;
   protected
      procedure UploadObject(obj: TEbObject); override;
      procedure DownloadObject(obj: TEbObject); override;
   end;

   TfrmEBEditNull<T: TEBObject> = class(TfrmEBEditNull)
   protected
      function NewClassType: TEbClass; override;
   end;


implementation
uses
   Windows;

{$R *.dfm}

{ TfrmEBEditNull }

procedure TfrmEBEditNull.CMActivate(var message: TMessage);
begin
   PostMessage(self.Handle, WM_HIDE, 0, 0);
end;

procedure TfrmEBEditNull.CMVisibleChanged(var Message: TMessage);
begin
end;

procedure TfrmEBEditNull.DownloadObject(obj: TEbObject);
begin
end;

procedure TfrmEBEditNull.UploadObject(obj: TEbObject);
begin
end;

procedure TfrmEBEditNull.WMHide(var message: TMessage);
begin
   self.ModalResult := mrOK;
end;

{ TfrmEBEditNull<T> }

function TfrmEBEditNull<T>.NewClassType: TEbClass;
begin
   result := T;
end;

end.
