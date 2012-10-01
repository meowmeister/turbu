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

unit delayedAction;

interface
uses
  SysUtils;

procedure DelayExec(const action: TProc);

implementation
uses
   Windows, Messages, Classes, Forms;

type
   TDelayQueue = class
   private
      FHandle: HWND;
      procedure MainWndProc(var Msg: TMessage);
   public
     constructor Create;
     destructor Destroy; override;
   end;

{ TDelayQueue }

constructor TDelayQueue.Create;
begin
   FHandle := AllocateHWnd(self.MainWndProc);
end;

destructor TDelayQueue.Destroy;
begin
   DeallocateHWnd(FHandle);
   inherited Destroy;
end;

Procedure AnonProcPerform(value: WPARAM);
var
  PProc : ^TProc;
  Proc : TProc;
begin
  PProc := Pointer(value);
  Proc := PProc^;
  Dispose(PProc);
  Proc();
end;

procedure TDelayQueue.MainWndProc(var Msg: TMessage);
begin
   try
      if Msg.Msg = WM_USER then
         AnonProcPerform(msg.WParam)
      else
         DefWindowProc(FHandle, msg.Msg, msg.WParam, msg.LParam);
   except
      Application.HandleException(Self);
   end;
end;

var
   delay: TDelayQueue;

Procedure DelayExec( const action: TProc);
var
  PProc: ^TProc;
begin
  New(PProc);
  PProc^ := action;
  PostMessage(delay.FHandle, WM_USER, WPARAM(PProc), 0);
end;

initialization
   delay := TDelayQueue.Create;
finalization
   delay.Free;
end.
