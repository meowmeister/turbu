unit ClipboardWatcher;

interface
uses
   Classes;

procedure RegisterClipboardViewer(const viewer: TNotifyEvent);
procedure UnregisterClipboardViewer(const viewer: TNotifyEvent);

implementation
uses
   Windows, Messages, Generics.Collections, Forms;

type
   TClipboardWatcher = class(TObject)
   private
      FHandle: HWND;
      FNext: HWND;
      FList: TList<TNotifyEvent>;
      procedure MainWndProc(var Msg: TMessage);
      procedure ChangeChain(Remove, Next: THandle);
      procedure Broadcast;
   public
      constructor Create;
      destructor Destroy; override;
      procedure RegisterViewer(const viewer: TNotifyEvent);
      procedure UnregisterViewer(const viewer: TNotifyEvent);
   end;

var
   watcher: TClipboardWatcher;

{ TClipboardWatcher }

constructor TClipboardWatcher.Create;
begin
   FHandle := AllocateHWnd(self.MainWndProc);
   FList := TList<TNotifyEvent>.Create;
   FNext := SetClipboardViewer(FHandle);
end;

destructor TClipboardWatcher.Destroy;
begin
   DeallocateHWnd(FHandle);
   FList.Free;
   ChangeClipboardChain(FHandle, FNext);
end;

procedure TClipboardWatcher.Broadcast;
var
   event: TNotifyEvent;
begin
   for event in FList do
      Event(self);
end;

procedure TClipboardWatcher.ChangeChain(Remove, Next: THandle);
begin
   if FNext = Remove then
      FNext := Next
   else if FNext <> 0 then
      SendMessage(FNext, WM_ChangeCBChain, Remove, Next);
end;

procedure TClipboardWatcher.MainWndProc(var Msg: TMessage);
begin
   try
      case Msg.Msg of
         WM_DRAWCLIPBOARD: Broadcast;
         WM_CHANGECBCHAIN: ChangeChain(msg.WParam, Msg.LParam);
      end;
   except
      Application.HandleException(Self);
   end;
end;

procedure TClipboardWatcher.RegisterViewer(const viewer: TNotifyEvent);
begin
   FList.Add(viewer);
end;

procedure TClipboardWatcher.UnregisterViewer(const viewer: TNotifyEvent);
begin
   FList.Remove(Viewer);
end;

procedure RegisterClipboardViewer(const viewer: TNotifyEvent);
begin
   watcher.RegisterViewer(viewer);
end;

procedure UnregisterClipboardViewer(const viewer: TNotifyEvent);
begin
   watcher.UnregisterViewer(viewer);
end;

initialization
   watcher := TClipboardWatcher.Create;
finalization
   watcher.Free;
end.
