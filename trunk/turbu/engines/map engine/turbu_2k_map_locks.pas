unit turbu_2k_map_locks;

interface
uses
   SyncObjs;

var
   GEventLock: TCriticalSection;
implementation

initialization
   GEventLock := TCriticalSection.Create;
finalization
   GEventLock.Free;
end.
