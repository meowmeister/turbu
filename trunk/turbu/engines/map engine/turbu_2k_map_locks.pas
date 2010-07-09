unit turbu_2k_map_locks;

interface
uses
   SyncObjs;

var
   GEventLock: TCriticalSection;
   GMoveLock: TCriticalSection;

implementation

initialization
   GEventLock := TCriticalSection.Create;
   GMoveLock := TCriticalSection.Create;
finalization
   GEventLock.Free;
   GMoveLock.Free;
end.
