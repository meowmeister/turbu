unit PackageRegistry;

interface

uses
  SysUtils, Windows, Classes,
  DeHL.Collections.List, DeHL.Collections.DistinctMultiMap;

type
  PPackageInfo = ^TPackageInfo;
  TPackageInfo = record
    PackageFileName: string;
    PackageHandle: HMODULE;
    GroupID: Integer;
  end;

  { TPackageList }
  TPackageList = class
  private
    FList: TList<PPackageInfo>;
    FInitCounter: TDistinctMultimap<HMODULE, PPackageInfo>;
    function GetCount: Integer;
    function GetItem(Index: Integer): PPackageInfo;
    function NewPackage(name: string): PPackageInfo;
    procedure InitializePackage(item: PPackageInfo);
    procedure FinalizePackage(item: PPackageInfo);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function AddPackage(const FileName: string; handle: HMODULE; OrigHandle: HMODULE = 0): PPackageInfo;
    procedure RemovePackage(const FileName: string);
    procedure Verify;

    function FindPackage(const PackageFile: string): PPackageInfo; overload;
    function FindPackage(Module: HMODULE): PPackageInfo; overload;

    property Count: Integer read GetCount;
    property Item[Index: Integer]: PPackageInfo read GetItem; default;
  end;

var
  Packages: TPackageList;
  PackageGroup: Integer = 0;

procedure GetDependencies(Module: PPackageInfo; Strings: TStrings);

implementation

uses
  SysConst;

{ Package info structures }

type
  PPkgName = ^TPkgName;
  TPkgName = packed record
    HashCode: Byte;
    Name: array[0..255] of AnsiChar;
  end;

  { PackageUnitFlags:
    bit      meaning
    -----------------------------------------------------------------------------------------
    0      | main unit
    1      | package unit (dpk source)
    2      | $WEAKPACKAGEUNIT unit
    3      | original containment of $WEAKPACKAGEUNIT (package into which it was compiled)
    4      | implicitly imported
    5..7   | reserved
  }
  PUnitName = ^TUnitName;
  TUnitName = packed record
    Flags : Byte;
    HashCode: Byte;
    Name: array[0..255] of AnsiChar;
  end;

  { Package flags:
    bit     meaning
    -----------------------------------------------------------------------------------------
    0     | 1: never-build                  0: always build
    1     | 1: design-time only             0: not design-time only      on => bit 2 = off
    2     | 1: run-time only                0: not run-time only         on => bit 1 = off
    3     | 1: do not check for dup units   0: perform normal dup unit check
    4..25 | reserved
    26..27| (producer) 0: pre-V4, 1: undefined, 2: c++, 3: Pascal
    28..29| reserved
    30..31| 0: EXE, 1: Package DLL, 2: Library DLL, 3: undefined
  }
  PPackageInfoHeader = ^TPackageInfoHeader;
  TPackageInfoHeader = packed record
    Flags: Cardinal;
    RequiresCount: Integer;
    {Requires: array[0..9999] of TPkgName;
    ContainsCount: Integer;
    Contains: array[0..9999] of TUnitName;}
  end;

function PackageInfoTable(Module: HMODULE): PPackageInfoHeader; overload;
var
  ResInfo: HRSRC;
  Data: THandle;
begin
  Result := nil;
  ResInfo := FindResource(Module, 'PACKAGEINFO', RT_RCDATA);
  if ResInfo <> 0 then
  begin
    Data := LoadResource(Module, ResInfo);
    if Data <> 0 then
    try
      Result := LockResource(Data);
      UnlockResource(Data);
    finally
      FreeResource(Data);
    end;
  end;
end;


procedure GetDependencies(Module: PPackageInfo; Strings: TStrings);
var
  InfoTable: PPackageInfoHeader;
  I: Integer;
  PkgName: PPkgName;
begin
  InfoTable := PackageInfoTable(Module.PackageHandle);
  if not Assigned(InfoTable) then
    raise EPackageError.CreateFmt(SCannotReadPackageInfo,
      [ExtractFileName(Module.PackageFileName)]);

  PkgName := PPkgName(Integer(InfoTable) + SizeOf(InfoTable^));
  for I := 0 to InfoTable.RequiresCount - 1 do
  begin
    strings.add(string(PkgName.Name));
    Inc(Integer(PkgName), StrLen(PkgName.Name) + 2);
  end;
end;

function GetGroupID: Integer;
begin
  Result := InterlockedIncrement(PackageGroup);
end;

{ TPackageList }

function TPackageList.NewPackage(name: string): PPackageInfo;
begin
  New(Result);
  Result.PackageFileName := name;
  Result.GroupID := GetGroupID;
  FList.Add(Result);
end;

function TPackageList.AddPackage(const FileName: string; handle: HMODULE; OrigHandle: HMODULE = 0): PPackageInfo;
var
   list: TStringList;
   package: string;
   dep: PPackageInfo;
begin
  assert(FindPackage(FileName) = nil);
  result := NewPackage(FileName);
  result.PackageHandle := handle;
  if OrigHandle = 0 then
    OrigHandle := handle;
  OutputDebugString(PChar('Adding package ' + filename));

  list := TStringList.Create;
  try
    GetDependencies(result, list);
    for package in list do
      if UpperCase(ExtractFileExt(package)) = '.TEP' then
      begin
        dep := FindPackage(package);
        if dep = nil then
          dep := AddPackage(package, Windows.GetModuleHandle(PChar(package)), OrigHandle);
        if not FInitCounter.KeyHasValue(OrigHandle, dep) then
        begin
           InitializePackage(dep);
           FInitCounter.Add(OrigHandle, dep);
        end;
      end;
  finally
    list.Free;
  end;
end;

procedure TPackageList.RemovePackage(const FileName: string);
var
   info, subInfo: PPackageInfo;
begin
  OutputDebugString(PChar('Removing package ' + filename));

  info := FindPackage(FileName);
  try
    // Reversed to finalize in reverse order of initialization
    for subInfo in FInitCounter[info.PackageHandle].Reversed do
      FinalizePackage(subInfo);
  finally
    FList.Remove(info);
    FInitCounter.Remove(info.PackageHandle);
    Dispose(info);
  end;
end;

procedure TPackageList.Verify;
var
  i: Integer;
begin
  for i := FList.Count - 1 downto 0 do
    if GetModuleHandle(PChar(FList[i].PackageFileName)) = 0 then
    begin
      Dispose(FList[i]);
      FList.RemoveAt(i);
    end;
end;

procedure TPackageList.Clear;
var
  I: Integer;
begin
  for I := 0 to FList.Count - 1 do
    Dispose(PPackageInfo(FList[I]));
  FList.Clear;
end;

constructor TPackageList.Create;
begin
  inherited;
  FList := TList<PPackageInfo>.Create;
  FInitCounter := TDistinctMultimap<HMODULE, PPackageInfo>.Create;
end;

destructor TPackageList.Destroy;
begin
  Clear;
  FList.Free;
  FInitCounter.Free;
  inherited;
end;

function TPackageList.FindPackage(const PackageFile: string): PPackageInfo;
var
  info: PPackageInfo;
begin
  for info in FList do
    if info.PackageFileName = PackageFile then
      exit(info);
  Result := nil;
end;

procedure TPackageList.InitializePackage(item: PPackageInfo);
begin
   OutputDebugString(PChar('Initializing ' + item.PackageFileName));
   SysUtils.InitializePackage(item.PackageHandle);
end;

procedure TPackageList.FinalizePackage(item: PPackageInfo);
begin
   OutputDebugString(PChar('Finalizing ' + item.PackageFileName));
   SysUtils.FinalizePackage(item.PackageHandle);
end;

function TPackageList.FindPackage(Module: HMODULE): PPackageInfo;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FList.Count - 1 do
    if PPackageInfo(FList[I]).PackageHandle = Module then
    begin
      Result := FList[I];
      Exit;
    end;
end;

function TPackageList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TPackageList.GetItem(Index: Integer): PPackageInfo;
begin
  Result := FList[Index];
end;

initialization
  Packages := TPackageList.Create;

finalization
  Packages.Free;

end.
