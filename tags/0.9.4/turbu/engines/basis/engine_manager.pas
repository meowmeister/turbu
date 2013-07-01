unit engine_manager;

interface

uses
  SysUtils, Classes, JvComponentBase, JvPluginManager;

type
  TdmEngineManager = class(TDataModule)
    pluginManager: TJvPluginManager;
    procedure pluginManagerAfterLoad(Sender: TObject; FileName: string;
      const ALibHandle: Cardinal; var AllowLoad: Boolean);
    procedure pluginManagerBeforeUnload(Sender: TObject; FileName: string;
      const ALibHandle: Cardinal);
    procedure pluginManagerAfterUnload(Sender: TObject; FileName: string);
  end;

var
  dmEngineManager: TdmEngineManager;

implementation
uses
   Forms, RTTI,
   PackageRegistry;

{$R *.dfm}

procedure TdmEngineManager.pluginManagerAfterLoad(Sender: TObject; FileName: string;
  const ALibHandle: Cardinal; var AllowLoad: Boolean);
begin
   Packages.AddPackage(FileName, ALibHandle);
end;

procedure TdmEngineManager.pluginManagerAfterUnload(Sender: TObject;
  FileName: string);
begin
   Packages.Verify;
end;

//Before unloading a package, ensure that TThread.CurrentThread isn't holding
//a reference to an anonymous method that's part of this package's address space.
//(TThread.synchronize should clear this, but it doesn't.)
procedure ClearSyncRec;
var
   thread: TThread;
   ctx: TRttiContext;
   sync: TRttiField;
   val: TValue;
begin
   ctx := TRttiContext.Create;
   thread := TThread.CurrentThread;
   sync := ctx.GetType(TThread).GetField('FSynchronize');
   val := sync.GetValue(thread);
   sync.FieldType.GetField('FProcedure').SetValue(val.GetReferenceToRawData, nil);
   sync.SetValue(thread, val);
end;

procedure TdmEngineManager.pluginManagerBeforeUnload(Sender: TObject;
  FileName: string; const ALibHandle: Cardinal);
begin
   ClearSyncRec;
   Packages.RemovePackage(FileName);
end;

initialization
  dmEngineManager := TdmEngineManager.Create(Application);
end.
