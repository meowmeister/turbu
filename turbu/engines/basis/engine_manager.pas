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
   Forms,
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

procedure TdmEngineManager.pluginManagerBeforeUnload(Sender: TObject;
  FileName: string; const ALibHandle: Cardinal);
begin
   Packages.RemovePackage(FileName);
end;

initialization
  dmEngineManager := TdmEngineManager.Create(Application);
end.
