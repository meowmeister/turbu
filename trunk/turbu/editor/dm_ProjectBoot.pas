unit dm_ProjectBoot;

interface

uses
  SysUtils, Classes, DB, SqlExpr,
  turbu_map_engine, FMTBcd;

type
  TdmProjectBoot = class(TDataModule)
    Connection: TSQLConnection;
    dsBoot: TSQLQuery;
    dsBootEngineName: TWideStringField;
    dsBootversion: TIntegerField;
  public
    function Boot(const filename: string): IMapEngine;
  end;

implementation
uses
   Forms,
   turbu_engines, turbu_plugin_interface, turbu_versioning, logs;

{$R *.dfm}

{ TdmProjectBoot }

function TdmProjectBoot.Boot(const filename: string): IMapEngine;
begin
   connection.DriverName := 'Firebird';
   connection.Params.Values['User_Name'] := 'SYSDBA';
   connection.Params.Values['Password'] := 'masterkey';
   connection.Params.Values['Database'] := filename;
   connection.Params.Values['ServerCharSet'] := 'UTF8';
   try
      try
         connection.Open;
         dsBoot.Open;
         dsBoot.First;
         assert(not dsBoot.Eof);
         result := retrieveEngine(et_map, dsBootEngineName.Value, TVersion.Create(dsBootversion.Value)) as IMapEngine;
      except
         on E: Exception do
         begin
            logText(format('Database validation error: %s: %s', [E.ClassName, e.Message]));
            Application.MessageBox('This is not a valid TURBU database.', 'Database validation failed');
            Abort;
         end;
      end;
   finally
      dsBoot.Close;
      connection.Close;
   end;
end;

end.
