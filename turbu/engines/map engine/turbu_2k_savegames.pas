unit turbu_2k_savegames;

interface

procedure SaveTo(const filename: string; mapID: integer; explicit: boolean);

implementation
uses
   SysUtils, IOUtils,
   Windows, Diagnostics,
   turbu_2k_environment,
   dwsJSON;

procedure SaveTo(const filename: string; mapID: integer; explicit: boolean);
var
   writer: TdwsJSONWriter;
   timer: TStopwatch;
begin
   timer := TStopwatch.StartNew;
   writer := TdwsJSONWriter.Create(nil);
   try
      writer.BeginObject;
         writer.WriteName('Map');
         writer.WriteInteger(mapID);
         writer.WriteName('Environment');
         GEnvironment.Serialize(writer, explicit);
      writer.EndObject;
      TFile.WriteAllText(filename, writer.ToString, TEncoding.UTF8);
      timer.Stop;
      OutputDebugString(PChar(format('Saved to %s in %d milliseconds', [filename, timer.ElapsedMilliseconds])));
   finally
      writer.Free;
   end;
end;

end.
