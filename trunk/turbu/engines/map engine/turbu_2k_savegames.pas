unit turbu_2k_savegames;

interface

procedure SaveTo(const filename: string; mapID: integer; explicit: boolean);
procedure Load(const filename: string);

implementation
uses
   SysUtils, IOUtils, Windows, Diagnostics,
   logs,
   turbu_2k_environment, turbu_2k_map_engine, turbu_classes,
   rs_media, rs_message,
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
         writer.WriteName('Sound');
         rs_media.SerializeSound(writer);
         writer.WriteName('Messages');
         rs_message.SerializeMessageState(writer);
      writer.EndObject;
      TFile.WriteAllText(filename, writer.ToString, TEncoding.UTF8);
      timer.Stop;
      OutputDebugString(PChar(format('Saved to %s in %d milliseconds', [filename, timer.ElapsedMilliseconds])));
   finally
      writer.Free;
   end;
end;

procedure Load(const filename: string);
var
   obj: TdwsJSONObject;
   value: TdwsJSONValue;
begin
   obj := TdwsJSONObject.ParseFile(filename) as TdwsJSONObject;
   if obj = nil then
      raise Exception.Create('Invalid save file');
   try
      value := obj.Items['Map'];
      GGameEngine.loadMap(value.AsInteger);
      value.Free;
      value := obj.Items['Environment'];
      GEnvironment.Deserialize(value as TdwsJSONObject);
      if value.ElementCount = 0 then
         value.free;
      value := obj.Items['Sound'];
      if assigned(value) then
      begin
         rs_media.DeserializeSound(value as TdwsJSONObject);
         value.Free;
      end;
      value := obj.Items['Messages'];
      if assigned(value) then
      begin
         rs_message.DeserializeMessageState(value as TdwsJSONObject);
         value.Free;
      end;

      obj.checkEmpty;
   finally
      obj.Free;
   end;
end;

end.
