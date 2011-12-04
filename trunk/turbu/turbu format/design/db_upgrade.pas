unit db_upgrade;

interface
uses
   dm_database;

procedure UpgradeDatabase(database: TdmDatabase; const filename: string);

implementation
uses
   Types, DB, SimpleDS, DBXCommon, Generics.collections, SysUtils, IOUtils, Classes,
   EventBuilder, EB_Maps, EB_Expressions, conversion_report, conversion_report_form;

type
   TDBUpgradeProc = procedure(database: TdmDatabase; const path: string; const report: IConversionReport);
var
   table: TDictionary<integer, TDBUpgradeProc>;

function UpgradeStep(database: TdmDatabase; const path: string; const report: IConversionReport): boolean;
var
   ds: TDataset;
   version: integer;
   proc: TDBUpgradeProc;
begin
   ds := database.dbData;
   TSimpleDataset(ds).DataSet.CommandText := UpperCase(TSimpleDataset(ds).DataSet.CommandText);
   ds.Active := true;
   version := ds.FieldByName('id').AsInteger;
   if table.TryGetValue(version, proc) then
   begin
      proc(database, path, report);
      ds.Edit;
      ds.FieldByName('id').AsInteger := version + 1;
      ds.Post;
      result := true;
      report.setCurrentTask('Saving data');
      database.SaveAll(
         procedure (name: string)
         begin
            report.newStep(name);
         end);
   end
   else result := false;
end;

procedure UpgradeDatabase(database: TdmDatabase; const filename: string);
var
   path: string;
   thread: TThread;
begin
   frmConversionReport := TfrmConversionReport.Create(nil);
   frmConversionReport.Caption := 'Database upgrade progress';
   frmConversionReport.btnDone.Visible := false;
   frmConversionReport.thread := TThread.CreateAnonymousThread(
     procedure
     begin
      path := ExtractFilePath(filename);
      repeat
      until UpgradeStep(database, path, frmConversionReport) = false;
      (frmConversionReport as IConversionReport).makeReport;
     end);
   try
      frmConversionReport.ShowModal;
   finally
      FreeAndNil(frmConversionReport);
   end;
end;

type
   TEBUpdateProc = procedure (var obj: TEBObject);

procedure rScan(obj, parent: TEBObject; cls: TEBObjectClass; handler: TEBUpdateProc);
var
   old, child: TEBObject;
   idx: integer;
begin
   if obj.ClassType = cls then
   begin
      old := obj;
      handler(obj);
      if old <> obj then
      begin
         idx := parent.children.IndexOf(old);
         parent.children.Extract(old);
         parent.Children.Add(obj);
         parent.children.Extract(obj);
         parent.children.Insert(idx, obj);
         old.Free;
      end;
   end;
   for child in obj do
      rScan(child, obj, cls, handler);
end;

procedure InternalScanEvents(cls: TEBObjectClass; handler: TEBUpdateProc; const filename: string);
var
   stream: TStream;
   obj: TEBObject;
   script: utf8String;
begin
    stream := TFile.OpenRead(filename);
    obj := nil;
    try
       obj := TEBObject.LoadFromStream(stream);
       rScan(obj, nil, cls, handler);
       script := utf8String(obj.Serialize);
       stream.Free;
       stream := TFile.Open(filename, TFileMode.fmCreate);
       stream.write(script[1], length(script));
    finally
       stream.Free;
       obj.Free;
    end;
end;

procedure InternalScanField(cls: TEBObjectClass; handler: TEBUpdateProc; field: TWideMemoField);
var
   stream: TStream;
   obj: TEBObject;
begin
    stream := TStringStream.Create(field.AsString);
    obj := nil;
    try
       obj := TEBObject.LoadFromStream(stream);
       rScan(obj, nil, cls, handler);
       field.DataSet.Edit;
       field.AsString := obj.Serialize;
       field.DataSet.Post;
    finally
       stream.Free;
       obj.Free;
    end;
end;

procedure ScanEvents(database: TdmDatabase; cls: TEBObjectClass; handler: TEBUpdateProc;
   const path: string; const report: IConversionReport);
var
   name, filename: string;
   filenames: TStringDynArray;
   events: TSimpleDataset;
   field: TWideMemoField;
begin
   name := cls.ClassName;
   filenames := TDirectory.GetFiles(TPath.Combine(path, 'scripts'));
   report.setCurrentTask(format('Updating script object %s', [cls.ClassName]), length(filenames) + 1);
   for filename in filenames do
   begin
      report.newStep(TPath.GetFileNameWithoutExtension(filename));
      if pos(name, TFile.ReadAllText(filename)) > 0 then
         InternalScanEvents(cls, handler, filename)
   end;

   report.newStep('Battle scripts');
   events := database.mparties_events as TSimpleDataset;
   events.DataSet.CommandText := UpperCase(events.DataSet.CommandText);
   events.Active := true;
   field := events.FieldByName('eventText') as TWideMemoField;
   events.First;
   while not events.Eof do
   begin
      if pos(name, field.AsString) <> 0 then
         InternalScanField(cls, handler, field);
      events.Next;
   end;
end;

{examples}
procedure UpdateShakeScreen(var obj: TEBObject);
begin
   assert(obj.ClassType = TEBShakeScreen);
   if obj.Values.Count < 5 then
      obj.Values.Add(0)
   else if obj.Values[4] = 2 then
      obj := TEBEndShake.Create(nil);
end;

procedure UpdateArraySkillsName(var obj: TEBObject);
begin
   assert(obj.ClassType = TEBObjArrayValue);
   if obj.text = 'skills' then
      obj.Text := 'skill';
end;

initialization
   table := TDictionary<integer, TDBUpgradeProc>.Create;
finalization
   table.Free;
end.
