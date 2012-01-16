program TepRename;

uses
  SysUtils, Classes, Windows, IOUtils;

function BlobMatch(input: PByte; key: AnsiString): boolean;
var
   i: Integer;
   keyPtr: PByte;
begin
   keyPtr := @key[1];
   result := true;
   for i := 1 to length(key) do
   begin
      if input^ <> keyPtr^ then
         Exit(false);
      inc(input);
      inc(keyPtr);
   end;
end;

function BlobPos(const input, key: AnsiString; startFrom: integer): integer;
begin
   result := startFrom;
   if (key = '') then
      Exit(0);
   while result <= length(input) do
   begin
      if (input[result] = key[1]) and (BlobMatch(@input[result], key)) then
         exit
      else inc(result);
   end;
   result := 0;
end;

function BlobReplace(const input, key, newtext: AnsiString): AnsiString;
var
   index, oldIndex: integer;
begin
   result := '';
   index := 1;
   oldIndex := 1;
   while index <> 0 do
   begin
      index := BlobPos(input, key, oldIndex);
      if index = 0 then
         result := result + copy(input, oldIndex, MAXINT)
      else begin
         result := result + copy(input, oldIndex, index - oldIndex) + newtext;
         oldindex := index + length(key);
      end;
   end;
end;

function Match(const path, pattern: string): TStringList;
var
   item: string;
begin
   result := TStringList.Create;
   for item in TDirectory.GetFiles(path, pattern) do
      result.Add(item);
end;

procedure Patch(const filename: string; files: TStringList);
var
   stream: TStream;
   data: RawByteString;
   item: string;
   itemBase: AnsiString;
begin
   if not FileExists(filename) then
      Exit;
   stream := TFileStream.Create(filename, fmOpenRead);
   setLength(data, stream.size);
   stream.Read(data[1], stream.size);
   stream.free;
   for item in files do
   begin
      if item = filename then
         Continue;
      itemBase := AnsiString(ChangeFileExt(ExtractFileName(item), ''));
      data := blobReplace(data, itemBase + '.bpl', itemBase + '.tep');
   end;
   stream := TFileStream.Create(filename, fmOpenWrite);
   stream.write(data[1], length(data));
   stream.free;
end;

procedure run;
var
   path: string;
   filename: string;
   files: TStringList;
begin
   path := ExtractFilePath(ParamStr(0));
   files := Match(path, '*.tep');
   for filename in files do
      Patch(filename, files);
   Patch(TPath.Combine(path, 'Turbu.exe'), files);
   Patch(TPath.Combine(path, 'Turbu_player.exe'), files);
   files.free;
end;

begin
  try
   Run;
  except
    on E: Exception do
    begin
      MessageBox(0, PChar(E.Message), PChar(E.ClassName), 0);
      Halt(nativeInt(E.ClassType));
    end;
  end;
end.
