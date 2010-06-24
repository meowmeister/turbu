program TepRename;

uses
  SysUtils, Classes, Windows;

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

procedure run;
var
   path: string;
   filename: string;
   stream: TStream;
   data: RawByteString;
begin
   path := ExtractFilePath(ParamStr(0));
   filename := IncludeTrailingPathDelimiter(path) + 'map_default_design.tep';
   stream := TFileStream.Create(filename, fmOpenRead);
   setLength(data, stream.size);
   stream.Read(data[1], stream.size);
   stream.free;
   data := blobReplace(data, 'map_default.bpl', 'map_default.tep');
   stream := TFileStream.Create(filename, fmOpenWrite);
   stream.write(data[1], length(data));
   stream.free;
end;

begin
  try
   Run;
  except
    on E: Exception do
      MessageBox(0, PChar(E.Message), PChar(E.ClassType), 0);
  end;
end.
