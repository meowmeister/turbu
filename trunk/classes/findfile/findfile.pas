unit findfile;

interface

uses
  Classes;

type
   TFileAttrKind = (ffaReadOnly, ffaHidden, ffaSysFile, ffaDirectory, ffaArchive, ffaAnyFile);
   TFileAttr = set of TFileAttrKind;

   TFindFile = class(TObject)
   private
      s: TStringList;

      fSubFolder : boolean;
      fAttr: TFileAttr;
      FPath : string;
      FBasePath: string;
      fFileMask : string;
      FDepth: integer;

      procedure SetPath(Value: string);
      procedure FileSearch(const inPath : string);
      function cull(value: string): string;
   public
      constructor Create(path: string);
      destructor Destroy; override;

      function SearchForFiles: TStringList;

      property FileAttr: TFileAttr read fAttr write fAttr;
      property InSubFolders : boolean read fSubFolder write fSubFolder;
      property Path : string read fPath write SetPath;
      property FileMask : string read fFileMask write fFileMask ;
   end;

implementation

{$WARN SYMBOL_PLATFORM OFF}
{$WARN UNIT_PLATFORM OFF}
uses
   Windows, SysUtils, FileCtrl;

constructor TFindFile.Create(path: string);
begin
   inherited Create;
   FPath := path;
   FBasePath := path;
   FileMask := '*.*';
   FileAttr := [ffaAnyFile];
   s := TStringList.Create;
   FDepth := -1;
end;

procedure TFindFile.SetPath(Value: string);
begin
   if fPath <> Value then
   begin
      if (Value <> '') and (DirectoryExists(Value)) then
         fPath := IncludeTrailingPathDelimiter(Value);
   end;
end;

function TFindFile.SearchForFiles: TStringList;
begin
   s.Clear;
   try
      FileSearch(Path);
   finally
      Result := s;
   end;
end;

function TFindFile.cull(value: string): string;
begin
   result := StringReplace(value, FBasePath, '', []);
end;

destructor TFindFile.Destroy;
begin
   s.Free;
   inherited Destroy;
end;

procedure TFindFile.FileSearch(const InPath : string);
var Rec  : TSearchRec;
    Attr : integer;
begin
   inc(FDepth);
   try
      Attr := 0;
      if ffaReadOnly in FileAttr then
         Attr := Attr + faReadOnly;
      if ffaHidden in FileAttr then
         Attr := Attr + faHidden;
      if ffaSysFile in FileAttr then
         Attr := Attr + faSysFile;
      if ffaDirectory in FileAttr then
         Attr := Attr + faDirectory;
      if ffaArchive in FileAttr then
         Attr := Attr + faArchive;
      if ffaAnyFile in FileAttr then
         Attr := Attr + faAnyFile;

      if SysUtils.FindFirst(inPath + FileMask, Attr, Rec) = 0 then
         try
            repeat
               if (Rec.Name = '.') or (Rec.Name = '..') then
                  Continue;
               s.Add(cull(inPath) + Rec.Name);
            until SysUtils.FindNext(Rec) <> 0;
         finally
            SysUtils.FindClose(Rec);
         end;

      If not InSubFolders then
         Exit;

      if SysUtils.FindFirst(inPath + '*.*', faDirectory, Rec) = 0 then
         try
            repeat
               if ((Rec.Attr and faDirectory) <> 0)  and (Rec.Name <> '.') and (Rec.Name <> '..') then
               begin
                  FileSearch(IncludeTrailingPathDelimiter(inPath + Rec.Name));
               end;
            until SysUtils.FindNext(Rec) <> 0;
         finally
            SysUtils.FindClose(Rec);
         end;
   finally
      dec(FDepth);
   end;
end;

end.
