unit imageSelectorFrame;

interface

uses
   Controls, StdCtrls, Classes, ExtCtrls, Forms,
   ArchiveInterface;

type
   TframeImageSelector = class(TFrame)
      imgSelection: TImage;
      lstFilename: TListBox;
      procedure lstFilenameClick(Sender: TObject);
   private
      FArchive: IArchive;
      FPath: string;
      FSelection: string;
      procedure LoadImage(stream: TStream);
      procedure SetSelection(const Value: string);
   public
      procedure Setup(const archive: IArchive; const path: string; nullable: boolean = false);
      property Selection: string read FSelection write SetSelection;
   end;

implementation
uses
   SysUtils, Graphics, PngImage;

{$R *.dfm}

const NULL_ITEM = '*NONE';

procedure TframeImageSelector.LoadImage(stream: TStream);
var
  NewGraphic: TGraphic;
begin
  NewGraphic := TPngImage.Create;
  try
     try
       NewGraphic.LoadFromStream(stream);
       imgSelection.Picture.Graphic := NewGraphic;
     except
       FreeAndNil(NewGraphic);
       raise;
     end;
  finally
     NewGraphic.Free;
  end;
end;

procedure TframeImageSelector.lstFilenameClick(Sender: TObject);
var
   filename: string;
   oldpath: string;
   stream: TStream;
begin
   filename := lstFilename.Items[lstFilename.ItemIndex];
   if filename = NULL_ITEM then
   begin
      imgSelection.Picture := nil;
      FSelection := '';
   end
   else begin
      FSelection := filename;
      stream := nil;
      oldpath := FArchive.currentFolder;
      try
         FArchive.currentFolder := FPath;
         stream := FArchive.getFile(ChangeFileExt(filename, '.png'));
         LoadImage(stream);
      finally
         FArchive.currentFolder := oldpath;
         stream.Free;
      end;
   end;
end;

procedure TframeImageSelector.SetSelection(const Value: string);
begin
   FSelection := Value;
   if value = '' then
      lstFilename.ItemIndex := 0
   else lstFilename.ItemIndex := lstFilename.Items.IndexOf(value);
   if lstFilename.ItemIndex = -1 then
      lstFilename.ItemIndex := 0;
end;

procedure TframeImageSelector.Setup(const archive: IArchive; const path: string;
  nullable: boolean);
var
   filename, oldpath: string;
begin
   FArchive := archive;
   FPath := path;
   lstFilename.Clear;
   if nullable then
      lstFilename.AddItem(NULL_ITEM, nil);
   oldpath := FArchive.currentFolder;
   try
      for filename in FArchive.allFiles(path) do
         lstFilename.AddItem(ChangeFileExt(ExtractFileName(filename), ''), nil);
   finally
      FArchive.CurrentFolder := oldpath;
   end;
   lstFilename.ItemIndex := 0;
   lstFilenameClick(self);
end;

end.
