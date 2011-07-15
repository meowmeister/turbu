{*****************************************************************************
* The contents of this file are used with permission, subject to
* the Mozilla Public License Version 1.1 (the "License"); you may
* not use this file except in compliance with the License. You may
* obtain a copy of the License at
* http://www.mozilla.org/MPL/MPL-1.1.html
*
* Software distributed under the License is distributed on an
* "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
* implied. See the License for the specific language governing
* rights and limitations under the License.
*
*****************************************************************************
*
* This file was created by Mason Wheeler.  He can be reached for support at
* www.turbu-rpg.com.
*****************************************************************************}

unit ImageSelector;

interface

uses
  StdCtrls, Classes, Controls, ExtCtrls, Forms,
  ArchiveInterface;

type
   TfrmImageSelector = class(TForm)
      lstFilename: TListBox;
      btnClose: TButton;
      btnSelect: TButton;
      imgSelection: TImage;
      procedure lstFilenameClick(Sender: TObject);
   private
      FArchive: IArchive;
      FPath: string;
      FSelection: string;
      procedure LoadImage(stream: TStream);
      procedure SetSelection(const Value: string);
   public
      procedure Setup(const archive: IArchive; const path: string; nullable: boolean = false);
      function Select(const value: string): string;
   end;

implementation
uses
   SysUtils, Graphics, PngImage;

{$R *.dfm}

const NULL_ITEM = '*NONE';

{ TfrmImageSelector }

procedure TfrmImageSelector.LoadImage(stream: TStream);
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

procedure TfrmImageSelector.lstFilenameClick(Sender: TObject);
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

function TfrmImageSelector.Select(const value: string): string;
begin
   self.SetSelection(value);
   if self.ShowModal = mrCancel then
      result := value
   else result := FSelection;
end;

procedure TfrmImageSelector.SetSelection(const Value: string);
begin
   FSelection := Value;
   if value = '' then
      lstFilename.ItemIndex := 0
   else lstFilename.ItemIndex := lstFilename.Items.IndexOf(value);
   if lstFilename.ItemIndex = -1 then
      lstFilename.ItemIndex := 0;
end;

procedure TfrmImageSelector.Setup(const archive: IArchive; const path: string; nullable: boolean);
var
   filename: string;
begin
   FArchive := archive;
   FPath := path;
   lstFilename.Clear;
   if nullable then
      lstFilename.AddItem(NULL_ITEM, nil);
   for filename in FArchive.allFiles(path) do
      lstFilename.AddItem(ChangeFileExt(ExtractFileName(filename), ''), nil);
   lstFilename.ItemIndex := 0;
   lstFilenameClick(self);
end;

end.
