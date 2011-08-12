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
  ArchiveInterface, imageSelectorFrame;

type
   TfrmImageSelector = class(TForm)
      btnClose: TButton;
      btnSelect: TButton;
      frameImageSelector: TframeImageSelector;
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

function TfrmImageSelector.Select(const value: string): string;
begin
   frameImageSelector.Selection := value;
   if self.ShowModal = mrCancel then
      result := value
   else result := frameImageSelector.Selection;
end;

procedure TfrmImageSelector.Setup(const archive: IArchive; const path: string; nullable: boolean);
begin
   frameImageSelector.Setup(archive, path, nullable);
end;

end.
