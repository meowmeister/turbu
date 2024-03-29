{ *****************************************************************************
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
  ***************************************************************************** }

unit EditHeroSprite;

interface

uses
  Forms, DB, DBCtrls, StdCtrls, Controls, Classes, ExtCtrls,
  IDLookupCombo, sdl_frame, EbEdit, dm_database, EventBuilder,
  turbu_sprites, sg_defs;

type
   [EditorCategory('Characters', 'Change Hero Sprite')]
   [EditorContext('RM2K')]
   TfrmEBEditHeroSprite = class(TfrmEbEditBase)
      lstFilenames: TListBox;
      txtName: TStaticText;
      cboHero: TIDLookupCombo;
      srcHeroes: TDataSource;
      imgMapSprite: TSdlFrame;
      chkTranslucent: TCheckBox;
      procedure cboHeroClick(Sender: TObject);
      procedure FormShow(Sender: TObject);
      procedure imgMapSpriteTimer(Sender: TObject);
      procedure FormCreate(Sender: TObject);
      procedure chkTranslucentClick(Sender: TObject);
      procedure imgMapSpriteAvailable(Sender: TObject);
   private
      FNotNew: boolean;
      FMatrix: TMoveMatrix;
      FSpriteData: TSpriteData;
      FMatrixPos: TSgPoint;
      FRepCount: integer;
      procedure SetFilename(const name: string);
      function GetFilename: string;
   protected
      procedure UploadObject(obj: TEbObject); override;
      procedure DownloadObject(obj: TEbObject); override;
      function NewClassType: TEbClass; override;
   end;

   [EditorCategory('Characters', 'Change Vehicle Sprite')]
   TfrmEBEditVehicleSprite = class(TfrmEBEditHeroSprite)
      procedure FormShow(Sender: TObject);
      procedure cboHeroClick(Sender: TObject);
   protected
      function NewClassType: TEbClass; override;
   end;

implementation
uses
   SysUtils,
   sdl_frame_helper, ArchiveInterface, sdl_13,
   turbu_database, turbu_characters,
   EB_Characters;

{$R *.dfm}

procedure TfrmEBEditHeroSprite.FormCreate(Sender: TObject);
var
   filename: string;
begin
   for filename in GArchives[IMAGE_ARCHIVE].allFiles('mapsprite') do
     lstFilenames.Items.Add(ChangeFileExt(ExtractFileName(filename), ''));
end;

procedure TfrmEBEditHeroSprite.FormShow(Sender: TObject);
begin
   inherited FormShow(Sender);
   if not FNotNew then
      cboHero.id := 1;
   imgMapSprite.Active := true;
   FMatrixPos.x := 2; //sprite starts out facing down
end;

function TfrmEBEditHeroSprite.GetFilename: string;
begin
   result := lstFilenames.Items[lstFilenames.ItemIndex];
end;

procedure TfrmEBEditHeroSprite.imgMapSpriteAvailable(Sender: TObject);
begin
   imgMapSprite.loadMapSprite(getFilename, 0, 0, FSpriteData);
   chkTranslucentClick(self);
end;

procedure TfrmEBEditHeroSprite.imgMapSpriteTimer(Sender: TObject);
begin
   imgMapSprite.DemoWalk(getFilename, 0, FMatrix,
                         FSpriteData, FMatrixPos, FRepCount);
end;

procedure TfrmEBEditHeroSprite.SetFilename(const name: string);
begin
   lstFilenames.ItemIndex := lstFilenames.Items.IndexOf(name);
end;

procedure TfrmEBEditHeroSprite.cboHeroClick(Sender: TObject);
var
   hero: THeroTemplate;
begin
   hero := GDatabase.hero[cboHero.id];
   SetFilename(hero.mapSprite);
   chkTranslucent.Checked := hero.translucent;
   chkTranslucentClick(self);
   FMatrix := GDatabase.moveMatrix[hero.actionMatrix];
end;

procedure TfrmEBEditHeroSprite.chkTranslucentClick(Sender: TObject);
var
   opacity: byte;
   name: string;
begin
   name := format('mapsprite\%s.png', [GetFilename]);
   if imgMapSprite.IndexOfName(name) = -1 then
      Exit;

   if chkTranslucent.Checked then
      opacity := $A0
   else opacity := $FF;
   SDL_SetTextureAlphaMod(imgMapSprite.TextureByName[name], opacity);
end;

procedure TfrmEBEditHeroSprite.UploadObject(obj: TEbObject);
begin
   FNotNew := true;
   cboHero.id := obj.Values[0];
   SetFilename(obj.Text);
   chkTranslucent.Checked := boolean(obj.Values[1]);
   chkTranslucentClick(self);
end;

procedure TfrmEBEditHeroSprite.DownloadObject(obj: TEbObject);
begin
   obj.Clear;
   obj.Values.Add(cboHero.id);
   obj.Values.Add(ord(chkTranslucent.Checked));
   obj.Text := getFilename;
end;

function TfrmEBEditHeroSprite.NewClassType: TEbClass;
begin
   result := TEBHeroSprite;
end;

{ TfrmEBEditVehicleSprite }

procedure TfrmEBEditVehicleSprite.FormShow(Sender: TObject);
begin
   self.Caption := 'Change Vehicle Sprite';
   txtName.Caption := 'Vehicle:';
   srcHeroes.DataSet := dmDatabase.Vehicles;
   inherited FormShow(Sender);
end;

procedure TfrmEBEditVehicleSprite.cboHeroClick(Sender: TObject);
var
   vehicle: TVehicleTemplate;
begin
   vehicle := GDatabase.vehicles[cboHero.id];
   SetFilename(vehicle.mapSprite);
   chkTranslucent.Checked := vehicle.translucent;
   chkTranslucentClick(self);
   FMatrix := GDatabase.moveMatrix[0];
end;

function TfrmEBEditVehicleSprite.NewClassType: TEbClass;
begin
   result := TEBVehicleSprite;
end;

initialization
   RegisterEbEditor(TEBHeroSprite, TfrmEBEditHeroSprite);
   RegisterEbEditor(TEBVehicleSprite, TfrmEBEditVehicleSprite);
finalization
   UnRegisterEbEditor(TEBHeroSprite);
   UnRegisterEbEditor(TEBVehicleSprite);
end.
