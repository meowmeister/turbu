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

unit EditPortrait;

interface

uses
   Forms, DB, DBCtrls, StdCtrls, Controls, Classes, ExtCtrls,
   IDLookupCombo, sdl_frame, EbEdit, dm_database, EventBuilder,
   turbu_sprites, sg_defs, turbu_constants;

type
   [EditorCategory('Characters', 'Change Hero Portrait')]
   [EditorContext('RM2K')]
   TfrmEBEditPortrait = class(TfrmEbEditBase)
      srcHeroes: TDataSource;
      cboHero: TIDLookupCombo;
      StaticText1: TStaticText;
      imgPortrait: TSdlFrame;
      btnSetPortrait: TButton;
      procedure cboHeroClick(Sender: TObject);
      procedure btnSetPortraitClick(Sender: TObject);
      procedure imgPortraitAvailable(Sender: TObject);
      procedure FormShow(Sender: TObject);
   private
      FNotNew: boolean;
      FPortrait: string;
      FPortraitIndex: integer;
      procedure SetPortrait;
      procedure WMRender(var msg); message WM_RENDER;
   protected
      procedure UploadObject(obj: TEbObject); override;
      procedure DownloadObject(obj: TEbObject); override;
      function NewClassType: TEbClass; override;
   end;

implementation
uses
   Windows,
   turbu_characters, turbu_database, sdl_frame_helper, portrait_selector,
   EB_Characters;

{$R *.dfm}

procedure TfrmEBEditPortrait.btnSetPortraitClick(Sender: TObject);
begin
   TfrmPortraitSelector.SelectPortraitInto(imgPortrait, FPortrait, FPortraitIndex, false);
end;

procedure TfrmEBEditPortrait.cboHeroClick(Sender: TObject);
var
   hero: THeroTemplate;
begin
   hero := GDatabase.hero[cboHero.id];
   FPortrait := hero.portrait;
   FPortraitIndex := hero.portraitIndex;
   SetPortrait;
end;

procedure TfrmEBEditPortrait.SetPortrait;
begin
   if imgPortrait.Available then
      imgPortrait.setPortrait(FPortrait, FPortraitIndex, false);
end;

procedure TfrmEBEditPortrait.WMRender(var msg);
begin
   SetPortrait;
end;

procedure TfrmEBEditPortrait.FormShow(Sender: TObject);
begin
   inherited FormShow(Sender);
   if not FNotNew then
      cboHero.id := 1;
end;

procedure TfrmEBEditPortrait.imgPortraitAvailable(Sender: TObject);
begin
   PostMessage(self.Handle, WM_RENDER, 0, 0);
end;

procedure TfrmEBEditPortrait.UploadObject(obj: TEbObject);
begin
   FNotNew := true;
   FPortrait := obj.Text;
   cboHero.id := obj.Values[0];
   FPortraitIndex := obj.Values[1];
   SetPortrait;
end;

procedure TfrmEBEditPortrait.DownloadObject(obj: TEbObject);
begin
   obj.Clear;
   obj.Text := FPortrait;
   obj.Values.Add(cboHero.id);
   obj.Values.Add(FPortraitIndex);
end;

function TfrmEBEditPortrait.NewClassType: TEbClass;
begin
   result := TEBHeroPortrait;
end;

initialization
   RegisterEbEditor(TEBHeroPortrait, TfrmEBEditPortrait);
finalization
   UnRegisterEbEditor(TEBHeroPortrait);
end.
