unit EditPortrait;

interface

uses
  Forms, DB, DBCtrls, StdCtrls, Controls, Classes, ExtCtrls,
  IDLookupCombo, sdl_frame, EbEdit, dm_database, EventBuilder,
  turbu_sprites, sg_defs, turbu_constants;

type
   [EditorCategory('Characters', 'Change Hero Portrait')]
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
