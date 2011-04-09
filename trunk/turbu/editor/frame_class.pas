unit frame_class;
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

interface

uses
   SysUtils, Classes, Controls, Forms, ComCtrls, StdCtrls, ExtCtrls, Graphics,
   DBGrids, DB, Mask, DBCtrls, Grids, DBClient,
   JvDBSpinEdit, JvListBox, JvExStdCtrls, JvExMask, JvSpin,
   sdl_frame, turbu_listGrid,
   commons, frame_commands, turbu_sprites, turbu_characters, turbu_defs,
   sg_defs;

type
   TframeClass = class(TFrame)
      pnlClass: TPanel;
      grpClassName: TGroupBox;
      grpClassStats: TGroupBox;
      imgStats: TImage;
      cbxBaseStats: TComboBox;
      grpClassOptions: TGroupBox;
      grpClassExp: TGroupBox;
      lblExpVal1: TLabel;
      lblExpVal2: TLabel;
      lblExpVal3: TLabel;
      lblExpVal4: TLabel;
      btnExpCurveEditor: TButton;
      grpGraphics: TGroupBox;
      tabGraphics: TTabControl;
      grpRepertoire: TGroupBox;
      pageRepertoire: TPageControl;
      tshEquipment: TTabSheet;
      lblEquip1: TLabel;
      lblEquip2: TLabel;
      lblEquip3: TLabel;
      lblEquip4: TLabel;
      lblEquip5: TLabel;
      tshSkills: TTabSheet;
      frameHeroCommands: TframeHeroCommands;
      tshRepertoire: TTabSheet;
      grpScriptEvents: TGroupBox;
      lstScripts: TListView;
      grpUnarmed: TGroupBox;
      grpResistVuln: TGroupBox;
      lblClasses: TLabel;
      pageResists: TPageControl;
      tshAttributes: TTabSheet;
      tshConditions: TTabSheet;
      btnSetGfx: TButton;
      tmrAnim: TTimer;

      dsCharClass: TDataSource;
      dsSkillset: TDataSource;
      dsResist: TDataSource;
      dsCondition: TDataSource;
      dbTxtName: TDBEdit;
      spnExpVal1: TJvDBSpinEdit;
      spnExpVal2: TJvDBSpinEdit;
      spnExpVal3: TJvDBSpinEdit;
      spnExpVal4: TJvDBSpinEdit;
      radWeaponStyle: TDBRadioGroup;
      chkEqLocked: TDBCheckBox;
      chkStrongDef: TDBCheckBox;
      cbxUnarmedAnim: TDBLookupComboBox;
      grdClasses: TRpgListGrid;
      cbxEquip1: TDBLookupComboBox;
      cbxEquip2: TDBLookupComboBox;
      cbxEquip3: TDBLookupComboBox;
      cbxEquip4: TDBLookupComboBox;
      cbxEquip5: TDBLookupComboBox;
      lstSkills: TRpgListGrid;
      lstAttributes: TRpgListGrid;
      navAdd: TDBNavigator;
      btnEditAttributes: TButton;
      navDel: TDBNavigator;
      RpgListGrid1: TRpgListGrid;
      cbxExpAlgorithm: TDBLookupComboBox;
      imgMapSprite: TSdlFrame;

      procedure tabGraphicsChange(Sender: TObject);
      procedure tmrAnimTimer(Sender: TObject);
      procedure radWeaponStyleClick(Sender: TObject);
      procedure grdClassesRowEnter(Sender: TObject; FromIndex, ToIndex: Integer);
      procedure lstSkillsDblClick(Sender: TObject);
      procedure checkFor2Handed(Sender: TObject);
      procedure linkNav(Sender: TObject);
      procedure btnEditAttributesClick(Sender: TObject);
      procedure lstScriptsDblClick(Sender: TObject);
      procedure btnSetGfxClick(Sender: TObject);
      procedure imgMapSpriteAvailable(Sender: TObject);
   private
      FLoading: boolean;
      FLoaded: boolean;
      FId: integer;
      FTemplate: TClassTemplate;
      FSpriteData: TSpriteData;
      FMatrixPosition: TSgPoint;
      FRepCounter: integer;
      FSpriteIndexToLoad: integer;
      FSpriteToLoad: string;
      FOldWeaponScroll: TDatasetNotifyEvent;

      procedure loadPortrait(name: string; id: integer);
      procedure weaponsAfterScroll(DataSet: TDataSet);
   public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;

      procedure initClasses;
      procedure loadClass(data: word);
      procedure onShow;
      procedure onHide;
   end;

implementation
uses
   Types, zlib, math,
   turbu_database, archiveInterface, turbu_constants, turbu_items,
   turbu_tbi_lib, turbu_sdl_image, turbu_classes, dm_database,
   design_script_engine, skill_settings,
   attributes_editor,
   SDL, SDL_13, sdl_frame_helper;

{$R *.dfm}

procedure TframeClass.checkFor2Handed(Sender: TObject);
var
   base: TDataSet;
   field: TField;
begin
   base := cbxEquip1.DataSource.DataSet;
   field := base.FindField(cbxEquip1.DataField);
   if assigned(field) then
   begin
      base := field.LookupDataSet;
      field := base.FindField('twoHanded');
      if assigned(field) and (field.AsBoolean = true) then
      begin
         cbxEquip2.Enabled := false;
         lblEquip2.Enabled := false;
         Exit;
      end;
   end;

   base := cbxEquip2.DataSource.DataSet;
   field := base.FindField(cbxEquip2.DataField);
   if assigned(field) then
   begin
      field := base.FindField('twoHanded');
      if assigned(field) and (field.AsBoolean = true) then
      begin
         cbxEquip1.Enabled := false;
         lblEquip1.Enabled := false;
         Exit;
      end;
   end;

   cbxEquip1.Enabled := true;
   lblEquip1.Enabled := true;
   cbxEquip2.Enabled := radWeaponStyle.ItemIndex <> ord(ws_single);
   lblEquip2.Enabled := radWeaponStyle.ItemIndex <> ord(ws_single);
end;

constructor TframeClass.Create(AOwner: TComponent);
var
   i: byte;
begin
   inherited Create(AOwner);
   FMatrixPosition := point(2, 0);
   for i := 0 to radWeaponStyle.Items.Count - 1 do
      radWeaponStyle.Values.Add(intToStr(i));
   tshAttributes.Tag := integer(dsResist);
   tshConditions.Tag := integer(dsCondition);
   FOldWeaponScroll := dmDatabase.weapons.AfterScroll;
   dmDatabase.weapons.AfterScroll := self.weaponsAfterScroll;
end;

destructor TframeClass.Destroy;
begin
   dmDatabase.weapons.AfterScroll := FOldWeaponScroll;
   pnlClass.Parent := self;
   DestroyComponents;
   inherited Destroy;
end;

procedure TframeClass.grdClassesRowEnter(Sender: TObject; FromIndex,
  ToIndex: Integer);
begin
   FLoading := true;
   try
      loadClass(dsCharClass.DataSet.FieldByName('id').AsInteger);
   finally
      FLoading := false;
   end;
end;

procedure TframeClass.imgMapSpriteAvailable(Sender: TObject);
begin
   if FSpriteToLoad <> '' then
   begin
      loadPortrait(FSpriteToLoad, FSpriteIndexToLoad);
      imgMapSprite.Flip;
   end;
end;

procedure TframeClass.initClasses;
begin
   FLoading := true;
   try
      lstScripts.Clear;
      if GDatabase.charClasses > 0 then
         loadClass(0);
   finally
      FLoading := false;
   end;
end;

procedure TframeClass.linkNav(Sender: TObject);
begin
   navAdd.DataSource := TObject(TControl(sender).Tag) as TDataSource;
   navDel.DataSource := TObject(TControl(sender).Tag) as TDataSource;
end;

procedure TframeClass.loadClass(data: word);
var
   iterator: TDataSet;
   events: TStringList;
   i: integer;
   item: TListItem;
   eventName: string;
begin
   if data = 0 then
      Exit;

   FId := data;
   FTemplate := GDatabase.charClass[FId];

   for I := 0 to lstScripts.Items.Count - 1 do
      lstScripts.Items[i].SubItems.Clear;
   lstScripts.Clear;

   events := FTemplate.GetAllEvents;
   try
      for I := 0 to events.Count - 1 do
      begin
         if lstScripts.Tag = 0 then
         begin
            //prepare the initial list
            item := lstScripts.Items.Add;
            eventName := events[i];
            item.Caption := eventName;
            item.Data := FTemplate.signature[eventName];
         end;
      end;
   finally
      events.Free;
   end;

   tabGraphicsChange(tabGraphics); //load portrait or sprite
   frameHeroCommands.size := FTemplate.commands;
   radWeaponStyle.ItemIndex := ord(FTemplate.dualWield);
   chkEqLocked.Checked := FTemplate.staticEq;
   chkStrongDef.Checked := FTemplate.strongDef;
   self.radWeaponStyleClick(self); //adjust EQ box 2
   for iterator in dmDatabase.views do
   begin
      iterator.Filtered := false;
      iterator.Filtered := true;
   end;
end;

procedure TframeClass.loadPortrait(name: string; id: integer);
var
   filename: string;
   fileStream: TStream;
   image: TRpgSdlImage;
   spriteRect: TRect;
begin
   if not imgMapSprite.Available then
   begin
      FSpriteIndexToLoad := id;
      FSpriteToLoad := name;
      exit;
   end;

   if name <> '' then
   begin
     filename := format('portrait\%s.png', [name]);
     if imgMapSprite.IndexOfName(filename) = -1 then
     begin
        try
           fileStream := GArchives[IMAGE_ARCHIVE].getFile(filename);
           try
              image := TRpgSdlImage.CreateSprite(imgMapSprite.Renderer, loadFromTBI(fileStream), filename, imgMapSprite.Images);
           finally
              fileStream.Free;
           end;
        except
           on EArchiveError do
              image := TRpgSdlImage.CreateBlankSprite(imgMapSprite.Renderer, filename, imgMapSprite.Images, SPRITE_SIZE, 18);
        end;
     end
     else image := imgMapSprite.Images.image[filename] as TRpgSdlImage;

     spriteRect := image.spriteRect[id];
     imgMapSprite.DrawTexture(image.Texture, @spriteRect);
     imgMapSprite.Flip;
   end
   else imgMapSprite.Clear;
end;

procedure TframeClass.lstScriptsDblClick(Sender: TObject);
var
   item: TListItem;
begin
   item := lstScripts.Selected;
   if not assigned(item) then
      Exit;

   if item.SubItems.Count = 0 then
//      frmAlgorithmEditor.newFunc( item.Data
   else begin
      //edit the current script
   end;
end;

procedure TframeClass.lstSkillsDblClick(Sender: TObject);
var
   frmSkillLearning: TFrmSkillLearning;
begin
   frmSkillLearning := TFrmSkillLearning.Create(nil);
   try
      frmSkillLearning.ShowModal;
   finally
      frmSkillLearning.Free;
   end;
end;

procedure TframeClass.onShow;
begin
   tmrAnim.Enabled := true;
   imgMapSprite.Active := true;
   if not FLoaded then
   begin
      frameHeroCommands.dataSet := dsCharClass.DataSet;
      loadClass(1);
      FLoaded := true;
   end;
end;

procedure TframeClass.onHide;
begin
   tmrAnim.Enabled := false;
   imgMapSprite.Active := false;
end;

procedure TframeClass.radWeaponStyleClick(Sender: TObject);
begin
   cbxEquip2.DataField := ''; //needed to make sure the display changes

   case TWeaponStyle(radWeaponStyle.ItemIndex) of
      ws_single: cbxEquip2.DataField := '';
      ws_shield:
      begin
         cbxEquip2.DataField := 'shieldname';
         lblEquip2.Caption := 'Shield:';
      end;
      ws_dual:
      begin
         cbxEquip2.DataField := 'weapon2Name';
         lblEquip2.Caption := 'Weapon:';
      end;
      ws_all:
      begin
         cbxEquip2.DataField := 'offhandName';
         lblEquip2.Caption := 'Offhand:';
      end;
   end;
   cbxEquip2.Enabled := radWeaponStyle.ItemIndex <> ord(ws_single);
   lblEquip2.Enabled := radWeaponStyle.ItemIndex <> ord(ws_single);;
   checkFor2Handed(sender);
end;

procedure TframeClass.tabGraphicsChange(Sender: TObject);
begin
   case (sender as TTabControl).TabIndex of
      0: loadPortrait(dsCharClass.DataSet.FieldByName('portrait').AsString,
                      dsCharClass.DataSet.FieldByName('portraitIndex').AsInteger);
      1: imgMapSprite.loadMapSprite(dsCharClass.DataSet.FieldByName('mapSprite').asString,
                       max(0, (dsCharClass.DataSet.FieldByName('actionMatrix').asInteger)),
                       nextPosition(GDatabase.moveMatrix[FSpriteData.moveMatrix], FMatrixPosition),
                       FSpriteData);
      2: {fix this later};
      else assert(false);
   end;
end;

procedure TframeClass.tmrAnimTimer(Sender: TObject);
var
   matrix: TMoveMatrix;
begin
   if tabGraphics.TabIndex = 0 then
      Exit;

   matrix := GDatabase.moveMatrix[FSpriteData.moveMatrix];
   case tabGraphics.TabIndex of
      1: imgMapSprite.DemoWalk(dsCharClass.DataSet.FieldByName('mapSprite').AsString,
                       max(0, (dsCharClass.DataSet.FieldByName('actionMatrix').AsInteger)),
                       matrix, FSpriteData, FMatrixPosition, FRepCounter);
      2: {fix this later};
      else assert(false);
   end;
end;

procedure TframeClass.weaponsAfterScroll(DataSet: TDataSet);
begin
  if assigned(FOldWeaponScroll) then
     FOldWeaponScroll(DataSet);
  checkFor2Handed(DataSet);
end;

procedure TframeClass.btnEditAttributesClick(Sender: TObject);
begin
   frmAttributesEditor.ShowModal;
end;

procedure TframeClass.btnSetGfxClick(Sender: TObject);
begin
(*   case tabGraphics.TabIndex of
      0: ;
      1: ;
      2: {fix this later};
      else assert(false);
   end; *)
end;

end.
