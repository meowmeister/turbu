unit sprite_selector;

interface

uses
  SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls, ExtCtrls, Messages,
  Generics.Collections,
  sdl_frame,
  turbu_tilesets, turbu_constants;

type
  TfrmSpriteSelector = class(TForm)
    Panel1: TPanel;
    lstFilenames: TListBox;
    Panel2: TPanel;
    imgSelector: TSdlFrame;
    GroupBox1: TGroupBox;
    radUp: TRadioButton;
    radRight: TRadioButton;
    radDown: TRadioButton;
    radLeft: TRadioButton;
    grpFrame: TRadioGroup;
    btnOK: TButton;
    btnCancel: TButton;
    procedure FormShow(Sender: TObject);
    procedure lstFilenamesClick(Sender: TObject);
    procedure imgSelectorAvailable(Sender: TObject);
  private
    procedure WMRender(var message: TMessage); message WM_RENDER;
  private
    FFileList: TEnumerable<string>;
    FFilename: string;
    FFrame: integer;
    FTileset: TTileset;
    procedure loadTileGroups;
    procedure loadTileset(grouprec: TTileGroupRecord);
    procedure loadSprite(filename: string);
  public
    class procedure SelectSprite(tileset: TTileset; var current: string; var frame: integer);
  end;

var
  frmSpriteSelector: TfrmSpriteSelector;

implementation
uses
   Windows,
   ArchiveInterface, turbu_containers, turbu_tbi_lib,
   sdl_13;

{$R *.dfm}

{ TfrmSpriteSelector }

function UpperLayerFilter(value: TTileGroupRecord): boolean;
begin
   result := value.layers - [0] <> [];
end;

procedure TfrmSpriteSelector.loadTileGroups;
var
   tilegroups: TRpgObjectList<TTileGroupRecord>;
   groupRec: TTileGroupRecord;
begin
   tilegroups := FTileset.Records.where(upperLayerFilter);
   try
      for grouprec in tilegroups do
         lstFilenames.AddItem('*Tile Group ' + intToStr(grouprec.id), grouprec);
   finally
      tilegroups.Free;
   end;
end;

procedure TfrmSpriteSelector.FormShow(Sender: TObject);
var
   filename: string;
   index: integer;
begin
   assert(assigned(FFileList));
   assert(FFilename <> '');
   if assigned(FTileset) then
      loadTileGroups;
   for filename in FFileList do
      lstFilenames.AddItem(filename, nil);
   index := lstFilenames.Items.IndexOf(FFilename);
   if index <> -1 then
      lstFilenames.ItemIndex := index
   else lstFilenames.ItemIndex := 0;
end;

procedure TfrmSpriteSelector.loadTileset(grouprec: TTileGroupRecord);
var
   filename, oFilename: string;
   stream: TStream;
   surface: PSdlSurface;
begin
   oFilename := groupRec.group.filename;
   if not imgSelector.ContainsName(oFilename) then
   begin
      filename := format('tileset\%s.png', [oFilename]);
      assert(GArchives[IMAGE_ARCHIVE].fileExists(filename));
      stream := GArchives[IMAGE_ARCHIVE].getFile(filename);
      try
         surface := loadFromTBI(stream);
      finally
         stream.Free;
      end;
      imgSelector.AddTexture(surface, oFilename);
   end;

   imgSelector.Width := imgSelector.TextureByName[oFilename].size.X * 2;
   imgSelector.Height := imgSelector.TextureByName[oFilename].size.Y * 2;
   imgSelector.BringToFront;
   imgSelector.Update;
   imgSelector.Clear;
   imgSelector.DrawTexture(oFilename);
   imgSelector.Flip;
end;

procedure TfrmSpriteSelector.loadSprite(filename: string);
var
   stream: TStream;
   surface: PSdlSurface;
begin
   if not imgSelector.ContainsName(filename) then
   begin
      assert(GArchives[IMAGE_ARCHIVE].fileExists(filename));
      stream := GArchives[IMAGE_ARCHIVE].getFile(filename);
      try
         surface := loadFromTBI(stream);
      finally
         stream.Free;
      end;
      imgSelector.AddTexture(surface, filename);
   end;

   imgSelector.Height := 132;
   imgSelector.Clear;
   imgSelector.Update;
   imgSelector.DrawTexture(filename);
   imgSelector.Flip;
end;

procedure TfrmSpriteSelector.lstFilenamesClick(Sender: TObject);
var
   filename: string;
begin
   filename := lstFilenames.Items[lstFilenames.ItemIndex];
   if filename[1] = '*' then
      loadTileset(lstFilenames.Items.Objects[lstFilenames.ItemIndex] as TTileGroupRecord)
   else loadSprite(filename);
end;

procedure TfrmSpriteSelector.WMRender(var message: TMessage);
begin
   lstFilenamesClick(self);
end;

procedure TfrmSpriteSelector.imgSelectorAvailable(Sender: TObject);
begin
   PostMessage(self.Handle, WM_RENDER, 0, 0);
end;

class procedure TfrmSpriteSelector.SelectSprite(tileset: TTileset; var current: string;
  var frame: integer);
var
   form: TfrmSpriteSelector;
begin
   form := TfrmSpriteSelector.Create(nil);
   try
      form.FFileList := GArchives[IMAGE_ARCHIVE].allFiles('mapsprite');
      form.FFilename := current;
      form.FFrame := frame;
      form.FTileset := tileset;
      GArchives[IMAGE_ARCHIVE].currentFolder := '';
      if form.ShowModal = mrOK then
      begin
         current := form.FFilename;
         frame := form.FFrame;
      end;
   finally
      form.Release;
   end;
end;

end.
