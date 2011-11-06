unit EditFlashScreen;

interface

uses
  Classes, Controls, Forms, StdCtrls, ExtCtrls, Mask,
  JvExMask, JvSpin, JvExControls, JvxSlider,
  EventBuilder, EbEdit;

type
   [EditorCategory('Map', 'Flash Screen')]
   TfrmEBEditFlashScreen = class(TfrmEbEditBase)
      radFlashMode: TRadioGroup;
      GroupBox1: TGroupBox;
      sldRed: TJvxSlider;
      spnRed: TJvSpinEdit;
      StaticText1: TStaticText;
      sldGreen: TJvxSlider;
      spnGreen: TJvSpinEdit;
      sldBlue: TJvxSlider;
      spnBlue: TJvSpinEdit;
      sldAlpha: TJvxSlider;
      spnAlpha: TJvSpinEdit;
      pnlPreview: TPanel;
      spnLength: TJvSpinEdit;
      procedure FormCreate(Sender: TObject);
      procedure sldRedChange(Sender: TObject);
      procedure sldGreenChange(Sender: TObject);
      procedure sldBlueChange(Sender: TObject);
      procedure sldAlphaChange(Sender: TObject);
      procedure spnRedChange(Sender: TObject);
      procedure spnGreenChange(Sender: TObject);
      procedure spnBlueChange(Sender: TObject);
      procedure spnAlphaChange(Sender: TObject);
   private
      FAlpha : integer;
      FRed, FGreen, FBlue: integer;
      procedure SetRed(value: integer);
      procedure SetGreen(value: integer);
      procedure SetBlue(value: integer);
      procedure SetAlpha(value: integer);
      procedure Draw;
   protected
      procedure UploadObject(obj: TEbObject); override;
      procedure DownloadObject(obj: TEbObject); override;
      function NewClassType: TEbClass; override;
   end;

implementation
uses
   Graphics,
   EB_Maps;

{$R *.dfm}

procedure TfrmEBEditFlashScreen.FormCreate(Sender: TObject);
begin
   inherited;
   FRed := 31;
   FGreen := 31;
   FBlue := 31;
   FAlpha := 31;
   Draw;
end;

procedure TfrmEBEditFlashScreen.Draw;
var
   r, g, b: integer;
begin
   r := RGB32(FRed);
   g := RGB32(FGreen);
   b := RGB32(FBlue);
   pnlPreview.Color := TColor(r + (g shl 8) + (b shl 16));
end;

procedure TfrmEBEditFlashScreen.SetRed(value: integer);
begin
   FRed := value;
   sldRed.Value := value;
   spnRed.AsInteger := value;
end;

procedure TfrmEBEditFlashScreen.SetGreen(value: integer);
begin
   FGreen := value;
   sldGreen.Value := value;
   spnGreen.AsInteger := value;
end;

procedure TfrmEBEditFlashScreen.SetBlue(value: integer);
begin
   FBlue := value;
   sldBlue.Value := value;
   spnBlue.AsInteger := value;
end;

procedure TfrmEBEditFlashScreen.SetAlpha(value: integer);
begin
   FAlpha := value;
   sldAlpha.Value := value;
   spnAlpha.AsInteger := value;
end;

procedure TfrmEBEditFlashScreen.sldRedChange(Sender: TObject);
begin
   SetRed(sldRed.Value);
   Draw;
end;

procedure TfrmEBEditFlashScreen.sldGreenChange(Sender: TObject);
begin
   SetGreen(sldGreen.Value);
   Draw;
end;

procedure TfrmEBEditFlashScreen.sldBlueChange(Sender: TObject);
begin
   SetBlue(sldBlue.Value);
   Draw;
end;

procedure TfrmEBEditFlashScreen.sldAlphaChange(Sender: TObject);
begin
   SetAlpha(sldAlpha.Value);
   Draw;
end;

procedure TfrmEBEditFlashScreen.spnRedChange(Sender: TObject);
begin
   SetRed(spnRed.AsInteger);
   Draw;
end;

procedure TfrmEBEditFlashScreen.spnGreenChange(Sender: TObject);
begin
   SetGreen(spnGreen.AsInteger);
   Draw;
end;

procedure TfrmEBEditFlashScreen.spnBlueChange(Sender: TObject);
begin
   SetBlue(spnBlue.AsInteger);
   Draw;
end;

procedure TfrmEBEditFlashScreen.spnAlphaChange(Sender: TObject);
begin
   SetAlpha(spnAlpha.AsInteger);
   Draw;
end;

function TfrmEBEditFlashScreen.NewClassType: TEbClass;
begin
   result := TEBFlashScreen;
end;

procedure TfrmEBEditFlashScreen.DownloadObject(obj: TEbObject);
begin
   obj.Clear;
   obj.Values.AddRange(TArray<integer>.Create(FRed, FGreen, FBlue, FAlpha,
     spnLength.AsInteger));
   case radFlashMode.ItemIndex of
      0: obj.Values.AddRange([0, 0]);
      1: obj.Values.AddRange([1, 0]);
      2: obj.Values.AddRange([0, 1]);
   end;
end;

procedure TfrmEBEditFlashScreen.UploadObject(obj: TEbObject);
begin
   SetRed(obj.Values[0]);
   SetGreen(obj.Values[1]);
   SetBlue(obj.Values[2]);
   SetAlpha(obj.Values[3]);
   spnLength.AsInteger := obj.Values[4];
   if obj.Values[6] = 1 then
      radFlashMode.ItemIndex := 2
   else if obj.Values[5] = 1 then
      radFlashMode.ItemIndex := 1
   else radFlashMode.ItemIndex := 0;
end;

initialization
   RegisterEbEditor(TEBFlashScreen, TfrmEBEditFlashScreen);
finalization
   UnRegisterEbEditor(TEBFlashScreen);
end.
