unit EditShakeScreen;

interface

uses
  Classes, Forms, StdCtrls, ExtCtrls, Controls, Mask,
  JvExMask, JvSpin, JvExControls, JvxSlider,
  EventBuilder, EbEdit;

type
   [EditorCategory('Map', 'Shake Screen')]
   [EditorContext('RM2K')]
   TfrmEBEditShakeScreen = class(TfrmEbEditBase)
      radShakeMode: TRadioGroup;
      sldStrength: TJvxSlider;
      StaticText1: TStaticText;
      sldSpeed: TJvxSlider;
      grpShakeDuration: TGroupBox;
      spnLength: TJvSpinEdit;
   protected
      procedure EnableControlsProperly; override;
      procedure UploadObject(obj: TEbObject); override;
      procedure DownloadObject(obj: TEbObject); override;
      function NewClassType: TEbClass; override;
   end;

implementation
uses
   EB_Maps;

{$R *.dfm}

procedure TfrmEBEditShakeScreen.EnableControlsProperly;
begin
   EnableGroupBox(grpShakeDuration, radShakeMode.ItemIndex < 2);
end;

function TfrmEBEditShakeScreen.NewClassType: TEbClass;
begin
   result := TEBShakeScreen;
end;

procedure TfrmEBEditShakeScreen.UploadObject(obj: TEbObject);
begin
   sldStrength.Value := obj.Values[0];
   sldSpeed.Value := obj.Values[1];
   spnLength.Value := obj.Values[2];
   if obj.Values[4] = 1 then
      radShakeMode.ItemIndex := 2
   else if obj.Values[3] = 1 then
      radShakeMode.ItemIndex := 1
   else radShakeMode.ItemIndex := 0;
end;

procedure TfrmEBEditShakeScreen.DownloadObject(obj: TEbObject);
begin
   obj.Clear;
   obj.Values.AddRange(TArray<integer>.Create(sldStrength.Value, sldSpeed.Value, spnLength.AsInteger));
   case radShakeMode.ItemIndex of
      0: obj.Values.AddRange([0, 0]);
      1: obj.Values.AddRange([1, 0]);
      2: obj.Values.AddRange([0, 1]);
   end;
end;

initialization
   RegisterEbEditor(TEBShakeScreen, TfrmEBEditShakeScreen);
finalization
   UnRegisterEbEditor(TEBShakeScreen);
end.
