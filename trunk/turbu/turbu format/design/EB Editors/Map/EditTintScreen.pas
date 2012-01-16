unit EditTintScreen;

interface

uses
   Forms, StdCtrls, Controls, Classes, ExtCtrls, Mask, Messages,
   JvExMask, JvSpin, JvExControls, JvxSlider,
   sdl_frame, dm_shaders,
   EventBuilder, EbEdit;

type
   [EditorCategory('Map', 'Tint Screen')]
   TfrmEBEditTintScreen = class(TfrmEbEditBase)
      GroupBox1: TGroupBox;
      StaticText1: TStaticText;
      sldRed: TJvxSlider;
      spnRed: TJvSpinEdit;
      sldSat: TJvxSlider;
      spnSat: TJvSpinEdit;
      spnBlue: TJvSpinEdit;
      sldBlue: TJvxSlider;
      sldGreen: TJvxSlider;
      spnGreen: TJvSpinEdit;
      imgReference: TSdlFrame;
      spnLength: TJvSpinEdit;
      chkWait: TCheckBox;
      procedure imgReferenceAvailable(Sender: TObject);
      procedure sldRedChange(Sender: TObject);
      procedure sldGreenChange(Sender: TObject);
      procedure sldBlueChange(Sender: TObject);
      procedure sldSatChange(Sender: TObject);
      procedure spnRedChange(Sender: TObject);
      procedure spnGreenChange(Sender: TObject);
      procedure spnBlueChange(Sender: TObject);
      procedure spnSatChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
   private
      FShaders: TdmShaders;
      FProgramHandle: integer;
      FSaturation: integer;
      FRed, FGreen, FBlue: integer;
      procedure Render(var msg: TMessage); message WM_USER;
      procedure Draw;
      procedure SetRed(value: integer);
      procedure SetGreen(value: integer);
      procedure SetBlue(value: integer);
      procedure SetSat(value: integer);
   protected
      procedure UploadObject(obj: TEbObject); override;
      procedure DownloadObject(obj: TEbObject); override;
      function NewClassType: TEbClass; override;
   end;

implementation
uses
   Windows, OpenGL,
   Commons, SG_defs,
   EB_Maps;

{$R *.dfm}

const
   COLORBAR =
      '#version 120' + CRLF +
      'varying vec2 texture_coordinate;' + CRLF +
      'vec3 HSVtoRGB(vec3 color);' + CRLF +
      'vec3 ProcessShifts(vec3 rgbColor);' + CRLF + CRLF +
      'vec3 genHSV()' + CRLF +
      '{' + CRLF +
      '  vec3 result;' + CRLF +
      '  result.r = texture_coordinate.x;' + CRLF +
      '  result.g = (texture_coordinate.y * 1.25) + .25;' + CRLF +
      '  result.b = sqrt(mix(0.38, 0.95, 1.0 - texture_coordinate.y));' + CRLF +
      '  return(HSVtoRGB(result));' + CRLF +
      '}' + CRLF + CRLF +
      'void main(void)' + CRLF +
      '{gl_FragColor = vec4(ProcessShifts(genHSV()), 1.0);}';

   VERTEX =
      '#version 120' + CRLF +
      'varying vec2 texture_coordinate;' + CRLF + CRLF +
      'void main()' + CRLF +
      '{' + CRLF +
      '  gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;' + CRLF +
      '  texture_coordinate = vec2(mix(0.5, 1.0, gl_Position.x), mix(0.5, 1.0, gl_Position.y));' + CRLF +
      '  gl_FrontColor = vec4(1., 1., 1., 1.);' + CRLF +
      '}';

procedure TfrmEBEditTintScreen.FormCreate(Sender: TObject);
begin
   inherited;
   FRed := 100;
   FGreen := 100;
   FBlue := 100;
   FSaturation := 100;
end;

procedure TfrmEBEditTintScreen.Draw;
var
   rgba: TGLArrayf4;
begin
   if FShaders = nil then
      Exit;

   FShaders.SetUniformValue(FProgramHandle, 'satMult', FSaturation / 100);
   rgba[0] := FRed / 100;
   rgba[1] := FGreen / 100;
   rgba[2] := FBlue / 100;
   rgba[3] := 1;
   FShaders.SetUniformValue(FProgramHandle, 'rgbValues', rgba);
   glBegin(GL_QUADS);
      glVertex2i(0, 0);
      glVertex2i(0, imgReference.Height);
      glVertex2i(imgReference.Width, imgReference.Height);
      glVertex2i(imgReference.Width, 0);
   glEnd;
   imgReference.Flip;
end;

procedure TfrmEBEditTintScreen.Render(var msg: TMessage);
begin
   Draw;
end;

procedure TfrmEBEditTintScreen.imgReferenceAvailable(Sender: TObject);
begin
   FShaders := TdmShaders.Create(self);
   FProgramHandle := FShaders.ShaderProgram(VERTEX, COLORBAR, 'shift');
   FShaders.UseShaderProgram(FProgramHandle);
   FShaders.SetUniformValue(FProgramHandle, 'hShift', 0);
   FShaders.SetUniformValue(FProgramHandle, 'valMult', 1.0);
   PostMessage(self.Handle, WM_USER, 0, 0);
end;

procedure TfrmEBEditTintScreen.SetRed(value: integer);
begin
   FRed := value;
   sldRed.Value := value;
   spnRed.AsInteger := value;
end;

procedure TfrmEBEditTintScreen.SetGreen(value: integer);
begin
   FGreen := value;
   sldGreen.Value := value;
   spnGreen.AsInteger := value;
end;

procedure TfrmEBEditTintScreen.SetBlue(value: integer);
begin
   FBlue := value;
   sldBlue.Value := value;
   spnBlue.AsInteger := value;
end;

procedure TfrmEBEditTintScreen.SetSat(value: integer);
begin
   FSaturation := value;
   sldSat.Value := value;
   spnSat.AsInteger := value;
end;

procedure TfrmEBEditTintScreen.sldRedChange(Sender: TObject);
begin
   SetRed(sldRed.Value);
   Draw;
end;

procedure TfrmEBEditTintScreen.sldGreenChange(Sender: TObject);
begin
   SetGreen(sldGreen.Value);
   Draw;
end;

procedure TfrmEBEditTintScreen.sldBlueChange(Sender: TObject);
begin
   SetBlue(sldBlue.Value);
   Draw;
end;

procedure TfrmEBEditTintScreen.sldSatChange(Sender: TObject);
begin
   SetSat(sldSat.Value);
   Draw;
end;

procedure TfrmEBEditTintScreen.spnRedChange(Sender: TObject);
begin
   SetRed(spnRed.AsInteger);
   Draw;
end;

procedure TfrmEBEditTintScreen.spnGreenChange(Sender: TObject);
begin
   SetGreen(spnGreen.AsInteger);
   Draw;
end;

procedure TfrmEBEditTintScreen.spnBlueChange(Sender: TObject);
begin
   SetBlue(spnBlue.AsInteger);
   Draw;
end;

procedure TfrmEBEditTintScreen.spnSatChange(Sender: TObject);
begin
   SetSat(spnSat.AsInteger);
   Draw;
end;

function TfrmEBEditTintScreen.NewClassType: TEbClass;
begin
   result := TEBTintScreen;
end;

procedure TfrmEBEditTintScreen.UploadObject(obj: TEbObject);
begin
   SetRed(obj.Values[0]);
   SetGreen(obj.Values[1]);
   SetBlue(obj.Values[2]);
   SetSat(obj.Values[3]);
   spnLength.AsInteger := obj.values[4];
   chkWait.Checked := boolean(obj.values[5]);
end;

procedure TfrmEBEditTintScreen.DownloadObject(obj: TEbObject);
begin
   obj.Clear;
   obj.Values.AddRange(TArray<integer>.Create(FRed, FGreen, FBlue, FSaturation, spnLength.AsInteger, ord(chkWait.Checked)));
end;

initialization
   RegisterEbEditor(TEBTintScreen, TfrmEBEditTintScreen);
finalization
   UnRegisterEbEditor(TEBTintScreen);
end.
