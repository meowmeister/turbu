unit turbu_text_utils;

interface
uses
   SysUtils, Types, Generics.Collections,
   FTGL,
   sg_defs, sdl_canvas,
   dm_shaders,
   sdl_13;

type
   TRpgFont = class
   private
      FFont: PFtglFont;
      FSize: cardinal;
   public
      constructor Create(const name: string);
      destructor Destroy; override;
   end;

   TFontEngine = class
   private
      FCurrent: TRpgFont;
      FCharBlit: integer;
      FPass1: integer;
      FPass2: integer;
      FShaderEngine: TdmShaders;
      FTarget: TSdlRenderTarget;
      FGlyphs: TSdlTexture;
      FFonts: TObjectList<TRpgFont>;
      FOnGetColor: TFunc<TSdlTexture>;
      FOnGetDrawRect: TFunc<integer, TRect>;
      class var
         FFontPath: string;
      class constructor Create;
      procedure RenderChar(text: char);
      procedure DrawTargetPass1(x, y: single);
      procedure DrawTargetPass2(x, y: single; index: integer);
      procedure SetCurrent(const Value: TRpgFont);
      procedure RenderGlyph(index: integer);
   public
      constructor Create(shader: TdmShaders);
      destructor Destroy; override;

      function drawText(const text: string; x, y: single; colorIndex: integer): TsgFloatPoint;
      function drawChar(text: char; x, y: single; colorIndex: integer): TSgFloatPoint;
      function drawGlyph(index: integer; x, y: single; colorIndex: integer): TSgFloatPoint;
      function drawTextRightAligned(const text: string; x, y: single; colorIndex: integer): TsgFloatPoint;
      function drawTextCentered(const text: string; x, y: single;
        width, colorIndex: integer): TsgFloatPoint;

      property Current: TRpgFont read FCurrent write SetCurrent;
      property OnGetColor: TFunc<TSdlTexture> read FOnGetColor write FOnGetColor;
      property OnGetDrawRect: TFunc<integer, TRect> read FOnGetDrawRect write FOnGetDrawRect;
      property Glyphs: TSdlTexture read FGlyphs write FGlyphs;
   end;

   EFontError = class(Exception);

var
   GFontEngine: TFontEngine;

const TEXT_WIDTH = 6; //TODO: Find a way to make this customizable

implementation
uses
   Windows, ShlObj, OpenGL,
   turbu_OpenGL,
   SDL;

{ TRpgFont }

constructor TRpgFont.Create(const name: string);
var
   lName: utf8String;
begin
   lName := utf8String(TFontEngine.FFontPath + name);
   FFont := ftglCreateTextureFont(PAnsiChar(lName));
   if FFont = nil then
      raise EFontError.CreateFmt('Unable to load font %s.', [name]);
   ftglSetFontFaceSize(FFont, 11, 72);
   FSize := ftglGetFontFaceSize(FFont);
end;

destructor TRpgFont.Destroy;
begin
   ftglDestroyFont(FFont);
   inherited;
end;

{ TFontEngine }

class constructor TFontEngine.Create;
var
   aPath: array[0..MAX_PATH] of char;
begin
   SHGetFolderPath(0, CSIDL_FONTS, 0, 0, @aPath);
   FFontPath := IncludeTrailingPathDelimiter(string(aPath));
end;

constructor TFontEngine.Create(shader: TdmShaders);
begin
   assert(GFontEngine = nil);
   GFontEngine := self;
   FCharBlit := shader.ShaderProgram('textV', 'textBlit');
   FPass1 := shader.ShaderProgram('textV', 'textShadow');
   FPass2 := shader.ShaderProgram('textV', 'textF');
   FShaderEngine := shader;
   FTarget := TSdlRenderTarget.Create(sgPoint(16, 16));
   glBindTexture := SDL_GL_GetProcAddress('glBindTexture');
   FFonts := TObjectList<TRpgFont>.Create;
end;

destructor TFontEngine.Destroy;
begin
   assert(GFontEngine = self);
   GFontEngine := nil;
   FFonts.Free;
   FTarget.Free;
   inherited;
end;

procedure TFontEngine.RenderGlyph(index: integer);
const GLYPH_SIZE = 12;
var
   srcRect, dstRect: TRect;
begin
   FTarget.parent.pushRenderTarget;
   FTarget.SetRenderer;
   SDL_SetRenderDrawColor(FTarget.parent.Renderer, 0, 0, 0, 0);
   glClear(GL_COLOR_BUFFER_BIT);
   SDL_SetRenderDrawColor(FTarget.parent.Renderer, 255, 255, 255, 255);
   srcRect := rect((index mod 13) * GLYPH_SIZE, (index div 13) * GLYPH_SIZE, GLYPH_SIZE, GLYPH_SIZE);
   dstRect := rect(0, 0, GLYPH_SIZE, GLYPH_SIZE);
   SDL_RenderCopy(FTarget.parent.Renderer, FGlyphs, @srcRect, @dstRect);
   FTarget.parent.popRenderTarget;
end;

procedure TFontEngine.RenderChar(text: char);
var
   aText: UTF8String;
begin
   aText := utf8String(text);
   FTarget.parent.pushRenderTarget;
   FTarget.SetRenderer;
   SDL_SetRenderDrawColor(FTarget.parent.Renderer, 0, 0, 0, 0);
   glClear(GL_COLOR_BUFFER_BIT);
   FShaderEngine.UseShaderProgram(FCharBlit);

   //work around a glitch in FTGL
   glMatrixMode(GL_MODELVIEW);
   glPushMatrix;
   glTranslatef(0, 2, 0);
   ftglRenderFont(FCurrent.FFont, PAnsiChar(aText));
   glMatrixMode(GL_MODELVIEW);
   glPopMatrix;

   glBindTexture(GL_TEXTURE_2D, 0);
   SDL_SetRenderDrawColor(FTarget.parent.Renderer, 255, 255, 255, 255);
   FTarget.parent.popRenderTarget;
end;

procedure TFontEngine.SetCurrent(const Value: TRpgFont);
begin
   FCurrent := Value;
   if not FFonts.Contains(value) then
      FFonts.Add(value);
end;

procedure TFontEngine.DrawTargetPass1(x, y: single);
begin
   FShaderEngine.UseShaderProgram(FPass1);
   FShaderEngine.SetUniformValue(FPass1, 'strength', 0.7);
   glBegin(GL_QUADS);
      glTexCoord2i(0, 0); glVertex2f(x, y);
      glTexCoord2i(0, FTarget.Height); glVertex2f(x, y + FTarget.Height);
      glTexCoord2i(FTarget.Width, FTarget.Height); glVertex2f(x + FTarget.Width, y + FTarget.Height);
      glTexCoord2i(FTarget.Width, 0); glVertex2f(x + FTarget.Width, y);
   glEnd;
end;

procedure TFontEngine.DrawTargetPass2(x, y: single; index: integer);
var
   rect: TRect;
begin
   rect := FOnGetDrawRect(index);
   FShaderEngine.UseShaderProgram(FPass2);
   glEnable(GL_MULTISAMPLE);
   glActiveTextureARB(GL_TEXTURE1);
   glEnable(GL_TEXTURE_RECTANGLE_ARB);
   FOnGetColor().bind;
   glActiveTextureARB(GL_TEXTURE0);

   FShaderEngine.SetUniformValue(FPass2, 'texAlpha', 0);
   FShaderEngine.SetUniformValue(FPass2, 'texRGB', 1);
glCheckError;
   glBegin(GL_QUADS);
      glMultiTexCoord2iARB(GL_TEXTURE0, 0, 0);
      glMultiTexCoord2iARB(GL_TEXTURE1, rect.Left, rect.Top);
      glVertex2f(x, y);

      glMultiTexCoord2iARB(GL_TEXTURE0, 0, FTarget.Height);
      glMultiTexCoord2iARB(GL_TEXTURE1, rect.Left, rect.Top + rect.Bottom);
      glVertex2f(x, y + FTarget.Height);

      glMultiTexCoord2iARB(GL_TEXTURE0, FTarget.Width, FTarget.Height);
      glMultiTexCoord2iARB(GL_TEXTURE1, rect.Left + rect.right, rect.Top + rect.Bottom);
      glVertex2f(x + FTarget.Width, y + FTarget.Height);

      glMultiTexCoord2iARB(GL_TEXTURE0, FTarget.Width, 0);
      glMultiTexCoord2iARB(GL_TEXTURE1, rect.Left + rect.right, rect.top);
      glVertex2f(x + FTarget.Width, y);
   glEnd;
   glDisable(GL_MULTISAMPLE);
glCheckError;
end;

function TFontEngine.drawChar(text: char; x, y: single; colorIndex: integer): TSgFloatPoint;
var
   current: GLInt;
begin
   glGetIntegerv(GL_CURRENT_PROGRAM, @current);
   result := sgPointF(x + TEXT_WIDTH, y);

   glActiveTextureARB(GL_TEXTURE0);
   glEnable(GL_TEXTURE_RECTANGLE_ARB);
   FTarget.handle.bind;

   RenderChar(text);
   DrawTargetPass1(x + 1, y + 1);
   DrawTargetPass2(x, y, colorIndex);
   glUseProgram(current);
end;

function TFontEngine.drawGlyph(index: integer; x, y: single; colorIndex: integer): TSgFloatPoint;
var
   current: GLInt;
begin
   glGetIntegerv(GL_CURRENT_PROGRAM, @current);
   result := sgPointF(x + (TEXT_WIDTH * 2), y);

   glActiveTextureARB(GL_TEXTURE0);
   glEnable(GL_TEXTURE_RECTANGLE_ARB);

   RenderGlyph(index);
   FTarget.handle.bind;
   DrawTargetPass1(x + 1, y + 1);
   DrawTargetPass2(x, y, colorIndex);
   glUseProgram(current);
end;

function TFontEngine.drawText(const text: string; x, y: single;
  colorIndex: integer): TsgFloatPoint;
var
   aChar: char;
begin
   result := sgPointF(x, y);
   for aChar in text do
      result := drawChar(aChar, result.x, result.y, colorIndex);
end;

function TFontEngine.drawTextCentered(const text: string; x, y: single;
  width, colorIndex: integer): TsgFloatPoint;
var
   textWidth: integer;
   midpoint: single;
begin
   textWidth := (length(text) * TEXT_WIDTH);
   midpoint := x + (width / 2);
   drawText(text, midpoint - (textWidth div 2), y, colorIndex);
end;

function TFontEngine.drawTextRightAligned(const text: string; x, y: single;
  colorIndex: integer): TsgFloatPoint;
begin
   result := sgPointF(x - (length(text) * TEXT_WIDTH), y);
   drawText(text, result.x, result.y, colorIndex);
end;

initialization
finalization
   GFontEngine.Free;
end.
