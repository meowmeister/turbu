unit FTGL;

interface

const LIBFTGL = 'ftgl.dll';

type
   TFtglFont = record
   end;
   PFtglFont = ^TFtglFont;

   TFtglGlyph = record
   end;
   PFtglGlyph = ^TFtglGlyph;

   TFTGlyphSlot = record
   end;
   PFTGlyphSlot = ^TFTGlyphSlot;

   TFtglLayout = record
   end;
   PFtglLayout = ^TFtglLayout;

   TFloat6 = array[0..5] of single;

function ftglCreateBitmapFont(name: PAnsiChar): PftglFont; cdecl; external LIBFTGL;
function ftglCreateBufferFont(name: PAnsiChar): PftglFont; cdecl; external LIBFTGL;
function ftglCreateExtrudeFont(name: PAnsiChar): PftglFont; cdecl; external LIBFTGL;
function ftglCreateOutlineFont(name: PAnsiChar): PftglFont; cdecl; external LIBFTGL;
function ftglCreatePixmapFont(name: PAnsiChar): PftglFont; cdecl; external LIBFTGL;
function ftglCreatePolygonFont(name: PAnsiChar): PftglFont; cdecl; external LIBFTGL;
function ftglCreateTextureFont(name: PAnsiChar): PftglFont; cdecl; external LIBFTGL;
procedure ftglDestroyFont(F: PFtglFont); cdecl; external LIBFTGL;

function ftglCreateBitmapGlyph(glyph: PFTGlyphSlot): PFtglGlyph; cdecl; external LIBFTGL;
function ftglCreateExtrudeGlyph(glyph: PFTGlyphSlot; depth, frontOutset, backOutset: single;
   useDisplayList: integer): PFtglGlyph; cdecl; external LIBFTGL;
function ftglCreateOutlineGlyph(glyph: PFTGlyphSlot; outset: single;
   useDisplayList: integer): PFtglGlyph; cdecl; external LIBFTGL;
function ftglCreatePixmapGlyph(glyph: PFTGlyphSlot): PFtglGlyph; cdecl; external LIBFTGL;
function ftglCreatePolygonGlyph(glyph: PFTGlyphSlot; outset: single;
   useDisplayList: integer): PFtglGlyph; cdecl; external LIBFTGL;
function ftglCreateTextureGlyph(glyph: PFTGlyphSlot; id, xOffset, yOffset,
  width, height: integer): PFtglGlyph; cdecl; external LIBFTGL;
procedure ftglDetroyGlyph(glyph: PFtglGlyph); cdecl; external LIBFTGL;

function ftglCreateSimpleLayout: PFtglLayout; cdecl; external LIBFTGL;
procedure ftglDetroyLayout(l: PFtglLayout); cdecl; external LIBFTGL;

function ftglAttachFile(f:PFtglFont; fontFilePath: PAnsiChar): LongBool; cdecl; external LIBFTGL;
function ftglAttachData(f: PFtglFont; pBufferBytes: PAnsiChar;
  bufferSizeInBytes: integer): LongBool; cdecl; external LIBFTGL;
procedure ftglSetFontGlyphLoadFlags(f: PFtglFont; flags: integer); cdecl; external LIBFTGL;
function ftglSetFontFaceSize(f: PFtglFont; size, res: cardinal): LongBool; cdecl; external LIBFTGL;
function ftglGetFontFaceSize(f: PFtglFont): cardinal; cdecl; external LIBFTGL;
procedure ftglSetFontDepth(f: PFtglFont; depth: single); cdecl; external LIBFTGL;
procedure ftglSetFontOutset(f: PFtglFont; front, back: single); cdecl; external LIBFTGL;
procedure ftglSetFontDisplayList(f: PFtglFont; useList: LongBool); cdecl; external LIBFTGL;
function ftglGetFontAscender(f: PFtglFont): single; cdecl; external LIBFTGL;
function ftglGetFontDescender(f: PFtglFont): single; cdecl; external LIBFTGL;
function ftglGetFontLineHeight(f: PFtglFont): single; cdecl; external LIBFTGL;
procedure ftglGetFontBBox(f: PFtglFont; text: PAnsiChar; len: integer; var bbox: TFloat6); cdecl; external LIBFTGL;
function ftglGetFontAdvance(f: PFtglFont; text: PAnsiChar): single; cdecl; external LIBFTGL;
procedure ftglRenderFont(f: PFtglFont; text: PAnsiChar; mode: integer = 1); cdecl; external LIBFTGL;
function ftglGetFontError(f: PFtglFont): integer; cdecl; external LIBFTGL;

procedure ftglRenderGlyph(glyph: PFtglGlyph; penx, peny: double; renderMode: integer;
   var advancex, advancey: double); cdecl; external LIBFTGL;
function ftglGetGlyphAdvance(glyph: PFtglGlyph): single; cdecl; external LIBFTGL;
procedure ftglGetGlyphBBox(glyph: PFtglGlyph; var bounds: TFloat6); cdecl; external LIBFTGL;
function ftglGetGlyphError(glyph: PFtglGlyph): integer; cdecl; external LIBFTGL;

procedure ftglGetLayoutBBox(layout: PFtglLayout; text: PAnsiChar; var bounds: TFloat6); cdecl; external LIBFTGL;
procedure ftglRenderLayout(layout: PFtglLayout; text: PAnsiChar; mode: integer); cdecl; external LIBFTGL;
procedure ftglSetLayoutFont(layout: PFtglLayout; font: PFtglFont); cdecl; external LIBFTGL;
function ftglGetLayoutFont(layout: PFtglLayout): PFtglFont; cdecl; external LIBFTGL;
procedure ftglSetLayoutLineLength(layout: PFtglLayout; value: single); cdecl; external LIBFTGL;
function ftglGetLayoutLineLength(layout: PFtglLayout): single; cdecl; external LIBFTGL;
procedure ftglSetLayoutAlignment(layout: PFtglLayout; value: integer); cdecl; external LIBFTGL;
function ftglGetLayoutAlignement(layout: PFtglLayout): integer; cdecl; external LIBFTGL;
procedure ftglSetLayoutLineSpacing(layout: PFtglLayout; value: single); cdecl; external LIBFTGL;
function ftglGetLayoutLineSpacing(layout: PFtglLayout): single; cdecl; external LIBFTGL;
function ftglGetLayoutError(layout: PFtglLayout): integer; cdecl; external LIBFTGL;

(*// bool FTFont::CharMap(FT_Encoding encoding);
C_FUN(int, ftglSetFontCharMap, (FTGLfont *f, FT_Encoding enc),
      return 0, CharMap, (enc));

// FT_Encoding* FTFont::CharMapList();
C_FUN(FT_Encoding *, ftglGetFontCharMapList, (FTGLfont* f),
      return NULL, CharMapList, ());*)

implementation

end.
