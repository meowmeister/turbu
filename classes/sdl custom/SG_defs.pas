unit SG_defs;

interface
uses
   types,
   sdl_13;

type
   TSgPoint = record //TPoint with equality tests and
      x, y: integer; //basic vector math operators defined
      class operator Equal(a, b: TSgPoint): boolean; inline; static;
      class operator NotEqual(a, b: TSgPoint): boolean; inline; static;
      class operator Multiply(a: TSgPoint; b: integer): TSgPoint; inline; static;
      class operator Multiply(a, b: TSgPoint): TSgPoint; inline; static;
      class operator Multiply(a: TSgPoint; b: double): TSgPoint; inline; static;
      class operator Divide(a: TSgPoint; b: integer): TSgPoint; inline; static;
      class operator Divide(a: TSgPoint; b: TSgPoint): TSgPoint; inline; static;
      class operator Divide(a: TSgPoint; b: double): TSgPoint; inline; static;
      class operator Modulus(a, b: TSgPoint): TSgPoint; inline; static;
      class operator Add(a, b: TSgPoint): TSgPoint; inline; static;
      class operator Subtract(a, b: TSgPoint): TSgPoint; inline; static;
      class operator Implicit(a: TPoint): TSgPoint; inline; static;
      class operator Implicit(a: TSgPoint): TPoint; inline; static;
   end;

   TSgFloatPoint = record //floating-point TPoint with equality tests and
      x, y: single;       //basic vector math operators defined
      class operator Equal(a, b: TSgFloatPoint): boolean; inline; static;
      class operator NotEqual(a, b: TSgFloatPoint): boolean; inline; static;
      class operator Multiply(a: TSgFloatPoint; b: double): TSgFloatPoint; inline; static;
      class operator Divide(a: TSgFloatPoint; b: double): TSgFloatPoint; inline; static;
      class operator Add(a, b: TSgFloatPoint): TSgFloatPoint; inline; static;
      class operator Subtract(a, b: TSgFloatPoint): TSgFloatPoint; inline; static;
      class operator Implicit(a: TPoint): TSgFloatPoint; inline; static;
      class operator Implicit(a: TSgPoint): TSgFloatPoint; inline; static;
   end;

   TSgColor = record
      class operator Implicit(a: Cardinal): TSgcolor; inline; static;
      class operator Implicit(a: TSgcolor): Cardinal; inline; static;
      class operator Implicit(a: TSgcolor): SDL_Color; inline; static;
   case boolean of
      false: (color: Cardinal);
      true: (rgba: packed array[1..4] of byte);
   end;

const ORIGIN: TSgPoint = (x: 0; y: 0);

function sgPoint(x, y: integer): TSgPoint;
function sgPointF(x, y: single): TSgFloatPoint;

const
//TODO: AsphyreDefs backwards compatibility. Replace this when possible
 fxOneColor    = $7FFFFFF6;

const
   SDL_BLACK: sdl_13.TSDL_Color = (unused: $FF);
   SDL_WHITE: sdl_13.TSDL_Color = (r: $FF; g: $FF; b:$FF; unused: $FF);
   SDL_GREEN: sdl_13.TSDL_Color = (r: $00; g: $90; b:$35; unused: $FF);

type
   TDrawFX = integer;

implementation

{ TSgPoint }

class operator TSgPoint.Add(a, b: TSgPoint): TSgPoint;
begin
   result.x := a.x + b.x;
   result.y := a.y + b.y;
end;

class operator TSgPoint.Subtract(a, b: TSgPoint): TSGPoint;
begin
   result.x := a.x - b.x;
   result.y := a.y - b.y;
end;

class operator TSgPoint.Multiply(a: TSgPoint; b: integer): TSgPoint;
begin
   result.x := a.x * b;
   result.y := a.y * b;
end;

class operator TSgPoint.Multiply(a, b: TSgPoint): TSgPoint;
begin
   result.x := a.x * b.x;
   result.y := a.y * b.y;
end;

class operator TSgPoint.Divide(a: TSgPoint; b: integer): TSgPoint;
begin
   result.x := a.x div b;
   result.y := a.y div b;
end;

class operator TSgPoint.Divide(a: TSgPoint; b: TSgPoint): TSgPoint;
begin
   result.x := a.x div b.x;
   result.y := a.y div b.y;
end;

class operator TSgPoint.Modulus(a, b: TSgPoint): TSgPoint;
begin
   result.x := a.x mod b.x;
   result.y := a.y mod b.y;
end;

class operator TSgPoint.Equal(a, b: TSgPoint): boolean;
begin
   result := (a.x = b.x) and (a.y = b.y);
end;

class operator TSgPoint.Implicit(a: TSgPoint): TPoint;
begin
   system.Move(a, result, sizeof(TPoint));
end;

class operator TSgPoint.Implicit(a: TPoint): TSgPoint;
begin
   system.Move(a, result, sizeof(TPoint));
end;

class operator TSgPoint.NotEqual(a, b: TSgPoint): boolean;
begin
   result := not (a = b);
end;

function sgPoint(x, y: integer): TSgPoint;
begin
   result.x := x;
   result.y := y;
end;

class operator TSgPoint.Multiply(a: TSgPoint; b: double): TSgPoint;
begin
   result.x := round(a.x * b);
   result.y := round(a.y * b);
end;

class operator TSgPoint.Divide(a: TSgPoint; b: double): TSgPoint;
begin
   result.x := round(a.x / b);
   result.y := round(a.y / b);
end;

{ TSgFloatPoint }

class operator TSgFloatPoint.Add(a, b: TSgFloatPoint): TSgFloatPoint;
begin
   result.x := a.x + b.x;
   result.y := a.y + b.y;
end;

class operator TSgFloatPoint.Subtract(a, b: TSgFloatPoint): TSgFloatPoint;
begin
   result.x := a.x - b.x;
   result.y := a.y - b.y;
end;

class operator TSgFloatPoint.Multiply(a: TSgFloatPoint; b: double): TSgFloatPoint;
begin
   result.x := a.x * b;
   result.y := a.y * b;
end;

class operator TSgFloatPoint.Divide(a: TSgFloatPoint; b: double): TSgFloatPoint;
begin
   result.x := a.x / b;
   result.y := a.y / b;
end;

class operator TSgFloatPoint.Equal(a, b: TSgFloatPoint): boolean;
begin
   result := (a.x = b.x) and (a.y = b.y);
end;

class operator TSgFloatPoint.Implicit(a: TPoint): TSgFloatPoint;
begin
   result.x := a.X;
   result.y := a.Y;
end;

class operator TSgFloatPoint.Implicit(a: TSgPoint): TSgFloatPoint;
begin
   result.x := a.X;
   result.y := a.Y;
end;

class operator TSgFloatPoint.NotEqual(a, b: TSgFloatPoint): boolean;
begin
   result := not (a = b);
end;

function sgPointF(x, y: single): TSgFloatPoint;
begin
   result.x := x;
   result.y := y;
end;

{ TSgColor }

class operator TSgColor.Implicit(a: Cardinal): TSgcolor;
begin
   result.color := a;
end;

class operator TSgColor.Implicit(a: TSgcolor): Cardinal;
begin
   result := a.color;
end;

class operator TSgColor.Implicit(a: TSgcolor): SDL_Color;
begin
   result.r := a.rgba[1];
   result.g := a.rgba[2];
   result.b := a.rgba[3];
   result.unused := a.rgba[4];
end;

end.
