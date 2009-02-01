unit SG_defs;

interface
uses
   types;

type
   TSgPoint = record //TPoint with equality tests and
      x, y: integer; //basic vector math operators defined
      class operator Equal(a, b: TSgPoint): boolean; inline;
      class operator NotEqual(a, b: TSgPoint): boolean; inline;
      class operator Multiply(a: TSgPoint; b: integer): TSgPoint; inline;
      class operator Divide(a: TSgPoint; b: integer): TSgPoint; inline;
      class operator Add(a, b: TSgPoint): TSgPoint; inline;
      class operator Subtract(a, b: TSgPoint): TSgPoint; inline;
      class operator Implicit(a: TPoint): TSgPoint; inline;
      class operator Implicit(a: TSgPoint): TPoint; inline;
   end;

   TSgPoint2 = record //TPoint with equality tests and
      x, y: single; //basic vector math operators defined
      class operator Equal(a, b: TSgPoint2): boolean; inline;
      class operator NotEqual(a, b: TSgPoint2): boolean; inline;
      class operator Multiply(a: TSgPoint2; b: single): TSgPoint2; inline;
      class operator Divide(a: TSgPoint2; b: single): TSgPoint2; inline;
      class operator Add(a, b: TSgPoint2): TSgPoint2; inline;
      class operator Subtract(a, b: TSgPoint2): TSgPoint2; inline;
      class operator Implicit(a: TPoint): TSgPoint2; inline;
      class operator Implicit(a: TSgPoint2): TPoint; inline;
   end;

   TSgColor = record
      class operator Implicit(a: Cardinal): TSgcolor; inline;
      class operator Implicit(a: TSgcolor): Cardinal; inline;
   case boolean of
      false: (color: Cardinal);
      true: (rgba: packed array[1..4] of byte);
   end;


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

class operator TSgPoint.Divide(a: TSgPoint; b: integer): TSgPoint;
begin
   result.x := round(a.x / b);
   result.y := round(a.y / b);
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

{ TSgPoint2 }

class operator TSgPoint2.Add(a, b: TSgPoint2): TSgPoint2;
begin
   result.x := a.x + b.x;
   result.y := a.y + b.y;
end;

class operator TSgPoint2.Subtract(a, b: TSgPoint2): TSgPoint2;
begin
   result.x := a.x - b.x;
   result.y := a.y - b.y;
end;

class operator TSgPoint2.Multiply(a: TSgPoint2; b: single): TSgPoint2;
begin
   result.x := a.x * b;
   result.y := a.y * b;
end;

class operator TSgPoint2.Divide(a: TSgPoint2; b: single): TSgPoint2;
begin
   result.x := round(a.x / b);
   result.y := round(a.y / b);
end;

class operator TSgPoint2.Equal(a, b: TSgPoint2): boolean;
begin
   result := (a.x = b.x) and (a.y = b.y);
end;

class operator TSgPoint2.Implicit(a: TSgPoint2): TPoint;
begin
   system.Move(a, result, sizeof(TPoint));
end;

class operator TSgPoint2.Implicit(a: TPoint): TSgPoint2;
begin
   system.Move(a, result, sizeof(TPoint));
end;

class operator TSgPoint2.NotEqual(a, b: TSgPoint2): boolean;
begin
   result := not (a = b);
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

end.
