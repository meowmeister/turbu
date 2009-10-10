unit sg_utils;

interface
uses
   types,
   SG_defs;

function constrictRect(const value: TRect; amount: integer): TRect;
function expandRect(const value: TRect; amount: integer): TRect;
function multiplyRect(const value: TRect; amount: integer): TRect;
function pointToGridLoc(const point, cellSize: TSgPoint; const hScroll, vScroll,
                        scale: integer): TSgPoint;

implementation

function expandRect(const value: TRect; amount: integer): TRect;
begin
   result.Left := value.Left - amount;
   result.Top := value.Top - amount;
   result.Right := value.Right + amount;
   result.Bottom := value.Bottom + amount;
end;

function constrictRect(const value: TRect; amount: integer): TRect;
begin
   result := expandRect(value, -amount);
end;

function multiplyRect(const value: TRect; amount: integer): TRect;
begin
   result.Left := value.Left * amount;
   result.Top := value.Top * amount;
   result.Right := value.Right * amount;
   result.Bottom := value.Bottom * amount;
end;

function pointToGridLoc(const point, cellSize: TSgPoint; const hScroll, vScroll,
                        scale: integer): TSgPoint;
begin
   result := ((point / scale) + sgPoint(hScroll, vScroll)) / cellSize;
end;

end.
