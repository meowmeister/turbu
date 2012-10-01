unit sg_utils;

interface
uses
   types,
   SG_defs;

function constrictRect(const value: TRect; amount: integer): TRect;
function expandRect(const value: TRect; amount: integer): TRect;
function constrictSdlRect(const value: TRect; amount: integer): TRect;
function expandSdlRect(const value: TRect; amount: integer): TRect;
function multiplyRect(const value: TRect; amount: integer): TRect; overload;
function multiplyRect(const value: TRect; amount: TSgPoint): TRect; overload;
function multiplyRect(const value: TRect; amount: single): TRect; overload;
function pointToGridLoc(const point, cellSize: TSgPoint; const hScroll, vScroll: integer;
                        const scale: double): TSgPoint;
function gridLocToPoint(const point, cellSize: TSgPoint; const hScroll, vScroll: integer;
                        const scale: double): TSgPoint;
function TRectToSdlRect(const value: TRect): TRect;

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

function expandSdlRect(const value: TRect; amount: integer): TRect;
begin
   result.Left := value.Left - amount;
   result.Top := value.Top - amount;
   result.Right := value.Right + (amount * 2);
   result.Bottom := value.Bottom + (amount * 2);
end;

function constrictSdlRect(const value: TRect; amount: integer): TRect;
begin
   result := expandSdlRect(value, -amount);
end;

function multiplyRect(const value: TRect; amount: integer): TRect;
begin
   result.Left := value.Left * amount;
   result.Top := value.Top * amount;
   result.Right := value.Right * amount;
   result.Bottom := value.Bottom * amount;
end;

function multiplyRect(const value: TRect; amount: single): TRect;
begin
   result.Left := round(value.Left * amount);
   result.Top := round(value.Top * amount);
   result.Right := round(value.Right * amount);
   result.Bottom := round(value.Bottom * amount);
end;

function multiplyRect(const value: TRect; amount: TSgPoint): TRect;
begin
   result.Left := value.Left * amount.x;
   result.Top := value.Top * amount.y;
   result.Right := value.Right * amount.x;
   result.Bottom := value.Bottom * amount.y;
end;

function pointToGridLoc(const point, cellSize: TSgPoint; const hScroll, vScroll: integer;
                        const scale: double): TSgPoint;
begin
   result := ((point / scale) + sgPoint(hScroll, vScroll)) / cellSize;
end;

function gridLocToPoint(const point, cellSize: TSgPoint; const hScroll, vScroll: integer;
                        const scale: double): TSgPoint;
begin
   result.x := round(point.x * scale * cellsize.x) - hScroll;
   result.y := round(point.y * scale * cellsize.y) - vScroll;
end;


function TRectToSdlRect(const value: TRect): TRect;
begin
   result.TopLeft := value.TopLeft;
   result.Right := value.Right - value.Left;
   result.Bottom := value.Bottom - value.Top;
end;

end.
