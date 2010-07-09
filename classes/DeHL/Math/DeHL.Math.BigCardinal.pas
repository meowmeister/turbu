(*
* Copyright (c) 2009, Ciobanu Alexandru
* All rights reserved.
*
* Ideea based on Matt McCutchen's C++ Big Integer Library
* which was released to public domain. Site: http://mattmccutchen.net/bigint/index.html
*
* Redistribution and use in source and binary forms, with or without
* modification, are permitted provided that the following conditions are met:
*     * Redistributions of source code must retain the above copyright
*       notice, this list of conditions and the following disclaimer.
*     * Redistributions in binary form must reproduce the above copyright
*       notice, this list of conditions and the following disclaimer in the
*       documentation and/or other materials provided with the distribution.
*     * Neither the name of the <organization> nor the
*       names of its contributors may be used to endorse or promote products
*       derived from this software without specific prior written permission.
*
* THIS SOFTWARE IS PROVIDED BY THE AUTHOR ''AS IS'' AND ANY
* EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
* WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
* DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
* DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
* (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
* LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
* ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
* (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
* SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

{$I ../DeHL.Defines.inc}
unit DeHL.Math.BigCardinal;
interface
uses SysUtils,
     Variants,
     DeHL.Base,
     DeHL.Serialization,
     DeHL.Exceptions,
     DeHL.Math.Types,
     DeHL.Types;

type
  { Unsigned Number }
  BigCardinal = record
  private
  type
    TNumberPiece = Cardinal;
    TNumberPieceArray = array of TNumberPiece;

  const
    BytesInPiece  = SizeOf(TNumberPiece);
    BitsInPiece   = BytesInPiece * 8;
    BCDDigitBits  = 4;
    BCDMask       = $F;

  var
    FLength: Cardinal;
    FArray: TNumberPieceArray;

    { Length utils }
    procedure SetLength(const ALength: Cardinal); inline;
    procedure RemoveLeadingZeroes(); inline;

    { Internals }
    class function GetShiftedPiece(const A: BigCardinal; const Index, Count: Cardinal): TNumberPiece; static; inline;
    procedure CalcModulus(const Divisor: BigCardinal; var Quotient: BigCardinal);
    procedure CopyPieces(var Dest; const Count: Cardinal); inline;
    procedure SetPieces(const Source; const Count: Cardinal); inline;

    { For BCD support }
    function GetBCDDigitFrom(const Piece, Bit: Cardinal): Cardinal;
    function SetBCDDigitFrom(const Piece, Bit: Cardinal;  const Value: Cardinal): Cardinal;
    function BitLength(): Cardinal;

    function ToBCD(): BigCardinal;

  public
    { Constructors}
    constructor Create(const ANumber: UInt64); overload;
    constructor Create(const ANumber: Int64); overload;
    constructor Create(const ANumber: Cardinal); overload;
    constructor Create(const ANumber: Integer); overload;
    constructor Create(const ANumber: BigCardinal); overload;

    { Comparison }
    function CompareTo(const ANumber: BigCardinal): Integer;

    { Converters }
    function ToByte(): Byte; inline;
    function ToWord(): Word; inline;
    function ToCardinal(): Cardinal; inline;
    function ToUInt64(): UInt64; inline;
    function ToShortInt(): ShortInt; inline;
    function ToSmallInt(): SmallInt; inline;
    function ToInteger(): Integer; inline;
    function ToInt64(): Int64; inline;
    function ToAnsiChar(): AnsiChar; inline;
    function ToWideChar(): WideChar; inline;

    { Comparison operators }
    class operator Equal(const ALeft, ARight: BigCardinal): Boolean;
    class operator NotEqual(const ALeft, ARight: BigCardinal): Boolean;
    class operator GreaterThan(const ALeft, ARight: BigCardinal): Boolean;
    class operator GreaterThanOrEqual(const ALeft, ARight: BigCardinal): Boolean;
    class operator LessThan(const ALeft, ARight: BigCardinal): Boolean;
    class operator LessThanOrEqual(const ALeft, ARight: BigCardinal): Boolean;

    { Arithmetic operators }
    class operator Add(const ALeft, ARight: BigCardinal): BigCardinal;
    class operator Subtract(const ALeft, ARight: BigCardinal): BigCardinal;
    class operator Multiply(const ALeft, ARight: BigCardinal): BigCardinal;
    class operator IntDivide(const ALeft, ARight: BigCardinal): BigCardinal;
    class operator Modulus(const ALeft, ARight: BigCardinal): BigCardinal;
    class operator Negative(const AValue: BigCardinal): BigCardinal;
    class operator Positive(const AValue: BigCardinal): BigCardinal;

    class operator Inc(const AValue: BigCardinal): BigCardinal;
    class operator Dec(const AValue: BigCardinal): BigCardinal;

    { Bitwise/Logical operators }
    class operator LeftShift(const AValue: BigCardinal; const ACount: Cardinal): BigCardinal;
    class operator RightShift(const AValue: BigCardinal; const ACount: Cardinal): BigCardinal;
    class operator BitwiseAnd(const ALeft, ARight: BigCardinal): BigCardinal;
    class operator LogicalNot(const AValue: BigCardinal): BigCardinal;
    class operator BitwiseOr(const ALeft, ARight: BigCardinal): BigCardinal;
    class operator BitwiseXor(const ALeft, ARight: BigCardinal): BigCardinal;

    { Implicits }
    class operator Implicit(const ANumber: Byte): BigCardinal;
    class operator Implicit(const ANumber: Word): BigCardinal;
    class operator Implicit(const ANumber: Cardinal): BigCardinal;
    class operator Implicit(const ANumber: UInt64): BigCardinal;
    class operator Implicit(const ANumber: BigCardinal): Variant;

    { Explicits }
    class operator Explicit(const ANumber: BigCardinal): Byte;
    class operator Explicit(const ANumber: BigCardinal): Word;
    class operator Explicit(const ANumber: BigCardinal): Cardinal;
    class operator Explicit(const ANumber: BigCardinal): UInt64;

    class operator Explicit(const ANumber: BigCardinal): ShortInt;
    class operator Explicit(const ANumber: BigCardinal): SmallInt;
    class operator Explicit(const ANumber: BigCardinal): Integer;
    class operator Explicit(const ANumber: BigCardinal): Int64;

    class operator Explicit(const ANumber: BigCardinal): AnsiChar;
    class operator Explicit(const ANumber: BigCardinal): WideChar;
  end;

  { BigCardinal Support }
  TBigCardinalType = class(TRecordType<BigCardinal>)
  protected
    { Serialization }
    procedure DoSerialize(const AInfo: TValueInfo; const AValue: BigCardinal; const AContext: ISerializationContext); override;
    procedure DoDeserialize(const AInfo: TValueInfo; out AValue: BigCardinal; const AContext: IDeserializationContext); override;

  public
    { Comparator }
    function Compare(const AValue1, AValue2: BigCardinal): Integer; override;

    { Equality Comparator }
    function AreEqual(const AValue1, AValue2: BigCardinal): Boolean; override;

    { Hash code provider }
    function GenerateHashCode(const AValue: BigCardinal): Integer; override;

    { Get String representation }
    function GetString(const AValue: BigCardinal): String; override;

    { Type information }
    function Family(): TTypeFamily; override;

    { Variant Conversion }
    function TryConvertToVariant(const AValue: BigCardinal; out ORes: Variant): Boolean; override;
    function TryConvertFromVariant(const AValue: Variant; out ORes: BigCardinal): Boolean; override;
  end;

  { Math extensions for the BigCardinal type }
  TBigCardinalMathExtension = class sealed(TUnsignedIntegerMathExtension<BigCardinal>)
  public
    { Standard operations }
    function Add(const AValue1, AValue2: BigCardinal): BigCardinal; override;
    function Subtract(const AValue1, AValue2: BigCardinal): BigCardinal; override;
    function Multiply(const AValue1, AValue2: BigCardinal): BigCardinal; override;
    function IntegralDivide(const AValue1, AValue2: BigCardinal): BigCardinal; override;
    function Modulo(const AValue1, AValue2: BigCardinal): BigCardinal; override;

    { Neutral Math elements }
    function Zero: BigCardinal; override;
    function One: BigCardinal; override;
  end;


{ Convert a BigCardinal to a string }
function UIntToStr(Value: BigCardinal): String; overload;

{ Convert a BigCardinal to a hex }
function UIntToHex(Value: BigCardinal): String; overload;

{ Convert a string to a BigCardinal }
function StrToBigCardinal(const S: String): BigCardinal;

{ Convert a hex string to a BigCardinal }
function HexToBigCardinal(const S: String): BigCardinal;

{ The variant Id of BigCardinal }
var
  varBigCardinal: TVarType;

implementation

{ Disable overflow-cheks! but preserve the value first }
{$IFOPT Q+}
{$DEFINE BIGCARDINAL_OVERFLOW_CHECKS}
{$ENDIF}

{$Q-}

const
  { Zero! }
  BigCardinalZero: BigCardinal =
  (
    FLength: 0;
    FArray: nil;
  );

{ Utility functions }

procedure Fill4BitDigits(var S: String; const Value: BigCardinal);
const
  Digits: array[$0..$F] of Char = ('0', '1', '2', '3', '4', '5', '6', '7', '8',
    '9', 'A', 'B', 'C', 'D', 'E', 'F');

var
  PieceBytes: array[0..(BigCardinal.BytesInPiece - 1)] of Byte;
  Piece: BigCardinal.TNumberPiece absolute PieceBytes;

  I, X: Cardinal;
begin
  SetLength(S, Value.FLength * (BigCardinal.BytesInPiece * 2));

  for I := 0 to Value.FLength - 1 do
  begin
    Piece := Value.FArray[I];
    X := (Value.FLength - 1 - I) * (BigCardinal.BytesInPiece * 2);

    S[X + 8] := Digits[(PieceBytes[0] and $F)];
    S[X + 7] := Digits[(PieceBytes[0] shr 4)];

    S[X + 6] := Digits[(PieceBytes[1] and $F)];
    S[X + 5] := Digits[(PieceBytes[1] shr 4)];

    S[X + 4] := Digits[(PieceBytes[2] and $F)];
    S[X + 3] := Digits[(PieceBytes[2] shr 4)];

    S[X + 2] := Digits[(PieceBytes[3] and $F)];
    S[X + 1] := Digits[(PieceBytes[3] shr 4)];
  end;
end;

function TrimLeftZeros(const Str: String): String; inline;
var
  I, X: Cardinal;
begin
  Result := Str;

  if Str = '' then
    Exit;

  X := 0;

  for I := 1 to Length(Str) do
  begin
    if Str[I] <> '0' then
    begin
      X := I - 1;
      break;
    end;
  end;

  if X > 0 then
    Delete(Result, 1, X);
end;

function UIntToHex(Value: BigCardinal): String;
begin
  if Value = 0 then
    Exit('0');

  { For small numbers call the Cardinal converter }
  if SizeOf(Cardinal) >= (Value.FLength * BigCardinal.BytesInPiece) then
  begin
    Result := TrimLeftZeros(IntToHex(Value.ToCardinal(), 8));
    Exit;
  end;

  { For 64 bit numbers call the int64 converter }
  if SizeOf(UInt64) >= (Value.FLength * BigCardinal.BytesInPiece) then
  begin
    Result := TrimLeftZeros(IntToHex(Value.ToUInt64(), 16));
    Exit;
  end;

  { Fill in the digits }
  Fill4BitDigits(Result, Value);

  { Reduce the number of zeroes at the beggining }
  Result := TrimLeftZeros(Result);
end;

function UIntToStr(Value: BigCardinal): String;
var
  BCDValue: BigCardinal;

begin
  { Do nothing for value 0 }
  if Value = 0 then
    Exit('0');

  { For small numbers call the Cardinal converter }
  if SizeOf(Cardinal) >= (Value.FLength * BigCardinal.BytesInPiece) then
    Exit(UIntToStr(Value.ToCardinal()));

  { For 64 bit numbers call the int64 converter }
  if SizeOf(UInt64) >= (Value.FLength * BigCardinal.BytesInPiece) then
    Exit(UIntToStr(Value.ToUInt64()));

  { Generate a BCD version }
  BCDValue := Value.ToBCD();

  { Fill in the digits }
  Fill4BitDigits(Result, BCDValue);

  { Reduce the number of zeroes at the beggining }
  Result := TrimLeftZeros(Result);
end;

function StrToBigCardinal(const S: String): BigCardinal;
var
  M: BigCardinal;
  I: Cardinal;
  S2: String;
begin
  { Default = 0 }
  Result := 0;

  S2 := TrimLeft(S);

  { Empty string case }
  if S2 = '' then
    ExceptionHelper.Throw_ArgumentConverError('S');

  { Call the HEX version }
  if S2[1] = '$' then
  begin
    Delete(S2, 1, 1);
    Exit(HexToBigCardinal(S2));
  end;

  M := 1;

  for I := Length(S2) downto 1 do
  begin
    if CharInSet(S2[I], ['0' .. '9']) then
      Result := Result + (Cardinal(Ord(S2[I]) - Ord('0')) * M)
    else
      ExceptionHelper.Throw_ArgumentConverError('S');

    { + 1 base high }
    M := M * 10;
  end;
end;

function HexToBigCardinal(const S: String): BigCardinal;
var
  M: BigCardinal;
  I: Cardinal;
  S2: String;
begin
  { Default = 0 }
  Result := 0;

  S2 := TrimLeft(S);

  { Empty string case }
  if S2 = '' then
    ExceptionHelper.Throw_ArgumentConverError('S');

  M := 1;
  for I := Length(S2) downto 1 do
  begin
    if CharInSet(S2[I], ['0' .. '9']) then
      Result := Result + (Cardinal(Ord(S2[I]) - Ord('0')) * M)
    else if CharInSet(S2[I], ['A' .. 'F']) then
      Result := Result + (Cardinal(Ord(S2[I]) - Ord('A') + $A) * M)
    else if CharInSet(S2[I], ['a' .. 'f']) then
      Result := Result + (Cardinal(Ord(S2[I]) - Ord('a') + $A) * M)
    else
      ExceptionHelper.Throw_ArgumentConverError('S');

    { + 1 base high }
    M := M * 16;
  end;
end;


{ Variant Support }

type
  PBigCardinal = ^BigCardinal;

  { Mapping the BigCardinal into TVarData structure }
  TBigCardinalVarData = packed record
    { Var type, will be assigned at runtime }
    VType: TVarType;
    { Reserved stuff }
    Reserved1, Reserved2, Reserved3: Word;
    { A reference to the enclosed big cardinal }
    BigCardinalPtr: PBigCardinal;
    { Reserved stuff }
    Reserved4: LongWord;
  end;

  { Manager for our variant type }
  TBigCardinalVariantType = class(TCustomVariantType)
  private
    { Will create a big cardinal, or raise an error }
    function VarDataToBigCardinal(const Value: TVarData): BigCardinal;
    procedure BigCardinalToVarData(const Value: BigCardinal; var OutValue: TVarData);
  public
    procedure Clear(var V: TVarData); override;
    procedure Copy(var Dest: TVarData; const Source: TVarData; const Indirect: Boolean); override;
    procedure Cast(var Dest: TVarData; const Source: TVarData); override;
    procedure CastTo(var Dest: TVarData; const Source: TVarData; const AVarType: TVarType); override;
    procedure BinaryOp(var Left: TVarData; const Right: TVarData; const Operator: TVarOp); override;
    procedure UnaryOp(var Right: TVarData; const Operator: TVarOp); override;
    procedure Compare(const Left, Right: TVarData; var Relationship: TVarCompareResult); override;
    function IsClear(const V: TVarData): Boolean; override;
  end;

var
  { Our singleton that manages tour variant types }
  SgtBigCardinalVariantType: TBigCardinalVariantType;


{ TBigCardinalVariantType }

procedure TBigCardinalVariantType.BinaryOp(var Left: TVarData; const Right: TVarData; const &Operator: TVarOp);
begin
  { Select the appropriate operation }
  case &Operator of
    opAdd:
      BigCardinalToVarData(VarDataToBigCardinal(Left) + VarDataToBigCardinal(Right), Left);
    opAnd:
      BigCardinalToVarData(VarDataToBigCardinal(Left) and VarDataToBigCardinal(Right), Left);
    opIntDivide:
      BigCardinalToVarData(VarDataToBigCardinal(Left) div VarDataToBigCardinal(Right), Left);
    opModulus:
      BigCardinalToVarData(VarDataToBigCardinal(Left) mod VarDataToBigCardinal(Right), Left);
    opMultiply:
      BigCardinalToVarData(VarDataToBigCardinal(Left) * VarDataToBigCardinal(Right), Left);
    opOr:
      BigCardinalToVarData(VarDataToBigCardinal(Left) or VarDataToBigCardinal(Right), Left);
    opShiftLeft:
      BigCardinalToVarData(VarDataToBigCardinal(Left) shl VarDataToBigCardinal(Right), Left);
    opShiftRight:
      BigCardinalToVarData(VarDataToBigCardinal(Left) shr VarDataToBigCardinal(Right), Left);
    opSubtract:
      BigCardinalToVarData(VarDataToBigCardinal(Left) - VarDataToBigCardinal(Right), Left);
    opXor:
      BigCardinalToVarData(VarDataToBigCardinal(Left) xor VarDataToBigCardinal(Right), Left);
  else
    RaiseInvalidOp;
  end;
end;

procedure TBigCardinalVariantType.Cast(var Dest: TVarData; const Source: TVarData);
begin
  { Cast the source to our cardinal type }
  VarDataInit(Dest);
  BigCardinalToVarData(VarDataToBigCardinal(Source), Dest);
end;

procedure TBigCardinalVariantType.CastTo(var Dest: TVarData; const Source: TVarData; const AVarType: TVarType);
var
  Big: BigCardinal;
  Temp: TVarData;
  WStr: WideString;
begin
  if Source.VType = VarType then
  begin
    { Only continue if we're invoked for our data type }
    Big := TBigCardinalVarData(Source).BigCardinalPtr^;

    { Initilize the destination }
    VarDataInit(Dest);
    Dest.VType := AVarType;

    case AVarType of
      varByte:
        Dest.VByte := Big.ToByte();

      varShortInt:
        Dest.VShortInt := Big.ToShortInt();

      varWord:
        Dest.VWord := Big.ToWord();

      varSmallint:
        Dest.VSmallInt := Big.ToSmallInt();

      varInteger:
        Dest.VInteger := Big.ToInteger();

      varLongWord:
        Dest.VLongWord := Big.ToCardinal();

      varUInt64:
        Dest.VUInt64 := Big.ToUInt64();

      varInt64:
        Dest.VInt64 := Big.ToInt64();

      varOleStr:
      begin
        { Clear out the type to avoid the deep clear! }
        Dest.VType := 0;
        WStr := UIntToStr(Big);
        VarDataFromOleStr(Dest, WStr);
      end;

      varString, varUString:
      begin
        { Clear out the type to avoid the deep clear! }
        Dest.VType := 0;
        VarDataFromStr(Dest, UIntToStr(Big));
      end

      else
      begin
        { No default convertion found! Trying to use the string }
        try
          VarDataInit(Temp);
          VarDataFromStr(Temp, UIntToStr(Big));
          VarDataCastTo(Dest, Temp, AVarType);
        finally
          { Dispose our variant }
          VarDataClear(Temp);
        end;
      end;
    end;
  end else
    inherited;
end;

procedure TBigCardinalVariantType.Clear(var V: TVarData);
begin
  { Clear the variant type }
  V.VType := varEmpty;

  { And dispose the value }
  Dispose(TBigCardinalVarData(V).BigCardinalPtr);
  TBigCardinalVarData(V).BigCardinalPtr := nil;
end;

procedure TBigCardinalVariantType.Compare(const Left, Right: TVarData; var Relationship: TVarCompareResult);
var
  Res: Integer;
begin
  { Compare these values }
  Res := VarDataToBigCardinal(Left).CompareTo(VarDataToBigCardinal(Right));

  { Return the compare result }
  if Res < 0 then
    Relationship := crLessThan
  else if Res > 0 then
    Relationship := crGreaterThan
  else
    Relationship := crEqual;
end;

procedure TBigCardinalVariantType.Copy(var Dest: TVarData; const Source: TVarData; const Indirect: Boolean);
begin
  if Indirect and VarDataIsByRef(Source) then
    VarDataCopyNoInd(Dest, Source)
  else
  begin
    with TBigCardinalVarData(Dest) do
    begin
      { Copy the variant type }
      VType := VarType;

      { Initialize the pointer }
      New(BigCardinalPtr);

      { Copy by value }
      BigCardinalPtr^ := TBigCardinalVarData(Source).BigCardinalPtr^;
    end;
  end;
end;

function TBigCardinalVariantType.IsClear(const V: TVarData): Boolean;
begin
  if V.VType = varEmpty then
    Exit(true);

  { Signal clear value }
  Result := (TBigCardinalVarData(V).BigCardinalPtr = nil);
end;

procedure TBigCardinalVariantType.UnaryOp(var Right: TVarData; const &Operator: TVarOp);
begin
  { Select the appropriate operation }
  case &Operator of
    opNegate:
      BigCardinalToVarData(-VarDataToBigCardinal(Right), Right);
    opNot:
      BigCardinalToVarData(not VarDataToBigCardinal(Right), Right);
  else
    RaiseInvalidOp;
  end;
end;

function TBigCardinalVariantType.VarDataToBigCardinal(const Value: TVarData): BigCardinal;
begin
  { Check if the var data has a big cardinal inside }
  if Value.VType = VarType then
  begin
    { Copy the value to result }
    Exit(TBigCardinalVarData(Value).BigCardinalPtr^);
  end;

  { OK, try to convert the incoming var type to somethin useful }
  case Value.VType of
    varByte:
      Result := Value.VByte;

    varShortInt:
      Result := Value.VShortInt;

    varWord:
      Result := Value.VWord;

    varSmallint:
      Result := Value.VSmallInt;

    varInteger:
      Result := Value.VInteger;

    varLongWord:
      Result := Value.VLongWord;

    varUInt64:
      Result := Value.VUInt64;

    varInt64:
      Result := Value.VInt64;

    varString, varUString, varOleStr:
    begin
      { Be careful here, a string may not be a good number }
      try
        Result := StrToBigCardinal(VarDataToStr(Value));
      except
        on EConvertError do
          RaiseCastError;
      end;
    end;

    else
      RaiseCastError;
  end;
end;

procedure TBigCardinalVariantType.BigCardinalToVarData(const Value: BigCardinal; var OutValue: TVarData);
begin
  { Dispose of the old value. Check it it's ours first }
  if OutValue.VType = VarType then
    Clear(OutValue)
  else
    VarDataClear(OutValue);

  with TBigCardinalVarData(OutValue) do
  begin
    { Assign the new variant the var type that was allocated for us }
    VType := VarType;

    { Allocate space for our big cardinal pointer }
    New(BigCardinalPtr);

    { Copy self to this memory }
    BigCardinalPtr^ := Value;
  end;
end;


{ BigCardinal }

class operator BigCardinal.Add(const ALeft, ARight: BigCardinal): BigCardinal;
var
  I: Cardinal;
  A, B: ^BigCardinal;
  Temp: TNumberPiece;
  CarryIn, CarryOut: Boolean;
begin
  { Check for zeroes }
  if (ALeft.FArray = nil) and (ARight.FArray = nil) then
  begin
    Result := BigCardinalZero;
    Exit;
  end;

  if ALeft.FArray = nil then
  begin
    Result := ARight;
    Exit;
  end;

  if ARight.FArray = nil then
  begin
    Result := ALeft;
    Exit;
  end;

  { Get the maximum length }
  if ALeft.FLength >= ARight.FLength then
    begin A := @ALeft; B := @ARight; end
  else
    begin A := @ARight; B := @ALeft; end;

  { Initialize Result }
  Result.SetLength(A.FLength + 1);

  CarryIn := False;

	for I := 0 to B.FLength - 1 do
  begin
    { Disable overflow check in this code! }
    Temp := A.FArray[I] + B.FArray[I];

		CarryOut := (Temp < A.FArray[I]);

		if CarryIn then
    begin
      Inc(Temp);
      CarryOut := CarryOut or (Temp = 0);
    end;

		Result.FArray[I] := Temp;
		CarryIn := CarryOut;
  end;

  I := B.FLength;

  { Resolve carry }
  while (I < A.FLength) and (CarryIn) do
  begin
    Temp := A.FArray[I] + 1;
		CarryIn := (Temp = 0);
		Result.FArray[I] := Temp;

    { Increase the control variable }
    Inc(I);
  end;

  { Copy remaining cards }
  while (I < A.FLength) do
  begin
    Result.FArray[I] := A.FArray[I];
    { Increase the control variable }
    Inc(I);
  end;

  { Resolve carry }
	if (CarryIn) then
		Result.FArray[I] := 1
	else
		Dec(Result.FLength);
end;

class operator BigCardinal.BitwiseAnd(const ALeft, ARight: BigCardinal): BigCardinal;
var
  I: Cardinal;
begin
  { Init result }
  Result := BigCardinalZero;

  { In case of one 0 bad things happen }
  if (ALeft.FArray = nil) or (ARight.FArray = nil) then
  begin
    Result := BigCardinalZero;
    Exit;
  end;

  { Select the shortest int }
  if ALeft.FLength >= ARight.FLength then
    Result.SetLength(ARight.FLength)
  else
    Result.SetLength(ALeft.FLength);

	for I := 0 to Result.FLength - 1 do
		Result.FArray[I] := ALeft.FArray[I] and ARight.FArray[I];

  { Remove zeroes }
  Result.RemoveLeadingZeroes();
end;

class operator BigCardinal.LogicalNot(const AValue: BigCardinal): BigCardinal;
var
  I: Integer;
  X: TNumberPiece;
begin
  { Special case = nil }
  if AValue.FArray = nil then
  begin
    Result.SetLength(1);
    X := 0;
    Result.FArray[0] := not X;
    Exit;
  end;

  Result.SetLength(AValue.FLength);

  { Do the NOT operation }
  for I := 0 to AValue.FLength - 1 do
    Result.FArray[I] := not AValue.FArray[I];

  { Cleanup what remains }
  Result.RemoveLeadingZeroes();
end;

class operator BigCardinal.BitwiseOr(const ALeft, ARight: BigCardinal): BigCardinal;
var
  I: Cardinal;
  A, B: ^BigCardinal;

begin
  { In case of one 0 nothing happens }
  if ALeft.FArray = nil then
  begin
    Result := ARight;
    Exit;
  end;

  if ARight.FArray = nil then
  begin
    Result := ALeft;
    Exit;
  end;

	if (ALeft.FLength >= ARight.FLength) then
  begin
		A := @ALeft;
		B := @ARight;
	end else
  begin
		A := @ARight;
		B := @ALeft;
	end;

  { Initialize Result }
  Result.SetLength(A.FLength);

  { Do the OR operation }
	for I := 0 to B.FLength - 1 do
		Result.FArray[I] := A.FArray[I] or B.FArray[I];

  { And continue further ... }
  I := B.FLength;

  while I < A.FLength do
  begin
    Result.FArray[I] := A.FArray[I];
    Inc(I);
  end;

  Result.FLength := A.FLength;
end;

class operator BigCardinal.BitwiseXor(const ALeft, ARight: BigCardinal): BigCardinal;
var
  I: Cardinal;
  A, B: ^BigCardinal;

begin
  { In case of one 0 nothing happens }
  if ALeft.FArray = nil then
  begin
    Result := ARight;
    Exit;
  end;

  if ARight.FArray = nil then
  begin
    Result := ALeft;
    Exit;
  end;

	if (ALeft.FLength >= ARight.FLength) then
  begin
		A := @ALeft;
		B := @ARight;
	end else
  begin
		A := @ARight;
		B := @ALeft;
	end;

  { Initialize Result }
  Result.SetLength(A.FLength + 1);

  { Do the XOR operation }
	for I := 0 to B.FLength - 1 do
		Result.FArray[I] := A.FArray[I] xor B.FArray[I];

  { And continue further ... }
  I := B.FLength;

  while I < A.FLength do
  begin
    Result.FArray[I] := A.FArray[I];
    Inc(I);
  end;

  { Possible zeroes }
  Result.RemoveLeadingZeroes();
end;

function BigCardinal.CompareTo(const ANumber: BigCardinal): Integer;
var
  I: Integer;
begin
  if FLength < ANumber.FLength then
    Exit(-1)
  else if FLength > ANumber.FLength then
    Exit(1)
  else if FLength > 0 then
  begin
    { Check from the most important card to the less one }
    for I := FLength - 1 downto 0 do
    begin
      { Retun on two conditions if required }
      if FArray[I] > ANumber.FArray[I] then
        Exit(1)
      else if FArray[I] < ANumber.FArray[I] then
        Exit(-1);
    end;
  end;

  { Equality }
  Result := 0;
end;

procedure BigCardinal.CopyPieces(var Dest; const Count: Cardinal);
var
  RealCount: Cardinal;
begin
  FillChar(Dest, Count, 0);

  { Do nothing }
  if FArray = nil then
    Exit;

  { Find out what length is good }
  if (FLength * BytesInPiece) < Count then
    RealCount := FLength * BytesInPiece
  else
    RealCount := Count;

  { And now do a move operation }
  Move(FArray[0], Dest, RealCount);
end;

constructor BigCardinal.Create(const ANumber: BigCardinal);
begin
  { Just copy! }
  Self := ANumber;
end;

constructor BigCardinal.Create(const ANumber: Int64);
begin
  if ANumber <> 0 then
  begin
    SetPieces(ANumber, SizeOf(Int64));

    { Cleanup afterwards }
    RemoveLeadingZeroes();
  end
  else
    Self := BigCardinalZero;
end;

constructor BigCardinal.Create(const ANumber: Integer);
begin
  if ANumber <> 0 then
  begin
    SetPieces(ANumber, SizeOf(Integer));

    { Cleanup afterwards }
    RemoveLeadingZeroes();
  end
  else
    Self := BigCardinalZero;
end;

class operator BigCardinal.Dec(const AValue: BigCardinal): BigCardinal;
begin
  { Simply decrease 1 }
  Result := AValue - 1;
end;

class operator BigCardinal.Implicit(const ANumber: BigCardinal): Variant;
begin
  { Clear out the result }
  VarClear(Result);

  with TBigCardinalVarData(Result) do
  begin
    { Assign the new variant the var type that was allocated for us }
    VType := varBigCardinal;

    { Allocate space for our big cardinal pointer }
    New(BigCardinalPtr);

    { Copy self to this memory }
    BigCardinalPtr^ := ANumber;
  end;
end;

class operator BigCardinal.Inc(const AValue: BigCardinal): BigCardinal;
begin
  { Simply increase 1 }
  Result := AValue + 1;
end;

class operator BigCardinal.IntDivide(const ALeft, ARight: BigCardinal): BigCardinal;
var
  R: BigCardinal;
begin
  { Ensure everything is allocated }
  System.SetLength(R.FArray, ALeft.FLength);
  R.FLength := ALeft.FLength;
  Move(ALeft.FArray[0], R.FArray[0], ALeft.FLength * BytesInPiece);

  R.CalcModulus(ARight, Result);
end;

constructor BigCardinal.Create(const ANumber: UInt64);
begin
  if ANumber <> 0 then
  begin
    SetPieces(ANumber, SizeOf(UInt64));

    { Cleanup afterwards }
    RemoveLeadingZeroes();
  end
  else
    Self := BigCardinalZero;
end;

constructor BigCardinal.Create(const ANumber: Cardinal);
begin
  if ANumber <> 0 then
  begin
    SetPieces(ANumber, SizeOf(Cardinal));

    { Cleanup afterwards }
    RemoveLeadingZeroes();
  end
  else
    Self := BigCardinalZero;
end;

procedure BigCardinal.SetLength(const ALength: Cardinal);
begin
  { Assuming that all is initialized }
  System.SetLength(FArray, ALength);
  FLength := ALength;
  FillChar(FArray[0], BytesInPiece * ALength, 0);
end;

procedure BigCardinal.SetPieces(const Source; const Count: Cardinal);
var
  IncSize: Cardinal;
begin
  ASSERT(Count > 0);

  { Decide the new size of the array }
  IncSize := (Count div BytesInPiece);

  if (Count mod BytesInPiece) > 0 then
    Inc(IncSize);

  { Set the required length }
  SetLength(IncSize);

  { Copy the value in }
  Move(Source, FArray[0], Count);
end;

function BigCardinal.BitLength(): Cardinal;
var
  I, X: Cardinal;
begin
  { Do nothing on 0 length }
  if (FArray = nil) or (FLength = 0) then
    Exit(0);

  Result := FLength * BitsInPiece;

  for I := FLength - 1 to 0 do
  begin
    if FArray[I] = 0 then
      Dec(Result, BitsInPiece)
    else
    begin
      { Not an empty piece, Let's check the real last bit }
      for X := BitsInPiece - 1 downto 0 do
      begin
        { Fount a bit here, consider this to be the bit length }
        if (FArray[I] and (1 shl X)) <> 0 then
        begin
          Dec(Result, BitsInPiece - X - 1);
          Exit;
        end;
      end;
    end;
  end;
end;

function BigCardinal.GetBCDDigitFrom(const Piece, Bit: Cardinal): Cardinal;
const
  Offsets: array[1..3] of Cardinal = (1, 3, 7);
var
  Overflow: Cardinal;
begin
  { In case of no overflow do the usual }
  if (Bit <= (BitsInPiece - BCDDigitBits)) or (Piece = (FLength - 1)) then
    Exit((FArray[Piece] shr Bit) and BCDMask);

  { Calculate the overflow }
  Overflow := Bit - (BitsInPiece - BCDDigitBits);

  { Get the normal part and the overflowed }
  Result := (FArray[Piece] shr Bit) or ((FArray[Piece + 1] and Offsets[Overflow]) shl (BCDDigitBits - Overflow));
end;

function BigCardinal.SetBCDDigitFrom(const Piece, Bit: Cardinal; const Value: Cardinal): Cardinal;
const
  Offsets: array[1..3] of Cardinal = (1, 3, 7);

var
  Overflow: Cardinal;
begin
  Result := 0;

  { In case of no overflow do the usual }
  if (Bit <= (BitsInPiece - BCDDigitBits)) then
  begin
    FArray[Piece] := (FArray[Piece] and (not (BCDMask shl Bit))) or (Value shl Bit);
    Exit;
  end;

  { Calculate the overflow }
  Overflow := Bit - (BitsInPiece - BCDDigitBits);

  if (Piece = (FLength - 1)) then
  begin
    { We must extend the array! }
    System.SetLength(FArray, FLength + 1);
    FLength := FLength + 1;

    { Set the overflowed bits }
    Result := BitsInPiece;
  end;

  { Set the normal part first }
  FArray[Piece] := (FArray[Piece] and (not (BCDMask shl Bit))) or (Value shl Bit);
  FArray[Piece + 1] := (FArray[Piece + 1] and (not Offsets[Overflow])) or (Value shr (BCDDigitBits - Overflow));
end;

function BigCardinal.ToBCD(): BigCardinal;
var
  TotalBits: Cardinal;
  I, J: Cardinal;
  PieceIdx: Cardinal;
  BCDDigit, StartBit: Cardinal;

begin
  { Check array length first }
  if (FArray = nil) or (FLength = 0) then
     Exit;

  { Create a copy of Self }
  Result.SetLength(FLength);
  Move(FArray[0], Result.FArray[0], FLength * BytesInPiece);

  { Calculate the total number of bits }
  TotalBits := Result.BitLength();// FLength * BitsInPiece;

  { Iterate over all bits: Start at high and do not continue till the last bit! }
  for I := TotalBits - 1 downto 1 do
  begin
    { Start the BCD normalization cycle at the moving bit }
    J := I;

    while J <= (TotalBits - 1) do
    begin
      { Gather all info }
      PieceIdx := J div BitsInPiece;
      StartBit := J mod BitsInPiece;

      { Get the BCD Digit at starting point }
      { check for inter-piece BCDs }
      BCDDigit := Result.GetBCDDigitFrom(PieceIdx, StartBit);

      { If the digit >= 5 add 3 to it! }
      if BCDDigit >= 5 then
      begin
        { Add the number of bits }
        BCDDigit := BCDDigit + 3;

        { Set the BCD back and extend if necessary }
        Result.SetBCDDigitFrom(PieceIdx, StartBit, BCDDigit);

        { Is this the last iteration for this shift stage? If Yes, recalculate the bit length once again }
        if ((J + BCDDigitBits) > (TotalBits - 1)) then
          TotalBits := Result.BitLength();
      end;

      Inc(J, BCDDigitBits);
    end;
  end;

  { Remove leading 0's }
  Result.RemoveLeadingZeroes();
end;

class operator BigCardinal.Equal(const ALeft, ARight: BigCardinal): Boolean;
begin
  Result := (ALeft.CompareTo(ARight) = 0);
end;

class operator BigCardinal.Explicit(const ANumber: BigCardinal): UInt64;
begin
  { Call convertion code }
  Result := ANumber.ToUInt64();
end;

class operator BigCardinal.Explicit(const ANumber: BigCardinal): ShortInt;
begin
  { Call convertion code }
  Result := ANumber.ToShortInt();
end;

class operator BigCardinal.Explicit(const ANumber: BigCardinal): Cardinal;
begin
  { Call convertion code }
  Result := ANumber.ToCardinal();
end;

class operator BigCardinal.Explicit(const ANumber: BigCardinal): Byte;
begin
  { Call convertion code }
  Result := ANumber.ToByte();
end;

class operator BigCardinal.Explicit(const ANumber: BigCardinal): Word;
begin
  { Call convertion code }
  Result := ANumber.ToWord();
end;

class operator BigCardinal.Explicit(const ANumber: BigCardinal): AnsiChar;
begin
  { Call convertion code }
  Result := ANumber.ToAnsiChar();
end;

class operator BigCardinal.Explicit(const ANumber: BigCardinal): WideChar;
begin
  { Call convertion code }
  Result := ANumber.ToWideChar();
end;

class operator BigCardinal.Explicit(const ANumber: BigCardinal): Int64;
begin
  { Call convertion code }
  Result := ANumber.ToInt64();
end;

class operator BigCardinal.Explicit(const ANumber: BigCardinal): SmallInt;
begin
  { Call convertion code }
  Result := ANumber.ToSmallInt();
end;

class operator BigCardinal.Explicit(const ANumber: BigCardinal): Integer;
begin
  { Call convertion code }
  Result := ANumber.ToInteger();
end;

class function BigCardinal.GetShiftedPiece(const A: BigCardinal; const Index, Count: Cardinal): TNumberPiece;
var
  P1, P2: TNumberPiece;
begin
  { Calculate part 1 }
  if (Index = 0) or (Count = 0) then
    P1 := 0
  else
    P1 := (A.FArray[Index - 1] shr (BitsInPiece - Count));

  { Calculate part 2 }
  if (Index = A.FLength) then
    P2 := 0
  else
    P2 := (A.FArray[Index] shl Count);

  { Cumulate part 1 and 2 }
	Result := P1 or P2;
end;

class operator BigCardinal.GreaterThan(const ALeft, ARight: BigCardinal): Boolean;
begin
  Result := (ALeft.CompareTo(ARight) > 0);
end;

class operator BigCardinal.GreaterThanOrEqual(const ALeft, ARight: BigCardinal): Boolean;
begin
  Result := (ALeft.CompareTo(ARight) >= 0);
end;

class operator BigCardinal.Implicit(const ANumber: Word): BigCardinal;
begin
  { Simply call ctor }
  Result := BigCardinal.Create(ANumber);
end;

class operator BigCardinal.Implicit(const ANumber: Byte): BigCardinal;
begin
  { Simply call ctor }
  Result := BigCardinal.Create(ANumber);
end;

class operator BigCardinal.Implicit(const ANumber: UInt64): BigCardinal;
begin
  { Simply call ctor }
  Result := BigCardinal.Create(ANumber);
end;

class operator BigCardinal.LeftShift(const AValue: BigCardinal; const ACount: Cardinal): BigCardinal;
var
  ShiftedPieces: Cardinal;
  ShiftedBits: Cardinal;
  I, J: Cardinal;
begin
  { Do nothing on 0 }
  if (ACount = 0) or (AValue.FArray = nil) then
  begin
    Result := AValue;
    Exit;
  end;

  { Calculate shifts }
  ShiftedPieces := ACount div BitsInPiece;
  ShiftedBits := ACount mod BitsInPiece;

  { Init and ensure capacity }
  Result.SetLength(AValue.FLength + ShiftedPieces + 1);

  I := ShiftedPieces;

	for J := 0 to AValue.FLength do
  begin
    { Actually shift the bits in the card }
		Result.FArray[I] := GetShiftedPiece(AValue, J, ShiftedBits);
    Inc(I);
  end;

  { Remove leading 0's }
  Result.RemoveLeadingZeroes();
end;

class operator BigCardinal.LessThan(const ALeft, ARight: BigCardinal): Boolean;
begin
  Result := (ALeft.CompareTo(ARight) < 0);
end;

class operator BigCardinal.LessThanOrEqual(const ALeft, ARight: BigCardinal): Boolean;
begin
  Result := (ALeft.CompareTo(ARight) <= 0);
end;

class operator BigCardinal.Modulus(const ALeft, ARight: BigCardinal): BigCardinal;
var
  Q: BigCardinal;
begin
  System.SetLength(Result.FArray, ALeft.FLength);
  Result.FLength := ALeft.FLength;
  Move(ALeft.FArray[0], Result.FArray[0], ALeft.FLength * BytesInPiece);

  Result.CalcModulus(ARight, Q);
end;

class operator BigCardinal.Multiply(const ALeft, ARight: BigCardinal): BigCardinal;
var
  I, J, K, I2: Cardinal;
  Temp: TNumberPiece;
  CarryIn, CarryOut: Boolean;
begin
  { Check for zeroes: 0 * x = 0}
  if (ALeft.FArray = nil) or (ARight.FArray = nil) then
  begin
    Result := BigCardinalZero;
    Exit;
  end;

  { Ensure capacity }
  Result.SetLength(ALeft.FLength + ARight.FLength);

  { Calculate what we need }
  for I := 0 to ALeft.FLength - 1 do
  begin

		for I2 := 0 to BitsInPiece - 1 do
		begin
    	if ((ALeft.FArray[I] and (1 shl I2)) = 0) then
				continue;

      CarryIn := False;
      K := I;

      for J := 0 to ARight.FLength do
      begin
        { Disable overflow check in this code! }
        Temp := Result.FArray[K] + GetShiftedPiece(ARight, J, I2);

				CarryOut := (Temp < Result.FArray[K]);

				if (CarryIn) then
        begin
					Inc(Temp);
					CarryOut := CarryOut or (Temp = 0);
				end;

				Result.FArray[K] := Temp;
				CarryIn := CarryOut;

        Inc(K);
      end;

      while CarryIn do
      begin
        Inc(Result.FArray[K]);
        CarryIn := (Result.FArray[K] = 0);

        Inc(K);
      end;
    end;
  end;

  { Cleanup result }
  Result.RemoveLeadingZeroes();
end;

class operator BigCardinal.Implicit(const ANumber: Cardinal): BigCardinal;
begin
  { Simply call ctor }
  Result := BigCardinal.Create(ANumber);
end;

class operator BigCardinal.Negative(const AValue: BigCardinal): BigCardinal;
begin
  {$IFDEF BIGCARDINAL_OVERFLOW_CHECKS}
  ExceptionHelper.Throw_OverflowError();
  {$ENDIF}

  Result := 0 - AValue;
end;

class operator BigCardinal.NotEqual(const ALeft, ARight: BigCardinal): Boolean;
begin
  Result := (ALeft.CompareTo(ARight) <> 0);
end;

class operator BigCardinal.Positive(const AValue: BigCardinal): BigCardinal;
begin
  { Nothing ... }
  Result := AValue;
end;

procedure BigCardinal.RemoveLeadingZeroes;
begin
  { Repeat undefinetly }
  while (FLength > 0) do
  begin
    { Decrease the FLength variable is f it points to a 0}
    if FArray[FLength - 1] = 0 then
      Dec(FLength)
    else
     Break; { Finish when a non-zero found }
  end;

  { If no elemens are in the array, set to nil }
  { There is code that depends on the array being nil }
  if FLength = 0 then
    System.SetLength(FArray, 0);
end;

class operator BigCardinal.RightShift(const AValue: BigCardinal; const ACount: Cardinal): BigCardinal;
var
  ShiftedPieces: Cardinal;
  ShiftedBits: Cardinal;
  I, J: Cardinal;
begin
  { Do nothing on 0 count }
  if (ACount = 0) or (AValue.FArray = nil) then
  begin
    Result := AValue;
    Exit;
  end;

  { Calculate shifts }
  ShiftedPieces := (ACount + BitsInPiece - 1) div BitsInPiece;
  ShiftedBits := (ShiftedPieces * BitsInPiece) - ACount;

  { Check implicit shifts }
	if (ShiftedPieces >= AValue.FLength + 1) then
  begin
    Result := BigCardinalZero;
    Exit();
  end;

  { Initialize and ensure capacity }
  Result.SetLength(AValue.FLength - ShiftedPieces + 1);

  { Do the actual stuff }
  I := 0;

	for J := ShiftedPieces to AValue.FLength do
  begin
    { Actually shift the bits in the card }
		Result.FArray[I] := GetShiftedPiece(AValue, J, ShiftedBits);
    Inc(I);
  end;

  { Remove leading 0's }
  Result.RemoveLeadingZeroes();
end;

class operator BigCardinal.Subtract(const ALeft, ARight: BigCardinal): BigCardinal;
var
  I: Cardinal;
  Temp: TNumberPiece;
  LLeft: BigCardinal;
  BorrowIn, BorrowOut: Boolean;
begin
  { Check for zeroes }
  if (ALeft.FArray = nil) and (ARight.FArray = nil) then
  begin
    Result := BigCardinalZero;
    Exit;
  end;

  { Right is 0 - do nothing }
  if ARight.FArray = nil then
  begin
    Result := ALeft;
    Exit;
  end;

  { Left is 0 - set the temp lngth }

  if (ALeft.FArray = nil) or (ALeft.FLength < ARight.FLength) then
  begin
    {$IFDEF BIGCARDINAL_OVERFLOW_CHECKS}
    ExceptionHelper.Throw_OverflowError();
    {$ENDIF}

    { LLeft must be copied from the original }
    LLeft.SetLength(ARight.FLength);
    LLeft.FLength := ARight.FLength;

    if (ALeft.FArray <> nil) and (ALeft.FLength > 0) then
      Move(ALeft.FArray[0], LLeft.FArray[0], ALeft.FLength * BytesInPiece);
  end else
      LLeft := ALeft;

  { Ensure capacity }
  Result.SetLength(LLeft.FLength);

  BorrowIn := False;

  { Calculate subtraction for each card }
  for I := 0 to ARight.FLength - 1 do
  begin
    { Disable overflow check in this code! }
    Temp := LLeft.FArray[I] - ARight.FArray[I];

		BorrowOut := (Temp > LLeft.FArray[i]);

		if (BorrowIn) then
    begin
			BorrowOut := BorrowOut or (Temp = 0);
			Dec(Temp);
		end;

		Result.FArray[I] := Temp;
		BorrowIn := BorrowOut;
  end;

  { And continue ... }
  I := ARight.FLength;

  while (I < LLeft.FLength) and (BorrowIn) do
  begin
    BorrowIn := (LLeft.FArray[I] = 0);
    Result.FArray[I] := LLeft.FArray[I] - 1;

    Inc(I);
  end;

  {$IFDEF BIGCARDINAL_OVERFLOW_CHECKS}
  { A carry still wanted ... exception! }
  if (BorrowIn) then
  begin
    { Clean-up the result }
    Result := BigCardinalZero;
    ExceptionHelper.Throw_OverflowError();
  end;
  {$ENDIF}

  { Finish the subtraction - copy leftovers }
  while (I < LLeft.FLength) do
  begin
    Result.FArray[I] := LLeft.FArray[I];
    Inc(I);
  end;

  { Cleanup the result }
  Result.RemoveLeadingZeroes();
end;

function BigCardinal.ToAnsiChar: AnsiChar;
begin
  CopyPieces(Result, SizeOf(AnsiChar));
end;

function BigCardinal.ToByte: Byte;
begin
  CopyPieces(Result, SizeOf(Byte));
end;

function BigCardinal.ToCardinal: Cardinal;
begin
  CopyPieces(Result, SizeOf(Cardinal));
end;

function BigCardinal.ToInt64: Int64;
begin
  CopyPieces(Result, SizeOf(Int64));
end;

function BigCardinal.ToInteger: Integer;
begin
  CopyPieces(Result, SizeOf(Integer));
end;

function BigCardinal.ToShortInt: ShortInt;
begin
  CopyPieces(Result, SizeOf(ShortInt));
end;

function BigCardinal.ToSmallInt: SmallInt;
begin
  CopyPieces(Result, SizeOf(SmallInt));
end;

function BigCardinal.ToUInt64: UInt64;
begin
  CopyPieces(Result, SizeOf(UInt64));
end;

function BigCardinal.ToWideChar: WideChar;
begin
  CopyPieces(Result, SizeOf(WideChar));
end;

function BigCardinal.ToWord: Word;
begin
  CopyPieces(Result, SizeOf(Word));
end;

procedure BigCardinal.CalcModulus(const Divisor: BigCardinal; var Quotient: BigCardinal);
var
  I, J, K, I2: Cardinal;
  OrigLen: Cardinal;
  Temp: TNumberPiece;
  BorrowIn, BorrowOut: Boolean;
  XBuffer: TNumberPieceArray;
begin
  { Check for 0 divisor }
  if Divisor.FArray = nil then
    ExceptionHelper.Throw_DivByZeroError();

  { Special case }
  if (FArray = nil) or (FLength < Divisor.FLength) then
  begin
    { Q = 0 and R = DVD }
    Quotient := BigCardinalZero;
    Exit;
  end;

  { Reset the lengths }
	OrigLen := FLength;
  System.SetLength(FArray, FLength + 1);
  FArray[FLength] := 0;
  Inc(FLength);

  { Init a temporary buffer }
  System.SetLength(XBuffer, FLength);
  FillChar(XBuffer[0], BytesInPiece * FLength, 0);

  { Initialize quotient }
  Quotient.SetLength(OrigLen - Divisor.FLength + 1);

	I := Quotient.FLength;

	while (I > 0) do
  begin
		Dec(I);

		Quotient.FArray[I] := 0;
		I2 := BitsInPiece;

    while I2 > 0 do
    begin
      Dec(I2);

      BorrowIn := False;
      K := I;

      for J := 0 to Divisor.FLength do
      begin
        Temp := FArray[K] - GetShiftedPiece(Divisor, J, I2);
				BorrowOut := (Temp > FArray[k]);

				if (BorrowIn)  then
        begin
					BorrowOut := BorrowOut or (Temp = 0);
					Dec(Temp);
				end;

				XBuffer[K] := Temp;
				BorrowIn := BorrowOut;

        { Inc ... }
        Inc(K);
      end;

      while (K < OrigLen) and (BorrowIn) do
      begin
        BorrowIn := (FArray[K] = 0);
				XBuffer[K] := FArray[K] - 1;

        Inc(K);
      end;

			if (not BorrowIn) then
      begin
				Quotient.FArray[I] := Quotient.FArray[I] or (TNumberPiece(1) shl I2);

				while (K > I) do
        begin
          Dec(K);
					FArray[K] := XBuffer[k];
				end;
			end;

    end;
  end;

  { Clean-up variables }
  Quotient.RemoveLeadingZeroes();
  RemoveLeadingZeroes();
end;


{ TBigCardinalType }

function TBigCardinalType.AreEqual(const AValue1, AValue2: BigCardinal): Boolean;
begin
  Result := (AValue1.CompareTo(AValue2) = 0);
end;

function TBigCardinalType.Compare(const AValue1, AValue2: BigCardinal): Integer;
begin
  Result := AValue1.CompareTo(AValue2);
end;

procedure TBigCardinalType.DoDeserialize(const AInfo: TValueInfo; out AValue: BigCardinal; const AContext: IDeserializationContext);
var
  LStr: String;
begin
  AContext.GetValue(AInfo, LStr);
  AValue := StrToBigCardinal(LStr);
end;

procedure TBigCardinalType.DoSerialize(const AInfo: TValueInfo; const AValue: BigCardinal; const AContext: ISerializationContext);
begin
  AContext.AddValue(AInfo, UIntToStr(AValue));
end;

function TBigCardinalType.Family: TTypeFamily;
begin
  Result := tfUnsignedInteger;
end;

function TBigCardinalType.GenerateHashCode(const AValue: BigCardinal): Integer;
begin
  { Exit with 0 on 0 size }
  if AValue.FArray = nil then
    Exit(0);

  { Call the Type-Support provided function }
  Result := BinaryHash(Addr(AValue.FArray[0]), AValue.FLength * BigCardinal.BytesInPiece);
end;

function TBigCardinalType.GetString(const AValue: BigCardinal): String;
begin
  Result := UIntToStr(AValue);
end;

function TBigCardinalType.TryConvertFromVariant(const AValue: Variant; out ORes: BigCardinal): Boolean;
begin
  { May not be a valid BigCardinal }
  try
    ORes := SgtBigCardinalVariantType.VarDataToBigCardinal(TVarData(AValue));
  except
    Exit(false);
  end;

  Result := true;
end;

function TBigCardinalType.TryConvertToVariant(const AValue: BigCardinal; out ORes: Variant): Boolean;
begin
  { Simple variant conversion }
  ORes := AValue;
  Result := true;
end;

{ TBigCardinalMathExtension }

function TBigCardinalMathExtension.Add(const AValue1, AValue2: BigCardinal): BigCardinal;
begin
  Result := AValue1 + AValue2;
end;

function TBigCardinalMathExtension.IntegralDivide(const AValue1, AValue2: BigCardinal): BigCardinal;
begin
  Result := AValue1 div AValue2;
end;

function TBigCardinalMathExtension.Modulo(const AValue1, AValue2: BigCardinal): BigCardinal;
begin
  Result := AValue1 mod AValue2;
end;

function TBigCardinalMathExtension.Multiply(const AValue1, AValue2: BigCardinal): BigCardinal;
begin
  Result := AValue1 * AValue2;
end;

function TBigCardinalMathExtension.One: BigCardinal;
begin
  Result := 1;
end;

function TBigCardinalMathExtension.Subtract(const AValue1, AValue2: BigCardinal): BigCardinal;
begin
  Result := AValue1 - AValue2;
end;

function TBigCardinalMathExtension.Zero: BigCardinal;
begin
  Result := 0;
end;

initialization
  { Register custom type }
  TType<BigCardinal>.Register(TBigCardinalType);
  TMathExtension<BigCardinal>.Register(TBigCardinalMathExtension);

  { Register our custom variant type }
  SgtBigCardinalVariantType := TBigCardinalVariantType.Create();

  { Set the value of the varBigCardinal }
  varBigCardinal := SgtBigCardinalVariantType.VarType;

finalization
  { Unregister custom type }
  TMathExtension<BigCardinal>.Unregister();
  TType<BigCardinal>.Unregister();

  { Uregister our custom variant }
  FreeAndNil(SgtBigCardinalVariantType);

end.



