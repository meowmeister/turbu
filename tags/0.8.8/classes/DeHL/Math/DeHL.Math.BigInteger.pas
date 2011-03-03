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
unit DeHL.Math.BigInteger;
interface
uses SysUtils,
     Variants,
     DeHL.Base,
     DeHL.Exceptions,
     DeHL.Types,
     DeHL.Serialization,
     DeHL.Math.Types,
     DeHL.Math.BigCardinal;

type
  { Unsigned Number }
  BigInteger = record
  private
    FMagnitude: BigCardinal;
    FSign: SmallInt;

  public
    { Constructors}
    constructor Create(const ANumber: UInt64); overload;
    constructor Create(const ANumber: Int64); overload;
    constructor Create(const ANumber: Cardinal); overload;
    constructor Create(const ANumber: Integer); overload;
    constructor Create(const ANumber: BigInteger); overload;
    constructor Create(const ANumber: BigCardinal); overload;

    { Comparison }
    function CompareTo(const ANumber: BigInteger): Integer;

    { Converters }
    function ToShortInt(): ShortInt; inline;
    function ToSmallInt(): SmallInt; inline;
    function ToInteger(): Integer; inline;
    function ToInt64(): Int64; inline;

    { Comparison operators }
    class operator Equal(const ALeft, ARight: BigInteger): Boolean;
    class operator NotEqual(const ALeft, ARight: BigInteger): Boolean;
    class operator GreaterThan(const ALeft, ARight: BigInteger): Boolean;
    class operator GreaterThanOrEqual(const ALeft, ARight: BigInteger): Boolean;
    class operator LessThan(const ALeft, ARight: BigInteger): Boolean;
    class operator LessThanOrEqual(const ALeft, ARight: BigInteger): Boolean;

    { Arithmetic operators }
    class operator Add(const ALeft, ARight: BigInteger): BigInteger;
    class operator Subtract(const ALeft, ARight: BigInteger): BigInteger;
    class operator Multiply(const ALeft, ARight: BigInteger): BigInteger;
    class operator IntDivide(const ALeft, ARight: BigInteger): BigInteger;
    class operator Modulus(const ALeft, ARight: BigInteger): BigInteger;
    class operator Negative(const AValue: BigInteger): BigInteger;
    class operator Positive(const AValue: BigInteger): BigInteger;

    class operator Inc(const AValue: BigInteger): BigInteger;
    class operator Dec(const AValue: BigInteger): BigInteger;

    { Implicits }
    class operator Implicit(const ANumber: Byte): BigInteger;
    class operator Implicit(const ANumber: Word): BigInteger;
    class operator Implicit(const ANumber: Cardinal): BigInteger;
    class operator Implicit(const ANumber: UInt64): BigInteger;
    class operator Implicit(const ANumber: SmallInt): BigInteger;
    class operator Implicit(const ANumber: ShortInt): BigInteger;
    class operator Implicit(const ANumber: Integer): BigInteger;
    class operator Implicit(const ANumber: Int64): BigInteger;
    class operator Implicit(const ANumber: BigCardinal): BigInteger;
    class operator Implicit(const ANumber: BigInteger): Variant;

    { Explicits }
    class operator Explicit(const ANumber: BigInteger): ShortInt;
    class operator Explicit(const ANumber: BigInteger): SmallInt;
    class operator Explicit(const ANumber: BigInteger): Integer;
    class operator Explicit(const ANumber: BigInteger): Int64;
  end;

  { BigInteger Support }
  TBigIntegerType = class(TRecordType<BigInteger>)
  private
    FBigCardinalType: TBigCardinalType;

  protected
    { Serialization }
    procedure DoSerialize(const AInfo: TValueInfo; const AValue: BigInteger; const AContext: ISerializationContext); override;
    procedure DoDeserialize(const AInfo: TValueInfo; out AValue: BigInteger; const AContext: IDeserializationContext); override;

  public
    { Constructor }
    constructor Create(); override;

    { Destructor }
    destructor Destroy(); override;

    { Comparator }
    function Compare(const AValue1, AValue2: BigInteger): Integer; override;

    { Equality Comparator }
    function AreEqual(const AValue1, AValue2: BigInteger): Boolean; override;

    { Hash code provider }
    function GenerateHashCode(const AValue: BigInteger): Integer; override;

    { Get String representation }
    function GetString(const AValue: BigInteger): String; override;

    { Type information }
    function Family(): TTypeFamily; override;

    { Variant Conversion }
    function TryConvertToVariant(const AValue: BigInteger; out ORes: Variant): Boolean; override;
    function TryConvertFromVariant(const AValue: Variant; out ORes: BigInteger): Boolean; override;
  end;

  { Math extensions for the BigInteger type }
  TBigIntegerMathExtension = class sealed(TIntegerMathExtension<BigInteger>)
  public
    { Standard operations }
    function Add(const AValue1, AValue2: BigInteger): BigInteger; override;
    function Subtract(const AValue1, AValue2: BigInteger): BigInteger; override;
    function Multiply(const AValue1, AValue2: BigInteger): BigInteger; override;
    function IntegralDivide(const AValue1, AValue2: BigInteger): BigInteger; override;
    function Modulo(const AValue1, AValue2: BigInteger): BigInteger; override;

    { Sign-related operations }
    function Negate(const AValue: BigInteger): BigInteger; override;
    function Abs(const AValue: BigInteger): BigInteger; override;

    { Neutral Math elements }
    function Zero: BigInteger; override;
    function One: BigInteger; override;
    function MinusOne: BigInteger; override;
  end;

{ Convert a BigInteger to a string }
function IntToStr(Value: BigInteger): String; overload;

{ Convert a string to a BigInteger }
function StrToBigInteger(const S: String): BigInteger;

function Abs(const Value: BigInteger): BigInteger; overload;

{ The variant Id of BigInteger }
var
  varBigInteger: TVarType;

implementation

{ Utility functions }

function Abs(const Value: BigInteger): BigInteger;
begin
  { Copy result }
  Result := Value;

  { Change the sign }
  if Result.FSign = -1 then
    Result.FSign := 1;
end;

function IntToStr(Value: BigInteger): String;
begin
  if Value.FSign = 0 then
    Exit('0');

  Result := UIntToStr(Value.FMagnitude);

  if Value.FSign = -1 then
    Result := '-' + Result;
end;

function StrToBigInteger(const S: String): BigInteger;
var
  S2: String;
begin
  S2 := TrimLeft(S);

  if Length(S2) = 0 then
    ExceptionHelper.Throw_ArgumentConverError('S');

  { Check the sign part }
  if S2[1] = '-' then
  begin
    Result.FSign := -1;
    Delete(S2, 1, 1);
  end
  else if S2[1] = '+' then
  begin
    Result.FSign := 1;
    Delete(S2, 1, 1);
  end else Result.FSign := 1;

  Result.FMagnitude := StrToBigCardinal(S2);
end;


{ Variant Support }

type
  PBigInteger = ^BigInteger;

  { Mapping the BigCardinal into TVarData structure }
  TBigIntegerVarData = packed record
    { Var type, will be assigned at runtime }
    VType: TVarType;
    { Reserved stuff }
    Reserved1, Reserved2, Reserved3: Word;
    { A reference to the enclosed big cardinal }
    BigIntegerPtr: PBigInteger;
    { Reserved stuff }
    Reserved4: LongWord;
  end;

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

type
  { Manager for our variant type }
  TBigIntegerVariantType = class(TCustomVariantType)
  private
    { Will create a big cardinal, or raise an error }
    function VarDataToBigInteger(const Value: TVarData): BigInteger;
    procedure BigIntegerToVarData(const Value: BigInteger; var OutValue: TVarData);
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
  SgtBigIntegerVariantType: TBigIntegerVariantType;

{ TBigCardinalVariantType }

procedure TBigIntegerVariantType.BinaryOp(var Left: TVarData; const Right: TVarData; const &Operator: TVarOp);
begin
  { Select the appropriate operation }
  case &Operator of
    opAdd:
      BigIntegerToVarData(VarDataToBigInteger(Left) + VarDataToBigInteger(Right), Left);
    opIntDivide:
      BigIntegerToVarData(VarDataToBigInteger(Left) div VarDataToBigInteger(Right), Left);
    opModulus:
      BigIntegerToVarData(VarDataToBigInteger(Left) mod VarDataToBigInteger(Right), Left);
    opMultiply:
      BigIntegerToVarData(VarDataToBigInteger(Left) * VarDataToBigInteger(Right), Left);
    opSubtract:
      BigIntegerToVarData(VarDataToBigInteger(Left) - VarDataToBigInteger(Right), Left);
  else
    RaiseInvalidOp;
  end;
end;

procedure TBigIntegerVariantType.Cast(var Dest: TVarData; const Source: TVarData);
begin
  { Cast the source to our cardinal type }
  VarDataInit(Dest);
  BigIntegerToVarData(VarDataToBigInteger(Source), Dest);
end;

procedure TBigIntegerVariantType.CastTo(var Dest: TVarData; const Source: TVarData; const AVarType: TVarType);
var
  Big: BigInteger;
  Temp: TVarData;
  WStr: WideString;
begin
  if Source.VType = VarType then
  begin
    { Only continue if we're invoked for our data type }
    Big := TBigIntegerVarData(Source).BigIntegerPtr^;

    { Initilize the destination }
    VarDataInit(Dest);
    Dest.VType := AVarType;

    case AVarType of
      varShortInt:
        Dest.VShortInt := Big.ToShortInt();

      varSmallint:
        Dest.VSmallInt := Big.ToSmallInt();

      varInteger:
        Dest.VInteger := Big.ToInteger();

      varInt64:
        Dest.VInt64 := Big.ToInt64();

      varOleStr:
      begin
        { Clear out the type to avoid the deep clear! }
        Dest.VType := 0;
        WStr := IntToStr(Big);
        VarDataFromOleStr(Dest, WStr);
      end;

      varString, varUString:
      begin
        { Clear out the type to avoid the deep clear! }
        Dest.VType := 0;
        VarDataFromStr(Dest, IntToStr(Big));
      end

      else
      begin
        { No default convertion found! Trying to use the string }
        try
          VarDataInit(Temp);
          VarDataFromStr(Temp, IntToStr(Big));
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

procedure TBigIntegerVariantType.Clear(var V: TVarData);
begin
  { Clear the variant type }
  V.VType := varEmpty;

  { And dispose the value }
  Dispose(TBigIntegerVarData(V).BigIntegerPtr);
  TBigIntegerVarData(V).BigIntegerPtr := nil;
end;

procedure TBigIntegerVariantType.Compare(const Left, Right: TVarData; var Relationship: TVarCompareResult);
var
  Res: Integer;
begin
  { Compare these values }
  Res := VarDataToBigInteger(Left).CompareTo(VarDataToBigInteger(Right));

  { Return the compare result }
  if Res < 0 then
    Relationship := crLessThan
  else if Res > 0 then
    Relationship := crGreaterThan
  else
    Relationship := crEqual;
end;

procedure TBigIntegerVariantType.Copy(var Dest: TVarData; const Source: TVarData; const Indirect: Boolean);
begin
  if Indirect and VarDataIsByRef(Source) then
    VarDataCopyNoInd(Dest, Source)
  else
  begin
    with TBigIntegerVarData(Dest) do
    begin
      { Copy the variant type }
      VType := VarType;

      { Initialize the pointer }
      New(BigIntegerPtr);

      { Copy by value }
      BigIntegerPtr^ := TBigIntegerVarData(Source).BigIntegerPtr^;
    end;
  end;
end;

function TBigIntegerVariantType.IsClear(const V: TVarData): Boolean;
begin
  if V.VType = varEmpty then
    Exit(true);

  { Signal clear value }
  Result := (TBigIntegerVarData(V).BigIntegerPtr = nil);
end;

procedure TBigIntegerVariantType.UnaryOp(var Right: TVarData; const &Operator: TVarOp);
begin
  { Select the appropriate operation }
  case &Operator of
    opNegate:
      BigIntegerToVarData(VarDataToBigInteger(Right), Right);
  else
    RaiseInvalidOp;
  end;
end;

function TBigIntegerVariantType.VarDataToBigInteger(const Value: TVarData): BigInteger;
begin
  { Check if the var data has a big cardinal inside }
  if Value.VType = VarType then
  begin
    { Copy the value to result }
    Exit(TBigIntegerVarData(Value).BigIntegerPtr^);
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
        Result := StrToBigInteger(VarDataToStr(Value));
      except
        on EConvertError do
          RaiseCastError;
      end;
    end;
    else
    begin
      { If the incoming value is a big cardinal }
      if Value.VType = varBigCardinal then
        Result := TBigCardinalVarData(Value).BigCardinalPtr^
      else
        RaiseCastError;
    end;
  end;
end;

procedure TBigIntegerVariantType.BigIntegerToVarData(const Value: BigInteger; var OutValue: TVarData);
begin
  { Dispose of the old value. Check it it's ours first }
  if OutValue.VType = VarType then
    Clear(OutValue)
  else
    VarDataClear(OutValue);

  with TBigIntegerVarData(OutValue) do
  begin
    { Assign the new variant the var type that was allocated for us }
    VType := VarType;

    { Allocate space for our big cardinal pointer }
    New(BigIntegerPtr);

    { Copy self to this memory }
    BigIntegerPtr^ := Value;
  end;
end;

{ BigInteger }

class operator BigInteger.Add(const ALeft, ARight: BigInteger): BigInteger;
var
  MagCmp: Integer;
begin
  { On zeroes just use the other one }
  if ALeft.FSign = 0 then
    Exit(ARight);

  if ARight.FSign = 0 then
    Exit(ALeft);

  { Common sign: simply add taking the sign of one of them }
  if ALeft.FSign = ARight.FSign then
  begin
    Result.FSign := ALeft.FSign;
    Result.FMagnitude := ALeft.FMagnitude + ARight.FMagnitude;

    Exit;
  end;

  MagCmp := ALeft.FMagnitude.CompareTo(ARight.FMagnitude);

  if MagCmp = 0 then
  begin
    Result.FMagnitude := 0;
    Result.FSign := 0;

    Exit;
  end;

  if MagCmp > 0 then
  begin
    Result.FMagnitude := ALeft.FMagnitude - ARight.FMagnitude;
    Result.FSign := ALeft.FSign;

    Exit;
  end;

  if MagCmp < 0 then
  begin
    Result.FMagnitude := ARight.FMagnitude - ALeft.FMagnitude;
    Result.FSign := ARight.FSign;

    Exit;
  end;

  ASSERT(false);
  Result := 0;
end;

function BigInteger.CompareTo(const ANumber: BigInteger): Integer;
begin
  if FSign < ANumber.FSign then
    Result := -1
  else if FSign > ANumber.FSign then
    Result := 1
  else
  begin
    { Both signs are equal }

    if FSign = 0 then
      Result := 0
    else
    begin
      { Compare! }
      Result := FMagnitude.CompareTo(ANumber.FMagnitude);

      if FSign = -1 then
        Result := -1 * Result;
    end;
  end;
end;

constructor BigInteger.Create(const ANumber: UInt64);
begin
  FMagnitude := BigCardinal.Create(ANumber);
  FSign := 1;
end;

constructor BigInteger.Create(const ANumber: Cardinal);
begin
  FMagnitude := BigCardinal.Create(ANumber);
  FSign := 1;
end;

constructor BigInteger.Create(const ANumber: BigInteger);
begin
  { Just copy! }
  Self := ANumber;
end;

constructor BigInteger.Create(const ANumber: Int64);
begin
  if ANumber = 0 then
    FSign := 0
  else if ANumber < 0 then
    FSign := -1
  else
    FSign := 1;

  FMagnitude := BigCardinal.Create(ANumber * FSign);
end;

constructor BigInteger.Create(const ANumber: Integer);
begin
  if ANumber = 0 then
    FSign := 0
  else if ANumber < 0 then
    FSign := -1
  else
    FSign := 1;

  FMagnitude := BigCardinal.Create(ANumber * FSign);
end;

class operator BigInteger.Dec(const AValue: BigInteger): BigInteger;
begin
  { Simply decrease 1 }
  Result := AValue - 1;
end;

class operator BigInteger.Implicit(const ANumber: Cardinal): BigInteger;
begin
  { Call constructor }
  Result := BigInteger.Create(ANumber);
end;

class operator BigInteger.Implicit(const ANumber: UInt64): BigInteger;
begin
  { Call constructor }
  Result := BigInteger.Create(ANumber);
end;

class operator BigInteger.Implicit(const ANumber: Byte): BigInteger;
begin
  { Call constructor }
  Result := BigInteger.Create(ANumber);
end;

class operator BigInteger.Implicit(const ANumber: Word): BigInteger;
begin
  { Call constructor }
  Result := BigInteger.Create(ANumber);
end;

class operator BigInteger.Implicit(const ANumber: Integer): BigInteger;
begin
  { Call constructor }
  Result := BigInteger.Create(ANumber);
end;

class operator BigInteger.Implicit(const ANumber: Int64): BigInteger;
begin
  { Call constructor }
  Result := BigInteger.Create(ANumber);
end;

class operator BigInteger.Implicit(const ANumber: SmallInt): BigInteger;
begin
  { Call constructor }
  Result := BigInteger.Create(ANumber);
end;

class operator BigInteger.Implicit(const ANumber: ShortInt): BigInteger;
begin
  { Call constructor }
  Result := BigInteger.Create(ANumber);
end;

class operator BigInteger.Inc(const AValue: BigInteger): BigInteger;
begin
  { Simply increase 1 }
  Result := AValue + 1;
end;

class operator BigInteger.IntDivide(const ALeft, ARight: BigInteger): BigInteger;
begin
  { Left one is zero = 0 }
  if (ALeft.FSign = 0) then
  begin
    Result.FSign := 0;
    Result.FMagnitude := 0;

    Exit;
  end;

  if (ARight.FSign = 0) then
    ExceptionHelper.Throw_DivByZeroError();

  { Multiply the sign and then the magnitude }
  Result.FSign := ALeft.FSign * ARight.FSign;
  Result.FMagnitude := ALeft.FMagnitude div ARight.FMagnitude;
end;

class operator BigInteger.Equal(const ALeft, ARight: BigInteger): Boolean;
begin
  Result := (ALeft.CompareTo(ARight) = 0);
end;

class operator BigInteger.Explicit(const ANumber: BigInteger): ShortInt;
begin
  { Call convertion code }
  Result := ANumber.ToShortInt();
end;

class operator BigInteger.Explicit(const ANumber: BigInteger): Int64;
begin
  { Call convertion code }
  Result := ANumber.ToInt64();
end;

class operator BigInteger.Explicit(const ANumber: BigInteger): SmallInt;
begin
  { Call convertion code }
  Result := ANumber.ToSmallInt();
end;

class operator BigInteger.Explicit(const ANumber: BigInteger): Integer;
begin
  { Call convertion code }
  Result := ANumber.ToInteger();
end;

class operator BigInteger.GreaterThan(const ALeft, ARight: BigInteger): Boolean;
begin
  Result := (ALeft.CompareTo(ARight) > 0);
end;

class operator BigInteger.GreaterThanOrEqual(const ALeft, ARight: BigInteger): Boolean;
begin
  Result := (ALeft.CompareTo(ARight) >= 0);
end;

class operator BigInteger.LessThan(const ALeft, ARight: BigInteger): Boolean;
begin
  Result := (ALeft.CompareTo(ARight) < 0);
end;

class operator BigInteger.LessThanOrEqual(const ALeft, ARight: BigInteger): Boolean;
begin
  Result := (ALeft.CompareTo(ARight) <= 0);
end;

class operator BigInteger.Modulus(const ALeft, ARight: BigInteger): BigInteger;
begin
  { Left one is zero = 0 }
  if (ALeft.FSign = 0) then
  begin
    Result.FSign := 0;
    Result.FMagnitude := 0;

    Exit;
  end;

  if (ARight.FSign = 0) then
    ExceptionHelper.Throw_DivByZeroError();

  { Multiply the sign and then the magnitude }
  Result.FSign := ALeft.FSign;
  Result.FMagnitude := ALeft.FMagnitude mod ARight.FMagnitude;

  { Post check }
  if Result.FMagnitude = 0 then
    Result.FSign := 0;
end;

class operator BigInteger.Multiply(const ALeft, ARight: BigInteger): BigInteger;
begin
  { Either one is zero = 0 }
  if (ALeft.FSign = 0) or (ARight.FSign = 0) then
  begin
    Result.FSign := 0;
    Result.FMagnitude := 0;

    Exit;
  end;

  { Multiply the sign and then the magnitude }
  Result.FSign := ALeft.FSign * ARight.FSign;
  Result.FMagnitude := ALeft.FMagnitude * ARight.FMagnitude;
end;

class operator BigInteger.Negative(const AValue: BigInteger): BigInteger;
begin
  Result := AValue;
  Result.FSign := Result.FSign * -1;
end;

class operator BigInteger.NotEqual(const ALeft, ARight: BigInteger): Boolean;
begin
  Result := (ALeft.CompareTo(ARight) <> 0);
end;

class operator BigInteger.Positive(const AValue: BigInteger): BigInteger;
begin
  { Nothing ... }
  Result := AValue;
end;

class operator BigInteger.Subtract(const ALeft, ARight: BigInteger): BigInteger;
var
  MagCmp: Integer;
begin
  { On zeroes things are easy }
  if ALeft.FSign = 0 then
  begin
    Result.FSign := -1 * ARight.FSign;
    Result.FMagnitude := ARight.FMagnitude;

    Exit;
  end;

  if ARight.FSign = 0 then
    Exit(ALeft);

  if ALeft.FSign <> ARight.FSign then
  begin
    Result.FSign := ALeft.FSign;
    Result.FMagnitude := ALeft.FMagnitude + ARight.FMagnitude;

    Exit;
  end;

  MagCmp := ALeft.FMagnitude.CompareTo(ARight.FMagnitude);

  if MagCmp = 0 then
  begin
    Result.FMagnitude := 0;
    Result.FSign := 0;

    Exit;
  end;

  if MagCmp > 0 then
  begin
    Result.FMagnitude := ALeft.FMagnitude - ARight.FMagnitude;
    Result.FSign := ALeft.FSign;

    Exit;
  end;

  if MagCmp < 0 then
  begin
    Result.FMagnitude := ARight.FMagnitude - ALeft.FMagnitude;
    Result.FSign := -1 * ARight.FSign;

    Exit;
  end;

  ASSERT(false);
  Result := 0;
end;

function BigInteger.ToInt64: Int64;
begin
  Result := FMagnitude.ToUInt64() * FSign;
end;

function BigInteger.ToInteger: Integer;
begin
  Result := FMagnitude.ToInteger() * FSign;
end;

function BigInteger.ToShortInt: ShortInt;
begin
  Result := FMagnitude.ToShortInt() * FSign;
end;

function BigInteger.ToSmallInt: SmallInt;
begin
  Result := FMagnitude.ToSmallInt() * FSign;
end;

constructor BigInteger.Create(const ANumber: BigCardinal);
begin
  { Simply copy and make positive }
  FMagnitude := ANumber;
  FSign := 1;
end;

class operator BigInteger.Implicit(const ANumber: BigCardinal): BigInteger;
begin
  { Call constructor }
  Result := BigInteger.Create(ANumber);
end;

class operator BigInteger.Implicit(const ANumber: BigInteger): Variant;
begin
  { Clear out the result }
  VarClear(Result);

  with TBigIntegerVarData(Result) do
  begin
    { Assign the new variant the var type that was allocated for us }
    VType := varBigInteger;

    { Allocate space for our big cardinal pointer }
    New(BigIntegerPtr);

    { Copy self to this memory }
    BigIntegerPtr^ := ANumber;
  end;
end;

{ TBigIntegerType }

function TBigIntegerType.AreEqual(const AValue1, AValue2: BigInteger): Boolean;
begin
  Result := (AValue1.CompareTo(AValue2) = 0);
end;

function TBigIntegerType.Compare(const AValue1, AValue2: BigInteger): Integer;
begin
  Result := AValue1.CompareTo(AValue2);
end;

constructor TBigIntegerType.Create;
begin
  inherited;

  FBigCardinalType := TBigCardinalType.Create();
end;

destructor TBigIntegerType.Destroy;
begin
  FBigCardinalType.Free();
  inherited;
end;

procedure TBigIntegerType.DoDeserialize(const AInfo: TValueInfo; out AValue: BigInteger; const AContext: IDeserializationContext);
var
  LStr: String;
begin
  AContext.GetValue(AInfo, LStr);
  AValue := StrToBigInteger(LStr);
end;

procedure TBigIntegerType.DoSerialize(const AInfo: TValueInfo; const AValue: BigInteger; const AContext: ISerializationContext);
begin
  AContext.AddValue(AInfo, IntToStr(AValue));
end;

function TBigIntegerType.Family: TTypeFamily;
begin
  Result := tfSignedInteger;
end;

function TBigIntegerType.GenerateHashCode(const AValue: BigInteger): Integer;
begin
  { Exit with 0 on 0 size }
  if AValue.FSign = 0 then
    Exit(0);

  { Call the Type-Support provided function }
  Result := AValue.FSign * FBigCardinalType.GenerateHashCode(AValue.FMagnitude);
end;

function TBigIntegerType.GetString(const AValue: BigInteger): String;
begin
  Result := IntToStr(AValue);
end;

function TBigIntegerType.TryConvertFromVariant(const AValue: Variant; out ORes: BigInteger): Boolean;
begin
  { May not be a valid BigCardinal }
  try
    ORes := SgtBigIntegerVariantType.VarDataToBigInteger(TVarData(AValue));
  except
    Exit(false);
  end;

  Result := true;
end;

function TBigIntegerType.TryConvertToVariant(const AValue: BigInteger; out ORes: Variant): Boolean;
begin
  { Simple variant conversion }
  ORes := AValue;
  Result := true;
end;

{ TBigIntegerMathExtension }

function TBigIntegerMathExtension.Abs(const AValue: BigInteger): BigInteger;
begin
  Result := DeHL.Math.BigInteger.Abs(AValue);
end;

function TBigIntegerMathExtension.Add(const AValue1, AValue2: BigInteger): BigInteger;
begin
  Result := AValue1 + AValue2;
end;

function TBigIntegerMathExtension.IntegralDivide(const AValue1, AValue2: BigInteger): BigInteger;
begin
  Result := AValue1 div AValue2;
end;

function TBigIntegerMathExtension.MinusOne: BigInteger;
begin
  Result := -1
end;

function TBigIntegerMathExtension.Modulo(const AValue1, AValue2: BigInteger): BigInteger;
begin
  Result := AValue1 mod AValue2;
end;

function TBigIntegerMathExtension.Multiply(const AValue1, AValue2: BigInteger): BigInteger;
begin
  Result := AValue1 * AValue2;
end;

function TBigIntegerMathExtension.Negate(const AValue: BigInteger): BigInteger;
begin
  Result := -AValue;
end;

function TBigIntegerMathExtension.One: BigInteger;
begin
  Result := 1;
end;

function TBigIntegerMathExtension.Subtract(const AValue1, AValue2: BigInteger): BigInteger;
begin
  Result := AValue1 - AValue2;
end;

function TBigIntegerMathExtension.Zero: BigInteger;
begin
  Result := 0;
end;

initialization
  { Register custom type }
  TType<BigInteger>.Register(TBigIntegerType);
  TMathExtension<BigInteger>.Register(TBigIntegerMathExtension);

  { Register our custom variant type }
  SgtBigIntegerVariantType := TBigIntegerVariantType.Create();

  { Set the value of the varBigInteger }
  varBigInteger := SgtBigIntegerVariantType.VarType;

finalization
  { Unregister custom type }
  TMathExtension<BigInteger>.Unregister();
  TType<BigInteger>.Unregister();

  { Uregister our custom variant }
  FreeAndNil(SgtBigIntegerVariantType);
end.
