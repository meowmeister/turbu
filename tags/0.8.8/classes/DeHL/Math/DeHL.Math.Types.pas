(*
* Copyright (c) 2009, Ciobanu Alexandru
* All rights reserved.
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
unit DeHL.Math.Types;
interface
uses SysUtils,
     DeHL.Base,
     DeHL.Types,
     DeHL.Exceptions;

(*
  Type definitions. The following interfaces split the number math into
  different "species".
*)
type
  { Standard operations }
  IMathExtension<T> = interface
    function Add(const AValue1, AValue2: T): T;
    function Subtract(const AValue1, AValue2: T): T;
    function Multiply(const AValue1, AValue2: T): T;

    { Neutral Math elements }
    function Zero: T;
    function One: T;
  end;

  { Unsigned Integer-only operations }
  IUnsignedIntegerMathExtension<T> = interface(IMathExtension<T>)
    function IntegralDivide(const AValue1, AValue2: T): T;
    function Modulo(const AValue1, AValue2: T): T;
  end;

  { Integer-only operations }
  IIntegerMathExtension<T> = interface(IUnsignedIntegerMathExtension<T>)
    function Negate(const AValue: T): T;
    function Abs(const AValue: T): T;

    { Neutral element }
    function MinusOne: T;
  end;

  { Unsigned Integer-only operations }
  IRealMathExtension<T> = interface(IMathExtension<T>)
    function Divide(const AValue1, AValue2: T): T;
    function Negate(const AValue: T): T;
    function Abs(const AValue: T): T;

    { Neutral element }
    function MinusOne: T;
  end;

(*
  Abstract classes
*)
type
  { Used only as a base class to the generic version -- HACK to allow a class var }
  TMathExtension = class abstract(TTypeExtension)
  private class var
    FExtender: TTypeExtender;

  end;

  TMathExtensionClass = class of TMathExtension;

  { Base class to all up-coming stuff }
  TMathExtension<T> = class abstract(TMathExtension, IMathExtension<T>)
  public
    function Add(const AValue1, AValue2: T): T; virtual; abstract;
    function Subtract(const AValue1, AValue2: T): T; virtual; abstract;
    function Multiply(const AValue1, AValue2: T): T; virtual; abstract;

    { Neutral Math elements }
    function Zero: T; virtual; abstract;
    function One: T; virtual; abstract;

    { Class static methods }
    class function Common(const AType: IType<T>): IMathExtension<T>; overload; static;
    class function Common: IMathExtension<T>; overload; static;

    class function Natural(const AType: IType<T>): IUnsignedIntegerMathExtension<T>; overload; static;
    class function Natural: IUnsignedIntegerMathExtension<T>; overload; static;

    class function Integer(const AType: IType<T>): IIntegerMathExtension<T>; overload; static;
    class function Integer: IIntegerMathExtension<T>; overload; static;

    class function Real(const AType: IType<T>): IRealMathExtension<T>; overload; static;
    class function Real: IRealMathExtension<T>; overload; static;

    { Registration and un-registration }
    class procedure Register(const AClass: TMathExtensionClass); static;
    class procedure Unregister(); static;
  end;

  { Unsigned integer base }
  TUnsignedIntegerMathExtension<T> = class abstract(TMathExtension<T>, IUnsignedIntegerMathExtension<T>)
  public
    { IUnsignedIntegerMathExtension<T> }
    function IntegralDivide(const AValue1, AValue2: T): T; virtual; abstract;
    function Modulo(const AValue1, AValue2: T): T; virtual; abstract;
  end;

  { Real base }
  TRealMathExtension<T> = class abstract(TMathExtension<T>, IRealMathExtension<T>)
  public
    { IRealMathExtension<T> }
    function Divide(const AValue1, AValue2: T): T; virtual; abstract;
    function Negate(const AValue: T): T; virtual; abstract;
    function Abs(const AValue: T): T; virtual; abstract;
    function MinusOne: T; virtual; abstract;
  end;

  { Integer base }
  TIntegerMathExtension<T> = class abstract(TUnsignedIntegerMathExtension<T>, IIntegerMathExtension<T>)
  public
    { Sign-related operations }
    function Negate(const AValue: T): T; virtual; abstract;
    function Abs(const AValue: T): T; virtual; abstract;

    { Neutral Math elements }
    function MinusOne: T; virtual; abstract;
  end;

(*
  Actual implementations for each math family
*)
type
  { Math extensions for the Byte type }
  TByteMathExtension = class sealed(TUnsignedIntegerMathExtension<Byte>)
  public
    { Standard operations }
    function Add(const AValue1, AValue2: Byte): Byte; override;
    function Subtract(const AValue1, AValue2: Byte): Byte; override;
    function Multiply(const AValue1, AValue2: Byte): Byte; override;
    function IntegralDivide(const AValue1, AValue2: Byte): Byte; override;
    function Modulo(const AValue1, AValue2: Byte): Byte; override;

    { Neutral Math elements }
    function Zero: Byte; override;
    function One: Byte; override;
  end;

  { Math extensions for the ShortInt type }
  TShortIntMathExtension = class sealed(TIntegerMathExtension<ShortInt>)
  public
    { Standard operations }
    function Add(const AValue1, AValue2: ShortInt): ShortInt; override;
    function Subtract(const AValue1, AValue2: ShortInt): ShortInt; override;
    function Multiply(const AValue1, AValue2: ShortInt): ShortInt; override;
    function IntegralDivide(const AValue1, AValue2: ShortInt): ShortInt; override;
    function Modulo(const AValue1, AValue2: ShortInt): ShortInt; override;

    { Sign-related operations }
    function Negate(const AValue: ShortInt): ShortInt; override;
    function Abs(const AValue: ShortInt): ShortInt; override;

    { Neutral Math elements }
    function Zero: ShortInt; override;
    function One: ShortInt; override;
    function MinusOne: ShortInt; override;
  end;

  { Math extensions for the Word type }
  TWordMathExtension = class sealed(TUnsignedIntegerMathExtension<Word>)
  public
    { Standard operations }
    function Add(const AValue1, AValue2: Word): Word; override;
    function Subtract(const AValue1, AValue2: Word): Word; override;
    function Multiply(const AValue1, AValue2: Word): Word; override;
    function IntegralDivide(const AValue1, AValue2: Word): Word; override;
    function Modulo(const AValue1, AValue2: Word): Word; override;

    { Neutral Math elements }
    function Zero: Word; override;
    function One: Word; override;
  end;

  { Math extensions for the SmallInt type }
  TSmallIntMathExtension = class sealed(TIntegerMathExtension<SmallInt>)
  public
    { Standard operations }
    function Add(const AValue1, AValue2: SmallInt): SmallInt; override;
    function Subtract(const AValue1, AValue2: SmallInt): SmallInt; override;
    function Multiply(const AValue1, AValue2: SmallInt): SmallInt; override;
    function IntegralDivide(const AValue1, AValue2: SmallInt): SmallInt; override;
    function Modulo(const AValue1, AValue2: SmallInt): SmallInt; override;

    { Sign-related operations }
    function Negate(const AValue: SmallInt): SmallInt; override;
    function Abs(const AValue: SmallInt): SmallInt; override;

    { Neutral Math elements }
    function Zero: SmallInt; override;
    function One: SmallInt; override;
    function MinusOne: SmallInt; override;
  end;

  { Math extensions for the Cardinal type }
  TCardinalMathExtension = class sealed(TUnsignedIntegerMathExtension<Cardinal>)
  public
    { Standard operations }
    function Add(const AValue1, AValue2: Cardinal): Cardinal; override;
    function Subtract(const AValue1, AValue2: Cardinal): Cardinal; override;
    function Multiply(const AValue1, AValue2: Cardinal): Cardinal; override;
    function IntegralDivide(const AValue1, AValue2: Cardinal): Cardinal; override;
    function Modulo(const AValue1, AValue2: Cardinal): Cardinal; override;

    { Neutral Math elements }
    function Zero: Cardinal; override;
    function One: Cardinal; override;
  end;

  { Math extensions for the Integer type }
  TIntegerMathExtension = class sealed(TIntegerMathExtension<Integer>)
  public
    { Standard operations }
    function Add(const AValue1, AValue2: Integer): Integer; override;
    function Subtract(const AValue1, AValue2: Integer): Integer; override;
    function Multiply(const AValue1, AValue2: Integer): Integer; override;
    function IntegralDivide(const AValue1, AValue2: Integer): Integer; override;
    function Modulo(const AValue1, AValue2: Integer): Integer; override;

    { Sign-related operations }
    function Negate(const AValue: Integer): Integer; override;
    function Abs(const AValue: Integer): Integer; override;

    { Neutral Math elements }
    function Zero: Integer; override;
    function One: Integer; override;
    function MinusOne: Integer; override;
  end;

  { Math extensions for the UInt64 type }
  TUInt64MathExtension = class sealed(TUnsignedIntegerMathExtension<UInt64>)
  public
    { Standard operations }
    function Add(const AValue1, AValue2: UInt64): UInt64; override;
    function Subtract(const AValue1, AValue2: UInt64): UInt64; override;
    function Multiply(const AValue1, AValue2: UInt64): UInt64; override;
    function IntegralDivide(const AValue1, AValue2: UInt64): UInt64; override;
    function Modulo(const AValue1, AValue2: UInt64): UInt64; override;

    { Neutral Math elements }
    function Zero: UInt64; override;
    function One: UInt64; override;
  end;

  { Math extensions for the Int64 type }
  TInt64MathExtension = class sealed(TIntegerMathExtension<Int64>)
  public
    { Standard operations }
    function Add(const AValue1, AValue2: Int64): Int64; override;
    function Subtract(const AValue1, AValue2: Int64): Int64; override;
    function Multiply(const AValue1, AValue2: Int64): Int64; override;
    function IntegralDivide(const AValue1, AValue2: Int64): Int64; override;
    function Modulo(const AValue1, AValue2: Int64): Int64; override;

    { Sign-related operations }
    function Negate(const AValue: Int64): Int64; override;
    function Abs(const AValue: Int64): Int64; override;

    { Neutral Math elements }
    function Zero: Int64; override;
    function One: Int64; override;
    function MinusOne: Int64; override;
  end;

  { Math extensions for the Single type }
  TSingleMathExtension = class sealed(TRealMathExtension<Single>)
  public
    { Standard operations }
    function Add(const AValue1, AValue2: Single): Single; override;
    function Subtract(const AValue1, AValue2: Single): Single; override;
    function Multiply(const AValue1, AValue2: Single): Single; override;
    function Divide(const AValue1, AValue2: Single): Single; override;

    { Sign-related operations }
    function Negate(const AValue: Single): Single; override;
    function Abs(const AValue: Single): Single; override;

    { Neutral Math elements }
    function Zero: Single; override;
    function One: Single; override;
    function MinusOne: Single; override;
  end;

  { Math extensions for the Double type }
  TDoubleMathExtension = class sealed(TRealMathExtension<Double>)
  public
    { Standard operations }
    function Add(const AValue1, AValue2: Double): Double; override;
    function Subtract(const AValue1, AValue2: Double): Double; override;
    function Multiply(const AValue1, AValue2: Double): Double; override;
    function Divide(const AValue1, AValue2: Double): Double; override;

    { Sign-related operations }
    function Negate(const AValue: Double): Double; override;
    function Abs(const AValue: Double): Double; override;

    { Neutral Math elements }
    function Zero: Double; override;
    function One: Double; override;
    function MinusOne: Double; override;
  end;

  { Math extensions for the Extended type }
  TExtendedMathExtension = class sealed(TRealMathExtension<Extended>)
  public
    { Standard operations }
    function Add(const AValue1, AValue2: Extended): Extended; override;
    function Subtract(const AValue1, AValue2: Extended): Extended; override;
    function Multiply(const AValue1, AValue2: Extended): Extended; override;
    function Divide(const AValue1, AValue2: Extended): Extended; override;

    { Sign-related operations }
    function Negate(const AValue: Extended): Extended; override;
    function Abs(const AValue: Extended): Extended; override;

    { Neutral Math elements }
    function Zero: Extended; override;
    function One: Extended; override;
    function MinusOne: Extended; override;
  end;

  { Math extensions for the Currency type }
  TCurrencyMathExtension = class sealed(TRealMathExtension<Currency>)
  public
    { Standard operations }
    function Add(const AValue1, AValue2: Currency): Currency; override;
    function Subtract(const AValue1, AValue2: Currency): Currency; override;
    function Multiply(const AValue1, AValue2: Currency): Currency; override;
    function Divide(const AValue1, AValue2: Currency): Currency; override;

    { Sign-related operations }
    function Negate(const AValue: Currency): Currency; override;
    function Abs(const AValue: Currency): Currency; override;

    { Neutral Math elements }
    function Zero: Currency; override;
    function One: Currency; override;
    function MinusOne: Currency; override;
  end;

  { Math extensions for the Comp type }
  TCompMathExtension = class sealed(TRealMathExtension<Comp>)
  public
    { Standard operations }
    function Add(const AValue1, AValue2: Comp): Comp; override;
    function Subtract(const AValue1, AValue2: Comp): Comp; override;
    function Multiply(const AValue1, AValue2: Comp): Comp; override;
    function Divide(const AValue1, AValue2: Comp): Comp; override;

    { Sign-related operations }
    function Negate(const AValue: Comp): Comp; override;
    function Abs(const AValue: Comp): Comp; override;

    { Neutral Math elements }
    function Zero: Comp; override;
    function One: Comp; override;
    function MinusOne: Comp; override;
  end;

implementation

{ TByteMathExtension }

function TByteMathExtension.Add(const AValue1, AValue2: Byte): Byte;
begin
  Result := AValue1 + AValue2;
end;

function TByteMathExtension.IntegralDivide(const AValue1, AValue2: Byte): Byte;
begin
  Result := AValue1 div AValue2;
end;

function TByteMathExtension.Modulo(const AValue1, AValue2: Byte): Byte;
begin
  Result := AValue1 mod AValue2;
end;

function TByteMathExtension.Multiply(const AValue1, AValue2: Byte): Byte;
begin
  Result := AValue1 * AValue2;
end;

function TByteMathExtension.One: Byte;
begin
  Result := 1;
end;

function TByteMathExtension.Subtract(const AValue1, AValue2: Byte): Byte;
begin
  Result := AValue1 - AValue2;
end;

function TByteMathExtension.Zero: Byte;
begin
  Result := 0;
end;

{ TWordMathExtension }

function TWordMathExtension.Add(const AValue1, AValue2: Word): Word;
begin
  Result := AValue1 + AValue2;
end;

function TWordMathExtension.IntegralDivide(const AValue1, AValue2: Word): Word;
begin
  Result := AValue1 div AValue2;
end;

function TWordMathExtension.Modulo(const AValue1, AValue2: Word): Word;
begin
  Result := AValue1 mod AValue2;
end;

function TWordMathExtension.Multiply(const AValue1, AValue2: Word): Word;
begin
  Result := AValue1 * AValue2;
end;

function TWordMathExtension.One: Word;
begin
  Result := 1;
end;

function TWordMathExtension.Subtract(const AValue1, AValue2: Word): Word;
begin
  Result := AValue1 - AValue2;
end;

function TWordMathExtension.Zero: Word;
begin
  Result := 0;
end;

{ TCardinalMathExtension }

function TCardinalMathExtension.Add(const AValue1, AValue2: Cardinal): Cardinal;
begin
  Result := AValue1 + AValue2;
end;

function TCardinalMathExtension.IntegralDivide(const AValue1, AValue2: Cardinal): Cardinal;
begin
  Result := AValue1 div AValue2;
end;

function TCardinalMathExtension.Modulo(const AValue1, AValue2: Cardinal): Cardinal;
begin
  Result := AValue1 mod AValue2;
end;

function TCardinalMathExtension.Multiply(const AValue1, AValue2: Cardinal): Cardinal;
begin
  Result := AValue1 * AValue2;
end;

function TCardinalMathExtension.One: Cardinal;
begin
  Result := 1;
end;

function TCardinalMathExtension.Subtract(const AValue1, AValue2: Cardinal): Cardinal;
begin
  Result := AValue1 - AValue2;
end;

function TCardinalMathExtension.Zero: Cardinal;
begin
  Result := 0;
end;


{ TUInt64MathExtension }

function TUInt64MathExtension.Add(const AValue1, AValue2: UInt64): UInt64;
begin
  Result := AValue1 + AValue2;
end;

function TUInt64MathExtension.IntegralDivide(const AValue1, AValue2: UInt64): UInt64;
begin
  Result := AValue1 div AValue2;
end;

function TUInt64MathExtension.Modulo(const AValue1, AValue2: UInt64): UInt64;
begin
  Result := AValue1 mod AValue2;
end;

function TUInt64MathExtension.Multiply(const AValue1, AValue2: UInt64): UInt64;
begin
  Result := AValue1 * AValue2;
end;

function TUInt64MathExtension.One: UInt64;
begin
  Result := 1;
end;

function TUInt64MathExtension.Subtract(const AValue1, AValue2: UInt64): UInt64;
begin
  Result := AValue1 - AValue2;
end;

function TUInt64MathExtension.Zero: UInt64;
begin
  Result := 0;
end;



{ TShortIntMathExtension }

function TShortIntMathExtension.Abs(const AValue: ShortInt): ShortInt;
begin
  Result := System.Abs(AValue);
end;

function TShortIntMathExtension.Add(const AValue1, AValue2: ShortInt): ShortInt;
begin
  Result := AValue1 + AValue2;
end;

function TShortIntMathExtension.IntegralDivide(const AValue1, AValue2: ShortInt): ShortInt;
begin
  Result := AValue1 div AValue2;
end;

function TShortIntMathExtension.MinusOne: ShortInt;
begin
  Result := -1
end;

function TShortIntMathExtension.Modulo(const AValue1, AValue2: ShortInt): ShortInt;
begin
  Result := AValue1 mod AValue2;
end;

function TShortIntMathExtension.Multiply(const AValue1, AValue2: ShortInt): ShortInt;
begin
  Result := AValue1 * AValue2;
end;

function TShortIntMathExtension.Negate(const AValue: ShortInt): ShortInt;
begin
  Result := -AValue;
end;

function TShortIntMathExtension.One: ShortInt;
begin
  Result := 1;
end;

function TShortIntMathExtension.Subtract(const AValue1, AValue2: ShortInt): ShortInt;
begin
  Result := AValue1 - AValue2;
end;

function TShortIntMathExtension.Zero: ShortInt;
begin
  Result := 0;
end;

{ TSmallIntMathExtension }

function TSmallIntMathExtension.Abs(const AValue: SmallInt): SmallInt;
begin
  Result := System.Abs(AValue);
end;

function TSmallIntMathExtension.Add(const AValue1, AValue2: SmallInt): SmallInt;
begin
  Result := AValue1 + AValue2;
end;

function TSmallIntMathExtension.IntegralDivide(const AValue1, AValue2: SmallInt): SmallInt;
begin
  Result := AValue1 div AValue2;
end;

function TSmallIntMathExtension.MinusOne: SmallInt;
begin
  Result := -1
end;

function TSmallIntMathExtension.Modulo(const AValue1, AValue2: SmallInt): SmallInt;
begin
  Result := AValue1 mod AValue2;
end;

function TSmallIntMathExtension.Multiply(const AValue1, AValue2: SmallInt): SmallInt;
begin
  Result := AValue1 * AValue2;
end;

function TSmallIntMathExtension.Negate(const AValue: SmallInt): SmallInt;
begin
  Result := -AValue;
end;

function TSmallIntMathExtension.One: SmallInt;
begin
  Result := 1;
end;

function TSmallIntMathExtension.Subtract(const AValue1, AValue2: SmallInt): SmallInt;
begin
  Result := AValue1 - AValue2;
end;

function TSmallIntMathExtension.Zero: SmallInt;
begin
  Result := 0;
end;

{ TIntegerMathExtension }

function TIntegerMathExtension.Abs(const AValue: Integer): Integer;
begin
  Result := System.Abs(AValue);
end;

function TIntegerMathExtension.Add(const AValue1, AValue2: Integer): Integer;
begin
  Result := AValue1 + AValue2;
end;

function TIntegerMathExtension.IntegralDivide(const AValue1, AValue2: Integer): Integer;
begin
  Result := AValue1 div AValue2;
end;

function TIntegerMathExtension.MinusOne: Integer;
begin
  Result := -1
end;

function TIntegerMathExtension.Modulo(const AValue1, AValue2: Integer): Integer;
begin
  Result := AValue1 mod AValue2;
end;

function TIntegerMathExtension.Multiply(const AValue1, AValue2: Integer): Integer;
begin
  Result := AValue1 * AValue2;
end;

function TIntegerMathExtension.Negate(const AValue: Integer): Integer;
begin
  Result := -AValue;
end;

function TIntegerMathExtension.One: Integer;
begin
  Result := 1;
end;

function TIntegerMathExtension.Subtract(const AValue1, AValue2: Integer): Integer;
begin
  Result := AValue1 - AValue2;
end;

function TIntegerMathExtension.Zero: Integer;
begin
  Result := 0;
end;

{ TInt64MathExtension }

function TInt64MathExtension.Abs(const AValue: Int64): Int64;
begin
  Result := System.Abs(AValue);
end;

function TInt64MathExtension.Add(const AValue1, AValue2: Int64): Int64;
begin
  Result := AValue1 + AValue2;
end;

function TInt64MathExtension.IntegralDivide(const AValue1, AValue2: Int64): Int64;
begin
  Result := AValue1 div AValue2;
end;

function TInt64MathExtension.MinusOne: Int64;
begin
  Result := -1
end;

function TInt64MathExtension.Modulo(const AValue1, AValue2: Int64): Int64;
begin
  Result := AValue1 mod AValue2;
end;

function TInt64MathExtension.Multiply(const AValue1, AValue2: Int64): Int64;
begin
  Result := AValue1 * AValue2;
end;

function TInt64MathExtension.Negate(const AValue: Int64): Int64;
begin
  Result := -AValue;
end;

function TInt64MathExtension.One: Int64;
begin
  Result := 1;
end;

function TInt64MathExtension.Subtract(const AValue1, AValue2: Int64): Int64;
begin
  Result := AValue1 - AValue2;
end;

function TInt64MathExtension.Zero: Int64;
begin
  Result := 0;
end;

{ TMathExtension<T> }

class function TMathExtension<T>.Natural(const AType: IType<T>): IUnsignedIntegerMathExtension<T>;
var
  LObj: TUnsignedIntegerMathExtension<T>;
begin
  if AType = nil then
    ExceptionHelper.Throw_ArgumentNilError('AType');

  { Expect an unsigned integer or a signed integer }
  AType.RestrictTo([tfUnsignedInteger, tfSignedInteger]);

  { Try to obtain an extension object }
  LObj := TUnsignedIntegerMathExtension<T>(AType.GetExtension(FExtender));

  { Raise an exception when needed }
  if LObj = nil then
    ExceptionHelper.Throw_NoMathExtensionForType(AType.Name);

  { Return an interface to the caller }
  Result := LObj;
end;

class function TMathExtension<T>.Real: IRealMathExtension<T>;
begin
  { Call Upper }
  Result := Real(TType<T>.Default);
end;

class procedure TMathExtension<T>.Register(const AClass: TMathExtensionClass);
begin
  { Pass the call to the extender }
  FExtender.Register<T>(AClass);
end;

class procedure TMathExtension<T>.Unregister;
begin
  { Pass the call to the extender }
  FExtender.Unregister<T>();
end;

class function TMathExtension<T>.Common(const AType: IType<T>): IMathExtension<T>;
var
  LObj: TMathExtension<T>;
begin
  if AType = nil then
    ExceptionHelper.Throw_ArgumentNilError('AType');

  { Expect any number }
  AType.RestrictTo([tfUnsignedInteger, tfSignedInteger, tfReal]);

  { Try to obtain an extension object }
  LObj := TMathExtension<T>(AType.GetExtension(FExtender));

  { Raise an exception when needed }
  if LObj = nil then
    ExceptionHelper.Throw_NoMathExtensionForType(AType.Name);

  { Return an interface to the caller }
  Result := LObj;
end;

class function TMathExtension<T>.Common: IMathExtension<T>;
begin
  { Call Upper }
  Result := Common(TType<T>.Default);
end;

class function TMathExtension<T>.Integer: IIntegerMathExtension<T>;
begin
  { Call Upper }
  Result := Integer(TType<T>.Default);
end;

class function TMathExtension<T>.Integer(const AType: IType<T>): IIntegerMathExtension<T>;
var
  LObj: TIntegerMathExtension<T>;
begin
  if AType = nil then
    ExceptionHelper.Throw_ArgumentNilError('AType');

  { Expect an unsigned integer or a signed integer }
  AType.RestrictTo([tfSignedInteger]);

  { Try to obtain an extension object }
  LObj := TIntegerMathExtension<T>(AType.GetExtension(FExtender));

  { Raise an exception when needed }
  if LObj = nil then
    ExceptionHelper.Throw_NoMathExtensionForType(AType.Name);

  { Return an interface to the caller }
  Result := LObj;
end;

class function TMathExtension<T>.Natural: IUnsignedIntegerMathExtension<T>;
begin
  { Call Upper }
  Result := Natural(TType<T>.Default);
end;

class function TMathExtension<T>.Real(const AType: IType<T>): IRealMathExtension<T>;
var
  LObj: TRealMathExtension<T>;
begin
  if AType = nil then
    ExceptionHelper.Throw_ArgumentNilError('AType');

  { Expect a real number }
  AType.RestrictTo([tfReal]);

  { Try to obtain an extension object }
  LObj := TRealMathExtension<T>(AType.GetExtension(FExtender));

  { Raise an exception when needed }
  if LObj = nil then
    ExceptionHelper.Throw_NoMathExtensionForType(AType.Name);

  { Return an interface to the caller }
  Result := LObj;
end;

{ TSingleMathExtension }

function TSingleMathExtension.Abs(const AValue: Single): Single;
begin
  Result := System.Abs(AValue);
end;

function TSingleMathExtension.Add(const AValue1, AValue2: Single): Single;
begin
  Result := AValue1 + AValue2;
end;

function TSingleMathExtension.Divide(const AValue1, AValue2: Single): Single;
begin
  Result := AValue1 / AValue2;
end;

function TSingleMathExtension.MinusOne: Single;
begin
  Result := -1;
end;

function TSingleMathExtension.Multiply(const AValue1, AValue2: Single): Single;
begin
  Result := AValue1 * AValue2;
end;

function TSingleMathExtension.Negate(const AValue: Single): Single;
begin
  Result := -AValue;
end;

function TSingleMathExtension.One: Single;
begin
  Result := 1;
end;

function TSingleMathExtension.Subtract(const AValue1, AValue2: Single): Single;
begin
  Result := AValue1 - AValue2;
end;

function TSingleMathExtension.Zero: Single;
begin
  Result := 0;
end;

{ TDoubleMathExtension }

function TDoubleMathExtension.Abs(const AValue: Double): Double;
begin
  Result := System.Abs(AValue);
end;

function TDoubleMathExtension.Add(const AValue1, AValue2: Double): Double;
begin
  Result := AValue1 + AValue2;
end;

function TDoubleMathExtension.Divide(const AValue1, AValue2: Double): Double;
begin
  Result := AValue1 / AValue2;
end;

function TDoubleMathExtension.MinusOne: Double;
begin
  Result := -1;
end;

function TDoubleMathExtension.Multiply(const AValue1, AValue2: Double): Double;
begin
  Result := AValue1 * AValue2;
end;

function TDoubleMathExtension.Negate(const AValue: Double): Double;
begin
  Result := -AValue;
end;

function TDoubleMathExtension.One: Double;
begin
  Result := 1;
end;

function TDoubleMathExtension.Subtract(const AValue1, AValue2: Double): Double;
begin
  Result := AValue1 - AValue2;
end;

function TDoubleMathExtension.Zero: Double;
begin
  Result := 0;
end;

{ TExtendedMathExtension }

function TExtendedMathExtension.Abs(const AValue: Extended): Extended;
begin
  Result := System.Abs(AValue);
end;

function TExtendedMathExtension.Add(const AValue1, AValue2: Extended): Extended;
begin
  Result := AValue1 + AValue2;
end;

function TExtendedMathExtension.Divide(const AValue1, AValue2: Extended): Extended;
begin
  Result := AValue1 / AValue2;
end;

function TExtendedMathExtension.MinusOne: Extended;
begin
  Result := -1;
end;

function TExtendedMathExtension.Multiply(const AValue1, AValue2: Extended): Extended;
begin
  Result := AValue1 * AValue2;
end;

function TExtendedMathExtension.Negate(const AValue: Extended): Extended;
begin
  Result := -AValue;
end;

function TExtendedMathExtension.One: Extended;
begin
  Result := 1;
end;

function TExtendedMathExtension.Subtract(const AValue1, AValue2: Extended): Extended;
begin
  Result := AValue1 - AValue2;
end;

function TExtendedMathExtension.Zero: Extended;
begin
  Result := 0;
end;

{ TCurrencyMathExtension }

function TCurrencyMathExtension.Abs(const AValue: Currency): Currency;
begin
  Result := System.Abs(AValue);
end;

function TCurrencyMathExtension.Add(const AValue1, AValue2: Currency): Currency;
begin
  Result := AValue1 + AValue2;
end;

function TCurrencyMathExtension.Divide(const AValue1, AValue2: Currency): Currency;
begin
  Result := AValue1 / AValue2;
end;

function TCurrencyMathExtension.MinusOne: Currency;
begin
  Result := -1;
end;

function TCurrencyMathExtension.Multiply(const AValue1, AValue2: Currency): Currency;
begin
  Result := AValue1 * AValue2;
end;

function TCurrencyMathExtension.Negate(const AValue: Currency): Currency;
begin
  Result := -AValue;
end;

function TCurrencyMathExtension.One: Currency;
begin
  Result := 1;
end;

function TCurrencyMathExtension.Subtract(const AValue1, AValue2: Currency): Currency;
begin
  Result := AValue1 - AValue2;
end;

function TCurrencyMathExtension.Zero: Currency;
begin
  Result := 0;
end;

{ TCompMathExtension }

function TCompMathExtension.Abs(const AValue: Comp): Comp;
begin
  Result := System.Abs(AValue);
end;

function TCompMathExtension.Add(const AValue1, AValue2: Comp): Comp;
begin
  Result := AValue1 + AValue2;
end;

function TCompMathExtension.Divide(const AValue1, AValue2: Comp): Comp;
begin
  Result := AValue1 / AValue2;
end;

function TCompMathExtension.MinusOne: Comp;
begin
  Result := -1;
end;

function TCompMathExtension.Multiply(const AValue1, AValue2: Comp): Comp;
begin
  Result := AValue1 * AValue2;
end;

function TCompMathExtension.Negate(const AValue: Comp): Comp;
begin
  Result := -AValue;
end;

function TCompMathExtension.One: Comp;
begin
  Result := 1;
end;

function TCompMathExtension.Subtract(const AValue1, AValue2: Comp): Comp;
begin
  Result := AValue1 - AValue2;
end;

function TCompMathExtension.Zero: Comp;
begin
  Result := 0;
end;

initialization
  { Create a new type extender object and register our extensions }
  TMathExtension.FExtender := TTypeExtender.Create();
  TMathExtension.FExtender.Register<Byte>(TByteMathExtension);
  TMathExtension.FExtender.Register<ShortInt>(TShortIntMathExtension);
  TMathExtension.FExtender.Register<Word>(TWordMathExtension);
  TMathExtension.FExtender.Register<SmallInt>(TSmallIntMathExtension);
  TMathExtension.FExtender.Register<Cardinal>(TCardinalMathExtension);
  TMathExtension.FExtender.Register<Integer>(TIntegerMathExtension);
  TMathExtension.FExtender.Register<UInt64>(TUInt64MathExtension);
  TMathExtension.FExtender.Register<Int64>(TInt64MathExtension);
  TMathExtension.FExtender.Register<Single>(TSingleMathExtension);
  TMathExtension.FExtender.Register<Double>(TDoubleMathExtension);
  TMathExtension.FExtender.Register<Extended>(TExtendedMathExtension);
  TMathExtension.FExtender.Register<Currency>(TCurrencyMathExtension);
  TMathExtension.FExtender.Register<Comp>(TCompMathExtension);


finalization
  { Destroy the extension object }
  FreeAndNil(TMathExtension.FExtender);

end.

