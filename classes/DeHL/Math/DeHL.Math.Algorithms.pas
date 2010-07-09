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
unit DeHL.Math.Algorithms;
interface
uses
  SysUtils,
  Math,
  DeHL.Base,
  DeHL.Types,
  DeHL.Exceptions,
  DeHL.Collections.Base,
  DeHL.Math.Types;

type
  { Accumulator record }
  Accumulator = record
    class function Sum<T>(const AEnumerable: IEnexCollection<T>; const AType: IType<T>): T; overload; static;
    class function Sum<T>(const AEnumerable: IEnexCollection<T>): T; overload; static;

    class function Average<T>(const AEnumerable: IEnexCollection<T>; const AType: IType<T>): T; overload; static;
    class function Average<T>(const AEnumerable: IEnexCollection<T>): T; overload; static;
  end;

implementation

{ Accumulator }

class function Accumulator.Average<T>(const AEnumerable: IEnexCollection<T>; const AType: IType<T>): T;
var
  Op: TFunc<T, T, T>;
  NaturalExtension: IUnsignedIntegerMathExtension<T>;
  RealExtension: IRealMathExtension<T>;
  Count: Cardinal;
begin
  { Check arguments }
  if AEnumerable = nil then
    ExceptionHelper.Throw_ArgumentNilError('AEnumerable');

  if AType = nil then
    ExceptionHelper.Throw_ArgumentNilError('AType');

  { Apply restrictions! }
  AType.RestrictTo([tfUnsignedInteger, tfSignedInteger, tfReal]);

  { Start at 1 }
  Count := 1;

  if AType.Family = tfReal then
  begin
    RealExtension := TMathExtension<T>.Real(AType);

    { Create an aggregator for numbers }
    Op := function(Arg1, Arg2: T): T begin
      Inc(Count);
      Exit(RealExtension.Add(Arg1, Arg2));
    end;

    { Aggregate }
    Result := AEnumerable.AggregateOrDefault(Op, RealExtension.Zero);

    { Calculate the division }
    Result := RealExtension.Divide(Result, AType.ConvertFromVariant(Count));
  end else
  begin
    NaturalExtension := TMathExtension<T>.Natural(AType);

    { Create an aggregator for numbers }
    Op := function(Arg1, Arg2: T): T begin
      Inc(Count);
      Exit(NaturalExtension.Add(Arg1, Arg2));
    end;

    { Aggregate }
    Result := AEnumerable.AggregateOrDefault(Op, NaturalExtension.Zero);

    { Calculate the division }
    Result := NaturalExtension.IntegralDivide(Result, AType.ConvertFromVariant(Count));
  end;
end;

class function Accumulator.Sum<T>(const AEnumerable: IEnexCollection<T>; const AType: IType<T>): T;
var
  Op: TFunc<T, T, T>;
  Extension: IMathExtension<T>;
begin
  { Check arguments }
  if AEnumerable = nil then
    ExceptionHelper.Throw_ArgumentNilError('AEnumerable');

  if AType = nil then
    ExceptionHelper.Throw_ArgumentNilError('AType');

  { Apply restrictions! }
  AType.RestrictTo([tfUnsignedInteger, tfSignedInteger, tfReal]);

  { Obtain the extension for natural numbers }
  Extension := TMathExtension<T>.Common(AType);

  { Create an aggregator for numbers }
  Op := function(Arg1, Arg2: T): T begin
    Exit(Extension.Add(Arg1, Arg2));
  end;

  { Apply aggregator }
  Result := AEnumerable.AggregateOrDefault(Op, Extension.Zero);
end;

class function Accumulator.Average<T>(const AEnumerable: IEnexCollection<T>): T;
begin
  Result := Average<T>(AEnumerable, TType<T>.Default);
end;

class function Accumulator.Sum<T>(const AEnumerable: IEnexCollection<T>): T;
begin
  Result := Sum<T>(AEnumerable, TType<T>.Default);
end;

end.
