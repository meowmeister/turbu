(*
* Copyright (c) 2008-2010, Ciobanu Alexandru
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
unit DeHL.Date.TimeSpan;
interface
uses SysUtils, DateUtils,
     DeHL.Base,
     DeHL.Exceptions,
     DeHL.Serialization,
     DeHL.Types;

type
  { The TimeSpan record }
  TTimeSpan = record
  private
    FTotalMillis : UInt64;

    FDays        : UInt64;
    FHours       : UInt64;
    FMinutes     : UInt64;
    FSeconds     : UInt64;
    FMillis      : UInt64;

    { Private getters }
    function GetTotalDays: Double;
    function GetTotalHours: Double;
    function GetTotalMillis: Double;
    function GetTotalMinutes: Double;
    function GetTotalSeconds: Double;

  public
    { Constructors }
    constructor Create(const ADays, AHours, AMinutes, ASeconds, AMillis : UInt64); overload;
    constructor Create(const AHours, AMinutes, ASeconds, AMillis : UInt64); overload;
    constructor Create(const AMinutes, ASeconds, AMillis : UInt64); overload;
    constructor Create(const ASeconds, AMillis : UInt64); overload;
    constructor Create(const AMillis : UInt64); overload;

    { Static methods - constructors (C++ compatibility) }
    class function FromMilliseconds(const AValue : UInt64) : TTimeSpan; static;
    class function FromSeconds(const AValue : UInt64) : TTimeSpan; static;
    class function FromMinutes(const AValue : UInt64) : TTimeSpan; static;
    class function FromHours(const AValue : UInt64) : TTimeSpan; static;
    class function FromDays(const AValue : UInt64) : TTimeSpan; static;

    { Math }
    function AddMilliseconds(const AValue : Integer) : TTimeSpan;
    function AddSeconds(const AValue : Integer) : TTimeSpan;
    function AddMinutes(const AValue : Integer) : TTimeSpan;
    function AddHours(const AValue : Integer) : TTimeSpan;
    function AddDays(const AValue : Integer) : TTimeSpan;

    { Operator overloading }
    class operator Add(const ATimeSpan1, ATimeSpan2 : TTimeSpan) : TTimeSpan;
    class operator Subtract(const ATimeSpan1, ATimeSpan2 : TTimeSpan) : TTimeSpan;

    class operator Equal(const ATimeSpan1, ATimeSpan2 : TTimeSpan) : Boolean;
    class operator NotEqual(const ATimeSpan1, ATimeSpan2 : TTimeSpan) : Boolean;

    class operator GreaterThan(const ATimeSpan1, ATimeSpan2 : TTimeSpan) : Boolean;
    class operator GreaterThanOrEqual(const ATimeSpan1, ATimeSpan2 : TTimeSpan) : Boolean;

    class operator LessThan(const ATimeSpan1, ATimeSpan2 : TTimeSpan) : Boolean;
    class operator LessThanOrEqual(const ATimeSpan1, ATimeSpan2 : TTimeSpan) : Boolean;

    { Properties }
    property Days : UInt64 read FDays;
    property Hours : UInt64 read FHours;
    property Minutes : UInt64 read FMinutes;
    property Seconds : UInt64 read FSeconds;
    property Milliseconds : UInt64 read FMillis;

    property TotalDays : Double read GetTotalDays;
    property TotalHours : Double read GetTotalHours;
    property TotalMinutes : Double read GetTotalMinutes;
    property TotalSeconds : Double read GetTotalSeconds;
    property TotalMilliseconds : Double read GetTotalMillis;
  end;

 { TimeSpan Support }
  TTimeSpanType = class(TMagicType<TTimeSpan>)
  protected
    { Serialization }
    procedure DoSerialize(const AInfo: TValueInfo; const AValue: TTimeSpan; const AContext: ISerializationContext); override;
    procedure DoDeserialize(const AInfo: TValueInfo; out AValue: TTimeSpan; const AContext: IDeserializationContext); override;

  public
    { Comparator }
    function Compare(const AValue1, AValue2: TTimeSpan): Integer; override;

    { Equality Comparator }
    function AreEqual(const AValue1, AValue2: TTimeSpan): Boolean; override;

    { Hash code provider }
    function GenerateHashCode(const AValue: TTimeSpan): Integer; override;

    { Get String representation }
    function GetString(const AValue: TTimeSpan): String; override;

    { Type information }
    function Family(): TTypeFamily; override;

    { Variant Conversion }
    function TryConvertToVariant(const AValue: TTimeSpan; out ORes: Variant): Boolean; override;
    function TryConvertFromVariant(const AValue: Variant; out ORes: TTimeSpan): Boolean; override;
  end;

implementation
uses
  DeHL.StrConsts;

const
  MillisecondsInASecond = 1000;
  MillisecondsInAMinute = 60 * MillisecondsInASecond;
  MillisecondsInAHour = 60 * MillisecondsInAMinute;
  MillisecondsInADay = 24 * MillisecondsInAHour;


{ TTimeSpan }

class operator TTimeSpan.Add(const ATimeSpan1, ATimeSpan2: TTimeSpan): TTimeSpan;
begin
  { Simply generate a new TimeSpan }
  Result := TTimeSpan.Create(ATimeSpan1.FTotalMillis + ATimeSpan2.FTotalMillis);
end;

function TTimeSpan.AddDays(const AValue: Integer): TTimeSpan;
begin
  { Add a number of days in form of Millis }
  if (AValue < 0) and (FTotalMillis < (UInt64(-AValue * MillisecondsInADay))) then
     ExceptionHelper.Throw_ArgumentOutOfRangeError('AValue');

  Result := TTimeSpan.Create(FTotalMillis + (AValue * MillisecondsInADay));
end;

function TTimeSpan.AddHours(const AValue: Integer): TTimeSpan;
begin
  { Add a number of hours in form of Millis }
  if (AValue < 0) and (FTotalMillis < (UInt64(-AValue * MillisecondsInAHour))) then
     ExceptionHelper.Throw_ArgumentOutOfRangeError('AValue');

  Result := TTimeSpan.Create(FTotalMillis + (AValue * MillisecondsInAHour));
end;

function TTimeSpan.AddMilliseconds(const AValue: Integer): TTimeSpan;
begin
  { Add a number of millis in form of Millis }
  if (AValue < 0) and (FTotalMillis < (UInt64(-AValue))) then
     ExceptionHelper.Throw_ArgumentOutOfRangeError('AValue');

  Result := TTimeSpan.Create(FTotalMillis + AValue);
end;

function TTimeSpan.AddMinutes(const AValue: Integer): TTimeSpan;
begin
  { Add a number of minutes in form of Millis }
  if (AValue < 0) and (FTotalMillis < (UInt64(-AValue * MillisecondsInAMinute))) then
     ExceptionHelper.Throw_ArgumentOutOfRangeError('AValue');

  Result := TTimeSpan.Create(FTotalMillis + (AValue * MillisecondsInAMinute));
end;

function TTimeSpan.AddSeconds(const AValue: Integer): TTimeSpan;
begin
  { Add a number of seconds in form of Millis }
  if (AValue < 0) and (FTotalMillis < (UInt64(-AValue * MillisecondsInASecond))) then
     ExceptionHelper.Throw_ArgumentOutOfRangeError('AValue');

  Result := TTimeSpan.Create(FTotalMillis + (AValue * MillisecondsInASecond));
end;

constructor TTimeSpan.Create(const AHours, AMinutes, ASeconds,
  AMillis: UInt64);
begin
  { Call base constructor }
  Create(0, AHours, AMinutes, ASeconds, AMillis);
end;

constructor TTimeSpan.Create(const AMinutes, ASeconds, AMillis: UInt64);
begin
  { Call base constructor }
  Create(0, 0, AMinutes, ASeconds, AMillis);
end;

constructor TTimeSpan.Create(const ASeconds, AMillis: UInt64);
begin
  { Call base constructor }
  Create(0, 0, 0, ASeconds, AMillis);
end;

constructor TTimeSpan.Create(const ADays, AHours, AMinutes,
  ASeconds, AMillis: UInt64);
var
 Remainder   : UInt64;

begin
  { Generate the TimeSpan by add and multiply }
  FTotalMillis := (ADays * MillisecondsInADay) + (AHours * MillisecondsInAHour) + (AMinutes * MillisecondsInAMinute)
                  + (ASeconds * MillisecondsInASecond) + AMillis;


  { Calculate the separated values }
  FDays := FTotalMillis div MillisecondsInADay;
  Remainder := FTotalMillis mod MillisecondsInADay;

  FHours := Remainder div MillisecondsInAHour;
  Remainder := Remainder mod MillisecondsInAHour;

  FMinutes := Remainder div MillisecondsInAMinute;
  Remainder := Remainder mod MillisecondsInAMinute;

  FSeconds := Remainder div MillisecondsInASecond;
  Remainder := Remainder mod MillisecondsInASecond;

  FMillis := Remainder;
end;

class operator TTimeSpan.Equal(const ATimeSpan1,
  ATimeSpan2: TTimeSpan): Boolean;
begin
  { Test equality using millis }
  Result := ATimeSpan1.FTotalMillis = ATimeSpan2.FTotalMillis;
end;

class function TTimeSpan.FromDays(const AValue: UInt64): TTimeSpan;
begin
  { Simply invoke the constructor }
  Result := TTimeSpan.Create(AValue, 0, 0, 0, 0);
end;

class function TTimeSpan.FromHours(const AValue: UInt64): TTimeSpan;
begin
  { Simply invoke the constructor }
  Result := TTimeSpan.Create(AValue, 0, 0, 0);
end;

class function TTimeSpan.FromMilliseconds(const AValue: UInt64): TTimeSpan;
begin
  { Simply invoke the constructor }
  Result := TTimeSpan.Create(AValue);
end;

class function TTimeSpan.FromMinutes(const AValue: UInt64): TTimeSpan;
begin
  { Simply invoke the constructor }
  Result := TTimeSpan.Create(AValue, 0, 0);
end;

class function TTimeSpan.FromSeconds(const AValue: UInt64): TTimeSpan;
begin
  { Simply invoke the constructor }
  Result := TTimeSpan.Create(AValue, 0);
end;

function TTimeSpan.GetTotalDays: Double;
begin
  { Calculate total }
  Result := FTotalMillis / MillisecondsInADay;
end;

function TTimeSpan.GetTotalHours: Double;
begin
  { Calculate total }
  Result := FTotalMillis / MillisecondsInAHour;
end;

function TTimeSpan.GetTotalMillis: Double;
begin
  { Calculate total }
  Result := FTotalMillis;
end;

function TTimeSpan.GetTotalMinutes: Double;
begin
  { Calculate total }
  Result := FTotalMillis / MillisecondsInAMinute;
end;

function TTimeSpan.GetTotalSeconds: Double;
begin
  { Calculate total }
  Result := FTotalMillis / MillisecondsInASecond;
end;

class operator TTimeSpan.GreaterThan(const ATimeSpan1,
  ATimeSpan2: TTimeSpan): Boolean;
begin
  { Check millis directly }
  Result := ATimeSpan1.FTotalMillis > ATimeSpan2.FTotalMillis;
end;

class operator TTimeSpan.GreaterThanOrEqual(const ATimeSpan1,
  ATimeSpan2: TTimeSpan): Boolean;
begin
  { Check millis directly }
  Result := ATimeSpan1.FTotalMillis >= ATimeSpan2.FTotalMillis;
end;

class operator TTimeSpan.LessThan(const ATimeSpan1,
  ATimeSpan2: TTimeSpan): Boolean;
begin
  { Check millis directly }
  Result := ATimeSpan1.FTotalMillis < ATimeSpan2.FTotalMillis;
end;

class operator TTimeSpan.LessThanOrEqual(const ATimeSpan1,
  ATimeSpan2: TTimeSpan): Boolean;
begin
  { Check millis directly }
  Result := ATimeSpan1.FTotalMillis <= ATimeSpan2.FTotalMillis;
end;

class operator TTimeSpan.NotEqual(const ATimeSpan1,
  ATimeSpan2: TTimeSpan): Boolean;
begin
  { Check millis directly }
  Result := ATimeSpan1.FTotalMillis <> ATimeSpan2.FTotalMillis;
end;

class operator TTimeSpan.Subtract(const ATimeSpan1,
  ATimeSpan2: TTimeSpan): TTimeSpan;
begin
  { Do the subtraction }
  if (ATimeSpan1.FTotalMillis < ATimeSpan2.FTotalMillis) then
     ExceptionHelper.Throw_ArgumentOutOfRangeError('ATimeSpan2');

  Result := TTimeSpan.Create(ATimeSpan1.FTotalMillis - ATimeSpan2.FTotalMillis);
end;

constructor TTimeSpan.Create(const AMillis: UInt64);
begin
  { Call base constructor }
  Create(0, 0, 0, 0, AMillis);
end;

{ TTimeSpanType }
function TTimeSpanType.AreEqual(const AValue1, AValue2: TTimeSpan): Boolean;
begin
  Result := (AValue1.FTotalMillis = AValue2.FTotalMillis);
end;

function TTimeSpanType.Compare(const AValue1, AValue2: TTimeSpan): Integer;
begin
  if AValue1.FTotalMillis < AValue2.FTotalMillis then
    Result := -1
  else if AValue1.FTotalMillis > AValue2.FTotalMillis then
    Result := 1
  else
    Result := 0;
end;

procedure TTimeSpanType.DoDeserialize(const AInfo: TValueInfo; out AValue: TTimeSpan; const AContext: IDeserializationContext);
var
  LMillis: UInt64;
begin
  AContext.GetValue(AInfo, LMillis);
  AValue := TTimeSpan.Create(LMillis);
end;

procedure TTimeSpanType.DoSerialize(const AInfo: TValueInfo; const AValue: TTimeSpan; const AContext: ISerializationContext);
begin
  AContext.AddValue(AInfo, AValue.FTotalMillis);
end;

function TTimeSpanType.Family: TTypeFamily;
begin
  Result := tfUnsignedInteger;
end;

function TTimeSpanType.GenerateHashCode(const AValue: TTimeSpan): Integer;
var
  X : UInt64;
  I : array[0..1] of Integer absolute X;
begin
  X := AValue.FTotalMillis;
  Result := I[0] xor I[1];
end;

function TTimeSpanType.GetString(const AValue: TTimeSpan): String;
begin
  Result := Format(STimeSupportFormat, [AValue.FDays, AValue.FHours, AValue.FMinutes, AValue.FSeconds, AValue.FMillis]);
end;

function TTimeSpanType.TryConvertFromVariant(const AValue: Variant; out ORes: TTimeSpan): Boolean;
var
  U: UInt64;
begin
  { May fail }
  try
    U := AValue;
    ORes := TTimeSpan.Create(U);
  except
    Exit(false);
  end;

  Result := true;
end;

function TTimeSpanType.TryConvertToVariant(const AValue: TTimeSpan; out ORes: Variant): Boolean;
begin
  { Simple assignment }
  ORes := AValue.FTotalMillis;
  Result := true;
end;

initialization
  { Register custom type }
  TType<TTimeSpan>.Register(TTimeSpanType);

finalization
  { Unregister custom type }
  TType<TTimeSpan>.Unregister();

end.
