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
unit DeHL.Date.DateTime;
interface
uses SysUtils, DateUtils, Windows,
     DeHL.Exceptions,
     DeHL.Base,
     DeHL.Types,
     DeHL.Date.Date,
     DeHL.Date.Time,
     DeHL.Serialization,
     DeHL.Date.TimeSpan;

type
  { DateTime record }
  TDateTime = record
  private
    FDateTime : System.TDateTime;

    { Class functions }
    class function GetNow: TDateTime; static;
    class function GetLocTime: TDateTime; static;
    class function GetSysTime: TDateTime; static;

    { functions }
    function GetDate: TDate;
    function GetTime: TTime;
  public
    { Constructors }
    constructor Create(const ADateTime : System.TDateTime); overload;
    constructor Create(const ADateTime : String); overload;
    constructor Create(const ADateTime : String; const FormatSettings : TFormatSettings); overload;
    constructor Create(const AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilli : Word); overload;

    { Parts/properties of the date }
    property Date  : TDate read GetDate;
    property Time  : TTime read GetTime;

    { Maths }
    function AddYears(const AValue : Integer) : TDateTime;
    function AddMonths(const AValue : Integer) : TDateTime;
    function AddDays(const AValue : Integer) : TDateTime;
    function AddMilliseconds(const AValue : Integer) : TDateTime;
    function AddSeconds(const AValue : Integer) : TDateTime;
    function AddMinutes(const AValue : Integer) : TDateTime;
    function AddHours(const AValue : Integer) : TDateTime;

    { Operator overloading }
    class operator Implicit(const ADateTime : System.TDateTime) : TDateTime;
    class operator Implicit(const ADateTime : TDateTime) : System.TDateTime;

    class operator Add(const ADateTime : TDateTime; const ASpan : TTimeSpan) : TDateTime;
    class operator Add(const ASpan : TTimeSpan; const ADateTime : TDateTime) : TDateTime;

    class operator Subtract(const ADateTime : TDateTime; const ASpan : TTimeSpan) : TDateTime;
    class operator Subtract(const ADateTime1, ADateTime2 : TDateTime) : TTimeSpan;

    class operator Equal(const ADateTime1, ADateTime2 : TDateTime) : Boolean;
    class operator NotEqual(const ADateTime1, ADateTime2 : TDateTime) : Boolean;

    class operator GreaterThan(const ADateTime1, ADateTime2 : TDateTime) : Boolean;
    class operator GreaterThanOrEqual(const ADateTime1, ADateTime2 : TDateTime) : Boolean;

    class operator LessThan(const ADateTime1, ADateTime2 : TDateTime) : Boolean;
    class operator LessThanOrEqual(const ADateTime1, ADateTime2 : TDateTime) : Boolean;

    { Conversions }
    function ToString() : String; overload;
    function ToString(const FormatSettings : TFormatSettings) : String; overload;
    function ToString(const Format : String) : String; overload;
    function ToString(const Format : String; const FormatSettings : TFormatSettings) : String; overload;

    { To other time formats }
    function ToUnixTime() : Int64;

    { Static properties/functions }
    class function FromUnixTime(const UnixTime: Int64) : TDateTime; static;

    class property Now : TDateTime read GetNow;
    class property SystemNow : TDateTime read GetSysTime;
    class property LocalNow : TDateTime read GetLocTime;
  end;

 { Time Support }
  TDateTimeType = class(TMagicType<TDateTime>)
  protected
    { Serialization }
    procedure DoSerialize(const AInfo: TValueInfo; const AValue: TDateTime; const AContext: ISerializationContext); override;
    procedure DoDeserialize(const AInfo: TValueInfo; out AValue: TDateTime; const AContext: IDeserializationContext); override;

  public
    { Comparator }
    function Compare(const AValue1, AValue2: TDateTime): Integer; override;

    { Equality Comparator }
    function AreEqual(const AValue1, AValue2: TDateTime): Boolean; override;

    { Hash code provider }
    function GenerateHashCode(const AValue: TDateTime): Integer; override;

    { Get String representation }
    function GetString(const AValue: TDateTime): String; override;

    { Type information }
    function Family(): TTypeFamily; override;

    { Variant Conversion }
    function TryConvertToVariant(const AValue: TDateTime; out ORes: Variant): Boolean; override;
    function TryConvertFromVariant(const AValue: Variant; out ORes: TDateTime): Boolean; override;
  end;

implementation

{ TDateTime }

constructor TDateTime.Create(const ADateTime: String);
begin
  { Convert from string - should throw exception if failed }
  try
    FDateTime := StrToDateTime(ADateTime);
  except
    on Exception do
       ExceptionHelper.Throw_InvalidArgumentFormatError('ADateTime');
  end;
end;

constructor TDateTime.Create(const ADateTime: System.TDateTime);
begin
  { Just initialize internals }
  FDateTime := ADateTime;
end;

constructor TDateTime.Create(const AYear, AMonth, ADay, AHour, AMinute, ASecond,
  AMilli: Word);
begin
  if not IsValidDateTime(AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilli) then
     ExceptionHelper.Throw_ArgumentOutOfRangeError('AYear/AMonth/ADay/AHour/AMinute/ASecond/AMilli');

  { Create form variables. Should throw if error. }
  FDateTime := EncodeDateTime(AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilli);
end;

class operator TDateTime.Add(const ASpan: TTimeSpan;
  const ADateTime: TDateTime): TDateTime;
begin
  { Use supplied milliseconds value }
  Result := TDateTime.Create(IncMilliSecond(ADateTime.FDateTime, Round(ASpan.TotalMilliseconds)));
end;

class operator TDateTime.Add(const ADateTime: TDateTime;
  const ASpan: TTimeSpan): TDateTime;
begin
  { Use supplied milliseconds value }
  Result := TDateTime.Create(IncMilliSecond(ADateTime.FDateTime, Round(ASpan.TotalMilliseconds)));
end;

function TDateTime.AddDays(const AValue: Integer): TDateTime;
begin
  { Create a new date +N }
  try
    Result := TDateTime.Create(IncDay(FDateTime, AValue));
  except
    on Exception do
       ExceptionHelper.Throw_ArgumentOutOfRangeError('AValue');
  end;
end;

function TDateTime.AddHours(const AValue: Integer): TDateTime;
begin
  { Create a new date +N }
  try
    Result := TDateTime.Create(IncHour(FDateTime, AValue));
  except
    on Exception do
       ExceptionHelper.Throw_ArgumentOutOfRangeError('AValue');
  end;
end;

function TDateTime.AddMilliseconds(const AValue: Integer): TDateTime;
begin
  { Create a new date +N }
  try
    Result := TDateTime.Create(IncMilliSecond(FDateTime, AValue));
  except
    on Exception do
       ExceptionHelper.Throw_ArgumentOutOfRangeError('AValue');
  end;
end;

function TDateTime.AddMinutes(const AValue: Integer): TDateTime;
begin
  { Create a new date +N }
  try
    Result := TDateTime.Create(IncMinute(FDateTime, AValue));
  except
    on Exception do
       ExceptionHelper.Throw_ArgumentOutOfRangeError('AValue');
  end;
end;

function TDateTime.AddMonths(const AValue: Integer): TDateTime;
begin
  { Create a new date +N }
  try
    Result := TDateTime.Create(IncMonth(FDateTime, AValue));
  except
    on Exception do
       ExceptionHelper.Throw_ArgumentOutOfRangeError('AValue');
  end;
end;

function TDateTime.AddSeconds(const AValue: Integer): TDateTime;
begin
  { Create a new date +N }
  try
    Result := TDateTime.Create(IncSecond(FDateTime, AValue));
  except
    on Exception do
       ExceptionHelper.Throw_ArgumentOutOfRangeError('AValue');
  end;
end;

function TDateTime.AddYears(const AValue: Integer): TDateTime;
begin
  { Create a new date +N }
  try
    Result := TDateTime.Create(IncYear(FDateTime, AValue));
  except
    on Exception do
       ExceptionHelper.Throw_ArgumentOutOfRangeError('AValue');
  end;
end;

class operator TDateTime.Equal(const ADateTime1,
  ADateTime2: TDateTime): Boolean;
begin
  { Simple check }
  Result := CompareDateTime(ADateTime1.FDateTime, ADateTime2.FDateTime) = 0;
end;

class function TDateTime.FromUnixTime(const UnixTime: Int64): TDateTime;
begin
  { Simply call the sys function }
  Result := TDateTime.Create(UnixToDateTime(UnixTime));
end;

constructor TDateTime.Create(const ADateTime: String;
  const FormatSettings: TFormatSettings);
begin
  { Convert from string - should throw exception if failed }
  try
    FDateTime := StrToDateTime(ADateTime, FormatSettings);
  except
    on Exception do
       ExceptionHelper.Throw_InvalidArgumentFormatError('ADateTime');
  end;
end;

function TDateTime.GetDate: TDate;
begin
  { Pass internal field }
  Result := TDate.Create(FDateTime);
end;

class function TDateTime.GetLocTime: TDateTime;
var
  LTime : TSystemTime;
begin
  { Read the local time/date and convert }
  GetLocalTime(LTime);
  Result := TDateTime.Create(SystemTimeToDateTime(LTime));
end;

class function TDateTime.GetNow: TDateTime;
begin
  { Get current Date }
  Result := TDateTime.Create(SysUtils.Now());
end;

class function TDateTime.GetSysTime: TDateTime;
var
  LTime : TSystemTime;
begin
  { Read the local time/date and convert }
  GetSystemTime(LTime);
  Result := TDateTime.Create(SystemTimeToDateTime(LTime));
end;

function TDateTime.GetTime: TTime;
begin
  { Pass internal field }
  Result := TTime.Create(FDateTime);
end;

class operator TDateTime.GreaterThan(const ADateTime1,
  ADateTime2: TDateTime): Boolean;
begin
  { Simple check }
  Result := CompareDateTime(ADateTime1.FDateTime, ADateTime2.FDateTime) > 0;
end;

class operator TDateTime.GreaterThanOrEqual(const ADateTime1,
  ADateTime2: TDateTime): Boolean;
begin
  { Simple check }
  Result := CompareDateTime(ADateTime1.FDateTime, ADateTime2.FDateTime) >= 0;
end;

class operator TDateTime.Implicit(const ADateTime: TDateTime): System.TDateTime;
begin
  { Pass copy of internal field }
  Result := ADateTime.FDateTime;
end;

class operator TDateTime.Implicit(const ADateTime: System.TDateTime): TDateTime;
begin
  { Simple copy }
  Result := TDateTime.Create(ADateTime);
end;

class operator TDateTime.LessThan(const ADateTime1,
  ADateTime2: TDateTime): Boolean;
begin
  { Simple check }
  Result := CompareDateTime(ADateTime1.FDateTime, ADateTime2.FDateTime) < 0;
end;

class operator TDateTime.LessThanOrEqual(const ADateTime1,
  ADateTime2: TDateTime): Boolean;
begin
  { Simple check }
  Result := CompareDateTime(ADateTime1.FDateTime, ADateTime2.FDateTime) <= 0;
end;

class operator TDateTime.NotEqual(const ADateTime1,
  ADateTime2: TDateTime): Boolean;
begin
  { Simple check }
  Result := CompareDateTime(ADateTime1.FDateTime, ADateTime2.FDateTime) <> 0;
end;

class operator TDateTime.Subtract(const ADateTime: TDateTime;
  const ASpan: TTimeSpan): TDateTime;
begin
  { Use supplied milliseconds value }
  Result := TDateTime.Create(IncMilliSecond(ADateTime.FDateTime, -1 * Round(ASpan.TotalMilliseconds)));
end;

class operator TDateTime.Subtract(const ADateTime1,
  ADateTime2: TDateTime): TTimeSpan;
begin
  Result := TTimeSpan.Create(MilliSecondsBetween(ADateTime1.FDateTime, ADateTime2.FDateTime));
end;

function TDateTime.ToString: String;
begin
  { Use Delphi functions }
  Result := DateTimeToStr(FDateTime);
end;

function TDateTime.ToString(const FormatSettings: TFormatSettings): String;
begin
  { Use Delphi functions }
  Result := DateTimeToStr(FDateTime, FormatSettings);
end;

function TDateTime.ToString(const Format: String): String;
begin
  { Use Delphi functions }
  DateTimeToString(Result, Format, FDateTime);
end;

function TDateTime.ToString(const Format: String;
  const FormatSettings: TFormatSettings): String;
begin
  DateTimeToString(Result, Format, FDateTime, FormatSettings);
end;

function TDateTime.ToUnixTime: Int64;
begin
  { Call the sys function }
  Result := DateTimeToUnix(FDateTime);
end;

{ TDateTimeType }

function TDateTimeType.AreEqual(const AValue1, AValue2: TDateTime): Boolean;
begin
  Result := (CompareDateTime(AValue1.FDateTime, AValue2.FDateTime) = 0);
end;

function TDateTimeType.Compare(const AValue1, AValue2: TDateTime): Integer;
begin
  Result := CompareDateTime(AValue1.FDateTime, AValue2.FDateTime);
end;

procedure TDateTimeType.DoDeserialize(const AInfo: TValueInfo; out AValue: TDateTime; const AContext: IDeserializationContext);
var
  LDT: System.TDateTime;
begin
  AContext.GetValue(AInfo, LDT);
  AValue := TDateTime.Create(LDT);
end;

procedure TDateTimeType.DoSerialize(const AInfo: TValueInfo; const AValue: TDateTime; const AContext: ISerializationContext);
begin
  AContext.AddValue(AInfo, AValue.FDateTime);
end;

function TDateTimeType.Family: TTypeFamily;
begin
  Result := tfDate;
end;

function TDateTimeType.GenerateHashCode(const AValue: TDateTime): Integer;
var
  X: System.TDateTime;
  LongOp : array[0..1] of Integer absolute X;
begin
  X := AValue.FDateTime;

  if X = 0 then
     Result := 0
  else
     Result := LongOp[1] xor LongOp[0];
end;

function TDateTimeType.GetString(const AValue: TDateTime): String;
begin
  Result := DateTimeToStr(AValue.FDateTime);
end;

function TDateTimeType.TryConvertFromVariant(const AValue: Variant; out ORes: TDateTime): Boolean;
begin
  { May fail }
  try
    ORes := TDateTime.Create(System.TDateTime(AValue));
  except
    Exit(false);
  end;

  Result := true;
end;

function TDateTimeType.TryConvertToVariant(const AValue: TDateTime; out ORes: Variant): Boolean;
begin
  { Simple assignment }
  ORes := AValue.FDateTime;
  Result := true;
end;

initialization
  { Register custom type }
  TType<TDateTime>.Register(TDateTimeType);

finalization
  { Unregister custom type }
  TType<TDateTime>.Unregister();

end.
