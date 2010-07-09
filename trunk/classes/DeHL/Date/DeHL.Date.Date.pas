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
unit DeHL.Date.Date;
interface
uses SysUtils,
     DateUtils,
     DeHL.Base,
     DeHL.Types,
     DeHL.Exceptions,
     DeHL.Serialization,
     DeHL.Date.TimeSpan;

type
  { Days in the week }
  TDayOfTheWeek =
  (
    dowSunday,
    dowMonday,
    dowTuesday,
    dowWednesday,
    dowThursday,
    dowFriday,
    dowSaturday
  );

  { Date record }
  TDate = record
  private
    FDateTime : System.TDateTime;
    FYear     : Word;
    FMonth    : Word;
    FDay      : Word;
    FDOW      : TDayOfTheWeek;

    class function GetDate: TDate; static;
    class function GetLocDate: TDate; static;
    class function GetSysDate: TDate; static;

    function GetIsLeapYear: Boolean;
    function GetIsToday: Boolean;

    function ReadDOW(const ADateTime: System.TDateTime): TDayOfTheWeek;

  public
    { Constructors }
    constructor Create(const AYear, AMonth, ADay : Word); overload;
    constructor Create(const ADateTime : System.TDateTime); overload;
    constructor Create(const ADate : String); overload;
    constructor Create(const ADate : String; const FormatSettings : TFormatSettings); overload;

    { Properties }
    property Year         : Word read FYear;
    property Month        : Word read FMonth;
    property Day          : Word read FDay;
    property DayOfTheWeek : TDayOfTheWeek read FDOW;

    property IsLeapYear : Boolean read GetIsLeapYear;
    property IsToday : Boolean read GetIsToday;

    { Maths }
    function AddDays(const AValue : Integer) : TDate;
    function AddMonths(const AValue : Integer) : TDate;
    function AddYears(const AValue : Integer) : TDate;

   { Operator overloading }
    class operator Implicit(const ADateTime : System.TDateTime) : TDate;
    class operator Implicit(const ADate : TDate) : System.TDateTime;

    class operator Add(const ADate : TDate; const ASpan : TTimeSpan) : TDate;
    class operator Add(const ASpan : TTimeSpan; const ADate : TDate) : TDate;
    class operator Subtract(const ADate : TDate; const ASpan : TTimeSpan) : TDate;
    class operator Subtract(const ADate1, ADate2 : TDate) : TTimeSpan;

    class operator Equal(const ADate1, ADate2 : TDate) : Boolean;
    class operator NotEqual(const ADate1, ADate2 : TDate) : Boolean;

    class operator GreaterThan(const ADate1, ADate2 : TDate) : Boolean;
    class operator GreaterThanOrEqual(const ADate1, ADate2 : TDate) : Boolean;

    class operator LessThan(const ADate1, ADate2 : TDate) : Boolean;
    class operator LessThanOrEqual(const ADate1, ADate2 : TDate) : Boolean;

    { Conversion }
    function ToString() : String; overload;
    function ToString(const FormatSettings : TFormatSettings) : String; overload;

    { Static properties }
    class property Now : TDate read GetDate;
    class property SystemNow : TDate read GetSysDate;
    class property LocalNow : TDate read GetLocDate;
  end;

  { Date Support }
  TDateType = class(TMagicType<TDate>)
  protected
    { Serialization }
    procedure DoSerialize(const AInfo: TValueInfo; const AValue: TDate; const AContext: ISerializationContext); override;
    procedure DoDeserialize(const AInfo: TValueInfo; out AValue: TDate; const AContext: IDeserializationContext); override;

  public
    { Comparator }
    function Compare(const AValue1, AValue2: TDate): Integer; override;

    { Equality Comparator }
    function AreEqual(const AValue1, AValue2: TDate): Boolean; override;

    { Hash code provider }
    function GenerateHashCode(const AValue: TDate): Integer; override;

    { Get String representation }
    function GetString(const AValue: TDate): String; override;

    { Type information }
    function Family(): TTypeFamily; override;

    { Variant Conversion }
    function TryConvertToVariant(const AValue: TDate; out ORes: Variant): Boolean; override;
    function TryConvertFromVariant(const AValue: Variant; out ORes: TDate): Boolean; override;
  end;

implementation
uses Windows;

{ TDate }

constructor TDate.Create(const AYear, AMonth, ADay: Word);
begin
  if not IsValidDate(AYear, AMonth, ADay) then
     ExceptionHelper.Throw_ArgumentOutOfRangeError('AYear/AMonth/ADay is wrong!');

  FYear := AYear;
  FMonth := AMonth;
  FDay := ADay;

  { Try to create a valid date - should throw an exception if it fails }
  FDateTime := EncodeDate(AYear, AMonth, ADay);

  { Get the DOW value }
  FDOW := ReadDOW(FDateTime);
end;

constructor TDate.Create(const ADateTime: System.TDateTime);
begin
  { Decode into fields - should throw exception if failed }
  DecodeDate(ADateTime, FYear, FMonth, FDay);

  if not IsValidDate(FYear, FMonth, FDay) then
     ExceptionHelper.Throw_InvalidArgumentFormatError('ADateTime');

  { Get the DOW value }
  FDOW := ReadDOW(ADateTime);

  FDateTime := ADateTime;
end;

constructor TDate.Create(const ADate: String);
begin
  { Convert from string - should throw exception if failed }
  try
    FDateTime := StrToDate(ADate);
  except
    on Exception do
       ExceptionHelper.Throw_InvalidArgumentFormatError('ADate');
  end;

  DecodeDate(FDateTime, FYear, FMonth, FDay);

  if not IsValidDate(FYear, FMonth, FDay) then
     ExceptionHelper.Throw_InvalidArgumentFormatError('ADate');

  { Get the DOW value }
  FDOW := ReadDOW(FDateTime);
end;

constructor TDate.Create(const ADate: String;
  const FormatSettings: TFormatSettings);
begin
  { Convert from string - should throw exception if failed }
  try
    FDateTime := StrToDate(ADate, FormatSettings);
  except
    on Exception do
       ExceptionHelper.Throw_InvalidArgumentFormatError('ADate');
  end;

  DecodeDate(FDateTime, FYear, FMonth, FDay);

  if not IsValidDate(FYear, FMonth, FDay) then
     ExceptionHelper.Throw_InvalidArgumentFormatError('ADate');

  { Get the DOW value }
  FDOW := ReadDOW(FDateTime);
end;

class function TDate.GetDate : TDate;
begin
  { Forward to system function }
  Result := TDate.Create(SysUtils.Date());
end;

function TDate.GetIsLeapYear: Boolean;
begin
  { Pass over }
  Result := DateUtils.IsInLeapYear(FDateTime);
end;

function TDate.GetIsToday: Boolean;
begin
  { Pass over }
  Result := DateUtils.IsToday(FDateTime);
end;

class function TDate.GetLocDate: TDate;
var
  LTime : TSystemTime;
begin
  { Read the local time/date and convert }
  GetLocalTime(LTime);
  Result := TDate.Create(SystemTimeToDateTime(LTime));
end;

class function TDate.GetSysDate: TDate;
var
  LTime : TSystemTime;
begin
  { Read the system time/date and convert }
  GetSystemTime(LTime);
  Result := TDate.Create(SystemTimeToDateTime(LTime));
end;

class operator TDate.GreaterThan(const ADate1, ADate2 : TDate): Boolean;
begin
  { Simple comparison }
  Result := CompareDate(ADate1.FDateTime, ADate2.FDateTime) > 0;
end;

class operator TDate.GreaterThanOrEqual(const ADate1, ADate2: TDate): Boolean;
begin
  { Simple comparison }
  Result := CompareDate(ADate1.FDateTime, ADate2.FDateTime) >= 0;
end;

class operator TDate.Implicit(const ADate: TDate): System.TDateTime;
begin
  { Generate the System.TDateTime equivalent }
  Result := ADate.FDateTime;
end;

class operator TDate.Implicit(const ADateTime: System.TDateTime): TDate;
begin
  { Use the constructor }
  Result := TDate.Create(ADateTime);
end;

class operator TDate.LessThan(const ADate1, ADate2: TDate): Boolean;
begin
  { Simple comparison }
  Result := CompareDate(ADate1.FDateTime, ADate2.FDateTime) < 0;
end;

class operator TDate.LessThanOrEqual(const ADate1, ADate2: TDate): Boolean;
begin
  { Simple comparison }
  Result := CompareDate(ADate1.FDateTime, ADate2.FDateTime) <= 0;
end;

class operator TDate.NotEqual(const ADate1, ADate2: TDate): Boolean;
begin
  { Simple comparison }
  Result := CompareDate(ADate1.FDateTime, ADate2.FDateTime) <> 0;
end;

function TDate.ReadDOW(const ADateTime: System.TDateTime): TDayOfTheWeek;
const
  Map : array[1..7] of TDayOfTheWeek =
  (
    dowSunday,
    dowMonday,
    dowTuesday,
    dowWednesday,
    dowThursday,
    dowFriday,
    dowSaturday
  );
var
  Idx: Word;
begin
  Idx := DayOfWeek(ADateTime);
  ASSERT((Idx > 0) and (Idx < 8));
  Result := Map[Idx];
end;

class operator TDate.Equal(const ADate1, ADate2: TDate): Boolean;
begin
  { Simple comparison }
  Result := CompareDate(ADate1.FDateTime, ADate2.FDateTime) = 0;
end;

class operator TDate.Subtract(const ADate: TDate;
  const ASpan: TTimeSpan): TDate;
begin
  { Use supplied milliseconds value }
  try
    Result := TDate.Create(IncMilliSecond(ADate.FDateTime, -1 * Round(ASpan.TotalMilliseconds)));
  except
    on Exception do
       ExceptionHelper.Throw_ArgumentOutOfRangeError('ASpan');
  end;
end;

class operator TDate.Subtract(const ADate1, ADate2 : TDate): TTimeSpan;
begin
  if ADate1 < ADate2 then
     ExceptionHelper.Throw_ArgumentOutOfRangeError('ADate2');

  { Use supplied milliseconds value }
  Result := TTimeSpan.Create(MilliSecondsBetween(ADate1.FDateTime, ADate2.FDateTime));
end;

function TDate.ToString(const FormatSettings: TFormatSettings): String;
begin
  { Use system function to generate a string }
  Result := DateToStr(FDateTime, FormatSettings);
end;

function TDate.ToString: String;
begin
  { Use system function to generate a string }
  Result := DateToStr(FDateTime);
end;

class operator TDate.Add(const ASpan: TTimeSpan; const ADate: TDate): TDate;
begin
  { Use supplied milliseconds value }
  Result := TDate.Create(IncMilliSecond(ADate.FDateTime, Round(ASpan.TotalMilliseconds)));
end;

class operator TDate.Add(const ADate: TDate; const ASpan: TTimeSpan): TDate;
begin
  { Use supplied milliseconds value }
  Result := TDate.Create(IncMilliSecond(ADate.FDateTime, Round(ASpan.TotalMilliseconds)));
end;

function TDate.AddDays(const AValue: Integer): TDate;
begin
  { Create a new date +N }
  try
    Result := TDate.Create(IncDay(FDateTime, AValue));
  except
    on Exception do
       ExceptionHelper.Throw_ArgumentOutOfRangeError('AValue');
  end;
end;

function TDate.AddMonths(const AValue: Integer): TDate;
begin
  { Create a new date +N }
  try
    Result := TDate.Create(IncMonth(FDateTime, AValue));
  except
    on Exception do
       ExceptionHelper.Throw_ArgumentOutOfRangeError('AValue');
  end;
end;

function TDate.AddYears(const AValue: Integer): TDate;
begin
  { Create a new date +N }
  try
    Result := TDate.Create(IncYear(FDateTime, AValue));
  except
    on Exception do
       ExceptionHelper.Throw_ArgumentOutOfRangeError('AValue');
  end;
end;

{ TDateType }

function TDateType.AreEqual(const AValue1, AValue2: TDate): Boolean;
begin
  Result := (CompareDate(AValue1.FDateTime, AValue2.FDateTime) = 0);
end;

function TDateType.Compare(const AValue1, AValue2: TDate): Integer;
begin
  Result := CompareDate(AValue1.FDateTime, AValue2.FDateTime);
end;

procedure TDateType.DoDeserialize(const AInfo: TValueInfo; out AValue: TDate; const AContext: IDeserializationContext);
var
  LDT: System.TDateTime;
begin
  AContext.GetValue(AInfo, LDT);
  AValue := TDate.Create(LDT);
end;

procedure TDateType.DoSerialize(const AInfo: TValueInfo; const AValue: TDate; const AContext: ISerializationContext);
begin
  AContext.AddValue(AInfo, AValue.FDateTime);
end;

function TDateType.Family: TTypeFamily;
begin
  Result := tfDate;
end;

function TDateType.GenerateHashCode(const AValue: TDate): Integer;
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

function TDateType.GetString(const AValue: TDate): String;
begin
  Result := DateToStr(AValue.FDateTime);
end;

function TDateType.TryConvertFromVariant(const AValue: Variant; out ORes: TDate): Boolean;
begin
  { May fail }
  try
    ORes := TDate.Create(System.TDateTime(AValue));
  except
    Exit(false);
  end;

  Result := true;
end;

function TDateType.TryConvertToVariant(const AValue: TDate; out ORes: Variant): Boolean;
begin
  { Simple assignment }
  ORes := AValue.FDateTime;
  Result := true;
end;

initialization
  { Register custom type }
  TType<TDate>.Register(TDateType);

finalization
  { Unregister custom type }
  TType<TDate>.Unregister();

end.
