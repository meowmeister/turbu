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
unit DeHL.Date.Time;
interface
uses SysUtils, DateUtils,
     DeHL.Base,
     DeHL.Types,
     DeHL.Exceptions,
     DeHL.Serialization,
     DeHL.Date.TimeSpan;

type
  { Time record }
  TTime = record
  private
    FDateTime : System.TDateTime;
    FHour     : Word;
    FMinute   : Word;
    FSecond   : Word;
    FMilli    : Word;

    class function GetTime: TTime; static;
    class function GetSysTime: TTime; static;
    class function GetLocTime: TTime; static;

    function GetIsPM: Boolean;
  public
    { Constructors }
    constructor Create(const AHour, AMinute, ASecond, AMilli : Word); overload;
    constructor Create(const ADateTime : System.TDateTime); overload;
    constructor Create(const ATime : String); overload;
    constructor Create(const ATime : String; const FormatSettings : TFormatSettings); overload;

    { Properties }
    property Hour : Word read FHour;
    property Minute : Word read FMinute;
    property Second : Word read FSecond;
    property Millisecond : Word read FMilli;

    property IsPM : Boolean read GetIsPM;

    { Conversion }
    function ToString() : String; overload;
    function ToString(const FormatSettings : TFormatSettings) : String; overload;

    { Maths }
    function AddMilliseconds(const AValue : Integer) : TTime;
    function AddSeconds(const AValue : Integer) : TTime;
    function AddMinutes(const AValue : Integer) : TTime;
    function AddHours(const AValue : Integer) : TTime;

    { Operator overloading }
    class operator Implicit(const ADateTime : System.TDateTime) : TTime;
    class operator Implicit(const ATime : TTime) : System.TDateTime;

    class operator Add(const ATime : TTime; const ASpan : TTimeSpan) : TTime;
    class operator Add(const ASpan : TTimeSpan; const ATime : TTime) : TTime;
    class operator Subtract(const ATime : TTime; const ASpan : TTimeSpan) : TTime;
    class operator Subtract(const ATime1, ATime2 : TTime) : TTimeSpan;

    class operator Equal(const ATime1, ATime2 : TTime) : Boolean;
    class operator NotEqual(const ATime1, ATime2 : TTime) : Boolean;

    class operator GreaterThan(const ATime1, ATime2 : TTime) : Boolean;
    class operator GreaterThanOrEqual(const ATime1, ATime2 : TTime) : Boolean;

    class operator LessThan(const ATime1, ATime2 : TTime) : Boolean;
    class operator LessThanOrEqual(const ATime1, ATime2 : TTime) : Boolean;

    { Static properties }
    class property Now : TTime read GetTime;
    class property SystemNow : TTime read GetSysTime;
    class property LocalNow : TTime read GetLocTime;
  end;

 { Time Support }
  TTimeType = class(TMagicType<TTime>)
  protected
    { Serialization }
    procedure DoSerialize(const AInfo: TValueInfo; const AValue: TTime; const AContext: ISerializationContext); override;
    procedure DoDeserialize(const AInfo: TValueInfo; out AValue: TTime; const AContext: IDeserializationContext); override;

  public
    { Comparator }
    function Compare(const AValue1, AValue2: TTime): Integer; override;

    { Equality Comparator }
    function AreEqual(const AValue1, AValue2: TTime): Boolean; override;

    { Hash code provider }
    function GenerateHashCode(const AValue: TTime): Integer; override;

    { Get String representation }
    function GetString(const AValue: TTime): String; override;

    { Type information }
    function Family(): TTypeFamily; override;

    { Variant Conversion }
    function TryConvertToVariant(const AValue: TTime; out ORes: Variant): Boolean; override;
    function TryConvertFromVariant(const AValue: Variant; out ORes: TTime): Boolean; override;
  end;


implementation
uses Windows;

{ TTime }

constructor TTime.Create(const ADateTime: System.TDateTime);
begin
  { Crete from the given date }
  FDateTime := ADateTime;

  { Decode }
  DecodeTime(ADateTime, FHour, FMinute, FSecond, FMilli);

  if not IsValidTime(FHour, FMinute, FSecond, FMilli) then
     ExceptionHelper.Throw_ArgumentOutOfRangeError('ADateTime');
end;

constructor TTime.Create(const AHour, AMinute, ASecond, AMilli: Word);
begin
  if not IsValidTime(AHour, AMinute, ASecond, AMilli) then
     ExceptionHelper.Throw_ArgumentOutOfRangeError('AHour/AMinute/ASecond/AMilli');

  { Create a valid date-time - will throw exception if invalid}
  FDateTime := EncodeTime(AHour, AMinute, ASecond, AMilli);

  { Copy }
  FHour := AHour;
  FMinute := AMinute;
  FSecond := ASecond;
  FMilli := AMilli;
end;

class operator TTime.Add(const ASpan: TTimeSpan; const ATime: TTime): TTime;
begin
  { Create new Time by performing an Inc }
  Result := TTime.Create(IncMilliSecond(ATime.FDateTime, Round(ASpan.TotalMilliseconds)));
end;

class operator TTime.Add(const ATime: TTime; const ASpan: TTimeSpan): TTime;
begin
  { Create new Time by performing an Inc }
  Result := TTime.Create(IncMilliSecond(ATime.FDateTime, Round(ASpan.TotalMilliseconds)));
end;

function TTime.AddHours(const AValue: Integer): TTime;
begin
  { Simply increase hour }
  try
    Result := TTime.Create(IncHour(FDateTime, AValue));
  except
    on Exception do
       ExceptionHelper.Throw_ArgumentOutOfRangeError('AValue');
  end;
end;

function TTime.AddMilliseconds(const AValue: Integer): TTime;
begin
  { Simply increase milli }
  try
    Result := TTime.Create(IncMilliSecond(FDateTime, AValue));
  except
    on Exception do
       ExceptionHelper.Throw_ArgumentOutOfRangeError('AValue');
  end;
end;

function TTime.AddMinutes(const AValue: Integer): TTime;
begin
  { Simply increase minute }
  try
    Result := TTime.Create(IncMinute(FDateTime, AValue));
  except
    on Exception do
       ExceptionHelper.Throw_ArgumentOutOfRangeError('AValue');
  end;
end;

function TTime.AddSeconds(const AValue: Integer): TTime;
begin
  { Simply increase second }
  try
    Result := TTime.Create(IncSecond(FDateTime, AValue));
  except
    on Exception do
       ExceptionHelper.Throw_ArgumentOutOfRangeError('AValue');
  end;
end;

constructor TTime.Create(const ATime: String);
begin
  { Convert to datetime - will throw if erorr }
  try
    FDateTime := StrToTime(ATime);
  except
    on Exception do
       ExceptionHelper.Throw_InvalidArgumentFormatError('ATime');
  end;

  { Decode }
  DecodeTime(FDateTime, FHour, FMinute, FSecond, FMilli);
end;

class operator TTime.Equal(const ATime1, ATime2: TTime): Boolean;
begin
  { Simple check }
  Result := CompareTime(ATime1.FDateTime, ATime2.FDateTime) = 0;
end;

function TTime.GetIsPM: Boolean;
begin
  { Pass over }
  Result := DateUtils.IsPM(FDateTime);
end;

class function TTime.GetLocTime: TTime;
var
  LTime : TSystemTime;
begin
  { Read the local time/date and convert }
  GetLocalTime(LTime);
  Result := TTime.Create(SystemTimeToDateTime(LTime));
end;

class function TTime.GetSysTime: TTime;
var
  STime : TSystemTime;
begin
  { Read the system time/date and convert }
  GetSystemTime(STime);
  Result := TTime.Create(SystemTimeToDateTime(STime));
end;

class function TTime.GetTime: TTime;
begin
  { Call delphi function }
  Result := SysUtils.Time();
end;

class operator TTime.GreaterThan(const ATime1, ATime2: TTime): Boolean;
begin
  { Simple check }
  Result := CompareTime(ATime1.FDateTime, ATime2.FDateTime) > 0;
end;

class operator TTime.GreaterThanOrEqual(const ATime1, ATime2: TTime): Boolean;
begin
  { Simple check }
  Result := CompareTime(ATime1.FDateTime, ATime2.FDateTime) >= 0;
end;

class operator TTime.Implicit(const ADateTime: System.TDateTime): TTime;
begin
  { Call constructor }
  Result := TTime.Create(ADateTime);
end;

class operator TTime.Implicit(const ATime: TTime): System.TDateTime;
begin
  { Simple Copy }
  Result := ATime.FDateTime;
end;

class operator TTime.LessThan(const ATime1, ATime2: TTime): Boolean;
begin
  { Simple check }
  Result := CompareTime(ATime1.FDateTime, ATime2.FDateTime) < 0;
end;

class operator TTime.LessThanOrEqual(const ATime1, ATime2: TTime): Boolean;
begin
  { Simple check }
  Result := CompareTime(ATime1.FDateTime, ATime2.FDateTime) <= 0;
end;

class operator TTime.NotEqual(const ATime1, ATime2: TTime): Boolean;
begin
  { Simple check }
  Result := CompareTime(ATime1.FDateTime, ATime2.FDateTime) <> 0;
end;

class operator TTime.Subtract(const ATime1, ATime2: TTime): TTimeSpan;
begin
  { Create diff }
  Result := TTimeSpan.Create(MilliSecondsBetween(ATime1.FDateTime, ATime2.FDateTime));
end;

class operator TTime.Subtract(const ATime: TTime;
  const ASpan: TTimeSpan): TTime;
begin
  { Use Inc with negatve msecs as base }
  Result := TTime.Create(IncMilliSecond(ATime.FDateTime, -1 * Round(ASpan.TotalMilliseconds)));
end;

function TTime.ToString: String;
begin
  { Call delphi function }
  Result := TimeToStr(FDateTime);
end;

function TTime.ToString(const FormatSettings: TFormatSettings): String;
begin
  { Call delphi function }
  Result := TimeToStr(FDateTime, FormatSettings);
end;

constructor TTime.Create(const ATime: String;
  const FormatSettings: TFormatSettings);
begin
  { Convert to datetime - will throw if erorr }
  try
    FDateTime := StrToTime(ATime, FormatSettings);
  except
    on Exception do
       ExceptionHelper.Throw_InvalidArgumentFormatError('ATime');
  end;

  { Decode }
  DecodeTime(FDateTime, FHour, FMinute, FSecond, FMilli);
end;

{ TTimeType }

function TTimeType.AreEqual(const AValue1, AValue2: TTime): Boolean;
begin
  Result := (CompareTime(AValue1.FDateTime, AValue2.FDateTime) = 0);
end;

function TTimeType.Compare(const AValue1, AValue2: TTime): Integer;
begin
  Result := CompareTime(AValue1.FDateTime, AValue2.FDateTime);
end;

procedure TTimeType.DoDeserialize(const AInfo: TValueInfo; out AValue: TTime; const AContext: IDeserializationContext);
var
  LDT: System.TDateTime;
begin
  AContext.GetValue(AInfo, LDT);
  AValue := TTime.Create(LDT);
end;

procedure TTimeType.DoSerialize(const AInfo: TValueInfo; const AValue: TTime; const AContext: ISerializationContext);
begin
  AContext.AddValue(AInfo, AValue.FDateTime);
end;

function TTimeType.Family: TTypeFamily;
begin
  Result := tfDate;
end;

function TTimeType.GenerateHashCode(const AValue: TTime): Integer;
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

function TTimeType.GetString(const AValue: TTime): String;
begin
  Result := TimeToStr(AValue.FDateTime);
end;

function TTimeType.TryConvertFromVariant(const AValue: Variant; out ORes: TTime): Boolean;
begin
  { May fail }
  try
    ORes := TTime.Create(System.TDateTime(AValue));
  except
    Exit(false);
  end;

  Result := true;
end;

function TTimeType.TryConvertToVariant(const AValue: TTime; out ORes: Variant): Boolean;
begin
  { Simple assignment }
  ORes := AValue.FDateTime;
  Result := true;
end;

initialization
  { Register custom type }
  TType<TTime>.Register(TTimeType);

finalization
  { Unregister custom type }
  TType<TTime>.Unregister();

end.
