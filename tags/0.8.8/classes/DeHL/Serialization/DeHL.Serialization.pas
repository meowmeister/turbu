(*
* Copyright (c) 2010, Ciobanu Alexandru
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
unit DeHL.Serialization;
interface
uses
  DeHL.Base,
  DeHL.Exceptions,
  TypInfo,
  SysUtils,
  Rtti;

type
  TValueInfo = record
  private
    FObject: TRttiNamedObject;
    FLabel: String;

  public
    { constructors }
    constructor Create(const AType: TRttiType); overload;
    constructor Create(const AField: TRttiField); overload;
    constructor Create(const ALabel: string); overload;

    class function Indexed(): TValueInfo; static; inline;

    { Properties }
    property &Object: TRttiNamedObject read FObject;
    property Name: string read FLabel;
  end;

  { Base serialization stuff }
  IContext = interface
    { -- Control functions -- }
    function GetTypeInformation(const ATypeInfo: PTypeInfo): TRttiType;
    function GetTypeObject(const ATypeInfo: PTypeInfo; const ADelegate: TFunc<TObject>): TObject;
  end;

  { Serialization context }
  ISerializationContext = interface(IContext)
    procedure AddValue(const AInfo: TValueInfo; const AValue: Byte); overload;
    procedure AddValue(const AInfo: TValueInfo; const AValue: ShortInt); overload;
    procedure AddValue(const AInfo: TValueInfo; const AValue: Word); overload;
    procedure AddValue(const AInfo: TValueInfo; const AValue: SmallInt); overload;
    procedure AddValue(const AInfo: TValueInfo; const AValue: Cardinal); overload;
    procedure AddValue(const AInfo: TValueInfo; const AValue: Integer); overload;
    procedure AddValue(const AInfo: TValueInfo; const AValue: UInt64); overload;
    procedure AddValue(const AInfo: TValueInfo; const AValue: Int64); overload;
    procedure AddValue(const AInfo: TValueInfo; const AValue: Single); overload;
    procedure AddValue(const AInfo: TValueInfo; const AValue: Double); overload;
    procedure AddValue(const AInfo: TValueInfo; const AValue: Extended); overload;
    procedure AddValue(const AInfo: TValueInfo; const AValue: Currency); overload;
    procedure AddValue(const AInfo: TValueInfo; const AValue: Comp); overload;
    procedure AddValue(const AInfo: TValueInfo; const AValue: AnsiChar); overload;
    procedure AddValue(const AInfo: TValueInfo; const AValue: WideChar); overload;
    procedure AddValue(const AInfo: TValueInfo; const AValue: AnsiString); overload;
    procedure AddValue(const AInfo: TValueInfo; const AValue: UnicodeString); overload;
    procedure AddValue(const AInfo: TValueInfo; const AValue: Boolean); overload;
    procedure AddValue(const AInfo: TValueInfo; const AValue: TDateTime); overload;
    procedure AddBinaryValue(const AInfo: TValueInfo; const AValue; const ASize: Cardinal); overload;

    { Composites }
    procedure StartRecordType(const AInfo: TValueInfo); overload;
    function StartRecordType(const AInfo: TValueInfo; const AReference: Pointer): Boolean; overload;
    function StartClassType(const AInfo: TValueInfo; const AClass: TClass; const AReference: TObject): Boolean; overload;

    procedure StartArrayType(const AInfo, AElementInfo: TValueInfo; const AElementCount: Cardinal); overload;
    function StartArrayType(const AInfo, AElementInfo: TValueInfo; const AElementCount: Cardinal; const AReference: Pointer): Boolean; overload;

    procedure EndComplexType();
  end;

  { Method used to call back into the calling code to obtain a memory block. }
  TGetBinaryMethod = reference to function(const ASize: Cardinal): Pointer;

  { Deserialization context }
  IDeserializationContext = interface(IContext)
    procedure GetValue(const AInfo: TValueInfo; out AValue: Byte); overload;
    procedure GetValue(const AInfo: TValueInfo; out AValue: ShortInt); overload;
    procedure GetValue(const AInfo: TValueInfo; out AValue: Word); overload;
    procedure GetValue(const AInfo: TValueInfo; out AValue: SmallInt); overload;
    procedure GetValue(const AInfo: TValueInfo; out AValue: Cardinal); overload;
    procedure GetValue(const AInfo: TValueInfo; out AValue: Integer); overload;
    procedure GetValue(const AInfo: TValueInfo; out AValue: UInt64); overload;
    procedure GetValue(const AInfo: TValueInfo; out AValue: Int64); overload;
    procedure GetValue(const AInfo: TValueInfo; out AValue: Single); overload;
    procedure GetValue(const AInfo: TValueInfo; out AValue: Double); overload;
    procedure GetValue(const AInfo: TValueInfo; out AValue: Extended); overload;
    procedure GetValue(const AInfo: TValueInfo; out AValue: Currency); overload;
    procedure GetValue(const AInfo: TValueInfo; out AValue: Comp); overload;
    procedure GetValue(const AInfo: TValueInfo; out AValue: AnsiChar); overload;
    procedure GetValue(const AInfo: TValueInfo; out AValue: WideChar); overload;
    procedure GetValue(const AInfo: TValueInfo; out AValue: AnsiString); overload;
    procedure GetValue(const AInfo: TValueInfo; out AValue: UnicodeString); overload;
    procedure GetValue(const AInfo: TValueInfo; out AValue: Boolean); overload;
    procedure GetValue(const AInfo: TValueInfo; out AValue: TDateTime); overload;
    procedure GetBinaryValue(const AInfo: TValueInfo; const ASupplier: TGetBinaryMethod); overload;

    { Composites }
    procedure ExpectRecordType(const AInfo: TValueInfo); overload;
    function ExpectRecordType(const AInfo: TValueInfo; out AReference: Pointer): Boolean; overload;
    function ExpectClassType(const AInfo: TValueInfo; var AClass: TClass; out AReference: TObject): Boolean; overload;

    procedure ExpectArrayType(const AInfo, AElementInfo: TValueInfo; out OArrayLength: Cardinal); overload;
    function ExpectArrayType(const AInfo, AElementInfo: TValueInfo; out OArrayLength: Cardinal; out AReference: Pointer): Boolean; overload;

    procedure RegisterReference(const AReference: Pointer); overload;
    procedure EndComplexType(); overload;
  end;

  { Serialization data (wrapper for ease of use) }
  TSerializationData = record
  private
    FInContext: ISerializationContext;
    FMicroStack: TArray<TValueInfo>;
    FStackPtr: Cardinal;
    FElementInfo: TValueInfo;

    procedure MicroPush(const AInfo: TValueInfo);
     function MicroPop(): TValueInfo;
  public
    { The constructor }
    constructor Create(const AContext: ISerializationContext);

    { Adds values to the context }
    procedure AddValue(const ALabel: String; const AValue: Byte); overload;
    procedure AddValue(const ALabel: String; const AValue: ShortInt); overload;
    procedure AddValue(const ALabel: String; const AValue: Word); overload;
    procedure AddValue(const ALabel: String; const AValue: SmallInt); overload;
    procedure AddValue(const ALabel: String; const AValue: Cardinal); overload;
    procedure AddValue(const ALabel: String; const AValue: Integer); overload;
    procedure AddValue(const ALabel: String; const AValue: UInt64); overload;
    procedure AddValue(const ALabel: String; const AValue: Int64); overload;
    procedure AddValue(const ALabel: String; const AValue: Single); overload;
    procedure AddValue(const ALabel: String; const AValue: Double); overload;
    procedure AddValue(const ALabel: String; const AValue: Extended); overload;
    procedure AddValue(const ALabel: String; const AValue: Currency); overload;
    procedure AddValue(const ALabel: String; const AValue: Comp); overload;
    procedure AddValue(const ALabel: String; const AValue: AnsiChar); overload;
    procedure AddValue(const ALabel: String; const AValue: WideChar); overload;
    procedure AddValue(const ALabel: String; const AValue: AnsiString); overload;
    procedure AddValue(const ALabel: String; const AValue: UnicodeString); overload;
    procedure AddValue(const ALabel: String; const AValue: Boolean); overload;
    procedure AddValue(const ALabel: String; const AValue: TDateTime); overload;
    procedure AddValue(const ALabel: String; const AValue; const ASize: Cardinal); overload;
    procedure AddValue<T>(const ALabel: String; const AValue: T); overload;

    { Adds array elements to the context }
    procedure AddElement(const AValue: Byte); overload;
    procedure AddElement(const AValue: ShortInt); overload;
    procedure AddElement(const AValue: Word); overload;
    procedure AddElement(const AValue: SmallInt); overload;
    procedure AddElement(const AValue: Cardinal); overload;
    procedure AddElement(const AValue: Integer); overload;
    procedure AddElement(const AValue: UInt64); overload;
    procedure AddElement(const AValue: Int64); overload;
    procedure AddElement(const AValue: Single); overload;
    procedure AddElement(const AValue: Double); overload;
    procedure AddElement(const AValue: Extended); overload;
    procedure AddElement(const AValue: Currency); overload;
    procedure AddElement(const AValue: Comp); overload;
    procedure AddElement(const AValue: AnsiChar); overload;
    procedure AddElement(const AValue: WideChar); overload;
    procedure AddElement(const AValue: AnsiString); overload;
    procedure AddElement(const AValue: UnicodeString); overload;
    procedure AddElement(const AValue: Boolean); overload;
    procedure AddElement(const AValue: TDateTime); overload;
    procedure AddElement(const AValue; const ASize: Cardinal); overload;
    procedure AddElement<T>(const AIndex: Cardinal; const AValue: T); overload;

    { Blocks }
    procedure StartBlock(const ALabel: String); overload;
    procedure StartBlock(); overload;

    procedure StartListBlock(const ALabel, AElementLabel: String; const AElementCount: Cardinal); overload;
    procedure StartListBlock(const AElementLabel: String; const AElementCount: Cardinal); overload;

    procedure EndBlock();

    { The enclosed context }
    property Context: ISerializationContext read FInContext;
    property CurrentElementInfo: TValueInfo read FElementInfo;
  end;

  { Serialization data (wrapper for ease of use) }
  TDeserializationData = record
  private
    FOutContext: IDeserializationContext;
    FMicroStack: TArray<TValueInfo>;
    FStackPtr: Cardinal;
    FElementInfo: TValueInfo;

    procedure MicroPush(const AInfo: TValueInfo);
     function MicroPop(): TValueInfo;
  public
    { The constructor }
    constructor Create(const AContext: IDeserializationContext);

    { Reads values from the context }
    procedure GetValue(const ALabel: String; out AValue: Byte); overload;
    procedure GetValue(const ALabel: String; out AValue: ShortInt); overload;
    procedure GetValue(const ALabel: String; out AValue: Word); overload;
    procedure GetValue(const ALabel: String; out AValue: SmallInt); overload;
    procedure GetValue(const ALabel: String; out AValue: Cardinal); overload;
    procedure GetValue(const ALabel: String; out AValue: Integer); overload;
    procedure GetValue(const ALabel: String; out AValue: UInt64); overload;
    procedure GetValue(const ALabel: String; out AValue: Int64); overload;
    procedure GetValue(const ALabel: String; out AValue: Single); overload;
    procedure GetValue(const ALabel: String; out AValue: Double); overload;
    procedure GetValue(const ALabel: String; out AValue: Extended); overload;
    procedure GetValue(const ALabel: String; out AValue: Currency); overload;
    procedure GetValue(const ALabel: String; out AValue: Comp); overload;
    procedure GetValue(const ALabel: String; out AValue: AnsiChar); overload;
    procedure GetValue(const ALabel: String; out AValue: WideChar); overload;
    procedure GetValue(const ALabel: String; out AValue: AnsiString); overload;
    procedure GetValue(const ALabel: String; out AValue: UnicodeString); overload;
    procedure GetValue(const ALabel: String; out AValue: Boolean); overload;
    procedure GetValue(const ALabel: String; out AValue: TDateTime); overload;
    procedure GetValue(const ALabel: String; out AValue; const ASize: Cardinal); overload;
    procedure GetValue<T>(const ALabel: String; out AValue: T); overload;

    { Reads elements from the context }
    procedure GetElement(out AValue: Byte); overload;
    procedure GetElement(out AValue: ShortInt); overload;
    procedure GetElement(out AValue: Word); overload;
    procedure GetElement(out AValue: SmallInt); overload;
    procedure GetElement(out AValue: Cardinal); overload;
    procedure GetElement(out AValue: Integer); overload;
    procedure GetElement(out AValue: UInt64); overload;
    procedure GetElement(out AValue: Int64); overload;
    procedure GetElement(out AValue: Single); overload;
    procedure GetElement(out AValue: Double); overload;
    procedure GetElement(out AValue: Extended); overload;
    procedure GetElement(out AValue: Currency); overload;
    procedure GetElement(out AValue: Comp); overload;
    procedure GetElement(out AValue: AnsiChar); overload;
    procedure GetElement(out AValue: WideChar); overload;
    procedure GetElement(out AValue: AnsiString); overload;
    procedure GetElement(out AValue: UnicodeString); overload;
    procedure GetElement(out AValue: Boolean); overload;
    procedure GetElement(out AValue: TDateTime); overload;
    procedure GetElement(out AValue; const ASize: Cardinal); overload;
    procedure GetElement<T>(out AValue: T); overload;

    { Blocks }
    procedure ExpectBlock(const ALabel: String); overload;
    procedure ExpectBlock(); overload;

    function ExpectListBlock(const ALabel, AElementLabel: String): Cardinal; overload;
    function ExpectListBlock(const AElementLabel: String): Cardinal; overload;

    procedure EndBlock();

    { The enclosed context }
    property Context: IDeserializationContext read FOutContext;
    property CurrentElementInfo: TValueInfo read FElementInfo;
  end;

  { Serialization interface for classes }
  ISerializable = interface
    ['{0570DB9B-F9C8-430F-B635-3757CCAF3C47}']
    procedure Serialize(const AData: TSerializationData);
    procedure Deserialize(const AData: TDeserializationData);
  end;

  { Deserilization callback interface for classes }
  IDeserializationCallback = interface
    ['{7BB78B2C-CFCC-4000-9321-3E15F08FCDC5}']
    procedure Deserialized(const AData: TDeserializationData);
  end;

type
  { Special class for "no-serialization" mark }
  NonSerialized = class sealed(TCustomAttribute)
  end;

implementation
uses DeHL.Types;

{ TValueInfo }

constructor TValueInfo.Create(const AType: TRttiType);
begin
  if AType = nil then
    ExceptionHelper.Throw_ArgumentNilError('AType');

  FLabel  := AType.Name;
  FObject := AType;
end;

constructor TValueInfo.Create(const AField: TRttiField);
begin
  if AField = nil then
    ExceptionHelper.Throw_ArgumentNilError('AField');

  FLabel  := AField.Name;
  FObject := AField;
end;

constructor TValueInfo.Create(const ALabel: string);
begin
  if ALabel = '' then
    ExceptionHelper.Throw_ArgumentNilError('ALabel');

  FLabel  := ALabel;
  FObject := nil;
end;

class function TValueInfo.Indexed: TValueInfo;
begin
  Result.FObject := nil;
  Result.FLabel  := '';
end;

{ TSerializationData }


procedure TSerializationData.AddValue(const ALabel: String; const AValue: Int64);
begin
  FInContext.AddValue(TValueInfo.Create(ALabel), AValue);
end;

procedure TSerializationData.AddValue(const ALabel: String; const AValue: UInt64);
begin
  FInContext.AddValue(TValueInfo.Create(ALabel), AValue);
end;

procedure TSerializationData.AddValue(const ALabel: String; const AValue: Single);
begin
  FInContext.AddValue(TValueInfo.Create(ALabel), AValue);
end;

procedure TSerializationData.AddValue(const ALabel: String; const AValue: Extended);
begin
  FInContext.AddValue(TValueInfo.Create(ALabel), AValue);
end;

procedure TSerializationData.AddValue(const ALabel: String; const AValue: Double);
begin
  FInContext.AddValue(TValueInfo.Create(ALabel), AValue);
end;

procedure TSerializationData.AddValue(const ALabel: String; const AValue: Integer);
begin
  FInContext.AddValue(TValueInfo.Create(ALabel), AValue);
end;

procedure TSerializationData.AddValue(const ALabel: String; const AValue: ShortInt);
begin
  FInContext.AddValue(TValueInfo.Create(ALabel), AValue);
end;

procedure TSerializationData.AddValue(const ALabel: String; const AValue: Byte);
begin
  FInContext.AddValue(TValueInfo.Create(ALabel), AValue);
end;

procedure TSerializationData.AddValue(const ALabel: String; const AValue: Word);
begin
  FInContext.AddValue(TValueInfo.Create(ALabel), AValue);
end;

procedure TSerializationData.AddValue(const ALabel: String; const AValue: Cardinal);
begin
  FInContext.AddValue(TValueInfo.Create(ALabel), AValue);
end;

procedure TSerializationData.AddValue(const ALabel: String; const AValue: SmallInt);
begin
  FInContext.AddValue(TValueInfo.Create(ALabel), AValue);
end;

procedure TSerializationData.AddValue(const ALabel: String; const AValue: Currency);
begin
  FInContext.AddValue(TValueInfo.Create(ALabel), AValue);
end;

procedure TSerializationData.AddValue(const ALabel: String; const AValue: Boolean);
begin
  FInContext.AddValue(TValueInfo.Create(ALabel), AValue);
end;

procedure TSerializationData.AddValue(const ALabel: String; const AValue: UnicodeString);
begin
  FInContext.AddValue(TValueInfo.Create(ALabel), AValue);
end;

procedure TSerializationData.AddElement(const AValue: UInt64);
begin
  { Write the value }
  FInContext.AddValue(FElementInfo, AValue);
end;

procedure TSerializationData.AddElement(const AValue: Integer);
begin
  { Write the value }
  FInContext.AddValue(FElementInfo, AValue);
end;

procedure TSerializationData.AddElement(const AValue: Int64);
begin
  { Write the value }
  FInContext.AddValue(FElementInfo, AValue);
end;

procedure TSerializationData.AddElement(const AValue: Double);
begin
  { Write the value }
  FInContext.AddValue(FElementInfo, AValue);
end;

procedure TSerializationData.AddElement(const AValue: Single);
begin
  { Write the value }
  FInContext.AddValue(FElementInfo, AValue);
end;

procedure TSerializationData.AddElement(const AValue: ShortInt);
begin
  { Write the value }
  FInContext.AddValue(FElementInfo, AValue);
end;

procedure TSerializationData.AddElement(const AValue: Byte);
begin
  { Write the value }
  FInContext.AddValue(FElementInfo, AValue);
end;

procedure TSerializationData.AddElement(const AValue: Word);
begin
  { Write the value }
  FInContext.AddValue(FElementInfo, AValue);
end;

procedure TSerializationData.AddElement(const AValue: Cardinal);
begin
  { Write the value }
  FInContext.AddValue(FElementInfo, AValue);
end;

procedure TSerializationData.AddElement(const AValue: SmallInt);
begin
  { Write the value }
  FInContext.AddValue(FElementInfo, AValue);
end;

procedure TSerializationData.AddElement(const AValue: UnicodeString);
begin
  { Write the value }
  FInContext.AddValue(FElementInfo, AValue);
end;

procedure TSerializationData.AddElement(const AValue: AnsiString);
begin
  { Write the value }
  FInContext.AddValue(FElementInfo, AValue);
end;

procedure TSerializationData.AddElement(const AValue: Boolean);
begin
  { Write the value }
  FInContext.AddValue(FElementInfo, AValue);
end;

procedure TSerializationData.AddElement(const AValue; const ASize: Cardinal);
begin
  { Write the value }
  FInContext.AddBinaryValue(FElementInfo, AValue, ASize);
end;

procedure TSerializationData.AddElement(const AValue: TDateTime);
begin
  { Write the value }
  FInContext.AddValue(FElementInfo, AValue);
end;

procedure TSerializationData.AddElement(const AValue: Currency);
begin
  { Write the value }
  FInContext.AddValue(FElementInfo, AValue);
end;

procedure TSerializationData.AddElement(const AValue: Extended);
begin
  { Write the value }
  FInContext.AddValue(FElementInfo, AValue);
end;

procedure TSerializationData.AddElement(const AValue: Comp);
begin
  { Write the value }
  FInContext.AddValue(FElementInfo, AValue);
end;

procedure TSerializationData.AddElement(const AValue: WideChar);
begin
  { Write the value }
  FInContext.AddValue(FElementInfo, AValue);
end;

procedure TSerializationData.AddElement(const AValue: AnsiChar);
begin
  { Write the value }
  FInContext.AddValue(FElementInfo, AValue);
end;

procedure TSerializationData.AddElement<T>(const AIndex: Cardinal; const AValue: T);
var
  LType: IType<T>;
begin
  { Get the type class }
  LType := TType<T>.Default;

  { Write the value }
  LType.Serialize(FElementInfo, AValue, FInContext);
end;

procedure TSerializationData.AddValue(const ALabel: String; const AValue; const ASize: Cardinal);
begin
  FInContext.AddBinaryValue(TValueInfo.Create(ALabel), AValue, ASize);
end;

procedure TSerializationData.AddValue(const ALabel: String; const AValue: TDateTime);
begin
  FInContext.AddValue(TValueInfo.Create(ALabel), AValue);
end;

procedure TSerializationData.AddValue(const ALabel: String; const AValue: AnsiChar);
begin
  FInContext.AddValue(TValueInfo.Create(ALabel), AValue);
end;

procedure TSerializationData.AddValue(const ALabel: String; const AValue: Comp);
begin
  FInContext.AddValue(TValueInfo.Create(ALabel), AValue);
end;

procedure TSerializationData.AddValue(const ALabel: String; const AValue: AnsiString);
begin
  FInContext.AddValue(TValueInfo.Create(ALabel), AValue);
end;

procedure TSerializationData.AddValue(const ALabel: String; const AValue: WideChar);
begin
  FInContext.AddValue(TValueInfo.Create(ALabel), AValue);
end;

procedure TSerializationData.AddValue<T>(const ALabel: String; const AValue: T);
var
  FType: IType<T>;
begin
  FType := TType<T>.Default;
  FType.Serialize(TValueInfo.Create(ALabel), AValue, FInContext);
end;

constructor TSerializationData.Create(const AContext: ISerializationContext);
begin
  if AContext = nil then
    ExceptionHelper.Throw_ArgumentNilError('AContext');

  { Simple copy }
  FInContext := AContext;

  { Initialize the microstack }
  SetLength(FMicroStack, 16);
  FStackPtr := 0;
  FElementInfo := TValueInfo.Indexed;
end;

procedure TSerializationData.EndBlock;
begin
  { First end the composite type }
  FInContext.EndComplexType();

  { Extract the last stored element }
  FElementInfo := MicroPop();
end;

function TSerializationData.MicroPop: TValueInfo;
begin
  ASSERT(FStackPtr > 0); // Must fail since context should fail first

  { Move back }
  Dec(FStackPtr);

  { And get the value  }
  Result := FMicroStack[FStackPtr];
end;

procedure TSerializationData.MicroPush(const AInfo: TValueInfo);
var
  LLength: Cardinal;
begin
  LLength := Length(FMicroStack);

  { Check whether to extend the array }
  if FStackPtr = LLength then
    SetLength(FMicroStack, LLength * 2);

  { Store the value }
  FMicroStack[FStackPtr] := AInfo;

  { And move forward }
  Inc(FStackPtr);
end;

procedure TSerializationData.StartBlock(const ALabel: String);
begin
  { Start a composite }
  FInContext.StartRecordType(TValueInfo.Create(ALabel));

  MicroPush(FElementInfo);
  FElementInfo := TValueInfo.Indexed;
end;

procedure TSerializationData.StartListBlock(const ALabel, AElementLabel: String; const AElementCount: Cardinal);
begin
  { Start a composite }
  FInContext.StartArrayType(TValueInfo.Create(ALabel), TValueInfo.Create(AElementLabel), AElementCount);

  MicroPush(FElementInfo);
  FElementInfo := TValueInfo.Indexed;
end;

procedure TSerializationData.StartBlock();
begin
  { Start a composite }
  FInContext.StartRecordType(FElementInfo);

  { Store elements }
  MicroPush(FElementInfo);
  FElementInfo := TValueInfo.Indexed;
end;

procedure TSerializationData.StartListBlock(const AElementLabel: String; const AElementCount: Cardinal);
begin
  { Start a composite }
  FInContext.StartArrayType(FElementInfo, TValueInfo.Create(AElementLabel), AElementCount);

  { Store elements }
  MicroPush(FElementInfo);
  FElementInfo := TValueInfo.Indexed;
end;

{ TDeserializationData }

constructor TDeserializationData.Create(const AContext: IDeserializationContext);
begin
  if AContext = nil then
    ExceptionHelper.Throw_ArgumentNilError('AContext');

  { Simple copy }
  FOutContext := AContext;

  { Initialize the microstack }
  SetLength(FMicroStack, 16);
  FStackPtr := 0;
  FElementInfo := TValueInfo.Indexed;
end;


procedure TDeserializationData.EndBlock;
begin
  { First end the composite type }
  FOutContext.EndComplexType();

  { Extract the last stored element }
  FElementInfo := MicroPop();
end;

procedure TDeserializationData.ExpectBlock(const ALabel: String);
begin
  { Start a composite }
  FOutContext.ExpectRecordType(TValueInfo.Create(ALabel));

  MicroPush(FElementInfo);
  FElementInfo := TValueInfo.Indexed;
end;

procedure TDeserializationData.ExpectBlock;
begin
  { Start a composite }
  FOutContext.ExpectRecordType(FElementInfo);

  { Store elements }
  MicroPush(FElementInfo);
  FElementInfo := TValueInfo.Indexed;
end;

function TDeserializationData.ExpectListBlock(const AElementLabel: String): Cardinal;
begin
  { Start a composite }
  FOutContext.ExpectArrayType(FElementInfo, TValueInfo.Create(AElementLabel), Result);

  { Store elements }
  MicroPush(FElementInfo);
  FElementInfo := TValueInfo.Indexed;
end;

function TDeserializationData.ExpectListBlock(const ALabel, AElementLabel: String): Cardinal;
begin
  { Start a composite }
  FOutContext.ExpectArrayType(TValueInfo.Create(ALabel), TValueInfo.Create(AElementLabel), Result);

  MicroPush(FElementInfo);
  FElementInfo := TValueInfo.Indexed;
end;

procedure TDeserializationData.GetElement(out AValue: UInt64);
begin
  { Read the value }
  FOutContext.GetValue(FElementInfo, AValue);
end;

procedure TDeserializationData.GetElement(out AValue: Integer);
begin
  { Read the value }
  FOutContext.GetValue(FElementInfo, AValue);
end;

procedure TDeserializationData.GetElement(out AValue: Single);
begin
  { Read the value }
  FOutContext.GetValue(FElementInfo, AValue);
end;

procedure TDeserializationData.GetElement(out AValue: Int64);
begin
  { Read the value }
  FOutContext.GetValue(FElementInfo, AValue);
end;

procedure TDeserializationData.GetElement(out AValue: Cardinal);
begin
  { Read the value }
  FOutContext.GetValue(FElementInfo, AValue);
end;

procedure TDeserializationData.GetElement(out AValue: ShortInt);
begin
  { Read the value }
  FOutContext.GetValue(FElementInfo, AValue);
end;

procedure TDeserializationData.GetElement(out AValue: Byte);
begin
  { Read the value }
  FOutContext.GetValue(FElementInfo, AValue);
end;

procedure TDeserializationData.GetElement(out AValue: SmallInt);
begin
  { Read the value }
  FOutContext.GetValue(FElementInfo, AValue);
end;

procedure TDeserializationData.GetElement(out AValue: Word);
begin
  { Read the value }
  FOutContext.GetValue(FElementInfo, AValue);
end;

procedure TDeserializationData.GetElement(out AValue: UnicodeString);
begin
  { Read the value }
  FOutContext.GetValue(FElementInfo, AValue);
end;

procedure TDeserializationData.GetElement(out AValue: AnsiString);
begin
  { Read the value }
  FOutContext.GetValue(FElementInfo, AValue);
end;

procedure TDeserializationData.GetElement(out AValue: Boolean);
begin
  { Read the value }
  FOutContext.GetValue(FElementInfo, AValue);
end;

procedure TDeserializationData.GetElement(out AValue; const ASize: Cardinal);
var
  LPtr: Pointer;
  LName: String;
begin
  { Capture }
  LPtr := @AValue;
  LName := FElementInfo.Name;

  { Read the value }
  FOutContext.GetBinaryValue(FElementInfo,
    function(const ASize: Cardinal): Pointer
    begin
      { Size mismatch? }
      if ASize <> ASize then
        ExceptionHelper.Throw_BinaryValueSizeMismatch(LName, '?');

      { Otherwise give it the pointer }
      Result := LPtr;
    end
  );
end;

procedure TDeserializationData.GetElement(out AValue: TDateTime);
begin
  { Read the value }
  FOutContext.GetValue(FElementInfo, AValue);
end;

procedure TDeserializationData.GetElement(out AValue: WideChar);
begin
  { Read the value }
  FOutContext.GetValue(FElementInfo, AValue);
end;

procedure TDeserializationData.GetElement(out AValue: Extended);
begin
  { Read the value }
  FOutContext.GetValue(FElementInfo, AValue);
end;

procedure TDeserializationData.GetElement(out AValue: Double);
begin
  { Read the value }
  FOutContext.GetValue(FElementInfo, AValue);
end;

procedure TDeserializationData.GetElement(out AValue: Currency);
begin
  { Read the value }
  FOutContext.GetValue(FElementInfo, AValue);
end;

procedure TDeserializationData.GetElement(out AValue: AnsiChar);
begin
  { Read the value }
  FOutContext.GetValue(FElementInfo, AValue);
end;

procedure TDeserializationData.GetElement(out AValue: Comp);
begin
  { Read the value }
  FOutContext.GetValue(FElementInfo, AValue);
end;

procedure TDeserializationData.GetElement<T>(out AValue: T);
var
  LType: IType<T>;
begin
  { Get the type class }
  LType := TType<T>.Default;

  { Read the value }
  LType.Deserialize(FElementInfo, AValue, FOutContext);
end;

procedure TDeserializationData.GetValue(const ALabel: String; out AValue: Integer);
begin
  FOutContext.GetValue(TValueInfo.Create(ALabel), AValue);
end;

procedure TDeserializationData.GetValue(const ALabel: String; out AValue: Cardinal);
begin
  FOutContext.GetValue(TValueInfo.Create(ALabel), AValue);
end;

procedure TDeserializationData.GetValue(const ALabel: String; out AValue: Int64);
begin
  FOutContext.GetValue(TValueInfo.Create(ALabel), AValue);
end;

procedure TDeserializationData.GetValue(const ALabel: String; out AValue: UInt64);
begin
  FOutContext.GetValue(TValueInfo.Create(ALabel), AValue);
end;

procedure TDeserializationData.GetValue(const ALabel: String; out AValue: ShortInt);
begin
  FOutContext.GetValue(TValueInfo.Create(ALabel), AValue);
end;

procedure TDeserializationData.GetValue(const ALabel: String; out AValue: Byte);
begin
  FOutContext.GetValue(TValueInfo.Create(ALabel), AValue);
end;

procedure TDeserializationData.GetValue(const ALabel: String; out AValue: SmallInt);
begin
  FOutContext.GetValue(TValueInfo.Create(ALabel), AValue);
end;

procedure TDeserializationData.GetValue(const ALabel: String; out AValue: Word);
begin
  FOutContext.GetValue(TValueInfo.Create(ALabel), AValue);
end;

procedure TDeserializationData.GetValue(const ALabel: String; out AValue: Single);
begin
  FOutContext.GetValue(TValueInfo.Create(ALabel), AValue);
end;

procedure TDeserializationData.GetValue(const ALabel: String; out AValue: UnicodeString);
begin
  FOutContext.GetValue(TValueInfo.Create(ALabel), AValue);
end;

procedure TDeserializationData.GetValue(const ALabel: String; out AValue: AnsiString);
begin
  FOutContext.GetValue(TValueInfo.Create(ALabel), AValue);
end;

procedure TDeserializationData.GetValue(const ALabel: String; out AValue: Boolean);
begin
  FOutContext.GetValue(TValueInfo.Create(ALabel), AValue);
end;

procedure TDeserializationData.GetValue(const ALabel: String; out AValue; const ASize: Cardinal);
var
  LPtr: Pointer;
  LName: String;
begin
  { Capture }
  LPtr := @AValue;
  LName := ALabel;

  { Read the value }
  FOutContext.GetBinaryValue(TValueInfo.Create(ALabel),
    function(const xSize: Cardinal): Pointer
    begin
      { Size mismatch? }
      if xSize <> ASize then
        ExceptionHelper.Throw_BinaryValueSizeMismatch(LName, '?');

      { Otherwise give it the pointer }
      Result := LPtr;
    end
  );
end;

procedure TDeserializationData.GetValue(const ALabel: String; out AValue: TDateTime);
begin
  FOutContext.GetValue(TValueInfo.Create(ALabel), AValue);
end;

procedure TDeserializationData.GetValue(const ALabel: String; out AValue: WideChar);
begin
  FOutContext.GetValue(TValueInfo.Create(ALabel), AValue);
end;

procedure TDeserializationData.GetValue(const ALabel: String; out AValue: Double);
begin
  FOutContext.GetValue(TValueInfo.Create(ALabel), AValue);
end;

procedure TDeserializationData.GetValue(const ALabel: String; out AValue: Extended);
begin
  FOutContext.GetValue(TValueInfo.Create(ALabel), AValue);
end;

procedure TDeserializationData.GetValue(const ALabel: String; out AValue: Currency);
begin
  FOutContext.GetValue(TValueInfo.Create(ALabel), AValue);
end;

procedure TDeserializationData.GetValue(const ALabel: String; out AValue: AnsiChar);
begin
  FOutContext.GetValue(TValueInfo.Create(ALabel), AValue);
end;

procedure TDeserializationData.GetValue(const ALabel: String; out AValue: Comp);
begin
  FOutContext.GetValue(TValueInfo.Create(ALabel), AValue);
end;

procedure TDeserializationData.GetValue<T>(const ALabel: String; out AValue: T);
var
  FType: IType<T>;
begin
  FType := TType<T>.Default;
  FType.Deserialize(TValueInfo.Create(ALabel), AValue, FOutContext);
end;

function TDeserializationData.MicroPop: TValueInfo;
begin
  ASSERT(FStackPtr > 0); // Must fail since context should fail first

  { Move back }
  Dec(FStackPtr);

  { And get the value  }
  Result := FMicroStack[FStackPtr];
end;

procedure TDeserializationData.MicroPush(const AInfo: TValueInfo);
var
  LLength: Cardinal;
begin
  LLength := Length(FMicroStack);

  { Check whether to extend the array }
  if FStackPtr = LLength then
    SetLength(FMicroStack, LLength * 2);

  { Store the value }
  FMicroStack[FStackPtr] := AInfo;

  { And move forward }
  Inc(FStackPtr);
end;

end.
