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

{$I DeHL.Defines.inc}
unit DeHL.Types;
interface

uses SysUtils,
     Classes,
     TypInfo,
     Rtti,
     DeHL.Base,
     DeHL.StrConsts,
     DeHL.Math.Primes,
     DeHL.Exceptions,
     DeHL.Serialization;

type
  TTypeManagement = (tmNone, tmManual, tmCompiler);

  TTypeFamily = (tfUnknown, tfUnsignedInteger, tfSignedInteger, tfPointer, tfBoolean, tfMethod,
    tfReal, tfCharacter, tfString, tfDate, tfInterface, tfClass, tfClassReference, tfVariant, tfArray, tfRecord);

type
  TTypeFamilySet = set of TTypeFamily;

  { Type Extension base class }
  TTypeExtension = class abstract(TRefCountedObject)
  public
    constructor Create(); virtual;
  end;

  { Type extension meta-class }
  TTypeExtensionClass = class of TTypeExtension;

  { Type extender }
  TTypeExtender = class sealed(TRefCountedObject)
  private
    { Cannot make it typed here! }
    FExtensions: TObject;

    { Creates a new type extension specific to the type }
    function CreateExtensionFor(const AObject: TObject): TTypeExtension;
  public
    { Constructors }
    constructor Create();

    { Destructors }
    destructor Destroy(); override;

    { Extension management }
    procedure Register<T>(const AExtension: TTypeExtensionClass);
    procedure Unregister<T>();
  end;

  { Type support interface }
  IType = interface
    { Type information }
    function Name(): String;
    function Size(): Cardinal;
    function TypeInfo(): PTypeInfo;
    function Management(): TTypeManagement;
    function Family(): TTypeFamily;

    { Restrictions }
    procedure RestrictTo(const Families: TTypeFamilySet);

    { Extensions }
    function GetExtension(const AExtender: TTypeExtender): TTypeExtension;
  end;

{ Support for conversions }
  IConvertibleType<T> = interface(IType)
    { To Variant }
    function TryConvertToVariant(const AValue: T; out ORes: Variant): Boolean;
    function ConvertToVariant(const AValue: T): Variant;

    { From Variant }
    function TryConvertFromVariant(const AValue: Variant; out ORes: T): Boolean;
    function ConvertFromVariant(const AValue: Variant): T;
  end;

  { Type support interface }
  IType<T> = interface(IConvertibleType<T>)
    { Standard support routines }
    function Compare(const AValue1, AValue2: T): Integer;
    function AreEqual(const AValue1, AValue2: T): Boolean;
    function GenerateHashCode(const AValue: T): Integer;
    function GetString(const AValue: T): String;

    { Life-time management }
    procedure Cleanup(var AValue: T);

    { Serialization }
    procedure Serialize(const AInfo: TValueInfo; const AValue: T; const AContext: ISerializationContext); overload;
    procedure Serialize(const ALabel: String; const AValue: T; const AData: TSerializationData); overload;
    procedure Serialize(const AValue: T; const AData: TSerializationData); overload;

    procedure Deserialize(const AInfo: TValueInfo; out AValue: T; const AContext: IDeserializationContext); overload;
    procedure Deserialize(const ALabel: String; out AValue: T; const AData: TDeserializationData); overload;
    procedure Deserialize(out AValue: T; const AData: TDeserializationData); overload;
  end;

  { Type support interface }
  IClassType<T: class> = interface(IType<T>)
    { Life-time management toggle }
    procedure SetShouldCleanup(const ShouldCleanup: Boolean);
  end;

  { Meta-class }
  TTypeClass = class of TType;

  { Base abstract class for the generic TType }
  TType = class abstract(TRefCountedObject, IType)
  private type
    { Internal Ptr/Ptr dictionary }
    TPointerDictionary = class
    private type
      TEntry = record
        FHashCode : Integer;
        FNext     : Integer;
        FKey      : Pointer;
        FValue    : Pointer;
      end;

      TBucketArray = array of Integer;
      TEntryArray = TArray<TEntry>;

    var
      FBucketArray   : TBucketArray;
      FEntryArray    : TEntryArray;

      FCount         : Cardinal;
      FFreeCount     : Integer;
      FFreeList      : Integer;

      { Internal }
      procedure InitializeInternals(const Capacity: Cardinal);
      procedure Insert(const AKey: Pointer; const AValue: Pointer; const ShouldAdd: Boolean = true);
      function FindEntry(const AKey : Pointer) : Integer;
      procedure Resize();
      function Hash(const AKey: Pointer) : Integer;

    protected
      { Key getter and setter }
      function GetItem(const Key: Pointer): Pointer;
      procedure SetItem(const Key: Pointer; const Value: Pointer);

    public
      { Constructors }
      constructor Create(); overload;
      constructor Create(const InitialCapacity: Cardinal); overload;

      { Destructor }
      destructor Destroy(); override;

      {  Modification }
      procedure Clear();

      procedure Add(const AKey: Pointer; const AValue: Pointer); overload;
      procedure Remove(const AKey: Pointer); overload;

      { Lookup }
      function ContainsKey(const AKey: Pointer): Boolean;
      function TryGetValue(const AKey: Pointer; out FoundValue: Pointer): Boolean;

      { Properties }
      property Items[const Key: Pointer]: Pointer read GetItem write SetItem; default;
      property Count: Cardinal read FCount;
    end;

    TSerializationGuts = record
      FType: TRttiType;
      FInContext: ISerializationContext;
      FOutContext: IDeserializationContext;

      constructor Create(const AType: TRttiType; const AInContext: ISerializationContext;
        const AOutContext: IDeserializationContext);
    end;

  private class var
    { HACK -- Cannot reference the dictionary/list directly }
    FCustomTypes: TObject;
    FStoredInterfaces: TInterfaceList;

  private var
    { Fields }
    FTypeName: String;
    FTypeSize: Cardinal;
    FTypeInfo: PTypeInfo;
    FTypeFamily: TTypeFamily;

    { Serialization utils }
    class function Skippable(const AField: TRttiField): Boolean;

    class procedure SerProcessFields(const AGuts: TSerializationGuts; const AInfo: TValueInfo;
      const ACount: Cardinal; const APtrToField: Pointer; const ASerialize: Boolean);

    class procedure SerProcessStaticArray(const AGuts: TSerializationGuts; const AInfo: TValueInfo; const APtrToFirst: Pointer; const ASerialize: Boolean);
    class procedure SerProcessStructClass(const AGuts: TSerializationGuts; const APtrToInstance: Pointer; const ASerialize: Boolean);

    procedure InternalSerialize(const AInfo: TValueInfo; const APtrToValue: Pointer; const AContext: ISerializationContext); virtual; abstract;
    procedure InternalDeserialize(const AInfo: TValueInfo; const APtrToValue: Pointer; const AContext: IDeserializationContext); virtual; abstract;

    class function IsClassStructSerializable(const AType: TRttiType): Boolean;

    { Statics }
    class function GetParentTypeInfo(const ClassInfo: PTypeInfo): PTypeInfo; static;
    class function CreateBinaryType(const Size: Integer): Pointer; static;
    class function CreateCharType(const Size: Integer): Pointer; static;
    class function CreateIntegerType(const OrdinalType: TOrdType): Pointer; static;
    class function CreateFloatType(const FloatType: TFloatType): Pointer; static;
    class function CreateStringType(const Kind: TTypeKind): Pointer; static;
    class function CreateClassType(): Pointer; static;
    class function CreateVariantType(): Pointer; static;
    class function CreateInt64Type(const TypeData: PTypeData): Pointer; static;
    class function CreateDynamicArrayType(const ElementSize: Integer; const TypeInfo: PTypeInfo): Pointer; static;

    class function CreateDefault(const TypeInfo: PTypeInfo; const TypeSize: Cardinal;
      const AllowCustom: Boolean; const AArrayClass, ARecordClass: TTypeClass): Pointer; static;

    class function CreateCustomType(const TypeInfo: PTypeInfo): Pointer; static;
  public
    { Default constructor }
    constructor Create(); virtual; abstract;

    { Basic Type information }
    function Name(): String; virtual;
    function Size(): Cardinal; virtual;
    function TypeInfo(): PTypeInfo; virtual;
    function Management(): TTypeManagement; virtual;
    function Family(): TTypeFamily; virtual;

    { Restrictions and types }
    procedure RestrictTo(const AllowedFamilies: TTypeFamilySet); virtual;

    { Extensions }
    function GetExtension(const AExtender: TTypeExtender): TTypeExtension; virtual;
  end;

  { Type support base }
  TType<T> = class abstract(TType, IType<T>)
  private class var
    FCachedDefaultInstance,
      FCachedStandardInstance: TType<T>;

    { Creates custom stuff }
    class procedure DisposeCachedDefaultInstance(); static;
    class function CreateDefault(const AllowCustom: Boolean): TType<T>; static;

  protected type
    TValRef = ^T;

  protected
    { Serialization helper }
    procedure InternalSerialize(const AInfo: TValueInfo; const APtrToValue: Pointer; const AContext: ISerializationContext); override;
    procedure InternalDeserialize(const AInfo: TValueInfo; const APtrToValue: Pointer; const AContext: IDeserializationContext); override;

    { Override this to support serialization }
    procedure DoSerialize(const AInfo: TValueInfo; const AValue: T; const AContext: ISerializationContext); virtual;
    procedure DoDeserialize(const AInfo: TValueInfo; out AValue: T; const AContext: IDeserializationContext); virtual;
  public
    { Default constructor }
    constructor Create(); override;

    { Comparator }
    function Compare(const AValue1, AValue2: T): Integer; virtual; abstract;

    { Equality Comparator }
    function AreEqual(const AValue1, AValue2: T): Boolean; virtual; abstract;

    { Hash code provider }
    function GenerateHashCode(const AValue: T): Integer; virtual; abstract;

    { Cleanup the given value }
    procedure Cleanup(var AValue: T); virtual;

    { Get String representation }
    function GetString(const AValue: T): String; virtual; abstract;

    { Serialization }
    procedure Serialize(const AInfo: TValueInfo; const AValue: T; const AContext: ISerializationContext); overload;
    procedure Serialize(const ALabel: String; const AValue: T; const AData: TSerializationData); overload;
    procedure Serialize(const AValue: T; const AData: TSerializationData); overload;

    procedure Deserialize(const AInfo: TValueInfo; out AValue: T; const AContext: IDeserializationContext); overload;
    procedure Deserialize(const ALabel: String; out AValue: T; const AData: TDeserializationData); overload;
    procedure Deserialize(out AValue: T; const AData: TDeserializationData); overload;

    { Variant Conversion }
    function TryConvertToVariant(const AValue: T; out ORes: Variant): Boolean; virtual;
    function ConvertToVariant(const AValue: T): Variant;
    function TryConvertFromVariant(const AValue: Variant; out ORes: T): Boolean; virtual;
    function ConvertFromVariant(const AValue: Variant): T;

    { Default/Standard static }
    class function Default: IType<T>; overload; static;
    class function Default(const AllowedFamilies: TTypeFamilySet): IType<T>; overload; static;
    class function Standard: IType<T>; overload; static;
    class function Standard(const AllowedFamilies: TTypeFamilySet): IType<T>; overload; static;

    { Custom types }
    class procedure Register(const AType: TTypeClass); static;
    class procedure Unregister(); static;
    class function IsRegistered(): Boolean; static;
  end;

  { Base for magic types }
  TMagicType<T> = class abstract(TType<T>)
  public
    { Gets who is responsable for managing the life-time of a type }
    function Management(): TTypeManagement; override;
  end;

  { Base for magic types }
  TManualType<T> = class abstract(TType<T>)
  public
    { Gets who is responsable for managing the life-time of a type }
    function Management(): TTypeManagement; override;
  end;

  { Suppressed Cleanup Type Support }
  TSuppressedWrapperType<T> = class(TType<T>)
  private
    FType: IType<T>;
    FAllowCleanup: Boolean;

  protected
    { Serialization }
    procedure DoSerialize(const AInfo: TValueInfo; const AValue: T; const AContext: ISerializationContext); override;
    procedure DoDeserialize(const AInfo: TValueInfo; out AValue: T; const AContext: IDeserializationContext); override;
  public
    { Comparator }
    function Compare(const AValue1, AValue2: T): Integer; override;

    { Equality Comparator }
    function AreEqual(const AValue1, AValue2: T): Boolean; override;

    { Hash code provider }
    function GenerateHashCode(const AValue: T): Integer; override;

    { Get String representation }
    function GetString(const AValue: T): String; override;

    { Cleanup the given value }
    procedure Cleanup(var AValue: T); override;

    { Basic Type information }
    function Name(): String; override;
    function Size(): Cardinal; override;
    function TypeInfo(): PTypeInfo; override;
    function Management(): TTypeManagement; override;
    function Family(): TTypeFamily; override;

    { Variant Conversion }
    function TryConvertToVariant(const AValue: T; out ORes: Variant): Boolean; override;
    function TryConvertFromVariant(const AValue: Variant; out ORes: T): Boolean; override;

    { Default constructor }
    constructor Create(); overload; override;
    constructor Create(const AType: IType<T>); reintroduce; overload;

    { Cleanup shunt }
    property AllowCleanup: Boolean read FAllowCleanup write FAllowCleanup;
  end;

  { Suppressed Object type }
  TObjectWrapperType<T: class> = class(TSuppressedWrapperType<T>)
  public
    procedure Cleanup(var AValue: T); override;
    function Management(): TTypeManagement; override;
  end;

  { Byte Support }
  TByteType = class sealed(TType<Byte>)
  protected
    { Serialization }
    procedure DoSerialize(const AInfo: TValueInfo; const AValue: Byte; const AContext: ISerializationContext); override;
    procedure DoDeserialize(const AInfo: TValueInfo; out AValue: Byte; const AContext: IDeserializationContext); override;

  public
    { Comparator }
    function Compare(const AValue1, AValue2: Byte): Integer; override;

    { Equality Comparator }
    function AreEqual(const AValue1, AValue2: Byte): Boolean; override;

    { Hash code provider }
    function GenerateHashCode(const AValue: Byte): Integer; override;

    { Get String representation }
    function GetString(const AValue: Byte): String; override;

    { Variant Conversion }
    function TryConvertToVariant(const AValue: Byte; out ORes: Variant): Boolean; override;
    function TryConvertFromVariant(const AValue: Variant; out ORes: Byte): Boolean; override;

    { Default constructor }
    constructor Create(); override;
  end;

  { ShortInt Support }
  TShortIntType = class sealed(TType<ShortInt>)
  protected
    { Serialization }
    procedure DoSerialize(const AInfo: TValueInfo; const AValue: ShortInt; const AContext: ISerializationContext); override;
    procedure DoDeserialize(const AInfo: TValueInfo; out AValue: ShortInt; const AContext: IDeserializationContext); override;

  public
    { Comparator }
    function Compare(const AValue1, AValue2: ShortInt): Integer; override;

    { Equality Comparator }
    function AreEqual(const AValue1, AValue2: ShortInt): Boolean; override;

    { Hash code provider }
    function GenerateHashCode(const AValue: ShortInt): Integer; override;

    { Get String representation }
    function GetString(const AValue: ShortInt): String; override;

    { Variant Conversion }
    function TryConvertToVariant(const AValue: ShortInt; out ORes: Variant): Boolean; override;
    function TryConvertFromVariant(const AValue: Variant; out ORes: ShortInt): Boolean; override;

    { Default constructor }
    constructor Create(); override;
  end;

  { Word Support }
  TWordType = class sealed(TType<Word>)
  protected
    { Serialization }
    procedure DoSerialize(const AInfo: TValueInfo; const AValue: Word; const AContext: ISerializationContext); override;
    procedure DoDeserialize(const AInfo: TValueInfo; out AValue: Word; const AContext: IDeserializationContext); override;

  public
    { Comparator }
    function Compare(const AValue1, AValue2: Word): Integer; override;

    { Equality Comparator }
    function AreEqual(const AValue1, AValue2: Word): Boolean; override;

    { Hash code provider }
    function GenerateHashCode(const AValue: Word): Integer; override;

    { Get String representation }
    function GetString(const AValue: Word): String; override;

    { Variant Conversion }
    function TryConvertToVariant(const AValue: Word; out ORes: Variant): Boolean; override;
    function TryConvertFromVariant(const AValue: Variant; out ORes: Word): Boolean; override;

    { Default constructor }
    constructor Create(); override;
  end;

  { SmallInt Support }
  TSmallIntType = class sealed(TType<SmallInt>)
  protected
    { Serialization }
    procedure DoSerialize(const AInfo: TValueInfo; const AValue: SmallInt; const AContext: ISerializationContext); override;
    procedure DoDeserialize(const AInfo: TValueInfo; out AValue: SmallInt; const AContext: IDeserializationContext); override;

  public
    { Comparator }
    function Compare(const AValue1, AValue2: SmallInt): Integer; override;

    { Equality Comparator }
    function AreEqual(const AValue1, AValue2: SmallInt): Boolean; override;

    { Hash code provider }
    function GenerateHashCode(const AValue: SmallInt): Integer; override;

    { Get String representation }
    function GetString(const AValue: SmallInt): String; override;

    { Variant Conversion }
    function TryConvertToVariant(const AValue: SmallInt; out ORes: Variant): Boolean; override;
    function TryConvertFromVariant(const AValue: Variant; out ORes: SmallInt): Boolean; override;

    { Default constructor }
    constructor Create(); override;
  end;

  { Cardinal Support }
  TCardinalType = class sealed(TType<Cardinal>)
  protected
    { Serialization }
    procedure DoSerialize(const AInfo: TValueInfo; const AValue: Cardinal; const AContext: ISerializationContext); override;
    procedure DoDeserialize(const AInfo: TValueInfo; out AValue: Cardinal; const AContext: IDeserializationContext); override;

  public
    { Comparator }
    function Compare(const AValue1, AValue2: Cardinal): Integer; override;

    { Equality Comparator }
    function AreEqual(const AValue1, AValue2: Cardinal): Boolean; override;

    { Hash code provider }
    function GenerateHashCode(const AValue: Cardinal): Integer; override;

    { Get String representation }
    function GetString(const AValue: Cardinal): String; override;

    { Variant Conversion }
    function TryConvertToVariant(const AValue: Cardinal; out ORes: Variant): Boolean; override;
    function TryConvertFromVariant(const AValue: Variant; out ORes: Cardinal): Boolean; override;

    { Default constructor }
    constructor Create(); override;
  end;

  { Integer Support }
  TIntegerType = class sealed(TType<Integer>)
  protected
    { Serialization }
    procedure DoSerialize(const AInfo: TValueInfo; const AValue: Integer; const AContext: ISerializationContext); override;
    procedure DoDeserialize(const AInfo: TValueInfo; out AValue: Integer; const AContext: IDeserializationContext); override;

  public
    { Comparator }
    function Compare(const AValue1, AValue2: Integer): Integer; override;

    { Equality Comparator }
    function AreEqual(const AValue1, AValue2: Integer): Boolean; override;

    { Hash code provider }
    function GenerateHashCode(const AValue: Integer): Integer; override;

    { Get String representation }
    function GetString(const AValue: Integer): String; override;

    { Variant Conversion }
    function TryConvertToVariant(const AValue: Integer; out ORes: Variant): Boolean; override;
    function TryConvertFromVariant(const AValue: Variant; out ORes: Integer): Boolean; override;

    { Default constructor }
    constructor Create(); override;
  end;

  { Int64 Support }
  TInt64Type = class sealed(TType<Int64>)
  protected
    { Serialization }
    procedure DoSerialize(const AInfo: TValueInfo; const AValue: Int64; const AContext: ISerializationContext); override;
    procedure DoDeserialize(const AInfo: TValueInfo; out AValue: Int64; const AContext: IDeserializationContext); override;

  public
    { Comparator }
    function Compare(const AValue1, AValue2: Int64): Integer; override;

    { Equality Comparator }
    function AreEqual(const AValue1, AValue2: Int64): Boolean; override;

    { Hash code provider }
    function GenerateHashCode(const AValue: Int64): Integer; override;

    { Get String representation }
    function GetString(const AValue: Int64): String; override;

    { Variant Conversion }
    function TryConvertToVariant(const AValue: Int64; out ORes: Variant): Boolean; override;
    function TryConvertFromVariant(const AValue: Variant; out ORes: Int64): Boolean; override;

    { Default constructor }
    constructor Create(); override;
  end;

  { UInt64 Support }
  TUInt64Type = class sealed(TType<UInt64>)
  protected
    { Serialization }
    procedure DoSerialize(const AInfo: TValueInfo; const AValue: UInt64; const AContext: ISerializationContext); override;
    procedure DoDeserialize(const AInfo: TValueInfo; out AValue: UInt64; const AContext: IDeserializationContext); override;

  public
    { Comparator }
    function Compare(const AValue1, AValue2: UInt64): Integer; override;

    { Equality Comparator }
    function AreEqual(const AValue1, AValue2: UInt64): Boolean; override;

    { Hash code provider }
    function GenerateHashCode(const AValue: UInt64): Integer; override;

    { Get String representation }
    function GetString(const AValue: UInt64): String; override;

    { Variant Conversion }
    function TryConvertToVariant(const AValue: UInt64; out ORes: Variant): Boolean; override;
    function TryConvertFromVariant(const AValue: Variant; out ORes: UInt64): Boolean; override;

    { Default constructor }
    constructor Create(); override;
  end;

  { Pointer Support }
  TPointerType = class sealed(TType<Pointer>)
  private
    FCanBeSerialized, FCanBeSerializedVerified: Boolean;

    { Cannot return nil }
    function GetElementType(const AContext: IContext; const AInfo: TValueInfo): TRttiType;

  protected
    { Serialization }
    procedure DoSerialize(const AInfo: TValueInfo; const AValue: Pointer; const AContext: ISerializationContext); override;
    procedure DoDeserialize(const AInfo: TValueInfo; out AValue: Pointer; const AContext: IDeserializationContext); override;

  public
    { Comparator }
    function Compare(const AValue1, AValue2: Pointer): Integer; override;

    { Equality Comparator }
    function AreEqual(const AValue1, AValue2: Pointer): Boolean; override;

    { Hash code provider }
    function GenerateHashCode(const AValue: Pointer): Integer; override;

    { Get String representation }
    function GetString(const AValue: Pointer): String; override;

    { Variant Conversion }
    function TryConvertToVariant(const AValue: Pointer; out ORes: Variant): Boolean; override;
    function TryConvertFromVariant(const AValue: Variant; out ORes: Pointer): Boolean; override;

    { Default constructor }
    constructor Create(); override;
  end;

  { AnsiChar Support }
  TAnsiCharType = class sealed(TType<AnsiChar>)
  protected
    { Serialization }
    procedure DoSerialize(const AInfo: TValueInfo; const AValue: AnsiChar; const AContext: ISerializationContext); override;
    procedure DoDeserialize(const AInfo: TValueInfo; out AValue: AnsiChar; const AContext: IDeserializationContext); override;

  public
    { Comparator }
    function Compare(const AValue1, AValue2: AnsiChar): Integer; override;

    { Equality Comparator }
    function AreEqual(const AValue1, AValue2: AnsiChar): Boolean; override;

    { Hash code provider }
    function GenerateHashCode(const AValue: AnsiChar): Integer; override;

    { Get String representation }
    function GetString(const AValue: AnsiChar): String; override;

    { Variant Conversion }
    function TryConvertToVariant(const AValue: AnsiChar; out ORes: Variant): Boolean; override;
    function TryConvertFromVariant(const AValue: Variant; out ORes: AnsiChar): Boolean; override;

    { Default constructor }
    constructor Create(); override;
  end;

  { WideChar Support }
  TWideCharType = class sealed(TType<WideChar>)
  protected
    { Serialization }
    procedure DoSerialize(const AInfo: TValueInfo; const AValue: WideChar; const AContext: ISerializationContext); override;
    procedure DoDeserialize(const AInfo: TValueInfo; out AValue: WideChar; const AContext: IDeserializationContext); override;

  public
    { Comparator }
    function Compare(const AValue1, AValue2: WideChar): Integer; override;

    { Equality Comparator }
    function AreEqual(const AValue1, AValue2: WideChar): Boolean; override;

    { Hash code provider }
    function GenerateHashCode(const AValue: WideChar): Integer; override;

    { Get String representation }
    function GetString(const AValue: WideChar): String; override;

    { Variant Conversion }
    function TryConvertToVariant(const AValue: WideChar; out ORes: Variant): Boolean; override;
    function TryConvertFromVariant(const AValue: Variant; out ORes: WideChar): Boolean; override;

    { Default constructor }
    constructor Create(); override;
  end;

  { UCS4Char Support }
  TUCS4CharType = class sealed(TType<UCS4Char>)
  protected
    { Serialization }
    procedure DoSerialize(const AInfo: TValueInfo; const AValue: UCS4Char; const AContext: ISerializationContext); override;
    procedure DoDeserialize(const AInfo: TValueInfo; out AValue: UCS4Char; const AContext: IDeserializationContext); override;

  public
    { Comparator }
    function Compare(const AValue1, AValue2: UCS4Char): Integer; override;

    { Equality Comparator }
    function AreEqual(const AValue1, AValue2: UCS4Char): Boolean; override;

    { Hash code provider }
    function GenerateHashCode(const AValue: UCS4Char): Integer; override;

    { Get String representation }
    function GetString(const AValue: UCS4Char): String; override;

    { Variant Conversion }
    function TryConvertToVariant(const AValue: UCS4Char; out ORes: Variant): Boolean; override;
    function TryConvertFromVariant(const AValue: Variant; out ORes: UCS4Char): Boolean; override;

    { Default constructor }
    constructor Create(); override;
  end;

  { Single Support }
  TSingleType = class sealed(TType<Single>)
  protected
    { Serialization }
    procedure DoSerialize(const AInfo: TValueInfo; const AValue: Single; const AContext: ISerializationContext); override;
    procedure DoDeserialize(const AInfo: TValueInfo; out AValue: Single; const AContext: IDeserializationContext); override;

  public
    { Comparator }
    function Compare(const AValue1, AValue2: Single): Integer; override;

    { Equality Comparator }
    function AreEqual(const AValue1, AValue2: Single): Boolean; override;

    { Hash code provider }
    function GenerateHashCode(const AValue: Single): Integer; override;

    { Get String representation }
    function GetString(const AValue: Single): String; override;

    { Variant Conversion }
    function TryConvertToVariant(const AValue: Single; out ORes: Variant): Boolean; override;
    function TryConvertFromVariant(const AValue: Variant; out ORes: Single): Boolean; override;

    { Default constructor }
    constructor Create(); override;
  end;

  { Double Support }
  TDoubleType = class sealed(TType<Double>)
  protected
    { Serialization }
    procedure DoSerialize(const AInfo: TValueInfo; const AValue: Double; const AContext: ISerializationContext); override;
    procedure DoDeserialize(const AInfo: TValueInfo; out AValue: Double; const AContext: IDeserializationContext); override;

  public
    { Comparator }
    function Compare(const AValue1, AValue2: Double): Integer; override;

    { Equality Comparator }
    function AreEqual(const AValue1, AValue2: Double): Boolean; override;

    { Hash code provider }
    function GenerateHashCode(const AValue: Double): Integer; override;

    { Get String representation }
    function GetString(const AValue: Double): String; override;

    { Variant Conversion }
    function TryConvertToVariant(const AValue: Double; out ORes: Variant): Boolean; override;
    function TryConvertFromVariant(const AValue: Variant; out ORes: Double): Boolean; override;

    { Default constructor }
    constructor Create(); override;
  end;

  { Extended Support }
  TExtendedType = class sealed(TType<Extended>)
  protected
    { Serialization }
    procedure DoSerialize(const AInfo: TValueInfo; const AValue: Extended; const AContext: ISerializationContext); override;
    procedure DoDeserialize(const AInfo: TValueInfo; out AValue: Extended; const AContext: IDeserializationContext); override;

  public
    { Comparator }
    function Compare(const AValue1, AValue2: Extended): Integer; override;

    { Equality Comparator }
    function AreEqual(const AValue1, AValue2: Extended): Boolean; override;

    { Hash code provider }
    function GenerateHashCode(const AValue: Extended): Integer; override;

    { Get String representation }
    function GetString(const AValue: Extended): String; override;

    { Variant Conversion }
    function TryConvertToVariant(const AValue: Extended; out ORes: Variant): Boolean; override;
    function TryConvertFromVariant(const AValue: Variant; out ORes: Extended): Boolean; override;

    { Default constructor }
    constructor Create(); override;
  end;

  { Comp Support }
  TCompType = class sealed(TType<Comp>)
  protected
    { Serialization }
    procedure DoSerialize(const AInfo: TValueInfo; const AValue: Comp; const AContext: ISerializationContext); override;
    procedure DoDeserialize(const AInfo: TValueInfo; out AValue: Comp; const AContext: IDeserializationContext); override;

  public
    { Comparator }
    function Compare(const AValue1, AValue2: Comp): Integer; override;

    { Equality Comparator }
    function AreEqual(const AValue1, AValue2: Comp): Boolean; override;

    { Hash code provider }
    function GenerateHashCode(const AValue: Comp): Integer; override;

    { Get String representation }
    function GetString(const AValue: Comp): String; override;

    { Variant Conversion }
    function TryConvertToVariant(const AValue: Comp; out ORes: Variant): Boolean; override;
    function TryConvertFromVariant(const AValue: Variant; out ORes: Comp): Boolean; override;

    { Default constructor }
    constructor Create(); override;
  end;

  { Currency Support }
  TCurrencyType = class sealed(TType<Currency>)
  protected
    { Serialization }
    procedure DoSerialize(const AInfo: TValueInfo; const AValue: Currency; const AContext: ISerializationContext); override;
    procedure DoDeserialize(const AInfo: TValueInfo; out AValue: Currency; const AContext: IDeserializationContext); override;

  public
    { Comparator }
    function Compare(const AValue1, AValue2: Currency): Integer; override;

    { Equality Comparator }
    function AreEqual(const AValue1, AValue2: Currency): Boolean; override;

    { Hash code provider }
    function GenerateHashCode(const AValue: Currency): Integer; override;

    { Get String representation }
    function GetString(const AValue: Currency): String; override;

    { Variant Conversion }
    function TryConvertToVariant(const AValue: Currency; out ORes: Variant): Boolean; override;
    function TryConvertFromVariant(const AValue: Variant; out ORes: Currency): Boolean; override;

    { Default constructor }
    constructor Create(); override;
  end;

  { String Support }
  TShortStringType = class sealed(TType<ShortString>)
  private
    FCaseInsensitive: Boolean;

  protected
    { Serialization }
    procedure DoSerialize(const AInfo: TValueInfo; const AValue: ShortString; const AContext: ISerializationContext); override;
    procedure DoDeserialize(const AInfo: TValueInfo; out AValue: ShortString; const AContext: IDeserializationContext); override;

  public
    { Comparator }
    function Compare(const AValue1, AValue2: ShortString): Integer; override;

    { Equality Comparator }
    function AreEqual(const AValue1, AValue2: ShortString): Boolean; override;

    { Hash code provider }
    function GenerateHashCode(const AValue: ShortString): Integer; override;

    { Get String representation }
    function GetString(const AValue: ShortString): String; override;

    { Variant Conversion }
    function TryConvertToVariant(const AValue: ShortString; out ORes: Variant): Boolean; override;
    function TryConvertFromVariant(const AValue: Variant; out ORes: ShortString): Boolean; override;

    { Costructors }
    constructor Create(); overload; override;
    constructor Create(const CaseInsensitive: Boolean); reintroduce; overload;
  end;

  { AnsiString Support }
  TAnsiStringType = class sealed(TMagicType<AnsiString>)
  private
    FCaseInsensitive: Boolean;

  protected
    { Serialization }
    procedure DoSerialize(const AInfo: TValueInfo; const AValue: AnsiString; const AContext: ISerializationContext); override;
    procedure DoDeserialize(const AInfo: TValueInfo; out AValue: AnsiString; const AContext: IDeserializationContext); override;

  public
    { Comparator }
    function Compare(const AValue1, AValue2: AnsiString): Integer; override;

    { Equality Comparator }
    function AreEqual(const AValue1, AValue2: AnsiString): Boolean; override;

    { Hash code provider }
    function GenerateHashCode(const AValue: AnsiString): Integer; override;

    { Get String representation }
    function GetString(const AValue: AnsiString): String; override;

    { Variant Conversion }
    function TryConvertToVariant(const AValue: AnsiString; out ORes: Variant): Boolean; override;
    function TryConvertFromVariant(const AValue: Variant; out ORes: AnsiString): Boolean; override;

    { Costructors }
    constructor Create(); overload; override;
    constructor Create(const CaseInsensitive: Boolean); reintroduce; overload;
  end;

  { WideString Support }
  TWideStringType = class sealed(TMagicType<WideString>)
  private
    FCaseInsensitive: Boolean;

  protected
    { Serialization }
    procedure DoSerialize(const AInfo: TValueInfo; const AValue: WideString; const AContext: ISerializationContext); override;
    procedure DoDeserialize(const AInfo: TValueInfo; out AValue: WideString; const AContext: IDeserializationContext); override;

  public
    { Comparator }
    function Compare(const AValue1, AValue2: WideString): Integer; override;

    { Equality Comparator }
    function AreEqual(const AValue1, AValue2: WideString): Boolean; override;

    { Hash code provider }
    function GenerateHashCode(const AValue: WideString): Integer; override;

    { Get String representation }
    function GetString(const AValue: WideString): String; override;

    { Variant Conversion }
    function TryConvertToVariant(const AValue: WideString; out ORes: Variant): Boolean; override;
    function TryConvertFromVariant(const AValue: Variant; out ORes: WideString): Boolean; override;

    { Costructors }
    constructor Create(); overload; override;
    constructor Create(const CaseInsensitive: Boolean); reintroduce; overload;
  end;

  { UnicodeString Support }
  TUnicodeStringType = class sealed(TMagicType<UnicodeString>)
  private
    FCaseInsensitive: Boolean;

  protected
    { Serialization }
    procedure DoSerialize(const AInfo: TValueInfo; const AValue: UnicodeString; const AContext: ISerializationContext); override;
    procedure DoDeserialize(const AInfo: TValueInfo; out AValue: UnicodeString; const AContext: IDeserializationContext); override;

  public
    { Comparator }
    function Compare(const AValue1, AValue2: UnicodeString): Integer; override;

    { Equality Comparator }
    function AreEqual(const AValue1, AValue2: UnicodeString): Boolean; override;

    { Hash code provider }
    function GenerateHashCode(const AValue: UnicodeString): Integer; override;

    { Get String representation }
    function GetString(const AValue: UnicodeString): String; override;

    { Variant Conversion }
    function TryConvertToVariant(const AValue: UnicodeString; out ORes: Variant): Boolean; override;
    function TryConvertFromVariant(const AValue: Variant; out ORes: UnicodeString): Boolean; override;

    { Costructors }
    constructor Create(); overload; override;
    constructor Create(const CaseInsensitive: Boolean); reintroduce; overload;
  end;

  { String Support - alias the unicode one }
  TStringType = TUnicodeStringType;

  { AnsiString Support }
  TUCS4StringType = class sealed(TMagicType<UCS4String>)
  private
    FCaseInsensitive: Boolean;

  protected
    { Serialization }
    procedure DoSerialize(const AInfo: TValueInfo; const AValue: UCS4String; const AContext: ISerializationContext); override;
    procedure DoDeserialize(const AInfo: TValueInfo; out AValue: UCS4String; const AContext: IDeserializationContext); override;

  public
    { Comparator }
    function Compare(const AValue1, AValue2: UCS4String): Integer; override;

    { Equality Comparator }
    function AreEqual(const AValue1, AValue2: UCS4String): Boolean; override;

    { Hash code provider }
    function GenerateHashCode(const AValue: UCS4String): Integer; override;

    { Get String representation }
    function GetString(const AValue: UCS4String): String; override;

    { Variant Conversion }
    function TryConvertToVariant(const AValue: UCS4String; out ORes: Variant): Boolean; override;
    function TryConvertFromVariant(const AValue: Variant; out ORes: UCS4String): Boolean; override;

    { Costructors }
    constructor Create(); overload; override;
    constructor Create(const CaseInsensitive: Boolean); reintroduce; overload;
  end;

  { WideString Support }
  TUTF8StringType = class sealed(TMagicType<UTF8String>)
  private
    FCaseInsensitive: Boolean;

  protected
    { Serialization }
    procedure DoSerialize(const AInfo: TValueInfo; const AValue: UTF8String; const AContext: ISerializationContext); override;
    procedure DoDeserialize(const AInfo: TValueInfo; out AValue: UTF8String; const AContext: IDeserializationContext); override;

  public
    { Comparator }
    function Compare(const AValue1, AValue2: UTF8String): Integer; override;

    { Equality Comparator }
    function AreEqual(const AValue1, AValue2: UTF8String): Boolean; override;

    { Hash code provider }
    function GenerateHashCode(const AValue: UTF8String): Integer; override;

    { Get String representation }
    function GetString(const AValue: UTF8String): String; override;

    { Variant Conversion }
    function TryConvertToVariant(const AValue: UTF8String; out ORes: Variant): Boolean; override;
    function TryConvertFromVariant(const AValue: Variant; out ORes: UTF8String): Boolean; override;

    { Costructors }
    constructor Create(); overload; override;
    constructor Create(const CaseInsensitive: Boolean); reintroduce; overload;
  end;

  { Interface Support }
  TInterfaceType = class sealed(TMagicType<IInterface>)
  public
    { Comparator }
    function Compare(const AValue1, AValue2: IInterface): Integer; override;

    { Equality Comparator }
    function AreEqual(const AValue1, AValue2: IInterface): Boolean; override;

    { Hash code provider }
    function GenerateHashCode(const AValue: IInterface): Integer; override;

    { Get String representation }
    function GetString(const AValue: IInterface): String; override;

    { Default constructor }
    constructor Create(); override;
  end;

  { Metaclass Support }
  TMetaclassType = class sealed(TType<TClass>)
  public
    { Comparator }
    function Compare(const AValue1, AValue2: TClass): Integer; override;

    { Equality Comparator }
    function AreEqual(const AValue1, AValue2: TClass): Boolean; override;

    { Hash code provider }
    function GenerateHashCode(const AValue: TClass): Integer; override;

    { Get String representation }
    function GetString(const AValue: TClass): String; override;

    { Default constructor }
    constructor Create(); override;
  end;

  { Class Support }
  TClassType<T: class> = class(TType<T>, IClassType<T>)
  private
    FMustKillClass, FCanBeSerialized, FCanBeSerializedVerified: Boolean;

  private
    procedure InternalGetInterface(const AObject: TObject; const AIID: TGUID; var AOut: Pointer);
    procedure CheckSerializable(const AInfo: TValueInfo; const AContext: IContext);

  protected
    { Serialization }
    procedure DoSerialize(const AInfo: TValueInfo; const AValue: T; const AContext: ISerializationContext); override;
    procedure DoDeserialize(const AInfo: TValueInfo; out AValue: T; const AContext: IDeserializationContext); override;

  public
    { Comparator }
    function Compare(const AValue1, AValue2: T): Integer; override;

    { Equality Comparator }
    function AreEqual(const AValue1, AValue2: T): Boolean; override;

    { Hash code provider }
    function GenerateHashCode(const AValue: T): Integer; override;

    { Get String representation }
    function GetString(const AValue: T): String; override;

    { Gets who is responsable for managing the life-time of a type }
    function Management(): TTypeManagement; override;

    { Life-time management toggle }
    procedure SetShouldCleanup(const ShouldCleanup: Boolean);

    { Cleanup the given value }
    procedure Cleanup(var AValue: T); override;

    { Costructors }
    constructor Create(); overload; override;
    constructor Create(const ShouldCleanup: Boolean); reintroduce; overload;
  end;

  { Variant Support }
  TVariantType = class sealed(TMagicType<Variant>)
  public
    { Comparator }
    function Compare(const AValue1, AValue2: Variant): Integer; override;

    { Equality Comparator }
    function AreEqual(const AValue1, AValue2: Variant): Boolean; override;

    { Hash code provider }
    function GenerateHashCode(const AValue: Variant): Integer; override;

    { Get String representation }
    function GetString(const AValue: Variant): String; override;

    { Variant Conversion }
    function TryConvertToVariant(const AValue: Variant; out ORes: Variant): Boolean; override;
    function TryConvertFromVariant(const AValue: Variant; out ORes: Variant): Boolean; override;

    { Default constructor }
    constructor Create(); override;
  end;

  { Binary Support }
  TBinaryType = class sealed(TType<Pointer>)
  protected
    { Serialization }
    procedure DoSerialize(const AInfo: TValueInfo; const AValue: Pointer; const AContext: ISerializationContext); override;
    procedure DoDeserialize(const AInfo: TValueInfo; out AValue: Pointer; const AContext: IDeserializationContext); override;

  public
    { Comparator }
    function Compare(const AValue1, AValue2: Pointer): Integer; override;

    { Equality Comparator }
    function AreEqual(const AValue1, AValue2: Pointer): Boolean; override;

    { Hash code provider }
    function GenerateHashCode(const AValue: Pointer): Integer; override;

    { Get String representation }
    function GetString(const AValue: Pointer): String; override;

    { Constructors }
    constructor Create(); overload; override;
    constructor Create(const Size: Cardinal); reintroduce; overload;
  end;

  __TMethod = procedure of object;

  { Method Support }
  TMethodType = class sealed(TType<__TMethod>)
  public
    { Comparator }
    function Compare(const AValue1, AValue2: __TMethod): Integer; override;

    { Equality Comparator }
    function AreEqual(const AValue1, AValue2: __TMethod): Boolean; override;

    { Hash code provider }
    function GenerateHashCode(const AValue: __TMethod): Integer; override;

    { Get String representation }
    function GetString(const AValue: __TMethod): String; override;

    { Constructors }
    constructor Create(); override;
  end;

  { Procedure Support }
  TProcedureType = class sealed(TType<Pointer>)
  public
    { Comparator }
    function Compare(const AValue1, AValue2: Pointer): Integer; override;

    { Equality Comparator }
    function AreEqual(const AValue1, AValue2: Pointer): Boolean; override;

    { Hash code provider }
    function GenerateHashCode(const AValue: Pointer): Integer; override;

    { Get String representation }
    function GetString(const AValue: Pointer): String; override;

    { Variant Conversion }
    function TryConvertToVariant(const AValue: Pointer; out ORes: Variant): Boolean; override;
    function TryConvertFromVariant(const AValue: Variant; out ORes: Pointer): Boolean; override;

    { Constructors }
    constructor Create(); override;
  end;

  { Static array Support }
  TArrayType<T> = class sealed(TType<T>)
  private
    FIsMagic: Boolean;

  protected
    { Serialization }
    procedure DoSerialize(const AInfo: TValueInfo; const AValue: T; const AContext: ISerializationContext); override;
    procedure DoDeserialize(const AInfo: TValueInfo; out AValue: T; const AContext: IDeserializationContext); override;

  public
    { Comparator }
    function Compare(const AValue1, AValue2: T): Integer; override;

    { Equality Comparator }
    function AreEqual(const AValue1, AValue2: T): Boolean; override;

    { Hash code provider }
    function GenerateHashCode(const AValue: T): Integer; override;

    { Get String representation }
    function GetString(const AValue: T): String; override;

    { Type management override }
    function Management(): TTypeManagement; override;

    { Default constructor }
    constructor Create(); override;
  end;

  { Record Support }
  TRecordType<T> = class(TType<T>)
  private
    FIsMagic, FCanBeSerialized, FCanBeSerializedVerified: Boolean;

    { Check if serializable }
    procedure CheckSerializable(const AContext: IContext; const AInfo: TValueInfo);

  protected
    { Serialization }
    procedure DoSerialize(const AInfo: TValueInfo; const AValue: T; const AContext: ISerializationContext); override;
    procedure DoDeserialize(const AInfo: TValueInfo; out AValue: T; const AContext: IDeserializationContext); override;

  public
    { Comparator }
    function Compare(const AValue1, AValue2: T): Integer; override;

    { Equality Comparator }
    function AreEqual(const AValue1, AValue2: T): Boolean; override;

    { Hash code provider }
    function GenerateHashCode(const AValue: T): Integer; override;

    { Get String representation }
    function GetString(const AValue: T): String; override;

    { Type management override }
    function Management(): TTypeManagement; override;

    { Default constructor }
    constructor Create(); override;
  end;

  __T3BytesRec = packed record
    b1, b2, b3: Byte;
  end;

  { Binary Support }
  T3BytesType = class sealed(TType<__T3BytesRec>)
  public
    { Comparator }
    function Compare(const AValue1, AValue2: __T3BytesRec): Integer; override;

    { Equality Comparator }
    function AreEqual(const AValue1, AValue2: __T3BytesRec): Boolean; override;

    { Hash code provider }
    function GenerateHashCode(const AValue: __T3BytesRec): Integer; override;

    { Get String representation }
    function GetString(const AValue: __T3BytesRec): String; override;

    { Default constructor }
    constructor Create(); override;
  end;

  { Binary Support }
  TDynArrayType = class sealed(TMagicType<TBoundArray>)
  private
    FSizeOfElement: Cardinal;
    FArrayTypeInfo: PTypeInfo;

    { Cannot return nil }
    function GetElementType(const AContext: IContext; const AInfo: TValueInfo): TRttiType;

  protected
    { Serialization }
    procedure DoSerialize(const AInfo: TValueInfo; const AValue: TBoundArray; const AContext: ISerializationContext); override;
    procedure DoDeserialize(const AInfo: TValueInfo; out AValue: TBoundArray; const AContext: IDeserializationContext); override;

  public
    { Comparator }
    function Compare(const AValue1, AValue2: TBoundArray): Integer; override;

    { Equality Comparator }
    function AreEqual(const AValue1, AValue2: TBoundArray): Boolean; override;

    { Hash code provider }
    function GenerateHashCode(const AValue: TBoundArray): Integer; override;

    { Get String representation }
    function GetString(const AValue: TBoundArray): String; override;

    { Variant Conversion }
    function TryConvertToVariant(const AValue: TBoundArray; out ORes: Variant): Boolean; override;
    function TryConvertFromVariant(const AValue: Variant; out ORes: TBoundArray): Boolean; override;

    { Constructor }
    constructor Create(); overload; override;
    constructor Create(const SizeOfElement: Cardinal; const TypeInfo: PTypeInfo); reintroduce; overload;
  end;

  { ******* CUSTOM REGISTERED TYPES ************ }

  { Boolean Support }
  TBooleanType = class sealed(TType<Boolean>)
  protected
    { Serialization }
    procedure DoSerialize(const AInfo: TValueInfo; const AValue: Boolean; const AContext: ISerializationContext); override;
    procedure DoDeserialize(const AInfo: TValueInfo; out AValue: Boolean; const AContext: IDeserializationContext); override;

  public
    { Comparator }
    function Compare(const AValue1, AValue2: Boolean): Integer; override;

    { Equality Comparator }
    function AreEqual(const AValue1, AValue2: Boolean): Boolean; override;

    { Hash code provider }
    function GenerateHashCode(const AValue: Boolean): Integer; override;

    { Get String representation }
    function GetString(const AValue: Boolean): String; override;

    { Variant Conversion }
    function TryConvertToVariant(const AValue: Boolean; out ORes: Variant): Boolean; override;
    function TryConvertFromVariant(const AValue: Variant; out ORes: Boolean): Boolean; override;

    { Constructor }
    constructor Create(); override;
  end;

  { Word Boolean Support }
  TByteBoolType = class sealed(TType<ByteBool>)
  protected
    { Serialization }
    procedure DoSerialize(const AInfo: TValueInfo; const AValue: ByteBool; const AContext: ISerializationContext); override;
    procedure DoDeserialize(const AInfo: TValueInfo; out AValue: ByteBool; const AContext: IDeserializationContext); override;

  public
    { Comparator }
    function Compare(const AValue1, AValue2: ByteBool): Integer; override;

    { Equality Comparator }
    function AreEqual(const AValue1, AValue2: ByteBool): Boolean; override;

    { Hash code provider }
    function GenerateHashCode(const AValue: ByteBool): Integer; override;

    { Get String representation }
    function GetString(const AValue: ByteBool): String; override;

    { Variant Conversion }
    function TryConvertToVariant(const AValue: ByteBool; out ORes: Variant): Boolean; override;
    function TryConvertFromVariant(const AValue: Variant; out ORes: ByteBool): Boolean; override;

    { Constructor }
    constructor Create(); override;
  end;

  { Word Boolean Support }
  TWordBoolType = class sealed(TType<WordBool>)
  protected
    { Serialization }
    procedure DoSerialize(const AInfo: TValueInfo; const AValue: WordBool; const AContext: ISerializationContext); override;
    procedure DoDeserialize(const AInfo: TValueInfo; out AValue: WordBool; const AContext: IDeserializationContext); override;

  public
    { Comparator }
    function Compare(const AValue1, AValue2: WordBool): Integer; override;

    { Equality Comparator }
    function AreEqual(const AValue1, AValue2: WordBool): Boolean; override;

    { Hash code provider }
    function GenerateHashCode(const AValue: WordBool): Integer; override;

    { Get String representation }
    function GetString(const AValue: WordBool): String; override;

    { Variant Conversion }
    function TryConvertToVariant(const AValue: WordBool; out ORes: Variant): Boolean; override;
    function TryConvertFromVariant(const AValue: Variant; out ORes: WordBool): Boolean; override;

    { Constructor }
    constructor Create(); override;
  end;

  { Long Boolean Support }
  TLongBoolType = class sealed(TType<LongBool>)
  protected
    { Serialization }
    procedure DoSerialize(const AInfo: TValueInfo; const AValue: LongBool; const AContext: ISerializationContext); override;
    procedure DoDeserialize(const AInfo: TValueInfo; out AValue: LongBool; const AContext: IDeserializationContext); override;

  public
    { Comparator }
    function Compare(const AValue1, AValue2: LongBool): Integer; override;

    { Equality Comparator }
    function AreEqual(const AValue1, AValue2: LongBool): Boolean; override;

    { Hash code provider }
    function GenerateHashCode(const AValue: LongBool): Integer; override;

    { Get String representation }
    function GetString(const AValue: LongBool): String; override;

    { Variant Conversion }
    function TryConvertToVariant(const AValue: LongBool; out ORes: Variant): Boolean; override;
    function TryConvertFromVariant(const AValue: Variant; out ORes: LongBool): Boolean; override;

    { Constructor }
    constructor Create(); override;
  end;

  { Date Support }
  TDateType = class sealed(TType<TDate>)
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

    { Variant Conversion }
    function TryConvertToVariant(const AValue: TDate; out ORes: Variant): Boolean; override;
    function TryConvertFromVariant(const AValue: Variant; out ORes: TDate): Boolean; override;

    { Constructor }
    constructor Create(); override;
  end;

  { Time Support }
  TTimeType = class sealed(TType<TTime>)
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

    { Variant Conversion }
    function TryConvertToVariant(const AValue: TTime; out ORes: Variant): Boolean; override;
    function TryConvertFromVariant(const AValue: Variant; out ORes: TTime): Boolean; override;

    { Constructor }
    constructor Create(); override;
  end;

  { Time Support }
  TDateTimeType = class sealed(TType<TDateTime>)
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

    { Variant Conversion }
    function TryConvertToVariant(const AValue: TDateTime; out ORes: Variant): Boolean; override;
    function TryConvertFromVariant(const AValue: Variant; out ORes: TDateTime): Boolean; override;

    { Constructor }
    constructor Create(); override;
  end;

  { Raw-byte String }
  TRawByteStringType = class sealed(TMagicType<RawByteString>)
  protected
    { Serialization }
    procedure DoSerialize(const AInfo: TValueInfo; const AValue: RawByteString; const AContext: ISerializationContext); override;
    procedure DoDeserialize(const AInfo: TValueInfo; out AValue: RawByteString; const AContext: IDeserializationContext); override;

  public
    { Comparator }
    function Compare(const AValue1, AValue2: RawByteString): Integer; override;

    { Equality Comparator }
    function AreEqual(const AValue1, AValue2: RawByteString): Boolean; override;

    { Hash code provider }
    function GenerateHashCode(const AValue: RawByteString): Integer; override;

    { Get String representation }
    function GetString(const AValue: RawByteString): String; override;

    { Constructor }
    constructor Create(); override;
  end;


{ Some utility functions that can be used externally }
function BinaryHash(Data: Pointer; Size: Integer): Integer;
function BinaryCompare(const Left, Right: Pointer; Size: Integer): Integer;

implementation
uses Windows,
     StrUtils,
     Character,
     Variants,
     AnsiStrings,
     DateUtils;

type
  TInternalDictionary = TType.TPointerDictionary;
  TExtensionDictionary = TType.TPointerDictionary;

{ Binary Support functions }
{$Q- R-}
function BinaryHash(Data: Pointer; Size: Integer): Integer;
const
  MAGIC_CONST = $7ED55D16;
var
  I        : Integer;
  Remaining: Integer;
  Temp     : Integer;
begin
  if (Size <= 0) or (Data = nil) then
  begin
    Result := 0;
    Exit;
  end;

  Result := MAGIC_CONST;
  Remaining := Size and 3;
  Size := Integer(Longword(Size) shr 2);

  for I := 0 to Size -1 do
  begin
    Inc(Result, PWord(Data)^);
    Temp := (PWordArray(Data)^[1] shl 11) xor Result;
    Result := (Result shl 16) xor Temp;
    Inc(Result, Result shr 11);
    Inc(PWord(Data), 2);
  end;

  case Remaining of
    3:
    begin
      Inc(Result, PWord(Data)^);
      Result := Result xor (Result shl 16);
      Result := Result xor (PByteArray(Data)^[2] shl 18);
      Inc(Result, Result shr 11);
    end;
    2:
    begin
      Inc(Result, PWord(Data)^);
      Result := Result xor (Result shl 11);
      Inc(Result, Result shr 17);
    end;
    1:
    begin
      Inc(Result, PByte(Data)^);
      Result := Result xor (Result shl 10);
      Inc(Result, Result shr 1);
    end;
  end;

  Result := Result xor (Result shl 3);
  Inc(Result, Result shr 5);
  Result := Result xor (Result shl 4);
  Inc(Result, Result shr 17);
  Result := Result xor (Result shl 25);
  Inc(Result, Result shr 6);
  Result := Result;
end;

function BinaryCompare(const Left, Right: Pointer; Size: Integer): Integer;
var
  pl, pr: PByte;
  len: Integer;
begin
  pl := Left;
  pr := Right;
  len := Size;
  while len > 0 do
  begin
    Result := pl^ - pr^;
    if Result <> 0 then
      Exit;
    Dec(len);
    Inc(pl);
    Inc(pr);
  end;
  Result := 0;
end;
{$Q+ R+}

{ Dynamic Array Support functions }

function DynArrayLength(const DynArray: Pointer): Integer; inline;
begin
  Result := 0;

  if DynArray = nil then
     Exit;

  Result := PInteger(PByte(DynArray) - SizeOf(Integer))^;
end;

{ TType<T> }

class procedure TType<T>.Register(const AType: TTypeClass);
var
  PInfo: PTypeInfo;
  Dict: TInternalDictionary;
begin
  { Extract type information }
  PInfo := System.TypeInfo(T);

  { Check for nil }
  if PInfo = nil then
    ExceptionHelper.Throw_CustomTypeHasNoRTTI();

  if AType = nil then
    ExceptionHelper.Throw_ArgumentNilError('AType');

  MonitorEnter(FCustomTypes);

  { Type-cast to what wee need }
  Dict := TInternalDictionary(FCustomTypes);

  { Check if this class is not registered yet }
  if Dict.ContainsKey(PInfo) then
  begin
    MonitorExit(FCustomTypes);
    ExceptionHelper.Throw_CustomTypeAlreadyRegistered(GetTypeName(PInfo));
  end;

  { Dispose of the old cached instance and add the new one }
  DisposeCachedDefaultInstance();
  Dict.Add(PInfo, AType);

  MonitorExit(FCustomTypes);
end;

class function TType<T>.Standard: IType<T>;
begin
  { Create a new class and extract an interface from it }
  Result := CreateDefault(false);
end;

procedure TType<T>.Serialize(const ALabel: String; const AValue: T; const AData: TSerializationData);
begin
  { Call the other serialize method }
  Serialize(TValueInfo.Create(ALabel), AValue, AData.Context);
end;

procedure TType<T>.Serialize(const AValue: T; const AData: TSerializationData);
begin
  { Call the other serialize method }
  Serialize(AData.CurrentElementInfo, AValue, AData.Context);
end;

class function TType<T>.Standard(const AllowedFamilies: TTypeFamilySet): IType<T>;
var
  Support: IType<T>;
begin
  { First, call the original method }
  Support := Standard();
  Support.RestrictTo(AllowedFamilies);
end;

class procedure TType<T>.Unregister();
var
  PInfo: PTypeInfo;
  Dict: TInternalDictionary;

begin
  { Extract type information }
  PInfo := System.TypeInfo(T);

  { Check for nil }
  if PInfo = nil then
    ExceptionHelper.Throw_CustomTypeHasNoRTTI();

  MonitorEnter(FCustomTypes);

  { Type-cast to what wee need }
  Dict := TInternalDictionary(FCustomTypes);

  { Check if this class is not registered yet }

{  if not Dict.ContainsKey(PInfo) then
  begin
    MonitorExit(FCustomTypes);
    ExceptionHelper.Throw_CustomTypeNotYetRegistered(GetTypeName(PInfo));
  end; }

  { Dispose of the old cached instance and remove from dictionary }
  DisposeCachedDefaultInstance();
  Dict.Remove(PInfo);

  MonitorExit(FCustomTypes);
end;

function TType<T>.TryConvertFromVariant(const AValue: Variant; out ORes: T): Boolean;
begin
  { Unsupported by default }
  Result := false;
end;

function TType<T>.TryConvertToVariant(const AValue: T; out ORes: Variant): Boolean;
begin
  { Unsupported by default }
  Result := False;
end;

procedure TType<T>.Serialize(const AInfo: TValueInfo; const AValue: T; const AContext: ISerializationContext);
begin
  { Check scope }
  if AContext = nil then
    ExceptionHelper.Throw_ArgumentNilError('AContext');

  { Call the actual code }
  DoSerialize(AInfo, AValue, AContext);
end;

procedure TType<T>.InternalDeserialize(const AInfo: TValueInfo; const APtrToValue: Pointer; const AContext: IDeserializationContext);
begin
  { Call the normal serialization }
  DoDeserialize(AInfo, TValRef(APtrToValue)^, AContext);
end;

procedure TType<T>.InternalSerialize(const AInfo: TValueInfo; const APtrToValue: Pointer; const AContext: ISerializationContext);
begin
  { Call the normal serialization }
  DoSerialize(AInfo, TValRef(APtrToValue)^, AContext);
end;

procedure TType<T>.Cleanup(var AValue: T);
begin
  { Nothing ... }
end;

function TType<T>.ConvertFromVariant(const AValue: Variant): T;
begin
  if not TryConvertFromVariant(AValue, Result) then
     ExceptionHelper.Throw_ConversionNotSupported(Name);
end;

function TType<T>.ConvertToVariant(const AValue: T): Variant;
begin
  if not TryConvertToVariant(AValue, Result) then
     ExceptionHelper.Throw_ConversionNotSupported('Variant');
end;

constructor TType<T>.Create;
begin
  { Defaults, are overriden where necessary }
  FTypeInfo := System.TypeInfo(T);
  FTypeSize := SizeOf(T);
  FTypeFamily := tfUnknown;

  if FTypeInfo <> nil then
    FTypeName := GetTypeName(FTypeInfo);
end;

class function TType<T>.CreateDefault(const AllowCustom: Boolean): TType<T>;
var
  Instance: TType<T>;
  FieldAddress: Pointer;
begin
  { Select the appropriate cached value }
  if AllowCustom then
  begin
    Result := FCachedDefaultInstance;
    FieldAddress := @FCachedDefaultInstance;
  end else
  begin
    Result := FCachedStandardInstance;
    FieldAddress := @FCachedStandardInstance;
  end;

  { No cached instance }
  if Result = nil then
  begin
    { Create a new type instance }
    Instance := TType.CreateDefault(System.TypeInfo(T), SizeOf(T), AllowCustom, TArrayType<T>, TRecordType<T>);
    Result := InterlockedCompareExchangePointer(Pointer(FieldAddress^), Instance, nil);

    { Select the type class }
    if Result = nil then
    begin
      Result := Instance;

      { Store the interface }
      FStoredInterfaces.Add(Result as IInterface);
    end else
      Instance.Free;
  end;
end;

class function TType<T>.Default(const AllowedFamilies: TTypeFamilySet): IType<T>;
var
  Support: IType<T>;
begin
  { First, call the original method }
  Support := Default;
  Support.RestrictTo(AllowedFamilies);
end;

procedure TType<T>.Deserialize(const ALabel: String; out AValue: T; const AData: TDeserializationData);
begin
  { Call the other serialize method }
  Deserialize(TValueInfo.Create(ALabel), AValue, AData.Context);
end;

procedure TType<T>.Deserialize(out AValue: T; const AData: TDeserializationData);
begin
  { Call the other serialize method }
  Deserialize(AData.CurrentElementInfo, AValue, AData.Context);
end;

procedure TType<T>.Deserialize(const AInfo: TValueInfo; out AValue: T; const AContext: IDeserializationContext);
begin
  { Check scope }
  if AContext = nil then
    ExceptionHelper.Throw_ArgumentNilError('AContext');

  { Call the actual code }
  DoDeserialize(AInfo, AValue, AContext);
end;

class procedure TType<T>.DisposeCachedDefaultInstance;
var
  Instance: TType<T>;
begin
  { Obtain the last cached value and set it to NIL }
  Instance := InterlockedExchangePointer(Pointer(FCachedDefaultInstance), nil);

  { If the instance is valid, destroy it by decreasing the ref count }
  if Instance <> nil then
    FStoredInterfaces.Remove(Instance as IInterface);
end;

procedure TType<T>.DoDeserialize(const AInfo: TValueInfo; out AValue: T; const AContext: IDeserializationContext);
begin
  { Unsupported by default }
  ExceptionHelper.Throw_Unserializable(AInfo.Name, FTypeName);
end;

procedure TType<T>.DoSerialize(const AInfo: TValueInfo; const AValue: T; const AContext: ISerializationContext);
begin
  { Unsupported by default }
  ExceptionHelper.Throw_Unserializable(AInfo.Name, FTypeName);
end;

class function TType<T>.IsRegistered(): Boolean;
var
  PInfo: PTypeInfo;
  Dict: TInternalDictionary;

begin
  { Extract type information }
  PInfo := System.TypeInfo(T);
  Result := false;

  { Check for nil }
  if PInfo = nil then
    Exit;

  MonitorEnter(FCustomTypes);

  { Type-cast to what wee need }
  Dict := TInternalDictionary(FCustomTypes);

  { Check if this class is not registered yet }
  if Dict.ContainsKey(PInfo) then
    Result := true;

  MonitorExit(FCustomTypes);
end;

{ TType<T> }

class function TType<T>.Default: IType<T>;
begin
  { Create a new class and extract an interface from it }
  Result := CreateDefault(true);
end;

{ TIntegerType }

function TIntegerType.Compare(const AValue1, AValue2: Integer): Integer;
begin
  if AValue1 > AValue2 then
     Result := 1
  else if AValue1 < AValue2 then
     Result := -1
  else
     Result := 0;
end;

constructor TIntegerType.Create;
begin
  inherited;
  FTypeFamily := tfSignedInteger;
end;

function TIntegerType.GenerateHashCode(const AValue: Integer): Integer;
begin
  Result := AValue;
end;

function TIntegerType.GetString(const AValue: Integer): String;
begin
  Result := IntToStr(AValue);
end;

function TIntegerType.AreEqual(const AValue1, AValue2: Integer): Boolean;
begin
  Result := AValue1 = AValue2;
end;

function TIntegerType.TryConvertFromVariant(const AValue: Variant; out ORes: Integer): Boolean;
begin
  { Variant type-cast }
  try
    ORes := AValue;
  except
    Exit(false);
  end;

  Result := true;
end;

function TIntegerType.TryConvertToVariant(const AValue: Integer; out ORes: Variant): Boolean;
begin
  { Simple variant assignment }
  ORes := AValue;
  Result := true;
end;

procedure TIntegerType.DoDeserialize(const AInfo: TValueInfo; out AValue: Integer; const AContext: IDeserializationContext);
begin
  AContext.GetValue(AInfo, AValue);
end;

procedure TIntegerType.DoSerialize(const AInfo: TValueInfo; const AValue: Integer; const AContext: ISerializationContext);
begin
  AContext.AddValue(AInfo, AValue);
end;

{ TDoubleType }

function TDoubleType.AreEqual(const AValue1, AValue2: Double): Boolean;
begin
  Result := (AValue1 = AValue2);
end;

function TDoubleType.Compare(const AValue1, AValue2: Double): Integer;
begin
  if AValue1 < AValue2 then
     Result := -1
  else if AValue1 > AValue2 then
     Result := 1
  else
     Result := 0;
end;

constructor TDoubleType.Create;
begin
  inherited;
  FTypeFamily := tfReal;
end;

function TDoubleType.GenerateHashCode(const AValue: Double): Integer;
var
  LongOp: array[0..1] of Integer absolute AValue;
begin
  if AValue = 0 then
     Result := 0
  else
     Result := LongOp[1] xor LongOp[0];
end;


function TDoubleType.GetString(const AValue: Double): String;
begin
  Result := FloatToStr(AValue);
end;

function TDoubleType.TryConvertFromVariant(const AValue: Variant; out ORes: Double): Boolean;
begin
  { Variant type-cast }
  try
    ORes := AValue;
  except
    Exit(false);
  end;

  Result := true;
end;

function TDoubleType.TryConvertToVariant(const AValue: Double; out ORes: Variant): Boolean;
begin
  { Simple variant assignment }
  ORes := AValue;
  Result := true;
end;

procedure TDoubleType.DoDeserialize(const AInfo: TValueInfo; out AValue: Double; const AContext: IDeserializationContext);
begin
  AContext.GetValue(AInfo, AValue);
end;

procedure TDoubleType.DoSerialize(const AInfo: TValueInfo; const AValue: Double; const AContext: ISerializationContext);
begin
  AContext.AddValue(AInfo, AValue);
end;

{ TByteType }

function TByteType.AreEqual(const AValue1, AValue2: Byte): Boolean;
begin
  Result := (AValue1 = AValue2);
end;

function TByteType.Compare(const AValue1, AValue2: Byte): Integer;
begin
  Result := Integer(AValue1) - Integer(AValue2);
end;

constructor TByteType.Create;
begin
  inherited;
  FTypeFamily := tfUnsignedInteger;
end;

function TByteType.GenerateHashCode(const AValue: Byte): Integer;
begin
  Result := AValue;
end;

function TByteType.GetString(const AValue: Byte): String;
begin
  Result := IntToStr(AValue);
end;

function TByteType.TryConvertFromVariant(const AValue: Variant; out ORes: Byte): Boolean;
begin
  { Variant type-cast }
  try
    ORes := AValue;
  except
    Exit(false);
  end;

  Result := true;
end;

function TByteType.TryConvertToVariant(const AValue: Byte; out ORes: Variant): Boolean;
begin
  { Simple variant assignment }
  ORes := AValue;
  Result := true;
end;

procedure TByteType.DoDeserialize(const AInfo: TValueInfo; out AValue: Byte; const AContext: IDeserializationContext);
begin
  AContext.GetValue(AInfo, AValue);
end;

procedure TByteType.DoSerialize(const AInfo: TValueInfo; const AValue: Byte; const AContext: ISerializationContext);
begin
  AContext.AddValue(AInfo, AValue);
end;

{ TShortIntType }

function TShortIntType.AreEqual(const AValue1, AValue2: ShortInt): Boolean;
begin
  Result := (AValue1 = AValue2);
end;

function TShortIntType.Compare(const AValue1, AValue2: ShortInt): Integer;
begin
  Result := Integer(AValue1) - Integer(AValue2);
end;

constructor TShortIntType.Create;
begin
  inherited;
  FTypeFamily := tfSignedInteger;
end;

function TShortIntType.GenerateHashCode(const AValue: ShortInt): Integer;
begin
  Result := AValue;
end;

function TShortIntType.GetString(const AValue: ShortInt): String;
begin
  Result := IntToStr(AValue);
end;

function TShortIntType.TryConvertFromVariant(const AValue: Variant; out ORes: ShortInt): Boolean;
begin
  { Variant type-cast }
  try
    ORes := AValue;
  except
    Exit(false);
  end;

  Result := true;
end;

function TShortIntType.TryConvertToVariant(const AValue: ShortInt; out ORes: Variant): Boolean;
begin
  { Simple variant assignment }
  ORes := AValue;
  Result := true;
end;

procedure TShortIntType.DoDeserialize(const AInfo: TValueInfo; out AValue: ShortInt; const AContext: IDeserializationContext);
begin
  AContext.GetValue(AInfo, AValue);
end;

procedure TShortIntType.DoSerialize(const AInfo: TValueInfo; const AValue: ShortInt; const AContext: ISerializationContext);
begin
  AContext.AddValue(AInfo, AValue);
end;

{ TWordType }

function TWordType.AreEqual(const AValue1, AValue2: Word): Boolean;
begin
  Result := (AValue1 = AValue2);
end;

function TWordType.Compare(const AValue1, AValue2: Word): Integer;
begin
  Result := Integer(AValue1) - Integer(AValue2);
end;

constructor TWordType.Create;
begin
  inherited;
  FTypeFamily := tfUnsignedInteger;
end;

function TWordType.GenerateHashCode(const AValue: Word): Integer;
begin
  Result := AValue;
end;

function TWordType.GetString(const AValue: Word): String;
begin
  Result := IntToStr(AValue);
end;

function TWordType.TryConvertFromVariant(const AValue: Variant; out ORes: Word): Boolean;
begin
  { Variant type-cast }
  try
    ORes := AValue;
  except
    Exit(false);
  end;

  Result := true;
end;

function TWordType.TryConvertToVariant(const AValue: Word; out ORes: Variant): Boolean;
begin
  { Simple variant assignment }
  ORes := AValue;
  Result := true;
end;

procedure TWordType.DoDeserialize(const AInfo: TValueInfo; out AValue: Word; const AContext: IDeserializationContext);
begin
  AContext.GetValue(AInfo, AValue);
end;

procedure TWordType.DoSerialize(const AInfo: TValueInfo; const AValue: Word; const AContext: ISerializationContext);
begin
  AContext.AddValue(AInfo, AValue);
end;

{ TSmallIntType }

function TSmallIntType.AreEqual(const AValue1, AValue2: SmallInt): Boolean;
begin
  Result := (AValue1 = AValue2);
end;

function TSmallIntType.Compare(const AValue1, AValue2: SmallInt): Integer;
begin
  Result := Integer(AValue1) - Integer(AValue2);
end;

constructor TSmallIntType.Create;
begin
  inherited;
  FTypeFamily := tfSignedInteger;
end;

function TSmallIntType.GenerateHashCode(const AValue: SmallInt): Integer;
begin
  Result := AValue;
end;

function TSmallIntType.GetString(const AValue: SmallInt): String;
begin
  Result := IntToStr(AValue);
end;

function TSmallIntType.TryConvertFromVariant(const AValue: Variant; out ORes: SmallInt): Boolean;
begin
  { Variant type-cast }
  try
    ORes := AValue;
  except
    Exit(false);
  end;

  Result := true;
end;

function TSmallIntType.TryConvertToVariant(const AValue: SmallInt; out ORes: Variant): Boolean;
begin
  { Simple variant assignment }
  ORes := AValue;
  Result := true;
end;

procedure TSmallIntType.DoDeserialize(const AInfo: TValueInfo; out AValue: SmallInt; const AContext: IDeserializationContext);
begin
  AContext.GetValue(AInfo, AValue);
end;

procedure TSmallIntType.DoSerialize(const AInfo: TValueInfo; const AValue: SmallInt; const AContext: ISerializationContext);
begin
  AContext.AddValue(AInfo, AValue);
end;

{ TCardinalType }

function TCardinalType.AreEqual(const AValue1, AValue2: Cardinal): Boolean;
begin
  Result := (AValue1 = AValue2);
end;

function TCardinalType.Compare(const AValue1, AValue2: Cardinal): Integer;
begin
  if AValue1 > AValue2 then
     Result := 1
  else if AValue1 < AValue2 then
     Result := -1
  else
     Result := 0;
end;

constructor TCardinalType.Create;
begin
  inherited;
  FTypeFamily := tfUnsignedInteger;
end;

function TCardinalType.GenerateHashCode(const AValue: Cardinal): Integer;
var
  I: Integer absolute AValue;
begin
  Result := I;
end;

function TCardinalType.GetString(const AValue: Cardinal): String;
begin
  Result := IntToStr(AValue);
end;

function TCardinalType.TryConvertFromVariant(const AValue: Variant; out ORes: Cardinal): Boolean;
begin
  { Variant type-cast }
  try
    ORes := AValue;
  except
    Exit(false);
  end;

  Result := true;
end;

function TCardinalType.TryConvertToVariant(const AValue: Cardinal; out ORes: Variant): Boolean;
begin
  { Simple variant assignment }
  ORes := AValue;
  Result := true;
end;

procedure TCardinalType.DoDeserialize(const AInfo: TValueInfo; out AValue: Cardinal; const AContext: IDeserializationContext);
begin
  AContext.GetValue(AInfo, AValue);
end;

procedure TCardinalType.DoSerialize(const AInfo: TValueInfo; const AValue: Cardinal; const AContext: ISerializationContext);
begin
  AContext.AddValue(AInfo, AValue);
end;

{ TInt64Type }

function TInt64Type.AreEqual(const AValue1, AValue2: Int64): Boolean;
begin
  Result := (AValue1 = AValue2);
end;

function TInt64Type.Compare(const AValue1, AValue2: Int64): Integer;
begin
  if AValue1 > AValue2 then
     Result := 1
  else if AValue1 < AValue2 then
     Result := -1
  else
     Result := 0;
end;

constructor TInt64Type.Create;
begin
  inherited;
  FTypeFamily := tfSignedInteger;
end;

function TInt64Type.GenerateHashCode(const AValue: Int64): Integer;
var
  I: array[0..1] of Integer absolute AValue;
begin
  Result := I[0] xor I[1];
end;

function TInt64Type.GetString(const AValue: Int64): String;
begin
  Result := IntToStr(AValue);
end;

function TInt64Type.TryConvertFromVariant(const AValue: Variant; out ORes: Int64): Boolean;
begin
  { Variant type-cast }
  try
    ORes := AValue;
  except
    Exit(false);
  end;

  Result := true;
end;

function TInt64Type.TryConvertToVariant(const AValue: Int64; out ORes: Variant): Boolean;
begin
  { Simple variant assignment }
  ORes := AValue;
  Result := true;
end;

procedure TInt64Type.DoDeserialize(const AInfo: TValueInfo; out AValue: Int64; const AContext: IDeserializationContext);
begin
  AContext.GetValue(AInfo, AValue);
end;

procedure TInt64Type.DoSerialize(const AInfo: TValueInfo; const AValue: Int64; const AContext: ISerializationContext);
begin
  AContext.AddValue(AInfo, AValue);
end;

{ TUInt64Type }

function TUInt64Type.AreEqual(const AValue1, AValue2: UInt64): Boolean;
begin
  Result := (AValue1 = AValue2);
end;

function TUInt64Type.Compare(const AValue1, AValue2: UInt64): Integer;
begin
  if AValue1 > AValue2 then
     Result := 1
  else if AValue1 < AValue2 then
     Result := -1
  else
     Result := 0;
end;

constructor TUInt64Type.Create;
begin
  inherited;
  FTypeFamily := tfUnsignedInteger;
end;

function TUInt64Type.GenerateHashCode(const AValue: UInt64): Integer;
var
  I: array[0..1] of Integer absolute AValue;
begin
  Result := I[0] xor I[1];
end;

function TUInt64Type.GetString(const AValue: UInt64): String;
begin
  Result := IntToStr(AValue);
end;

function TUInt64Type.TryConvertFromVariant(const AValue: Variant; out ORes: UInt64): Boolean;
begin
  { Variant type-cast }
  try
    ORes := AValue;
  except
    Exit(false);
  end;

  Result := true;
end;

function TUInt64Type.TryConvertToVariant(const AValue: UInt64; out ORes: Variant): Boolean;
begin
  { Simple variant assignment }
  ORes := AValue;
  Result := true;
end;

procedure TUInt64Type.DoDeserialize(const AInfo: TValueInfo; out AValue: UInt64; const AContext: IDeserializationContext);
begin
  AContext.GetValue(AInfo, AValue);
end;

procedure TUInt64Type.DoSerialize(const AInfo: TValueInfo; const AValue: UInt64; const AContext: ISerializationContext);
begin
  AContext.AddValue(AInfo, AValue);
end;

{ TSingleType }

function TSingleType.AreEqual(const AValue1, AValue2: Single): Boolean;
begin
  Result := (AValue1 = AValue2);
end;

function TSingleType.Compare(const AValue1, AValue2: Single): Integer;
begin
  if AValue1 < AValue2 then
     Result := -1
  else if AValue1 > AValue2 then
     Result := 1
  else
     Result := 0;
end;

constructor TSingleType.Create;
begin
  inherited;
  FTypeFamily := tfReal;
end;

function TSingleType.GenerateHashCode(const AValue: Single): Integer;
var
  LongOp: Integer absolute AValue;
begin
  Result := LongOp;
end;

function TSingleType.GetString(const AValue: Single): String;
begin
  Result := FloatToStr(AValue);
end;

function TSingleType.TryConvertFromVariant(const AValue: Variant; out ORes: Single): Boolean;
begin
  { Variant type-cast }
  try
    ORes := AValue;
  except
    Exit(false);
  end;

  Result := true;
end;

function TSingleType.TryConvertToVariant(const AValue: Single; out ORes: Variant): Boolean;
begin
  { Simple variant assignment }
  ORes := AValue;
  Result := true;
end;

procedure TSingleType.DoDeserialize(const AInfo: TValueInfo; out AValue: Single; const AContext: IDeserializationContext);
begin
  AContext.GetValue(AInfo, AValue);
end;

procedure TSingleType.DoSerialize(const AInfo: TValueInfo; const AValue: Single; const AContext: ISerializationContext);
begin
  AContext.AddValue(AInfo, AValue);
end;

{ TExtendedType }

function TExtendedType.AreEqual(const AValue1,
  AValue2: Extended): Boolean;
begin
  Result := (AValue1 = AValue2);
end;

function TExtendedType.Compare(const AValue1,
  AValue2: Extended): Integer;
begin
  if AValue1 < AValue2 then
     Result := -1
  else if AValue1 > AValue2 then
     Result := 1
  else
     Result := 0;
end;

constructor TExtendedType.Create;
begin
  inherited;
  FTypeFamily := tfReal;
end;

function TExtendedType.GenerateHashCode(const AValue: Extended): Integer;
var
  Words: array[0..4] of Word absolute AValue;
  Ints : array[0..1] of Integer absolute Words;

begin
  if AValue = 0 then
     Result := 0
  else
     Result := Ints[0] xor Ints[1] xor Words[4];
end;

function TExtendedType.GetString(const AValue: Extended): String;
begin
  Result := FloatToStr(AValue);
end;

function TExtendedType.TryConvertFromVariant(const AValue: Variant; out ORes: Extended): Boolean;
begin
  { Variant type-cast }
  try
    ORes := AValue;
  except
    Exit(false);
  end;

  Result := true;
end;

function TExtendedType.TryConvertToVariant(const AValue: Extended; out ORes: Variant): Boolean;
begin
  { Possible overflow }
  try
    ORes := AValue;
  except
    Exit(false);
  end;

  Result := true;
end;

procedure TExtendedType.DoDeserialize(const AInfo: TValueInfo; out AValue: Extended; const AContext: IDeserializationContext);
begin
  AContext.GetValue(AInfo, AValue);
end;

procedure TExtendedType.DoSerialize(const AInfo: TValueInfo; const AValue: Extended; const AContext: ISerializationContext);
begin
  AContext.AddValue(AInfo, AValue);
end;

{ TCompType }

function TCompType.AreEqual(const AValue1, AValue2: Comp): Boolean;
begin
  Result := (AValue1 = AValue2);
end;

function TCompType.Compare(const AValue1, AValue2: Comp): Integer;
begin
  if AValue1 < AValue2 then
     Result := -1
  else if AValue1 > AValue2 then
     Result := 1
  else
     Result := 0;
end;

constructor TCompType.Create;
begin
  inherited;

  FTypeFamily := tfReal;
end;

function TCompType.GenerateHashCode(const AValue: Comp): Integer;
var
  LongOp: array[0..1] of Integer absolute AValue;
begin
  if AValue = 0 then
     Result := 0
  else
     Result := LongOp[1] xor LongOp[0];
end;

function TCompType.GetString(const AValue: Comp): String;
begin
  Result := FloatToStr(AValue);
end;

function TCompType.TryConvertFromVariant(const AValue: Variant; out ORes: Comp): Boolean;
begin
  { Variant type-cast }
  try
    ORes := AValue;
  except
    Exit(false);
  end;

  Result := true;
end;

function TCompType.TryConvertToVariant(const AValue: Comp; out ORes: Variant): Boolean;
begin
  { Simple variant assignment }
  ORes := AValue;
  Result := true;
end;

procedure TCompType.DoDeserialize(const AInfo: TValueInfo; out AValue: Comp; const AContext: IDeserializationContext);
begin
  AContext.GetValue(AInfo, AValue);
end;

procedure TCompType.DoSerialize(const AInfo: TValueInfo; const AValue: Comp; const AContext: ISerializationContext);
begin
  AContext.AddValue(AInfo, AValue);
end;

{ TCurrencyType }

function TCurrencyType.AreEqual(const AValue1,
  AValue2: Currency): Boolean;
begin
  Result := (AValue1 = AValue2);
end;

function TCurrencyType.Compare(const AValue1,
  AValue2: Currency): Integer;
begin
  if AValue1 < AValue2 then
     Result := -1
  else if AValue1 > AValue2 then
     Result := 1
  else
     Result := 0;
end;

constructor TCurrencyType.Create;
begin
  inherited;
  FTypeFamily := tfReal;
end;

function TCurrencyType.GenerateHashCode(const AValue: Currency): Integer;
var
  LongOp: array[0..1] of Integer absolute AValue;
begin
  if AValue = 0 then
     Result := 0
  else
     Result := LongOp[1] xor LongOp[0];
end;

function TCurrencyType.GetString(const AValue: Currency): String;
begin
  Result := FloatToStr(AValue);
end;

function TCurrencyType.TryConvertFromVariant(const AValue: Variant; out ORes: Currency): Boolean;
begin
  { Variant type-cast }
  try
    ORes := AValue;
  except
    Exit(false);
  end;

  Result := true;
end;

function TCurrencyType.TryConvertToVariant(const AValue: Currency; out ORes: Variant): Boolean;
begin
  { Simple variant assignment }
  ORes := AValue;
  Result := true;
end;

procedure TCurrencyType.DoDeserialize(const AInfo: TValueInfo; out AValue: Currency; const AContext: IDeserializationContext);
begin
  AContext.GetValue(AInfo, AValue);
end;

procedure TCurrencyType.DoSerialize(const AInfo: TValueInfo; const AValue: Currency; const AContext: ISerializationContext);
begin
  AContext.AddValue(AInfo, AValue);
end;

{ TAnsiStringType }

function TAnsiStringType.AreEqual(const AValue1,
  AValue2: AnsiString): Boolean;
begin
  Result := (Compare(AValue1, AValue2) = 0);
end;

function TAnsiStringType.Compare(const AValue1,
  AValue2: AnsiString): Integer;
begin
  if FCaseInsensitive then
    Result := AnsiCompareText(AValue1, AValue2)
  else
    Result := AnsiCompareStr(AValue1, AValue2);
end;

constructor TAnsiStringType.Create;
begin
  inherited;

  FTypeFamily := tfString;
  FCaseInsensitive := false;
end;

constructor TAnsiStringType.Create(const CaseInsensitive: Boolean);
begin
  inherited Create();

  FTypeFamily := tfString;
  FCaseInsensitive := CaseInsensitive;
end;

function TAnsiStringType.GenerateHashCode(
  const AValue: AnsiString): Integer;
var
  Cpy: AnsiString;
begin
  { Call the generic hasher }
  if Length(AValue) > 0 then
  begin
    if not FCaseInsensitive then
      Result := BinaryHash(Pointer(AValue), Length(AValue) * SizeOf(AnsiChar))
    else
    begin
      Cpy := AnsiUpperCase(AValue);
      Result := BinaryHash(Pointer(Cpy), Length(AValue) * SizeOf(AnsiChar));
    end;
  end
  else
     Result := 0;
end;

function TAnsiStringType.GetString(const AValue: AnsiString): String;
begin
  Result := String(AValue);
end;

function TAnsiStringType.TryConvertFromVariant(const AValue: Variant; out ORes: AnsiString): Boolean;
begin
  { Variant type-cast }
  try
    ORes := AnsiString(AValue);
  except
    Exit(false);
  end;

  Result := true;
end;

function TAnsiStringType.TryConvertToVariant(const AValue: AnsiString; out ORes: Variant): Boolean;
begin
  { Simple variant assignment }
  ORes := AValue;
  Result := true;
end;

procedure TAnsiStringType.DoDeserialize(const AInfo: TValueInfo; out AValue: AnsiString; const AContext: IDeserializationContext);
begin
  AContext.GetValue(AInfo, AValue);
end;

procedure TAnsiStringType.DoSerialize(const AInfo: TValueInfo; const AValue: AnsiString; const AContext: ISerializationContext);
begin
  AContext.AddValue(AInfo, AValue);
end;

{ TUnicodeStringType }

function TUnicodeStringType.AreEqual(const AValue1, AValue2: UnicodeString): Boolean;
begin
  Result := Compare(AValue1, AValue2) = 0;
end;

function TUnicodeStringType.Compare(const AValue1, AValue2: UnicodeString): Integer;
begin
  if FCaseInsensitive then
    Result := CompareText(AValue1, AValue2)
  else
    Result := CompareStr(AValue1, AValue2);
end;

constructor TUnicodeStringType.Create;
begin
  inherited;

  FTypeFamily := tfString;
  FCaseInsensitive := false;
end;

constructor TUnicodeStringType.Create(const CaseInsensitive: Boolean);
begin
  inherited Create();

  FTypeFamily := tfString;
  FCaseInsensitive := CaseInsensitive;
end;

function TUnicodeStringType.GenerateHashCode(const AValue: UnicodeString): Integer;
var
  Cpy: String;
begin
  { Call the generic hasher }
  if Length(AValue) > 0 then
  begin
    if not FCaseInsensitive then
      Result := BinaryHash(Pointer(AValue), Length(AValue) * SizeOf(Char))
    else
    begin
      Cpy := UpperCase(AValue);
      Result := BinaryHash(Pointer(Cpy), Length(AValue) * SizeOf(Char));
    end;
  end
  else
     Result := 0;
end;

function TUnicodeStringType.GetString(const AValue: UnicodeString): String;
begin
  Result := AValue;
end;

function TUnicodeStringType.TryConvertFromVariant(const AValue: Variant; out ORes: UnicodeString): Boolean;
begin
  { Variant type-cast }
  try
    ORes := UnicodeString(AValue);
  except
    Exit(false);
  end;

  Result := true;
end;

function TUnicodeStringType.TryConvertToVariant(const AValue: UnicodeString; out ORes: Variant): Boolean;
begin
  { Simple variant assignment }
  ORes := AValue;
  Result := true;
end;

procedure TUnicodeStringType.DoDeserialize(const AInfo: TValueInfo; out AValue: UnicodeString; const AContext: IDeserializationContext);
var
  LRefValue: UnicodeString;
begin
  AContext.GetValue(AInfo, LRefValue);
  AValue := LRefValue;
end;

procedure TUnicodeStringType.DoSerialize(const AInfo: TValueInfo; const AValue: UnicodeString; const AContext: ISerializationContext);
begin
  AContext.AddValue(AInfo, AValue);
end;

{ TWideStringType }

function TWideStringType.AreEqual(const AValue1,
  AValue2: WideString): Boolean;
begin
  Result := (Compare(AValue1, AValue2) = 0);
end;

function TWideStringType.Compare(const AValue1,
  AValue2: WideString): Integer;
begin
  if FCaseInsensitive then
    Result := WideCompareText(AValue1, AValue2)
  else
    Result := WideCompareStr(AValue1, AValue2);
end;

constructor TWideStringType.Create;
begin
  inherited;

  FTypeFamily := tfString;
  FCaseInsensitive := false;
end;

constructor TWideStringType.Create(const CaseInsensitive: Boolean);
begin
  inherited Create();

  FTypeFamily := tfString;
  FCaseInsensitive := CaseInsensitive;
end;

function TWideStringType.GenerateHashCode(
  const AValue: WideString): Integer;
var
  Cpy: String;
begin
  { Call the generic hasher }
  if Length(AValue) > 0 then
  begin
    if not FCaseInsensitive then
      Result := BinaryHash(Pointer(AValue), Length(AValue) * SizeOf(WideChar))
    else
    begin
      Cpy := WideUpperCase(AValue);
      Result := BinaryHash(Pointer(Cpy), Length(AValue) * SizeOf(WideChar));
    end;
  end
  else
     Result := 0;
end;

function TWideStringType.GetString(const AValue: WideString): String;
begin
  Result := AValue;
end;

function TWideStringType.TryConvertFromVariant(const AValue: Variant; out ORes: WideString): Boolean;
begin
  { Variant type-cast }
  try
    ORes := WideString(AValue);
  except
    Exit(false);
  end;

  Result := true;
end;

function TWideStringType.TryConvertToVariant(const AValue: WideString; out ORes: Variant): Boolean;
begin
  { Simple variant assignment }
  ORes := AValue;
  Result := true;
end;

procedure TWideStringType.DoDeserialize(const AInfo: TValueInfo; out AValue: WideString; const AContext: IDeserializationContext);
var
  LValue: String;
begin
  AContext.GetValue(AInfo, LValue);
  AValue := WideString(LValue);
end;

procedure TWideStringType.DoSerialize(const AInfo: TValueInfo; const AValue: WideString; const AContext: ISerializationContext);
begin
  AContext.AddValue(AInfo, AValue);
end;

{ TClassType<T> }

function TClassType<T>.AreEqual(const AValue1, AValue2: T): Boolean;
begin
  if (AValue1 = nil) and (AValue2 = nil) then
     Exit(True);

  if (AValue1 = nil) or (AValue2 = nil) then
     Exit(False);

  Result := AValue1.Equals(AValue2);
end;

procedure TClassType<T>.CheckSerializable(const AInfo: TValueInfo; const AContext: IContext);
begin
  { Verify if the class can be serialized }
  if not FCanBeSerializedVerified then
  begin
    FCanBeSerializedVerified := true;
    FCanBeSerialized := IsClassStructSerializable(AContext.GetTypeInformation(FTypeInfo));
  end;

  { If the class cannot be serialized (not marked as such) fail! }
  if not FCanBeSerialized then
    ExceptionHelper.Throw_MarkedUnserializable(AInfo.Name, FTypeName);
end;

procedure TClassType<T>.Cleanup(var AValue: T);
begin
  if FMustKillClass then
    FreeAndNil(AValue);
end;

function TClassType<T>.Compare(const AValue1, AValue2: T): Integer;
begin
  { No actual ordering! }
  if (AValue1 = nil) and (AValue2 <> nil) then
    Exit(-1);

  if (AValue1 <> nil) and (AValue2 = nil) then
    Exit(1);

  if (AValue1 = nil) and (AValue2 = nil) then
    Exit(0);

  if AreEqual(AValue1, AValue2) then
     Result := 0
  else
     Result := 1;
end;

constructor TClassType<T>.Create;
begin
  { Call the other .ctor }
  Create(false);
end;

constructor TClassType<T>.Create(const ShouldCleanup: Boolean);
var
  LAttr: TCustomAttribute;
begin
  inherited Create();

  FTypeFamily := tfClass;
  FMustKillClass := ShouldCleanup;
  FCanBeSerializedVerified := false;
end;

procedure TClassType<T>.DoDeserialize(const AInfo: TValueInfo; out AValue: T; const AContext: IDeserializationContext);
var
  LSerializable: ISerializable;
  LDeserializationCallback: IDeserializationCallback;

  LClass: TClass;
begin
  { Check serialization is supported }
  CheckSerializable(AInfo, AContext);

  { Obtain the class of the object }
  LClass := GetTypeData(FTypeInfo)^.ClassType;

  { Open or create a new scope }
  if AContext.ExpectClassType(AInfo, LClass, TObject(AValue)) then
  begin
    { Create a new object instance }
    AValue := T(Activator.CreateInstance(LClass));

    try
      AContext.RegisterReference(TObject(AValue));

      { Check if the class has it's own serialization code }
      InternalGetInterface(AValue, ISerializable, Pointer(LSerializable));
      InternalGetInterface(AValue, IDeserializationCallback, Pointer(LDeserializationCallback));

      if LSerializable <> nil then
      begin
        { Deserialize }
        LSerializable.Deserialize(TDeserializationData.Create(AContext));

        { Force nil to the interface so we don't call _Release on it }
        Pointer(LSerializable) := nil;
      end else
        SerProcessStructClass(TSerializationGuts.Create(AContext.GetTypeInformation(LClass.ClassInfo),
          nil, AContext), TObject(AValue), false);

      if LDeserializationCallback <> nil then
      begin
        { Deserialize }
        LDeserializationCallback.Deserialized(TDeserializationData.Create(AContext));

        { Force nil to the interface so we don't call _Release on it }
        Pointer(LDeserializationCallback) := nil;
      end;

      { Close the block }
      AContext.EndComplexType();
    except
      { Make sure we kill the instace! }
      FreeAndNil(AValue);

      { re-raise the exception }
      raise;
    end;
  end;
end;

procedure TClassType<T>.DoSerialize(const AInfo: TValueInfo; const AValue: T; const AContext: ISerializationContext);
var
  LSerializable: ISerializable;
  LClass: TClass;
begin
  { Check serialization is supported }
  CheckSerializable(AInfo, AContext);

  { Check if the class has it's own serialization code }
  InternalGetInterface(AValue, ISerializable, Pointer(LSerializable));

  { Select the actual class type }
  if AValue <> nil then
    LClass := AValue.ClassType
  else
    LClass := TClass(T);

  { Open or create a new scope }
  if AContext.StartClassType(AInfo, LClass, TObject(AValue)) then
  begin
    if LSerializable <> nil then
      LSerializable.Serialize(TSerializationData.Create(AContext))
    else
      SerProcessStructClass(TSerializationGuts.Create(AContext.GetTypeInformation(LClass.ClassInfo), AContext, nil), TObject(AValue), true);

    { Force nil to the interface so we don't call _Release on it }
    Pointer(LSerializable) := nil;

    { Close the block }
    AContext.EndComplexType();
  end;
end;

function TClassType<T>.GenerateHashCode(const AValue: T): Integer;
begin
   if AValue = nil then
      Result := 0
   else
      Result := AValue.GetHashCode();
end;

function TClassType<T>.Management: TTypeManagement;
begin
  if FMustKillClass then
    Result := tmManual
  else
    Result := tmNone;
end;

procedure TClassType<T>.SetShouldCleanup(const ShouldCleanup: Boolean);
begin
  FMustKillClass := ShouldCleanup;
end;

function TClassType<T>.GetString(const AValue: T): String;
begin
  if AValue = nil then
    Result := ''
  else
    Result := AValue.ToString();
end;

procedure TClassType<T>.InternalGetInterface(const AObject: TObject; const AIID: TGUID; var AOut: Pointer);
var
  LIntfEntry: PInterfaceEntry;

begin
  AOut := nil;

  { Nothing on nil object }
  if AObject = nil then
    Exit;

  { Obtain the interface entry }
  LIntfEntry := AObject.GetInterfaceEntry(AIID);

  { If there is such an interface and it has an Object offset, get it }
  if (LIntfEntry <> nil) and (LIntfEntry^.IOffset <> 0) then
    AOut := Pointer(Integer(AObject) + LIntfEntry^.IOffset);

  { Note: No AddRef is performed since we have no idea if the object
    has ref cont > 0 already! We're only using the "pseudo-intf" entry }
end;

{ TVariantType }

function TVariantType.AreEqual(const AValue1, AValue2: Variant): Boolean;
begin
  Result := (Compare(AValue1, AValue2) = 0);
end;

function TVariantType.Compare(const AValue1, AValue2: Variant): Integer;
begin
  Result := 0;

  try
    { Try to compare }
    if AValue1 < AValue2 then
       Result := -1
    else if AValue1 > AValue2 then
       Result := 1;

  finally
  end;
end;

constructor TVariantType.Create;
begin
  inherited;
  FTypeFamily := tfVariant;
end;

function TVariantType.GenerateHashCode(const AValue: Variant): Integer;
var
  S: String;
begin
  try
    { Try to get hash code }
    S := AValue;
    Exit(BinaryHash(Pointer(S), Length(S) * SizeOf(Char)));
  finally
  end;

  Result := -1;
end;

function TVariantType.GetString(const AValue: Variant): String;
begin
  try
    Result := AValue;
  except
    begin
      Result := '';
    end;
  end;
end;

function TVariantType.TryConvertFromVariant(const AValue: Variant; out ORes: Variant): Boolean;
begin
  { Variant assignment }
  ORes := AValue;
  Result := true;
end;

function TVariantType.TryConvertToVariant(const AValue: Variant; out ORes: Variant): Boolean;
begin
  { Simple variant assignment }
  ORes := AValue;
  Result := true;
end;

{ TShortStringType }

function TShortStringType.AreEqual(const AValue1,
  AValue2: ShortString): Boolean;
begin
  Result := (Compare(AValue1, AValue2) = 0);
end;

function TShortStringType.Compare(const AValue1,
  AValue2: ShortString): Integer;
begin
  if FCaseInsensitive then
    Result := CompareText(String(AValue1), String(AValue2))
  else
    Result := CompareStr(String(AValue1), String(AValue2));
end;

constructor TShortStringType.Create;
begin
  inherited;

  FTypeFamily := tfString;
  FCaseInsensitive := false;
end;

constructor TShortStringType.Create(const CaseInsensitive: Boolean);
begin
  inherited Create();

  FTypeFamily := tfString;
  FCaseInsensitive := CaseInsensitive;
end;

function TShortStringType.GenerateHashCode(
  const AValue: ShortString): Integer;
var
  Cpy: String;
begin
  { Call the generic hasher }
  if Length(AValue) > 0 then
  begin
    if not FCaseInsensitive then
      Result := BinaryHash(@AValue[1], Length(AValue) * SizeOf(Char))
    else
    begin
      Cpy := UpperCase(String(AValue));
      Result := BinaryHash(@Cpy[1], Length(AValue) * SizeOf(Char));
    end;
  end
  else
     Result := 0;
end;

function TShortStringType.GetString(const AValue: ShortString): String;
begin
  Result := String(AValue);
end;

function TShortStringType.TryConvertFromVariant(const AValue: Variant; out ORes: ShortString): Boolean;
begin
  { Variant type-cast }
  try
    ORes := ShortString(AValue);
  except
    Exit(false);
  end;

  Result := true;
end;

function TShortStringType.TryConvertToVariant(const AValue: ShortString; out ORes: Variant): Boolean;
begin
  { Simple variant assignment }
  ORes := AValue;
  Result := true;
end;

procedure TShortStringType.DoDeserialize(const AInfo: TValueInfo; out AValue: ShortString; const AContext: IDeserializationContext);
var
  LValue: AnsiString;
begin
  AContext.GetValue(AInfo, LValue);
  AValue := ShortString(LValue);
end;

procedure TShortStringType.DoSerialize(const AInfo: TValueInfo; const AValue: ShortString; const AContext: ISerializationContext);
begin
  AContext.AddValue(AInfo, AnsiString(AValue));
end;

{ TBinaryType }

function TBinaryType.AreEqual(const AValue1, AValue2: Pointer): Boolean;
begin
  Result := BinaryCompare(AValue1, AValue2, FTypeSize) = 0;
end;

function TBinaryType.Compare(const AValue1, AValue2: Pointer): Integer;
begin
  Result := BinaryCompare(AValue1, AValue2, FTypeSize);
end;

constructor TBinaryType.Create;
begin
  ExceptionHelper.Throw_DefaultConstructorNotAllowedError();
end;

constructor TBinaryType.Create(const Size: Cardinal);
begin
  FTypeSize := Size;
  FTypeFamily := tfUnknown;
end;

procedure TBinaryType.DoDeserialize(const AInfo: TValueInfo; out AValue: Pointer; const AContext: IDeserializationContext);
var
  LName: String;
  LPtr: Pointer;
begin
  { For capture purposes }
  LName := AInfo.Name;
  LPtr := AValue;

  { Deserialize from binary }
  AContext.GetBinaryValue(AInfo,
    function(const ASize: Cardinal): Pointer
    begin
      { Size mismatch? }
      if ASize <> FTypeSize then
        ExceptionHelper.Throw_BinaryValueSizeMismatch(LName, FTypeName);

      { Supply the pointer }
      Result := LPtr;
    end
  );
end;

procedure TBinaryType.DoSerialize(const AInfo: TValueInfo; const AValue: Pointer; const AContext: ISerializationContext);
begin
  AContext.AddBinaryValue(AInfo, AValue, FTypeSize);
end;

function TBinaryType.GenerateHashCode(const AValue: Pointer): Integer;
begin
  Result := BinaryHash(AValue, FTypeSize);
end;

function TBinaryType.GetString(const AValue: Pointer): String;
begin
  Result := Format(SByteCount, [FTypeSize]);
end;

{ T3BytesType }

function T3BytesType.AreEqual(const AValue1, AValue2: __T3BytesRec): Boolean;
begin
  Result := BinaryCompare(@AValue1, @AValue2, 3) = 0;
end;

function T3BytesType.Compare(const AValue1, AValue2: __T3BytesRec): Integer;
begin
  Result := BinaryCompare(@AValue1, @AValue2, 3);
end;

constructor T3BytesType.Create;
begin
  inherited;
  FTypeFamily := tfUnknown;
end;

function T3BytesType.GenerateHashCode(const AValue: __T3BytesRec): Integer;
begin
  Result := BinaryHash(@AValue, 3);
end;

function T3BytesType.GetString(const AValue: __T3BytesRec): String;
begin
  Result := Format(SByteCount, [3]);
end;


{ TDynArrayType }

function TDynArrayType.AreEqual(const AValue1, AValue2: TBoundArray): Boolean;
var
  Len1, Len2: Integer;
begin
  { Get array lengths }
  Len1 := DynArrayLength(AValue1);
  Len2 := DynArrayLength(AValue2);

  if Len1 <> Len2 then
     begin Result := False; Exit; end;

  { Byte - by - byte comparison }
  Result := CompareMem(AValue1, AValue2, Integer(FSizeOfElement) * Len1);
end;

function TDynArrayType.Compare(const AValue1, AValue2: TBoundArray): Integer;
var
  Len, LenDiff: Integer;
begin
  { Protect from NILs }
  if (AValue1 = nil) and (AValue2 = nil) then
    Exit(0)
  else if (AValue1 = nil) then
    Exit(-1)
  else if (AValue2 = nil) then
    Exit(1);

  { And continue }

  Len     := DynArrayLength(AValue1);
  LenDiff := Len - DynArrayLength(AValue2);

  if LenDiff < 0 then
     Inc(Len, LenDiff);

  Result := BinaryCompare(AValue1, AValue2, Integer(FSizeOfElement) * Len);

  if Result = 0 then
     Result := LenDiff;
end;


constructor TDynArrayType.Create;
begin
  ExceptionHelper.Throw_DefaultConstructorNotAllowedError();
end;

constructor TDynArrayType.Create(const SizeOfElement: Cardinal; const TypeInfo: PTypeInfo);
begin
  FSizeOfElement := SizeOfElement;
  FArrayTypeInfo := TypeInfo;
  FTypeFamily    := tfArray;
end;

procedure TDynArrayType.DoDeserialize(const AInfo: TValueInfo; out AValue: TBoundArray; const AContext: IDeserializationContext);
var
  LLength: Cardinal;
  LElemType: TRttiType;
begin
  LElemType := GetElementType(AContext, AInfo);

  { Open a new array/check the ref }
  if AContext.ExpectArrayType(AInfo, TValueInfo.Create(LElemType), LLength, Pointer(AValue)) then
  begin
    { Set the new length of the array }
    DynArraySetLength(Pointer(AValue), FTypeInfo, 1, @LLength);

    try
      { The check is required since 0-length arrays tend to be NIL! }
      if LLength > 0 then
      begin
        { Register reference }
        AContext.RegisterReference(Pointer(AValue));

        { The actual deserialization of elements }
        SerProcessFields(TSerializationGuts.Create(LElemType, nil, AContext), TValueInfo.Indexed(), LLength, Pointer(AValue), false);
      end;

      { Close block }
      AContext.EndComplexType();
    except
      { On exception make sure we free the memory! }
      LLength := 0;
      DynArraySetLength(Pointer(AValue), FTypeInfo, 1, @LLength);
      Pointer(AValue) := nil;

      { re-raise }
      raise;
    end;
  end;
end;

procedure TDynArrayType.DoSerialize(const AInfo: TValueInfo; const AValue: TBoundArray; const AContext: ISerializationContext);
var
  LElemType: TRttiType;
  LLength: Cardinal;
begin
  LElemType := GetElementType(AContext, AInfo);
  LLength := DynArrayLength(AValue);

  { Call internal helper }
  if AContext.StartArrayType(AInfo, TValueInfo.Create(LElemType), LLength, Pointer(AValue)) then
  begin
    SerProcessFields(TSerializationGuts.Create(LElemType, AContext, nil), TValueInfo.Indexed(), LLength, Pointer(AValue), true);

    { Close block }
    AContext.EndComplexType();
  end;
end;

function TDynArrayType.GenerateHashCode(const AValue: TBoundArray): Integer;
begin
  Result := BinaryHash(AValue, Integer(FSizeOfElement) * DynArrayLength(AValue));
end;

function TDynArrayType.GetElementType(const AContext: IContext; const AInfo: TValueInfo): TRttiType;
var
  LElemType: TRttiType;
  LType: TRttiType;
{$IFDEF BUG_RTTI_ELEMENTTYPE}
  LElemPP: PPTypeInfo;
{$ENDIF}
begin
  LType := AContext.GetTypeInformation(FTypeInfo);

  { Exit if no rtti }
  if (LType = nil) or not (LType is TRttiDynamicArrayType) then
    ExceptionHelper.Throw_WrongOrMissingRTTI(AInfo.Name, FTypeName);

{$IFDEF BUG_RTTI_ELEMENTTYPE}
  LElemPP := GetTypeData(FTypeInfo)^.elType;

  if (LElemPP = nil) or (LElemPP^ = nil) then
    LElemType := TRttiDynamicArrayType(LType).ElementType
  else
    LElemType := AContext.GetTypeInformation(LElemPP^);
{$ELSE}
  LElemType := TRttiDynamicArrayType(LType).ElementType;
{$ENDIF}

  { Inline types are not serializable }
  if (LElemType = nil) then
    ExceptionHelper.Throw_WrongOrMissingRTTI(AInfo.Name, FTypeName);

  Result := LElemType;
end;

function TDynArrayType.GetString(const AValue: TBoundArray): String;
begin
  Result := Format(SElementCount, [DynArrayLength(AValue)]);
end;

function TDynArrayType.TryConvertFromVariant(const AValue: Variant; out ORes: TBoundArray): Boolean;
begin
  try
    { Transform the variant array into a normal array }
    DynArrayFromVariant(Pointer(ORes), AValue, FTypeInfo);
    Result := true;
  except
    Result := false;
  end;
end;

function TDynArrayType.TryConvertToVariant(const AValue: TBoundArray; out ORes: Variant): Boolean;
begin
  try
    { Try to convert the dynamic array to the variant }
    DynArrayToVariant(ORes, AValue, FTypeInfo);
    Result := true;
  except
    Result := false;
  end;
end;

{ TAnsiCharType }

function TAnsiCharType.AreEqual(const AValue1, AValue2: AnsiChar): Boolean;
begin
  Result := (AValue1 = AValue2);
end;

function TAnsiCharType.Compare(const AValue1, AValue2: AnsiChar): Integer;
begin
  Result := (Ord(AValue1) - Ord(AValue2));
end;

constructor TAnsiCharType.Create;
begin
  inherited;
  FTypeFamily := tfCharacter;
end;

function TAnsiCharType.GenerateHashCode(const AValue: AnsiChar): Integer;
begin
  Result := Ord(AValue);
end;

function TAnsiCharType.GetString(const AValue: AnsiChar): String;
begin
  Result := String(AValue);
end;

function TAnsiCharType.TryConvertFromVariant(const AValue: Variant; out ORes: AnsiChar): Boolean;
var
  S: AnsiString;
begin
  { Variant type-cast }
  try
    S := AnsiString(AValue);

    if S = '' then
      Exit(false);

    ORes := S[1];
  except
    Exit(false);
  end;

  Result := true;
end;

function TAnsiCharType.TryConvertToVariant(const AValue: AnsiChar; out ORes: Variant): Boolean;
begin
  { Simple variant assignment }
  ORes := AValue;
  Result := true;
end;

procedure TAnsiCharType.DoDeserialize(const AInfo: TValueInfo; out AValue: AnsiChar; const AContext: IDeserializationContext);
begin
  AContext.GetValue(AInfo, AValue);
end;

procedure TAnsiCharType.DoSerialize(const AInfo: TValueInfo; const AValue: AnsiChar; const AContext: ISerializationContext);
begin
  AContext.AddValue(AInfo, AValue);
end;

{ TWideCharType }

function TWideCharType.AreEqual(const AValue1,
  AValue2: WideChar): Boolean;
begin
  Result := (AValue1 = AValue2);
end;

function TWideCharType.Compare(const AValue1,
  AValue2: WideChar): Integer;
begin
  Result := (Integer(AValue1) - Integer(AValue2));
end;

constructor TWideCharType.Create;
begin
  inherited;
  FTypeFamily := tfCharacter;
end;

function TWideCharType.GenerateHashCode(const AValue: WideChar): Integer;
begin
  Result := Integer(AValue);
end;

function TWideCharType.GetString(const AValue: WideChar): String;
begin
  Result := AValue;
end;

function TWideCharType.TryConvertFromVariant(const AValue: Variant; out ORes: WideChar): Boolean;
var
  S: WideString;
begin
  { Variant type-cast }
  try
    S := AValue;

    if S = '' then
      Exit(false);

    ORes := S[1];
  except
    Exit(false);
  end;

  Result := true;
end;

function TWideCharType.TryConvertToVariant(const AValue: WideChar; out ORes: Variant): Boolean;
begin
  { Simple variant assignment }
  ORes := AValue;
  Result := true;
end;

procedure TWideCharType.DoDeserialize(const AInfo: TValueInfo; out AValue: WideChar; const AContext: IDeserializationContext);
begin
  AContext.GetValue(AInfo, AValue);
end;

procedure TWideCharType.DoSerialize(const AInfo: TValueInfo; const AValue: WideChar; const AContext: ISerializationContext);
begin
  AContext.AddValue(AInfo, AValue);
end;

{ TManualType<T> }

function TManualType<T>.Management: TTypeManagement;
begin
  Result := tmManual;
end;

{ TMagicType<T> }

function TMagicType<T>.Management: TTypeManagement;
begin
  Result := tmCompiler;
end;

{ TBooleanType }

function TBooleanType.AreEqual(const AValue1, AValue2: Boolean): Boolean;
begin
  Result := (AValue1 = AValue2);
end;

function TBooleanType.Compare(const AValue1, AValue2: Boolean): Integer;
begin
  Result := Integer(AValue1) - Integer(AValue2);
end;

constructor TBooleanType.Create;
begin
  inherited;
  FTypeFamily := tfBoolean;
end;

function TBooleanType.GenerateHashCode(const AValue: Boolean): Integer;
begin
  Result := Integer(AValue);
end;

function TBooleanType.GetString(const AValue: Boolean): String;
begin
  Result := BoolToStr(AValue, true);
end;

function TBooleanType.TryConvertFromVariant(const AValue: Variant; out ORes: Boolean): Boolean;
begin
  try
    ORes := AValue;
  except
    Exit(false);
  end;

  Result := true;
end;

function TBooleanType.TryConvertToVariant(const AValue: Boolean; out ORes: Variant): Boolean;
begin
  ORes := AValue;
  Result := true;
end;

procedure TBooleanType.DoDeserialize(const AInfo: TValueInfo; out AValue: Boolean; const AContext: IDeserializationContext);
begin
  AContext.GetValue(AInfo, AValue);
end;

procedure TBooleanType.DoSerialize(const AInfo: TValueInfo; const AValue: Boolean; const AContext: ISerializationContext);
begin
  AContext.AddValue(AInfo, AValue);
end;

{ TByteBoolType }

function TByteBoolType.AreEqual(const AValue1, AValue2: ByteBool): Boolean;
begin
  Result := (AValue1 = AValue2);
end;

function TByteBoolType.Compare(const AValue1, AValue2: ByteBool): Integer;
begin
  Result := Integer(Byte(AValue1)) - Integer(Byte(AValue2));
end;

constructor TByteBoolType.Create;
begin
  inherited;
  FTypeFamily := tfBoolean;
end;

function TByteBoolType.GenerateHashCode(const AValue: ByteBool): Integer;
begin
  Result := Integer(AValue);
end;

function TByteBoolType.GetString(const AValue: ByteBool): String;
begin
  Result := BoolToStr(AValue, true);
end;

function TByteBoolType.TryConvertFromVariant(const AValue: Variant; out ORes: ByteBool): Boolean;
begin
  try
    ORes := AValue;
  except
    Exit(false);
  end;

  Result := true;
end;

function TByteBoolType.TryConvertToVariant(const AValue: ByteBool; out ORes: Variant): Boolean;
begin
  ORes := AValue;
  Result := true;
end;

procedure TByteBoolType.DoDeserialize(const AInfo: TValueInfo; out AValue: ByteBool; const AContext: IDeserializationContext);
var
  LValue: Boolean;
begin
  AContext.GetValue(AInfo, LValue);
  AValue := LValue;
end;

procedure TByteBoolType.DoSerialize(const AInfo: TValueInfo; const AValue: ByteBool; const AContext: ISerializationContext);
begin
  AContext.AddValue(AInfo, AValue);
end;

{ TWordBoolType }

function TWordBoolType.AreEqual(const AValue1, AValue2: WordBool): Boolean;
begin
  Result := (AValue1 = AValue2);
end;

function TWordBoolType.Compare(const AValue1, AValue2: WordBool): Integer;
begin
  Result := Integer(Word(AValue1)) - Integer(Word(AValue2));
end;

constructor TWordBoolType.Create;
begin
  inherited;
  FTypeFamily := tfBoolean;
end;

function TWordBoolType.GenerateHashCode(const AValue: WordBool): Integer;
begin
  Result := Integer(AValue);
end;

function TWordBoolType.GetString(const AValue: WordBool): String;
begin
  Result := BoolToStr(AValue, true);
end;

function TWordBoolType.TryConvertFromVariant(const AValue: Variant; out ORes: WordBool): Boolean;
begin
  try
    ORes := AValue;
  except
    Exit(false);
  end;

  Result := true;
end;

function TWordBoolType.TryConvertToVariant(const AValue: WordBool; out ORes: Variant): Boolean;
begin
  ORes := AValue;
  Result := true;
end;

procedure TWordBoolType.DoDeserialize(const AInfo: TValueInfo; out AValue: WordBool; const AContext: IDeserializationContext);
var
  LValue: Boolean;
begin
  AContext.GetValue(AInfo, LValue);
  AValue := LValue;
end;

procedure TWordBoolType.DoSerialize(const AInfo: TValueInfo; const AValue: WordBool; const AContext: ISerializationContext);
begin
  AContext.AddValue(AInfo, AValue);
end;

{ TLongBoolType }

function TLongBoolType.AreEqual(const AValue1, AValue2: LongBool): Boolean;
begin
  Result := AValue1 = AValue2;
end;

function TLongBoolType.Compare(const AValue1, AValue2: LongBool): Integer;
begin
  Result := Integer(Cardinal(AValue2)) - Integer(Cardinal(AValue1));
end;

constructor TLongBoolType.Create;
begin
  inherited;
  FTypeFamily := tfBoolean;
end;

function TLongBoolType.GenerateHashCode(const AValue: LongBool): Integer;
begin
  Result := Integer(AValue);
end;

function TLongBoolType.GetString(const AValue: LongBool): String;
begin
  Result := BoolToStr(AValue, true);
end;

function TLongBoolType.TryConvertFromVariant(const AValue: Variant; out ORes: LongBool): Boolean;
begin
  try
    ORes := AValue;
  except
    Exit(false);
  end;

  Result := true;
end;

function TLongBoolType.TryConvertToVariant(const AValue: LongBool; out ORes: Variant): Boolean;
begin
  ORes := AValue;
  Result := true;
end;

procedure TLongBoolType.DoDeserialize(const AInfo: TValueInfo; out AValue: LongBool; const AContext: IDeserializationContext);
var
  LValue: Boolean;
begin
  AContext.GetValue(AInfo, LValue);
  AValue := LValue;
end;

procedure TLongBoolType.DoSerialize(const AInfo: TValueInfo; const AValue: LongBool; const AContext: ISerializationContext);
begin
  AContext.AddValue(AInfo, AValue);
end;

{ TDateType }

function TDateType.AreEqual(const AValue1, AValue2: TDate): Boolean;
begin
  Result := (CompareDate(AValue1, AValue2) = 0);
end;

function TDateType.Compare(const AValue1, AValue2: TDate): Integer;
begin
  Result := CompareDate(AValue1, AValue2);
end;

constructor TDateType.Create;
begin
  inherited;
  FTypeFamily := tfDate;
end;

function TDateType.GenerateHashCode(const AValue: TDate): Integer;
var
  X: TDate;
  LongOp : array[0..1] of Integer absolute X;
begin
  X := AValue;

  if X = 0 then
     Result := 0
  else
     Result := LongOp[1] xor LongOp[0];
end;

function TDateType.GetString(const AValue: TDate): String;
begin
  Result := DateToStr(AValue);
end;

function TDateType.TryConvertFromVariant(const AValue: Variant; out ORes: TDate): Boolean;
begin
  { May fail }
  try
    ORes := AValue;
  except
    Exit(false);
  end;

  Result := true;
end;

function TDateType.TryConvertToVariant(const AValue: TDate; out ORes: Variant): Boolean;
begin
  { Simple assignment }
  ORes := AValue;
  Result := true;
end;

procedure TDateType.DoDeserialize(const AInfo: TValueInfo; out AValue: TDate; const AContext: IDeserializationContext);
var
  LValue: TDateTime;
begin
  AContext.GetValue(AInfo, LValue);
  AValue := DateOf(LValue);
end;

procedure TDateType.DoSerialize(const AInfo: TValueInfo; const AValue: TDate; const AContext: ISerializationContext);
begin
  AContext.AddValue(AInfo, TDateTime(AValue));
end;

{ TTimeType }

function TTimeType.AreEqual(const AValue1, AValue2: TTime): Boolean;
begin
  Result := (CompareTime(AValue1, AValue2) = 0);
end;

function TTimeType.Compare(const AValue1, AValue2: TTime): Integer;
begin
  Result := CompareTime(AValue1, AValue2);
end;

constructor TTimeType.Create;
begin
  inherited;
  FTypeFamily := tfDate;
end;

function TTimeType.GenerateHashCode(const AValue: TTime): Integer;
var
  X: TTime;
  LongOp : array[0..1] of Integer absolute X;
begin
  X := AValue;

  if X = 0 then
     Result := 0
  else
     Result := LongOp[1] xor LongOp[0];
end;

function TTimeType.GetString(const AValue: TTime): String;
begin
  Result := TimeToStr(AValue);
end;

function TTimeType.TryConvertFromVariant(const AValue: Variant; out ORes: TTime): Boolean;
begin
  { May fail }
  try
    ORes := AValue;
  except
    Exit(false);
  end;

  Result := true;
end;

function TTimeType.TryConvertToVariant(const AValue: TTime; out ORes: Variant): Boolean;
begin
  { Simple assignment }
  ORes := AValue;
  Result := true;
end;

procedure TTimeType.DoDeserialize(const AInfo: TValueInfo; out AValue: TTime; const AContext: IDeserializationContext);
var
  LValue: TDateTime;
begin
  AContext.GetValue(AInfo, LValue);
  AValue := TimeOf(LValue);
end;

procedure TTimeType.DoSerialize(const AInfo: TValueInfo; const AValue: TTime; const AContext: ISerializationContext);
begin
  AContext.AddValue(AInfo, TDateTime(AValue));
end;

{ TDateTimeType }

function TDateTimeType.AreEqual(const AValue1, AValue2: TDateTime): Boolean;
begin
  Result := (CompareDateTime(AValue1, AValue2) = 0);
end;

function TDateTimeType.Compare(const AValue1, AValue2: TDateTime): Integer;
begin
  Result := CompareDateTime(AValue1, AValue2);
end;

constructor TDateTimeType.Create;
begin
  inherited;
  FTypeFamily := tfDate;
end;

function TDateTimeType.GenerateHashCode(const AValue: TDateTime): Integer;
var
  X: TDateTime;
  LongOp : array[0..1] of Integer absolute X;
begin
  X := AValue;

  if X = 0 then
     Result := 0
  else
     Result := LongOp[1] xor LongOp[0];
end;

function TDateTimeType.GetString(const AValue: TDateTime): String;
begin
  Result := DateTimeToStr(AValue);
end;

function TDateTimeType.TryConvertFromVariant(const AValue: Variant; out ORes: TDateTime): Boolean;
begin
  { May fail }
  try
    ORes := AValue;
  except
    Exit(false);
  end;

  Result := true;
end;

function TDateTimeType.TryConvertToVariant(const AValue: TDateTime; out ORes: Variant): Boolean;
begin
  { Simple assignment }
  ORes := AValue;
  Result := true;
end;

procedure TDateTimeType.DoDeserialize(const AInfo: TValueInfo; out AValue: TDateTime; const AContext: IDeserializationContext);
begin
  AContext.GetValue(AInfo, AValue);
end;

procedure TDateTimeType.DoSerialize(const AInfo: TValueInfo; const AValue: TDateTime; const AContext: ISerializationContext);
begin
  AContext.AddValue(AInfo, AValue);
end;

{ TUCS4CharType }

function TUCS4CharType.AreEqual(const AValue1, AValue2: UCS4Char): Boolean;
begin
  Result := (AValue1 = AValue2);
end;

function TUCS4CharType.Compare(const AValue1, AValue2: UCS4Char): Integer;
begin
  if AValue1 > AValue2 then
    Result := 1
  else if AValue1 < AValue2 then
    Result := -1
  else
    Result := 0;
end;

constructor TUCS4CharType.Create;
begin
  inherited;
  FTypeFamily := tfCharacter;
end;

function TUCS4CharType.GenerateHashCode(const AValue: UCS4Char): Integer;
var
  I: Integer absolute AValue;
begin
  Result := I;
end;

function TUCS4CharType.GetString(const AValue: UCS4Char): String;
begin
  Result := ConvertFromUtf32(AValue);
end;

function TUCS4CharType.TryConvertFromVariant(const AValue: Variant; out ORes: UCS4Char): Boolean;
var
  S: String;
begin
  { Variant type-cast }
  try
    S := AValue;
    ORes := ConvertToUtf32(S, 1);
  except
    Exit(false);
  end;

  Result := true;
end;

function TUCS4CharType.TryConvertToVariant(const AValue: UCS4Char; out ORes: Variant): Boolean;
begin
  ORes := ConvertFromUtf32(AValue);
  Result := true;
end;

procedure TUCS4CharType.DoDeserialize(const AInfo: TValueInfo; out AValue: UCS4Char; const AContext: IDeserializationContext);
var
  LValue: String;
begin
  AContext.GetValue(AInfo, LValue);
  AValue := ConvertToUtf32(LValue, 1);
end;

procedure TUCS4CharType.DoSerialize(const AInfo: TValueInfo; const AValue: UCS4Char; const AContext: ISerializationContext);
begin
  { Transform into an UTF16 string }
  AContext.AddValue(AInfo, ConvertFromUtf32(AValue));
end;

{ TUCS4StringType }

function TUCS4StringType.AreEqual(const AValue1, AValue2: UCS4String): Boolean;
begin
  Result := Compare(AValue1, AValue2) = 0;
end;

function TUCS4StringType.Compare(const AValue1, AValue2: UCS4String): Integer;
begin
  if FCaseInsensitive then
    Result := CompareText(UCS4StringToUnicodeString(AValue1), UCS4StringToUnicodeString(AValue2))
  else
    Result := CompareStr(UCS4StringToUnicodeString(AValue1), UCS4StringToUnicodeString(AValue2));
end;

constructor TUCS4StringType.Create(const CaseInsensitive: Boolean);
begin
  inherited Create();

  FTypeFamily := tfString;
  FCaseInsensitive := CaseInsensitive;
end;

constructor TUCS4StringType.Create;
begin
  inherited;

  FTypeFamily := tfString;
  FCaseInsensitive := false;
end;

function TUCS4StringType.GenerateHashCode(const AValue: UCS4String): Integer;
var
  Cpy: String;
begin
  { Call the generic hasher }
  if Length(AValue) > 0 then
  begin
    if not FCaseInsensitive then
      Result := BinaryHash(Pointer(AValue), Length(AValue) * SizeOf(UCS4Char))
    else
    begin
      Cpy := UpperCase(UCS4StringToUnicodeString(AValue));
      Result := BinaryHash(Pointer(Cpy), Length(AValue) * SizeOf(Char));
    end;
  end
  else
     Result := 0;
end;

function TUCS4StringType.GetString(const AValue: UCS4String): String;
begin
  Result := UCS4StringToUnicodeString(AValue);
end;

function TUCS4StringType.TryConvertFromVariant(const AValue: Variant; out ORes: UCS4String): Boolean;
begin
  { May fail! }
  try
    ORes := UnicodeStringToUCS4String(AValue);
  except
    Exit(false);
  end;

  Result := true;
end;

function TUCS4StringType.TryConvertToVariant(const AValue: UCS4String; out ORes: Variant): Boolean;
begin
  ORes := UCS4StringToUnicodeString(AValue);
  Result := true;
end;

procedure TUCS4StringType.DoDeserialize(const AInfo: TValueInfo; out AValue: UCS4String; const AContext: IDeserializationContext);
var
  LValue: String;
begin
  AContext.GetValue(AInfo, LValue);
  AValue := UnicodeStringToUCS4String(LValue);
end;

procedure TUCS4StringType.DoSerialize(const AInfo: TValueInfo; const AValue: UCS4String; const AContext: ISerializationContext);
begin
  { Transform into an UTF-16 string }
  AContext.AddValue(AInfo, UCS4StringToUnicodeString(AValue));
end;

{ TUTF8StringType }

function TUTF8StringType.AreEqual(const AValue1, AValue2: UTF8String): Boolean;
begin
  Result := Compare(AValue1, AValue2) = 0;
end;

function TUTF8StringType.Compare(const AValue1, AValue2: UTF8String): Integer;
begin
  if FCaseInsensitive then
    Result := CompareText(String(AValue1), String(AValue2))
  else
    Result := CompareStr(String(AValue1), String(AValue2));
end;

constructor TUTF8StringType.Create;
begin
  inherited;

  FTypeFamily := tfString;
  FCaseInsensitive := false;
end;

constructor TUTF8StringType.Create(const CaseInsensitive: Boolean);
begin
  inherited Create();

  FTypeFamily := tfString;
  FCaseInsensitive := CaseInsensitive;
end;

function TUTF8StringType.GenerateHashCode(const AValue: UTF8String): Integer;
var
  Cpy: String;
begin
  { Call the generic hasher }
  if Length(AValue) > 0 then
  begin
    if not FCaseInsensitive then
      Result := BinaryHash(Pointer(AValue), Length(AValue) * SizeOf(Char))
    else
    begin
      Cpy := UpperCase(String(AValue));
      Result := BinaryHash(Pointer(Cpy), Length(AValue) * SizeOf(Char));
    end;
  end
  else
     Result := 0;
end;

function TUTF8StringType.GetString(const AValue: UTF8String): String;
begin
  Result := String(AValue);
end;

function TUTF8StringType.TryConvertFromVariant(const AValue: Variant; out ORes: UTF8String): Boolean;
begin
  { Variant type-cast }
  try
    ORes := UTF8String(AValue);
  except
    Exit(false);
  end;

  Result := true;
end;

function TUTF8StringType.TryConvertToVariant(const AValue: UTF8String; out ORes: Variant): Boolean;
begin
  { Simple variant assignment }
  ORes := AValue;
  Result := true;
end;

procedure TUTF8StringType.DoDeserialize(const AInfo: TValueInfo; out AValue: UTF8String; const AContext: IDeserializationContext);
var
  LValue: String;
begin
  AContext.GetValue(AInfo, LValue);
  AValue := UTF8String(LValue);
end;

procedure TUTF8StringType.DoSerialize(const AInfo: TValueInfo; const AValue: UTF8String; const AContext: ISerializationContext);
begin
  { Transform into a UTF-16 string }
  AContext.AddValue(AInfo, string(AValue));
end;

{ TRawByteStringType }

function TRawByteStringType.AreEqual(const AValue1, AValue2: RawByteString): Boolean;
var
  Len1, Len2: Integer;
begin
  { Get array lengths }
  Len1 := Length(AValue1);
  Len2 := Length(AValue2);

  if Len1 <> Len2 then
     begin Result := False; Exit; end;

  { Byte - by - byte comparison }
  Result := CompareMem(@(AValue1[1]), @(AValue2[1]), Len1);
end;

function TRawByteStringType.Compare(const AValue1, AValue2: RawByteString): Integer;
var
  Len, LenDiff: Integer;
begin
  Len     := Length(AValue1);
  LenDiff := Len - Length(AValue2);

  if LenDiff < 0 then
     Inc(Len, LenDiff);

  Result := BinaryCompare(@(AValue1[1]), @(AValue2[1]), Len);

  if Result = 0 then
     Result := LenDiff;
end;

constructor TRawByteStringType.Create;
begin
  inherited;
  FTypeFamily := tfUnknown;
end;

function TRawByteStringType.GenerateHashCode(const AValue: RawByteString): Integer;
begin
  Result := BinaryHash(@(AValue[1]), Length(AValue));
end;

function TRawByteStringType.GetString(const AValue: RawByteString): String;
begin
  Result := Format(SElementCount, [Length(AValue)]);
end;

procedure TRawByteStringType.DoDeserialize(const AInfo: TValueInfo; out AValue: RawByteString; const AContext: IDeserializationContext);
var
  LName: String;
  LValue: RawByteString;
begin
  LName := AInfo.Name;

  AContext.GetBinaryValue(AInfo,
    function(const ASize: Cardinal): Pointer
    begin
      { Setup the raw byte string}
      SetLength(LValue, ASize);

      { Supply the pointer }
      Result := Addr(LValue[1]);
    end
  );

  { Finally set the out pointer }
  AValue := LValue;
end;

procedure TRawByteStringType.DoSerialize(const AInfo: TValueInfo; const AValue: RawByteString; const AContext: ISerializationContext);
begin
  { Write as binary block! }
  AContext.AddBinaryValue(AInfo, AValue[1], Length(AValue));
end;

{ TType }

class function TType.CreateCharType(const Size: Integer): Pointer;
begin
  case Size of
     1: Result := TAnsiCharType.Create();
     2: Result := TWideCharType.Create();
     else
         Result := TBinaryType.Create(Size);
  end;
end;

class function TType.CreateBinaryType(const Size: Integer): Pointer;
begin
  case Size of
     1: Result := TByteType.Create();
     2: Result := TWordType.Create();
     3: Result := T3BytesType.Create();
     4: Result := TCardinalType.Create();
     else
         Result := TBinaryType.Create(Size);
  end;
end;

class function TType.CreateIntegerType(const OrdinalType: TOrdType): Pointer;
begin
  Result := nil;

  case OrdinalType of
     otSByte: Result := TShortIntType.Create();
     otUByte: Result := TByteType.Create();
     otSWord: Result := TSmallIntType.Create();
     otUWord: Result := TWordType.Create();
     otSLong: Result := TIntegerType.Create();
     otULong: Result := TCardinalType.Create();
  end;
end;

class function TType.CreateFloatType(const FloatType: TFloatType): Pointer;
begin
  Result := nil;

  case FloatType of
     ftSingle  : Result := TSingleType.Create();
     ftDouble  : Result := TDoubleType.Create();
     ftExtended: Result := TExtendedType.Create();
     ftComp    : Result := TCompType.Create();
     ftCurr    : Result := TCurrencyType.Create();
  end;
end;

class function TType.CreateStringType(const Kind: TTypeKind): Pointer;
begin
  Result := nil;

  case Kind of
     tkString  : Result := TShortStringType.Create();
     tkLString : Result := TAnsiStringType.Create();
     tkWString : Result := TWideStringType.Create();
     tkUString : Result := TUnicodeStringType.Create();
  end;
end;

class function TType.CreateClassType(): Pointer;
begin
  { Hack around the class restriction, The real tyoe info will be added later }
  Result := TClassType<TObject>.Create();
end;

class function TType.CreateCustomType(const TypeInfo: PTypeInfo): Pointer;
var
  PInfo: PTypeInfo;
  Dict: TInternalDictionary;
begin
  Result := nil;
  PInfo := TypeInfo;

  { Check for nil }
  if PInfo = nil then
    ExceptionHelper.Throw_CustomTypeHasNoRTTI();

  MonitorEnter(FCustomTypes);

  { Type-cast to what wee need }
  Dict := TInternalDictionary(FCustomTypes);

  try
    { Check if this class is not registered yet }
    if Dict.ContainsKey(PInfo) then
      Result := TType(TTypeClass(Dict[PInfo]).Create())
    else if PInfo^.Kind = tkClass then
    begin
      { Did not find a direct match. For classes, try ancestry }
      while true do
      begin
        { Find the parent }
        PInfo := GetParentTypeInfo(PInfo);

        { Not a valid parent? break off the loop }
        if PInfo = nil then
          Exit;

        { If there is such a type class registered, use it! }
        if Dict.ContainsKey(PInfo) then
          Exit(TType(TTypeClass(Dict[PInfo]).Create()));
      end;
    end;
  finally
    { Do not forget to release the monitor lock always! }
    MonitorExit(FCustomTypes);
  end;
end;

class function TType.CreateVariantType(): Pointer;
begin
  Result := TVariantType.Create();
end;

function TType.Family: TTypeFamily;
begin
  Result := FTypeFamily;
end;

class function TType.CreateInt64Type(const TypeData: PTypeData): Pointer;
begin
  if TypeData^.MaxInt64Value > TypeData^.MinInt64Value then
     Result := TInt64Type.Create()
  else
     Result := TUInt64Type.Create();
end;

class function TType.CreateDefault(const TypeInfo: PTypeInfo; const TypeSize: Cardinal;
  const AllowCustom: Boolean; const AArrayClass, ARecordClass: TTypeClass): Pointer;
var
  ResultClass : TType;
  TypeData    : PTypeData;
begin
  ResultClass := nil;

  { No type information associated - try a different solution }
  if TypeInfo = nil then
  begin
    ResultClass := CreateBinaryType(TypeSize);
    Result := ResultClass;

    { And now do assign the name, size and type info to the custom type }
    ResultClass.FTypeName := '';
    ResultClass.FTypeSize := TypeSize;
    ResultClass.FTypeInfo := nil;
    ResultClass.FTypeFamily := tfUnknown;

    Exit;
  end;

  { Check maybe we have a cusm registered one }
  if AllowCustom and (TypeInfo <> nil) then
  begin
    ResultClass := CreateCustomType(TypeInfo);
    Result := ResultClass;

    if Result <> nil then
    begin
      { And now do assign the name, size and type info to the custom type }
      ResultClass.FTypeName   := GetTypeName(TypeInfo);
      ResultClass.FTypeSize   := TypeSize;
      ResultClass.FTypeInfo   := TypeInfo;

      Exit;
    end;
  end;

  { Retrieve type data }
  TypeData := GetTypeData(TypeInfo);

  case TypeInfo^.Kind of
    tkUnknown:
    begin
      ResultClass := CreateBinaryType(TypeSize);
      ResultClass.FTypeFamily := tfUnknown;
    end;

    tkInteger:     ResultClass := CreateIntegerType(TypeData^.OrdType);
    tkEnumeration: ResultClass := CreateIntegerType(TypeData^.OrdType);
    tkFloat:       ResultClass := CreateFloatType(TypeData^.FloatType);
    tkSet:         ResultClass := CreateBinaryType(TypeSize);
    tkClass:       ResultClass := CreateClassType();
    tkProcedure:   ResultClass := TProcedureType.Create();
    tkMethod:      ResultClass := TMethodType.Create();

    tkChar, tkWChar:
      ResultClass := CreateCharType(TypeSize);

    tkString, tkLString, tkWString, tkUString:
      ResultClass := CreateStringType(TypeInfo^.Kind);

    tkVariant:     ResultClass := CreateVariantType();
    tkArray:       ResultClass := AArrayClass.Create();
    tkRecord:      ResultClass := ARecordClass.Create();
    tkInterface:   ResultClass := TInterfaceType.Create();
    tkInt64:       ResultClass := CreateInt64Type(TypeData);
    tkDynArray:    ResultClass := CreateDynamicArrayType(TypeData^.elSize, TypeInfo);
    tkClassRef:    ResultClass := TMetaclassType.Create();
    tkPointer:     ResultClass := TPointerType.Create();
  end;

  if ResultClass = nil then
     ExceptionHelper.Throw_NoDefaultTypeError(UTF8ToString(TypeInfo^.Name));

  { And now do assign the name, size and type info to the object }
  ResultClass.FTypeName := GetTypeName(TypeInfo);
  ResultClass.FTypeSize := TypeSize;
  ResultClass.FTypeInfo := TypeInfo;

  Result := ResultClass;
end;

class function TType.CreateDynamicArrayType(const ElementSize: Integer; const TypeInfo: PTypeInfo): Pointer;
begin
  Result := TDynArrayType.Create(ElementSize, TypeInfo);
end;

function TType.GetExtension(const AExtender: TTypeExtender): TTypeExtension;
begin
  if AExtender = nil then
    ExceptionHelper.Throw_ArgumentNilError('AExtender');

  { Try to obtain an extension for my-self }
  Result := AExtender.CreateExtensionFor(Self);
end;

class function TType.GetParentTypeInfo(const ClassInfo: PTypeInfo): PTypeInfo;
var
  TypeData: PTypeData;
begin
  Result := nil;

  { Exit on nil class info }
  if ClassInfo = nil then
    Exit;

  TypeData := GetTypeData(ClassInfo);

  { Exit on nil type data }
  if (TypeData = nil) or (TypeData^.ParentInfo = nil) then
    Exit;

  Result := TypeData^.ParentInfo^;
end;

class function TType.IsClassStructSerializable(const AType: TRttiType): Boolean;
var
  LAttr: TCustomAttribute;
begin
  if AType <> nil then
    for LAttr in AType.GetAttributes() do
      if LAttr is NonSerialized then
        Exit(false);

  Result := true;
end;

function TType.Management: TTypeManagement;
begin
  Result := tmNone;
end;

function TType.Name: String;
begin
  Result := FTypeName;
end;

procedure TType.RestrictTo(const AllowedFamilies: TTypeFamilySet);
begin
  { Restrict the family }
  if not (Family in AllowedFamilies) then
    ExceptionHelper.Throw_RuntimeTypeRestrictionFailed(Name);
end;

class procedure TType.SerProcessFields(const AGuts: TSerializationGuts; const AInfo: TValueInfo;
      const ACount: Cardinal; const APtrToField: Pointer; const ASerialize: Boolean);
var
  LCustom: TType;
  LInfo: TValueInfo;
  LNext: PByte;
  I: Integer;
  LCommon: IContext;
  LHandle: PTypeInfo;
  LKind: TTypeKind;
  LSize: Cardinal;
begin
  { Check for registered types. Handle them specially. }
  LInfo := AInfo;
  LNext := APtrToField;

  if ASerialize then
    LCommon := AGuts.FInContext
  else
    LCommon := AGuts.FOutContext;

  LHandle := AGuts.FType.Handle;
  LKind := AGuts.FType.TypeKind;
  LSize := AGuts.FType.TypeSize;

  LCustom := LCommon.GetTypeObject(LHandle, function(): TObject begin
    { Obtain a custom type }
    Result := CreateCustomType(LHandle);

    { Obtain the normal type only of not and array or record }
    if (Result = nil) and not (LKind in [tkArray, tkRecord]) then
      Result := CreateDefault(LHandle, LSize, false, nil, nil);
  end) as TType;

  { Do the standard drill if we have a type object }
  if LCustom <> nil then
  begin
    for I := 0 to ACount - 1 do
    begin
      if ASerialize then
        LCustom.InternalSerialize(LInfo, LNext, AGuts.FInContext)
      else
        LCustom.InternalDeserialize(LInfo, LNext, AGuts.FOutContext);

      LNext := Ptr(Integer(LNext) + AGuts.FType.TypeSize);
    end;
  end else if (LKind = tkArray) then
  { Special case for arrays! We cannot reference generics here so we're just going to
    program the logic here instead on the generic TRecordType<T> or TArrayType<T> }
  begin
    for I := 0 to ACount - 1 do
    begin
      SerProcessStaticArray(AGuts, LInfo, LNext, ASerialize);
      LNext := Ptr(Integer(LNext) + AGuts.FType.TypeSize);
    end;
  end else if (LKind = tkRecord) then
  begin
    { Check if the structure is serializable }
    if not IsClassStructSerializable(AGuts.FType) then
      ExceptionHelper.Throw_MarkedUnserializable(LInfo.Name, AGuts.FType.Name);

    for I := 0 to ACount - 1 do
    begin
      { Start composite }
      if ASerialize then
        AGuts.FInContext.StartRecordType(LInfo)
      else
        AGuts.FOutContext.ExpectRecordType(LInfo);

      { Serializa/Deserialize }
      SerProcessStructClass(AGuts, LNext, ASerialize);

      { End composite }
      if ASerialize then
        AGuts.FInContext.EndComplexType()
      else
        AGuts.FOutContext.EndComplexType();

      LNext := Ptr(Integer(LNext) + AGuts.FType.TypeSize);
    end;
  end else
    ASSERT(false, 'Type object not obtained! Should never get here!');
end;

class procedure TType.SerProcessStaticArray(const AGuts: TSerializationGuts; const AInfo: TValueInfo;
  const APtrToFirst: Pointer; const ASerialize: Boolean);
var
  LElemType: TRttiType;
  LLength, LStoredLen: Cardinal;
begin
  LElemType := TRttiArrayType(AGuts.FType).ElementType;

  { Inline types are not serializable }
  if (LElemType = nil) then
    ExceptionHelper.Throw_WrongOrMissingRTTI(AInfo.Name, AGuts.FType.Name);

  LLength := TRttiArrayType(AGuts.FType).TotalElementCount;

  { Open a new block }
  if ASerialize then
    AGuts.FInContext.StartArrayType(AInfo, TValueInfo.Create(LElemType), LLength)
  else
  begin
    AGuts.FOutContext.ExpectArrayType(AInfo, TValueInfo.Create(LElemType), LStoredLen);

    if LStoredLen <> LLength then; // TODO: Fail on different array lengths

  end;

  { Spill you guts in! Or out :) }
  SerProcessFields(TSerializationGuts.Create(LElemType, AGuts.FInContext, AGuts.FOutContext), TValueInfo.Indexed(),
    LLength, APtrToFirst, ASerialize);

  { Close Block }
  if ASerialize then
    AGuts.FInContext.EndComplexType()
  else
    AGuts.FOutContext.EndComplexType();
end;

class procedure TType.SerProcessStructClass(const AGuts: TSerializationGuts; const APtrToInstance: Pointer; const ASerialize: Boolean);
var
  LFieldType: TRttiType;
  LField: TRttiField;
  LOffset: Pointer;
begin
  { Special case for nil instances }
  if APtrToInstance = nil then
    Exit;

  { Iterate over all fields and serialize them }
  for LField in AGuts.FType.GetFields() do
  begin
    if Skippable(LField) then
      continue;

    LFieldType := LField.FieldType;

    { This field has no RTTI for its type! Skip it }
    if LFieldType = nil then
      ExceptionHelper.Throw_WrongOrMissingRTTI(LField.Name, AGuts.FType.Name);

    { The offset of the field in the "instance" }
    LOffset := Ptr(LField.Offset + Integer(APtrToInstance));

    { Serialize! Also place the name label. Use attributes to get the name }
    SerProcessFields(TSerializationGuts.Create(LFieldType, AGuts.FInContext, AGuts.FOutContext),
      TValueInfo.Create(LField), 1, LOffset, ASerialize);
  end;
end;

function TType.Size: Cardinal;
begin
  Result := FTypeSize;
end;

class function TType.Skippable(const AField: TRttiField): Boolean;
var
  LAttr: TCustomAttribute;
begin
  { Check for [NonSerialized] attribute }
  for LAttr in AField.GetAttributes() do
    if LAttr is NonSerialized then
      Exit(true);

  Exit(false);
end;

function TType.TypeInfo: PTypeInfo;
begin
  Result := FTypeInfo;
end;

{ TSuppressedWrapperType<T> }

function TSuppressedWrapperType<T>.AreEqual(const AValue1, AValue2: T): Boolean;
begin
  { Call inner type }
  Result := FType.AreEqual(AValue1, AValue2);
end;

procedure TSuppressedWrapperType<T>.Cleanup(var AValue: T);
begin
  if FAllowCleanup then
    FType.Cleanup(AValue);
end;

function TSuppressedWrapperType<T>.Compare(const AValue1, AValue2: T): Integer;
begin
  { Call inner type }
  Result := FType.Compare(AValue1, AValue2);
end;

constructor TSuppressedWrapperType<T>.Create(const AType: IType<T>);
begin
  inherited Create();

  if AType = nil then
    ExceptionHelper.Throw_ArgumentNilError('AType');

  { Store the type }
  FType := AType;
end;

constructor TSuppressedWrapperType<T>.Create;
begin
  { Do not allow default constructor }
  ExceptionHelper.Throw_DefaultConstructorNotAllowedError();
end;

function TSuppressedWrapperType<T>.Family: TTypeFamily;
begin
  { Call the inner type }
  Result := FType.Family;
end;

function TSuppressedWrapperType<T>.GenerateHashCode(const AValue: T): Integer;
begin
  { Call the inner type }
  Result := FType.GenerateHashCode(AValue);
end;

function TSuppressedWrapperType<T>.GetString(const AValue: T): String;
begin
  { Call the inner type }
  Result := FType.GetString(AValue);
end;

function TSuppressedWrapperType<T>.Management: TTypeManagement;
begin
  { Report none! }
  if FAllowCleanup then
    Result := FType.Management
  else
    Result := tmNone;
end;

function TSuppressedWrapperType<T>.Name: String;
begin
  { Call the inner type }
  Result := FType.Name;
end;

function TSuppressedWrapperType<T>.Size: Cardinal;
begin
  { Call the inner type }
  Result := FType.Size;
end;

function TSuppressedWrapperType<T>.TryConvertFromVariant(const AValue: Variant; out ORes: T): Boolean;
begin
  { Call the inner type }
  Result := FType.TryConvertFromVariant(AValue, ORes);
end;

function TSuppressedWrapperType<T>.TryConvertToVariant(const AValue: T; out ORes: Variant): Boolean;
begin
  { Call the inner type }
  Result := FType.TryConvertToVariant(AValue, ORes);
end;

procedure TSuppressedWrapperType<T>.DoDeserialize(const AInfo: TValueInfo; out AValue: T; const AContext: IDeserializationContext);
begin
  FType.Deserialize(AInfo, AValue, AContext);
end;

procedure TSuppressedWrapperType<T>.DoSerialize(const AInfo: TValueInfo; const AValue: T; const AContext: ISerializationContext);
begin
  FType.Serialize(AInfo, AValue, AContext);
end;

function TSuppressedWrapperType<T>.TypeInfo: PTypeInfo;
begin
  { Call the inner type }
  Result := FType.TypeInfo;
end;

{ TObjectWrapperType<T> }

procedure TObjectWrapperType<T>.Cleanup(var AValue: T);
begin
  if FAllowCleanup then
    FreeAndNil(AValue);
end;

function TObjectWrapperType<T>.Management: TTypeManagement;
begin
  if FAllowCleanup then
    Result := tmManual
  else
    Result := tmNone;
end;

{ TTypeExtender }

constructor TTypeExtender.Create;
begin
  { Create an internal dictionary to hold the Type -> Extension relationship }
  FExtensions := TExtensionDictionary.Create();
end;

function TTypeExtender.CreateExtensionFor(const AObject: TObject): TTypeExtension;
var
  Dict: TExtensionDictionary;
  TheClass: TTypeExtensionClass;
begin
  Result := nil;

  { Grab monitor! }
  MonitorEnter(FExtensions);

  try
    Dict := TExtensionDictionary(FExtensions);

    if Dict.TryGetValue(AObject.ClassType, Pointer(TheClass)) then
      Result := TheClass.Create();
  finally
    { Make sure we're always getting here }
    MonitorExit(FExtensions);
  end;
end;

destructor TTypeExtender.Destroy;
begin
  { Destroy the internal object }
  FExtensions.Free;

  inherited;
end;

procedure TTypeExtender.Register<T>(const AExtension: TTypeExtensionClass);
var
  GotType: TType<T>;
  Dict: TExtensionDictionary;
begin
  { Obtain a type support class }
  GotType := TType<T>.CreateDefault(true);

  { Grab monitor! }
  MonitorEnter(FExtensions);

  try
    Dict := TExtensionDictionary(FExtensions);

    if Dict.ContainsKey(GotType.ClassType) then
      ExceptionHelper.Throw_TypeExtensionAlreadyRegistered(GotType.Name);

    Dict.Add(GotType.ClassType, AExtension);
  finally
    { Make sure we're always getting here }
    MonitorExit(FExtensions);
  end;
end;

procedure TTypeExtender.Unregister<T>;
var
  GotType: TType<T>;
  Dict: TExtensionDictionary;
begin
  { Obtain a type support class }
  GotType := TType<T>.CreateDefault(true);

  { Grab monitor! }
  MonitorEnter(FExtensions);

  try
    Dict := TExtensionDictionary(FExtensions);

    if not Dict.ContainsKey(GotType.ClassType) then
      ExceptionHelper.Throw_TypeExtensionNotYetRegistered(GotType.Name);

    Dict.Remove(GotType.ClassType);
  finally
    { Make sure we're always getting here }
    MonitorExit(FExtensions);
  end;
end;

{ TTypeExtension }

constructor TTypeExtension.Create;
begin
  { Do nothing here! }
end;

{ TType.TPointerDictionary }

const
  DefaultArrayLength = 32;

{ TPointerDictionary }

procedure TType.TPointerDictionary.Add(const AKey: Pointer; const AValue: Pointer);
begin
  { Call insert }
  Insert(AKey, AValue);
end;

procedure TType.TPointerDictionary.Clear;
var
  I: Cardinal;
begin
  if FCount > 0 then
    for I := 0 to Length(FBucketArray) - 1 do
      FBucketArray[I] := -1;

  if Length(FEntryArray) > 0 then
    FillChar(FEntryArray[0], Length(FEntryArray) * SizeOf(TEntry), 0);

  FFreeList := -1;
  FCount := 0;
  FFreeCount := 0;
end;

function TType.TPointerDictionary.ContainsKey(const AKey: Pointer): Boolean;
begin
  Result := (FindEntry(AKey) >= 0);
end;

constructor TType.TPointerDictionary.Create;
begin
  Create(DefaultArrayLength);
end;

constructor TType.TPointerDictionary.Create(const InitialCapacity: Cardinal);
begin
  FCount := 0;
  FFreeCount := 0;
  FFreeList := 0;

  InitializeInternals(DefaultArrayLength);
end;

destructor TType.TPointerDictionary.Destroy;
begin
  { Clear first }
  Clear();

  inherited;
end;

function TType.TPointerDictionary.FindEntry(const AKey: Pointer): Integer;
var
  HashCode : Integer;
  I        : Integer;
begin
  Result := -1;

  if Length(FBucketArray) > 0 then
  begin
    { Generate the hash code }
    HashCode := Hash(AKey);

    I := FBucketArray[HashCode mod Length(FBucketArray)];

    while I >= 0 do
    begin
      if (FEntryArray[I].FHashCode = HashCode) and (FEntryArray[I].FKey = AKey) then
         begin Result := I; Exit; end;

      I := FEntryArray[I].FNext;
    end;
  end;
end;

function TType.TPointerDictionary.GetItem(const Key: Pointer): Pointer;
begin
  if not TryGetValue(Key, Result) then
    ExceptionHelper.Throw_KeyNotFoundError(IntToHex(Integer(Key), 8));
end;

function TType.TPointerDictionary.Hash(const AKey: Pointer): Integer;
const
  PositiveMask = not Integer($80000000);
begin
  Result := PositiveMask and ((PositiveMask and Integer(AKey)) + 1);
end;

procedure TType.TPointerDictionary.InitializeInternals(const Capacity: Cardinal);
var
  XPrime : Integer;
  I     : Integer;
begin
  XPrime := Prime.GetNearestProgressionPositive(Capacity);

  SetLength(FBucketArray, XPrime);
  SetLength(FEntryArray, XPrime);

  for I := 0 to XPrime - 1 do
  begin
    FBucketArray[I] := -1;
    FEntryArray[I].FHashCode := -1;
  end;

  FFreeList := -1;
end;

procedure TType.TPointerDictionary.Insert(const AKey: Pointer; const AValue: Pointer; const ShouldAdd: Boolean);
var
  FreeList : Integer;
  Index    : Integer;
  HashCode : Integer;
  I        : Integer;
begin
  if Length(FBucketArray) = 0 then
     InitializeInternals(0);

  { Generate the hash code }
  HashCode := Hash(AKey);
  Index := HashCode mod Length(FBucketArray);

  I := FBucketArray[Index];

  while I >= 0 do
  begin
    if (FEntryArray[I].FHashCode = HashCode) and (FEntryArray[I].FKey = AKey) then
    begin
      if (ShouldAdd) then
        ExceptionHelper.Throw_DuplicateKeyError('AKey');

      FEntryArray[I].FValue := AValue;
      Exit;
    end;

    { Move to next }
    I := FEntryArray[I].FNext;
  end;

  { Adjust free spaces }
  if FFreeCount > 0 then
  begin
    FreeList := FFreeList;
    FFreeList := FEntryArray[FreeList].FNext;

    Dec(FFreeCount);
  end else
  begin
    { Adjust index if there is not enough free space }
    if FCount = Cardinal(Length(FEntryArray)) then
    begin
      Resize();
      Index := HashCode mod Length(FBucketArray);
    end;

    FreeList := FCount;
    Inc(FCount);
  end;

  { Insert the element at the right position and adjust arrays }
  FEntryArray[FreeList].FHashCode := HashCode;
  FEntryArray[FreeList].FKey := AKey;
  FEntryArray[FreeList].FValue := AValue;
  FEntryArray[FreeList].FNext := FBucketArray[Index];

  FBucketArray[Index] := FreeList;
end;

procedure TType.TPointerDictionary.Remove(const AKey: Pointer);
var
  HashCode : Integer;
  Index    : Integer;
  I        : Integer;
  RemIndex : Integer;
begin
  if Length(FBucketArray) > 0 then
  begin
    { Generate the hash code }
    HashCode := Hash(AKey);

    Index := HashCode mod Length(FBucketArray);
    RemIndex := -1;

    I := FBucketArray[Index];

    while I >= 0 do
    begin
      if (FEntryArray[I].FHashCode = HashCode) and (FEntryArray[I].FKey = AKey) then
      begin

        if RemIndex < 0 then
        begin
          FBucketArray[Index] := FEntryArray[I].FNext;
        end else
        begin
          FEntryArray[RemIndex].FNext := FEntryArray[I].FNext;
        end;

        FEntryArray[I].FHashCode := -1;
        FEntryArray[I].FNext := FFreeList;
        FEntryArray[I].FKey := default(Pointer);
        FEntryArray[I].FValue := default(Pointer);

        FFreeList := I;
        Inc(FFreeCount);

        Exit;
      end;

      RemIndex := I;
      I := FEntryArray[I].FNext;
    end;

  end;
end;

procedure TType.TPointerDictionary.Resize;
var
  XPrime : Integer;
  I      : Integer;
  Index  : Integer;
  NArr   : TBucketArray;
begin
  XPrime := Prime.GetNearestProgressionPositive(FCount * 2);

  SetLength(NArr, XPrime);

  for I := 0 to Length(NArr) - 1 do
  begin
    NArr[I] := -1;
  end;

  SetLength(FEntryArray, XPrime);

  for I := 0 to FCount - 1 do
  begin
    Index := FEntryArray[I].FHashCode mod XPrime;
    FEntryArray[I].FNext := NArr[Index];
    NArr[Index] := I;
  end;

  { Reset bucket array }
  FBucketArray := nil;
  FBucketArray := NArr;
end;

procedure TType.TPointerDictionary.SetItem(const Key: Pointer; const Value: Pointer);
begin
  { Simply call insert }
  Insert(Key, Value, false);
end;

function TType.TPointerDictionary.TryGetValue(const AKey: Pointer; out FoundValue: Pointer): Boolean;
var
  Index : Integer;
begin
  Index := FindEntry(AKey);

  if Index >= 0 then
     begin
       FoundValue := FEntryArray[Index].FValue;
       Exit(True);
     end;

  { Key not found, simply fail }
  FoundValue := Default(Pointer);
  Result := False;
end;

{ TInterfaceType }

function TInterfaceType.AreEqual(const AValue1, AValue2: IInterface): Boolean;
begin
  Result := (AValue1 = AValue2);
end;

function TInterfaceType.Compare(const AValue1, AValue2: IInterface): Integer;
begin
  if Integer(AValue1) > Integer(AValue2) then
    Result := 1
  else if Integer(AValue1) < Integer(AValue2) then
    Result := -1
  else
    Result := 0;
end;

constructor TInterfaceType.Create;
begin
  inherited;
  FTypeFamily := tfInterface;
end;

function TInterfaceType.GenerateHashCode(const AValue: IInterface): Integer;
begin
  Result := Integer(AValue);
end;

function TInterfaceType.GetString(const AValue: IInterface): String;
begin
  Result := Format(SAddress, [Integer(AValue)]);
end;

{ TPointerType }

function TPointerType.AreEqual(const AValue1, AValue2: Pointer): Boolean;
begin
  Result := (AValue1 = AValue2);
end;

function TPointerType.Compare(const AValue1, AValue2: Pointer): Integer;
begin
  if Integer(AValue1) > Integer(AValue2) then
     Result := 1
  else if Integer(AValue1) < Integer(AValue2) then
     Result := -1
  else
     Result := 0;
end;

constructor TPointerType.Create;
begin
  inherited;
  FCanBeSerializedVerified := false;
  FTypeFamily := tfPointer;
end;

function TPointerType.GenerateHashCode(const AValue: Pointer): Integer;
begin
  Result := Integer(AValue);
end;

function TPointerType.GetElementType(const AContext: IContext; const AInfo: TValueInfo): TRttiType;
var
  LElementType: TRttiType;
  LType: TRttiType;
begin
  { Obtain type information for the element }
  LType := AContext.GetTypeInformation(FTypeInfo);

  if (LType <> nil) then
    LElementType := TRttiPointerType(LType).ReferredType
  else
    LElementType := nil;

  { Serialize/Deserialize if supported }
  if (LElementType <> nil) and (LElementType.TypeKind = tkRecord) then
  begin
    { Verify if the record can be serialized }
    if not FCanBeSerializedVerified then
    begin
      FCanBeSerializedVerified := true;
      FCanBeSerialized := IsClassStructSerializable(LElementType);
    end;

    { If the struct cannot be serialized (not marked as such) fail! }
    if not FCanBeSerialized then
      ExceptionHelper.Throw_MarkedUnserializable(AInfo.Name, FTypeName);
  end else
    ExceptionHelper.Throw_Unserializable(AInfo.Name, FTypeName);

  { ... }
  Result := LElementType;
end;

function TPointerType.GetString(const AValue: Pointer): String;
begin
  Result := Format(SAddress, [Integer(AValue)]);
end;

function TPointerType.TryConvertFromVariant(const AValue: Variant; out ORes: Pointer): Boolean;
begin
  Result := true;

  try
    ORes := Pointer(Cardinal(AValue));
  except
    Result := false;
  end;
end;

function TPointerType.TryConvertToVariant(const AValue: Pointer; out ORes: Variant): Boolean;
begin
  Result := true;
  ORes := Cardinal(AValue);
end;


procedure TPointerType.DoDeserialize(const AInfo: TValueInfo; out AValue: Pointer; const AContext: IDeserializationContext);
var
  LElementType: TRttiType;
begin
  { Obtain the element type }
  LElementType := GetElementType(AContext, AInfo);

  { And now do deserialize }
  if AContext.ExpectRecordType(AInfo, AValue) then
  begin
    { Allocate enough memory for the value and initialize it }
    GetMem(AValue, LElementType.TypeSize);
    InitializeArray(AValue, LElementType.Handle, 1);

    try
      { Deserialize }
      AContext.RegisterReference(AValue);
      SerProcessStructClass(TSerializationGuts.Create(LElementType, nil, AContext), AValue, false);
      AContext.EndComplexType();
    except
      { Kill the pointer instance }
      FinalizeArray(AValue, LElementType.Handle, 1);
      FreeMem(AValue);

      AValue := nil;

      { Re-raise }
      raise;
    end;
  end;
end;

procedure TPointerType.DoSerialize(const AInfo: TValueInfo; const AValue: Pointer; const AContext: ISerializationContext);
var
  LElementType: TRttiType;
begin
  { Obtain the element type }
  LElementType := GetElementType(AContext, AInfo);

  if AContext.StartRecordType(AInfo, AValue) then
  begin
    SerProcessStructClass(TSerializationGuts.Create(LElementType, AContext, nil), AValue, true);
    AContext.EndComplexType();
  end;
end;

{ TMetaclassType }

function TMetaclassType.AreEqual(const AValue1, AValue2: TClass): Boolean;
begin
  Result := (AValue1 = AValue2);
end;

function TMetaclassType.Compare(const AValue1, AValue2: TClass): Integer;
begin
  if Integer(AValue1) > Integer(AValue2) then
     Result := 1
  else if Integer(AValue1) < Integer(AValue2) then
     Result := -1
  else
     Result := 0;
end;

constructor TMetaclassType.Create;
begin
  inherited;
  FTypeFamily := tfClassReference;
end;

function TMetaclassType.GenerateHashCode(const AValue: TClass): Integer;
begin
  Result := Integer(AValue);
end;

function TMetaclassType.GetString(const AValue: TClass): String;
begin
  Result := AValue.ClassName;
end;

{ TArrayType<T> }

function TArrayType<T>.AreEqual(const AValue1, AValue2: T): Boolean;
begin
  Result := BinaryCompare(@AValue1, @AValue2, SizeOf(T)) = 0;
end;

function TArrayType<T>.Compare(const AValue1, AValue2: T): Integer;
begin
  Result := BinaryCompare(@AValue1, @AValue2, SizeOf(T));
end;

constructor TArrayType<T>.Create;
begin
  inherited;

  FTypeFamily := tfArray;
  FIsMagic := IsManaged(TypeInfo);
end;

procedure TArrayType<T>.DoDeserialize(const AInfo: TValueInfo; out AValue: T; const AContext: IDeserializationContext);
begin
  { Call internal method }
  SerProcessStaticArray(
    TSerializationGuts.Create(AContext.GetTypeInformation(TypeInfo), nil, AContext),
    AInfo, @AValue, false);
end;

procedure TArrayType<T>.DoSerialize(const AInfo: TValueInfo; const AValue: T; const AContext: ISerializationContext);
begin
  { Call internal method }
  SerProcessStaticArray(
    TSerializationGuts.Create(AContext.GetTypeInformation(TypeInfo), AContext, nil),
    AInfo, @AValue, true);
end;

function TArrayType<T>.GenerateHashCode(const AValue: T): Integer;
begin
  Result := BinaryHash(@AValue, SizeOf(T));
end;

function TArrayType<T>.GetString(const AValue: T): String;
begin
  Result := Format(SByteCount, [SizeOf(T)]);
end;

function TArrayType<T>.Management: TTypeManagement;
begin
  if FIsMagic then
    Result := tmCompiler
  else
    Result := tmNone;
end;

{ TRecordType<T> }

function TRecordType<T>.AreEqual(const AValue1, AValue2: T): Boolean;
begin
  Result := BinaryCompare(@AValue1, @AValue2, SizeOf(T)) = 0;
end;

procedure TRecordType<T>.CheckSerializable(const AContext: IContext; const AInfo: TValueInfo);
begin
  { Verify if the record can be serialized }
  if not FCanBeSerializedVerified then
  begin
    FCanBeSerializedVerified := true;
    FCanBeSerialized := IsClassStructSerializable(AContext.GetTypeInformation(FTypeInfo));
  end;

  { If the struct cannot be serialized (not marked as such) fail! }
  if not FCanBeSerialized then
    ExceptionHelper.Throw_MarkedUnserializable(AInfo.Name, FTypeName);
end;

function TRecordType<T>.Compare(const AValue1, AValue2: T): Integer;
begin
  Result := BinaryCompare(@AValue1, @AValue2, SizeOf(T));
end;

constructor TRecordType<T>.Create;
begin
  inherited;

  FTypeFamily := tfRecord;
  FIsMagic := IsManaged(TypeInfo);
  FCanBeSerializedVerified := false;
end;

procedure TRecordType<T>.DoDeserialize(const AInfo: TValueInfo; out AValue: T; const AContext: IDeserializationContext);
begin
  { Make sure that this type can be serialized }
  CheckSerializable(AContext, AInfo);

  { Open a new block }
  AContext.ExpectRecordType(AInfo);

  { Call internal helper }
  SerProcessStructClass(TSerializationGuts.Create(AContext.GetTypeInformation(TypeInfo), nil, AContext), @AValue, false);

  AContext.EndComplexType();
end;

procedure TRecordType<T>.DoSerialize(const AInfo: TValueInfo; const AValue: T; const AContext: ISerializationContext);
begin
  { Make sure that this type can be serialized }
  CheckSerializable(AContext, AInfo);

  { Open a new block }
  AContext.StartRecordType(AInfo);

  { Call internal helper }
  SerProcessStructClass(TSerializationGuts.Create(AContext.GetTypeInformation(TypeInfo), AContext, nil), @AValue, true);

  AContext.EndComplexType();
end;

function TRecordType<T>.GenerateHashCode(const AValue: T): Integer;
begin
  Result := BinaryHash(@AValue, SizeOf(T));
end;

function TRecordType<T>.GetString(const AValue: T): String;
begin
  Result := Format(SByteCount, [SizeOf(T)]);
end;

function TRecordType<T>.Management: TTypeManagement;
begin
  if FIsMagic then
    Result := tmCompiler
  else
    Result := tmNone;
end;

{ TMethodType }

function TMethodType.AreEqual(const AValue1, AValue2: __TMethod): Boolean;
begin
  Result := BinaryCompare(@AValue1, @AValue2, SizeOf(__TMethod)) = 0;
end;

function TMethodType.Compare(const AValue1, AValue2: __TMethod): Integer;
begin
  Result := BinaryCompare(@AValue1, @AValue2, SizeOf(__TMethod));
end;

constructor TMethodType.Create;
begin
  inherited;
  FTypeFamily := tfMethod;
end;

function TMethodType.GenerateHashCode(const AValue: __TMethod): Integer;
begin
  Result := BinaryHash(@AValue, SizeOf(__TMethod));
end;

function TMethodType.GetString(const AValue: __TMethod): String;
begin
  Result := Format(SAddress, [Integer(TMethod(AValue).Code)]);
end;

{ TProcedureType }

function TProcedureType.AreEqual(const AValue1, AValue2: Pointer): Boolean;
begin
  Result := (AValue1 = AValue2);
end;

function TProcedureType.Compare(const AValue1, AValue2: Pointer): Integer;
begin
  if Integer(AValue1) > Integer(AValue2) then
     Result := 1
  else if Integer(AValue1) < Integer(AValue2) then
     Result := -1
  else
     Result := 0;
end;

constructor TProcedureType.Create;
begin
  inherited;
  FTypeFamily := tfMethod;
end;

function TProcedureType.GenerateHashCode(const AValue: Pointer): Integer;
begin
  Result := Integer(AValue);
end;

function TProcedureType.GetString(const AValue: Pointer): String;
begin
  Result := Format(SAddress, [Integer(AValue)]);
end;

function TProcedureType.TryConvertFromVariant(const AValue: Variant; out ORes: Pointer): Boolean;
begin
  Result := true;

  try
    ORes := Pointer(Cardinal(AValue));
  except
    Result := false;
  end;
end;

function TProcedureType.TryConvertToVariant(const AValue: Pointer; out ORes: Variant): Boolean;
begin
  Result := true;
  ORes := Cardinal(AValue);
end;

{ TType.TSerializationGuts }

constructor TType.TSerializationGuts.Create(const AType: TRttiType;
  const AInContext: ISerializationContext;
  const AOutContext: IDeserializationContext);
begin
  FType := AType;
  FInContext := AInContext;
  FOutContext := AOutContext;
end;

initialization
  { Create the list used to store interface refences for cached types }
  TType.FStoredInterfaces := TInterfaceList.Create();

  { Create the custom type support holder }
  TType.FCustomTypes := TType.TPointerDictionary.Create();

  { Register all system types }
  TType<Boolean>.Register(TBooleanType);
  TType<ByteBool>.Register(TByteBoolType);
  TType<WordBool>.Register(TWordBoolType);
  TType<LongBool>.Register(TLongBoolType);
  TType<TDate>.Register(TDateType);
  TType<TTime>.Register(TTimeType);
  TType<TDateTime>.Register(TDateTimeType);
  TType<OleVariant>.Register(TVariantType);
  TType<UCS4Char>.Register(TUCS4CharType);
  TType<UCS4String>.Register(TUCS4StringType);
  TType<UTF8String>.Register(TUTF8StringType);
  TType<RawByteString>.Register(TRawByteStringType);
  TType<Char>.Register(TWideCharType);

{$IFDEF ARCH_32_BIT}
  TType<NativeInt>.Register(TIntegerType);
  TType<NativeUInt>.Register(TCardinalType);
{$ELSE}
  {$IFDEF ARCH_64_BIT}
    TType<NativeInt>.Register(TInt64Type);
    TType<NativeUInt>.Register(TUInt64Type);
  {$ELSE}
    Unsupported architecture in types!
  {$ENDIF}
{$ENDIF}

finalization
  { Unregister all system types }
  TType<Boolean>.Unregister();
  TType<ByteBool>.Unregister();
  TType<WordBool>.Unregister();
  TType<LongBool>.Unregister();
  TType<TDate>.Unregister();
  TType<TTime>.Unregister();
  TType<TDateTime>.Unregister();
  TType<NativeInt>.Unregister();
  TType<NativeUInt>.Unregister();
  TType<Char>.Unregister();
  TType<OleVariant>.Unregister();
  TType<UCS4Char>.Unregister();
  TType<UCS4String>.Unregister();
  TType<UTF8String>.Unregister();
  TType<RawByteString>.Unregister();

  { Free the type support holder and the intf list }
  FreeAndNil(TType.FCustomTypes);
  FreeAndNil(TType.FStoredInterfaces);
end.
