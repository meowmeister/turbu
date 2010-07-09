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
unit DeHL.Exceptions;
interface
uses SysUtils;

type
  { Exception thrown when an Type Support exception happens }
  ETypeException = class(Exception);
  { Type Extension exceptions }
  ETypeExtensionException = class(Exception);
  { Exception thrown when a type-convertion is not really supported }
  ETypeConversionNotSupported = class(ETypeException);
  { Exception thrown when a there is an incompat between T and variant array }
  ETypeIncompatibleWithVariantArray = class(ETypeException);
  { Exception thrown when a private constructor is called - never happens}
  EDefaultConstructorNotAllowed = class(Exception);
  { Exception thrown when a null Nullable is being read }
  ENullValueException = class(Exception);
  { Exception thrown when trying to get a value from an empty box }
  EEmptyBoxException = class(Exception);
  { Exception thrown when an argument is nil }
  ENilArgumentException = class(EArgumentException);
  { Exception thrown when an argument is out of space to hold the results }
  EArgumentOutOfSpaceException = class(EArgumentOutOfRangeException);
  { Exception thrown when an expected object is not of the same class as another one }
  ENotSameTypeArgumentException = class(EArgumentException);
  { Exception thrown when a Ref object is keep-aliving itself! }
  ECannotSelfReferenceException = class(Exception);


  { Base exception for all collection errors }
  ECollectionException = class(Exception);
  { Exception throw when the parent collection has changed }
  ECollectionChangedException = class(ECollectionException);
  { Exception thrown when the collection is empty }
  ECollectionEmptyException = class(ECollectionException);
  { Exception thrown when the collection has more than one elements }
  ECollectionNotOneException = class(ECollectionException);
  { Exception thrown when a key is already presend in a collection }
  EDuplicateKeyException = class(ECollectionException);
  { Exception thrown when a key is not found in a collection }
  EKeyNotFoundException = class(ECollectionException);
  { Exception thrown when an element is not a part of that collection }
  EElementNotPartOfCollection = class(ECollectionException);
  { Exception thrown when an element is already a part of another collection }
  EElementAlreadyInACollection = class(ECollectionException);
  { Exception thrown when a required position is already occupied }
  EPositionOccupiedException = class(ECollectionException);
  { Exception thrown when an argument has a wrong format }
  EArgumentFormatException = class(Exception);

  { All serialization exceptions derive from this one }
  ESerializationException = class(Exception);
  EFieldMissingException = class(ESerializationException);
  ESerializationValueException = class(ESerializationException);
  ESerializationReferenceException = class(ESerializationException);

  ExceptionHelper = class sealed
  public
    { Type support }
    class procedure Throw_NoDefaultTypeError(const TypeName: String);
    class procedure Throw_CustomTypeHasNoRTTI();
    class procedure Throw_RuntimeTypeRestrictionFailed(const TypeName: String);
    class procedure Throw_CustomTypeAlreadyRegistered(const TypeName: String);
    class procedure Throw_CustomTypeNotYetRegistered(const TypeName: String);
    class procedure Throw_ConversionNotSupported(const ToTypeName: String);
    class procedure Throw_TypeIncompatibleWithVariantArray(const TypeName: String);
    class procedure Throw_MissingFieldError(const TypeName, FieldName: String);

    { Serialization: Entities }
    class procedure Throw_Unserializable(const EntityName, TypeName: String);
    class procedure Throw_MarkedUnserializable(const EntityName, TypeName: String);
    class procedure Throw_WrongOrMissingRTTI(const EntityName, TypeName: String);
    class procedure Throw_InvalidSerializationIdentifier(const IdName: String);

    { Serialization: Values }
    class procedure Throw_ValueSerializationFailed(const TypeName: String);  { KEEP }
    class procedure Throw_UnexpectedDeserializationEntity(const EntityName: String);
    class procedure Throw_InvalidDeserializationValue(const EntityName: String);
    class procedure Throw_BinaryValueSizeMismatch(const EntityName, TypeName: String);
    class procedure Throw_UnexpectedReferencedType(const EntityName: String);
    class procedure Throw_ExpectedReferencedType(const EntityName: String);

    { Serialization: Context }
    class procedure Throw_BadSerializationContext(const EntityName: String);
    class procedure Throw_MissingCompositeType();
    class procedure Throw_InvalidArray(const EntityName: String);
    class procedure Throw_DeserializationReadError(const EntityName: String);

    { Serialization: References }
    class procedure Throw_ReferencePointNotYetDeserialized(const EntityName: String);
    class procedure Throw_RefRegisteredOrIsNil(const EntityName: String);

    { Custom, mostly boxing and nullable }
    class procedure Throw_NullValueRequested();
    class procedure Throw_TheBoxIsEmpty();
    class procedure Throw_TypeExtensionAlreadyRegistered(const TypeName: String);
    class procedure Throw_TypeExtensionNotYetRegistered(const TypeName: String);

    { Maths }
    class procedure Throw_OverflowError();
    class procedure Throw_DivByZeroError();
    class procedure Throw_NoMathExtensionForType(const TypeName: String);

    { Internals }
    class procedure Throw_DefaultConstructorNotAllowedError();
    class procedure Throw_CannotSelfReferenceError();

    { Arguments }
    class procedure Throw_ArgumentNotSameTypeError(const ArgName: String);
    class procedure Throw_ArgumentNilError(const ArgName: String);
    class procedure Throw_ArgumentOutOfRangeError(const ArgName: String);
    class procedure Throw_ArgumentOutOfSpaceError(const ArgName: String);
    class procedure Throw_InvalidArgumentFormatError(const ArgName: String);
    class procedure Throw_ArgumentConverError(const ArgName: String);

    { Collection errors }
    class procedure Throw_CollectionChangedError();
    class procedure Throw_CollectionEmptyError();
    class procedure Throw_CollectionHasMoreThanOneElement();
    class procedure Throw_DuplicateKeyError(const ArgName: String);
    class procedure Throw_KeyNotFoundError(const ArgName: String);
    class procedure Throw_ElementNotPartOfCollectionError(const ArgName: String);
    class procedure Throw_ElementAlreadyPartOfCollectionError(const ArgName: String);
    class procedure Throw_PositionOccupiedError();
  end;

implementation
uses
  DeHL.StrConsts;

{ ExceptionHelper }

class procedure ExceptionHelper.Throw_ArgumentNilError(const ArgName: String);
begin
  raise ENilArgumentException.CreateFmt(SNilArgument, [ArgName]);
end;

class procedure ExceptionHelper.Throw_ArgumentNotSameTypeError(const ArgName: String);
begin
  raise ENotSameTypeArgumentException.CreateFmt(SNotSameTypeArgument, [ArgName]);
end;

class procedure ExceptionHelper.Throw_ArgumentOutOfRangeError(const ArgName: String);
begin
  raise EArgumentOutOfRangeException.CreateFmt(SOutOfRangeArgument, [ArgName]);
end;

class procedure ExceptionHelper.Throw_ArgumentOutOfSpaceError(const ArgName: String);
begin
  raise EArgumentOutOfSpaceException.CreateFmt(SOutOfSpaceArgument, [ArgName]);
end;

class procedure ExceptionHelper.Throw_BadSerializationContext(const EntityName: String);
begin
  raise ESerializationException.CreateFmt(SBadSerializationContext, [EntityName]);
end;

class procedure ExceptionHelper.Throw_CannotSelfReferenceError;
begin
  raise ECannotSelfReferenceException.Create(SCannotSelfReference);
end;

class procedure ExceptionHelper.Throw_CollectionChangedError;
begin
  raise ECollectionChangedException.Create(SParentCollectionChanged);
end;

class procedure ExceptionHelper.Throw_CollectionEmptyError;
begin
  raise ECollectionEmptyException.Create(SEmptyCollection);
end;

class procedure ExceptionHelper.Throw_CollectionHasMoreThanOneElement;
begin
  raise ECollectionNotOneException.Create(SCollectionHasMoreThanOneElements);
end;

class procedure ExceptionHelper.Throw_ConversionNotSupported(const ToTypeName: String);
begin
  raise ETypeConversionNotSupported.CreateFmt(STypeConversionNotSupported, [ToTypeName]);
end;

class procedure ExceptionHelper.Throw_CustomTypeAlreadyRegistered(const TypeName: String);
begin
  raise ETypeException.CreateFmt(SCustomTypeAlreadyRegistered, [TypeName]);
end;

class procedure ExceptionHelper.Throw_CustomTypeHasNoRTTI;
begin
  raise ETypeException.Create(SCustomTypeHasNoRTTI);
end;

class procedure ExceptionHelper.Throw_CustomTypeNotYetRegistered(const TypeName: String);
begin
  raise ETypeException.CreateFmt(SCustomTypeNotYetRegistered, [TypeName]);
end;

class procedure ExceptionHelper.Throw_ArgumentConverError(const ArgName: String);
begin
    raise EConvertError.CreateFmt(SConvertProblemArgument, [ArgName]);
end;

class procedure ExceptionHelper.Throw_DefaultConstructorNotAllowedError;
begin
  raise EDefaultConstructorNotAllowed.Create(SDefaultParameterlessCtorNotAllowed);
end;

class procedure ExceptionHelper.Throw_DeserializationReadError(const EntityName: String);
begin
  raise ESerializationException.CreateFmt(SDeserializationReadError, [EntityName]);
end;

class procedure ExceptionHelper.Throw_DivByZeroError();
begin
  raise EDivByZero.Create(SDivisionByZero);
end;

class procedure ExceptionHelper.Throw_DuplicateKeyError(const ArgName: String);
begin
  raise EDuplicateKeyException.CreateFmt(SDuplicateKey, [ArgName]);
end;

class procedure ExceptionHelper.Throw_ElementAlreadyPartOfCollectionError(const ArgName: String);
begin
  raise EElementAlreadyInACollection.CreateFmt(SElementAlreadyInAnotherCollection, [ArgName]);
end;

class procedure ExceptionHelper.Throw_ElementNotPartOfCollectionError(const ArgName: String);
begin
  raise EElementNotPartOfCollection.CreateFmt(SElementNotInCollection, [ArgName]);
end;

class procedure ExceptionHelper.Throw_ExpectedReferencedType(const EntityName: String);
begin
  raise ESerializationException.CreateFmt(SExpectedReferencedType, [EntityName]);
end;

class procedure ExceptionHelper.Throw_InvalidArgumentFormatError(const ArgName: String);
begin
  raise EArgumentFormatException.CreateFmt(SBrokenFormatArgument, [ArgName]);
end;

class procedure ExceptionHelper.Throw_InvalidArray(const EntityName: String);
begin
  raise ESerializationException.CreateFmt(SInvalidArray, [EntityName]);
end;

class procedure ExceptionHelper.Throw_InvalidDeserializationValue(const EntityName: String);
begin
  raise ESerializationValueException.CreateFmt(SInvalidDeserializationValue, [EntityName]);
end;

class procedure ExceptionHelper.Throw_InvalidSerializationIdentifier(const IdName: String);
begin
  raise ESerializationException.CreateFmt(SInvalidSerializationIdentifier, [IdName]);
end;

class procedure ExceptionHelper.Throw_KeyNotFoundError(const ArgName: String);
begin
  raise EKeyNotFoundException.CreateFmt(SKeyNotFound, [ArgName]);
end;

class procedure ExceptionHelper.Throw_MissingCompositeType;
begin
  raise EFieldMissingException.Create(SMissingCompositeType);
end;

class procedure ExceptionHelper.Throw_MissingFieldError(const TypeName, FieldName: String);
begin
  raise EFieldMissingException.CreateFmt(SNoSuchField, [TypeName, FieldName]);
end;

class procedure ExceptionHelper.Throw_NoDefaultTypeError(const TypeName: String);
begin
  raise ETypeException.CreateFmt(SNoDefaultType, [TypeName]);
end;

class procedure ExceptionHelper.Throw_UnexpectedDeserializationEntity(const EntityName: String);
begin
  raise ESerializationValueException.CreateFmt(SUnexpectedDeserializationEntity, [EntityName]);
end;

class procedure ExceptionHelper.Throw_NoMathExtensionForType(const TypeName: String);
begin
  raise ETypeExtensionException.CreateFmt(SNoMathExtensionForType, [TypeName]);
end;

class procedure ExceptionHelper.Throw_MarkedUnserializable(const EntityName, TypeName: String);
begin
  raise ESerializationException.CreateFmt(SMarkedUnserializable, [EntityName, TypeName]);
end;

class procedure ExceptionHelper.Throw_NullValueRequested;
begin
  raise ENullValueException.Create(SNullValueRequested);
end;

class procedure ExceptionHelper.Throw_OverflowError();
begin
  raise EOverflow.Create(SArithmeticOverflow);
end;

class procedure ExceptionHelper.Throw_PositionOccupiedError;
begin
  raise EPositionOccupiedException.Create(SRequestedPositionIsOccupied);
end;

class procedure ExceptionHelper.Throw_ReferencePointNotYetDeserialized(const EntityName: String);
begin
  raise ESerializationReferenceException.CreateFmt(SReferencePointNotYetDeserialized, [EntityName]);
end;

class procedure ExceptionHelper.Throw_RefRegisteredOrIsNil(const EntityName: String);
begin
  raise ESerializationReferenceException.CreateFmt(SRefRegisteredOrIsNil, [EntityName]);
end;

class procedure ExceptionHelper.Throw_RuntimeTypeRestrictionFailed(const TypeName: String);
begin
  raise ETypeException.CreateFmt(SRuntimeTypeRestrictionFailed, [TypeName]);
end;

class procedure ExceptionHelper.Throw_TheBoxIsEmpty;
begin
  raise EEmptyBoxException.Create(STheBoxIsEmpty);
end;

class procedure ExceptionHelper.Throw_TypeExtensionAlreadyRegistered(const TypeName: String);
begin
  raise ETypeExtensionException.CreateFmt(SExtensionTypeAlreadyRegistered, [TypeName]);
end;

class procedure ExceptionHelper.Throw_TypeExtensionNotYetRegistered(const TypeName: String);
begin
  raise ETypeExtensionException.CreateFmt(SExtensionTypeNotYetRegistered, [TypeName]);
end;

class procedure ExceptionHelper.Throw_TypeIncompatibleWithVariantArray(const TypeName: String);
begin
  raise ETypeIncompatibleWithVariantArray.CreateFmt(STypeIncompatibleWithVariantArray, [TypeName]);
end;

class procedure ExceptionHelper.Throw_UnexpectedReferencedType(const EntityName: String);
begin
  raise ESerializationException.CreateFmt(SUnexpectedReferencedType, [EntityName]);
end;

class procedure ExceptionHelper.Throw_Unserializable(const EntityName, TypeName: String);
begin
  raise ESerializationException.CreateFmt(SUnserializable, [EntityName, TypeName]);
end;

class procedure ExceptionHelper.Throw_ValueSerializationFailed(const TypeName: String);
begin
  raise ESerializationException.CreateFmt(SValueSerializationFailed, [TypeName]);
end;

class procedure ExceptionHelper.Throw_WrongOrMissingRTTI(const EntityName, TypeName: String);
begin
  raise ESerializationException.CreateFmt(SWrongOrMissingRTTI, [EntityName, TypeName]);
end;

class procedure ExceptionHelper.Throw_BinaryValueSizeMismatch(const EntityName, TypeName: String);
begin
  raise ESerializationValueException.CreateFmt(SBinaryValueSizeMismatch, [EntityName, TypeName]);
end;

end.
