(*
* Copyright (c) 2008-2009, Ciobanu Alexandru
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
unit DeHL.Base;
interface

uses
  Windows,
  SysUtils,
  Rtti,
  TypInfo;

type
{$HINTS OFF}
  { Use Reference Counted object for your RefCount needs }
  TRefCountedObject = class abstract(TInterfacedObject, IInterface)
  private
    FKeepAliveList: TArray<IInterface>;
    FInConstruction: Boolean;

  protected
    { Life-time }
    procedure KeepObjectAlive(const AObject: TRefCountedObject);
    procedure ReleaseObject(const AObject: TRefCountedObject; const FreeObject:
      Boolean = false);

    { Get optional reference }
    function ExtractReference(): IInterface;

    { Checks whether an object is constructing }
    property Constructing: Boolean read FInConstruction;

  private
    constructor Create();

    { Override the new instace class method }
    class function NewInstance: TObject; override;
    procedure AfterConstruction; override;
  end;
{$HINTS ON}

  { Use this object for singleton purposes }
  TSingletonObject = class abstract(TObject, IInterface)
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  end;

{$HINTS OFF}
  { A simple object }
  TSimpleObject = class abstract(TObject)
  public
    constructor Create();
  end;
{$HINTS ON}

type
  { Use Activator to create blind class instances }
  Activator = record
  public
    class function CreateInstance(const AQualifiedName: String): TObject; overload; static;
    class function CreateInstance(const AClassInfo: TClass): TObject; overload; static;
    class function CreateInstance(const ATypeInfo: PTypeInfo): TObject; overload; static;
    class function CreateInstance(const ARttiObject: TRttiInstanceType): TObject; overload; static;
  end;
var
  __Marker: IInterface;

implementation

uses
  DeHL.Exceptions;

{ TSingletonObject }

function TSingletonObject.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  { Do nothing }
  Result := E_NOINTERFACE;
end;

function TSingletonObject._AddRef: Integer;
begin
  { Do nothing }
  Result := -1;
end;

function TSingletonObject._Release: Integer;
begin
  { Do nothing }
  Result := -1;
end;

{ TSimpleObject }

constructor TSimpleObject.Create;
begin
  { Throw a default exception }
  ExceptionHelper.Throw_DefaultConstructorNotAllowedError();
end;

{ TRefCountedObject }

procedure TRefCountedObject.AfterConstruction;
begin
  FInConstruction := false;
  inherited AfterConstruction();
end;

constructor TRefCountedObject.Create;
begin
  { Throw a default exception }
  ExceptionHelper.Throw_DefaultConstructorNotAllowedError();
end;

function TRefCountedObject.ExtractReference(): IInterface;
var
  Ref: Integer;
begin
  { While constructing, an object has an implicit ref count of 1 }
  if FInConstruction then
    Ref := 1
  else
    Ref := 0;

  {
      If the object is referenced in other places as an
      interface, get a new one, otherwise return nil
   }
  if RefCount > Ref then
    Result := Self
  else
    Result := nil;
end;

procedure TRefCountedObject.KeepObjectAlive(const AObject: TRefCountedObject);
var
  I, L: Integer;
  II: IInterface;
begin
  { Skip nil references }
  if AObject = nil then
    Exit;

  { Cannot self-ref! }
  if AObject = Self then
    ExceptionHelper.Throw_CannotSelfReferenceError();

  { Extract an optional reference, do not continue if failed }
  II := AObject.ExtractReference();
  if II = nil then
    Exit;

  L := Length(FKeepAliveList);

  { Find a free spot }
  if L > 0 then
    for I := 0 to L - 1 do
      if FKeepAliveList[I] = nil then
      begin
        FKeepAliveList[I] := II;
        Exit;
      end;

  { No free spots, extend array and insert the ref there }
  SetLength(FKeepAliveList, L + 1);
  FKeepAliveList[L] := II;
end;

class function TRefCountedObject.NewInstance: TObject;
begin
  Result := inherited NewInstance();

  { Set in construction! }
  TRefCountedObject(Result).FInConstruction := true;
end;

procedure TRefCountedObject.ReleaseObject(const AObject: TRefCountedObject;
  const FreeObject: Boolean = false);
var
  I, L: Integer;
  II: IInterface;
begin
  { Do nothing on nil references, since it may be calle din destructors }
  if AObject = nil then
    Exit;

  { Cannot self-ref! }
  if AObject = Self then
    ExceptionHelper.Throw_CannotSelfReferenceError();

  { Extract an optional reference, if none received, exit }
  II := AObject.ExtractReference();
  if II = nil then
  begin
    if FreeObject then
      AObject.Free;

    Exit;
  end;

  L := Length(FKeepAliveList);

  { Find a free spot }
  if L > 0 then
    for I := 0 to L - 1 do
      if FKeepAliveList[I] = II then
      begin
        { Release the spot and kill references to the interface }
        FKeepAliveList[I] := nil;
        II := nil;
        Exit;
      end;
end;


{ Activator }

class function Activator.CreateInstance(const ATypeInfo: PTypeInfo): TObject;
var
  LCtx: TRttiContext;
  LType: TRttiType;
begin
  if ATypeInfo = nil then
    ExceptionHelper.Throw_ArgumentNilError('ATypeInfo');

  LType := LCtx.GetType(ATypeInfo);

  if LType is TRttiInstanceType then
    Result := CreateInstance(TRttiInstanceType(LType))
  else
    Result := nil;
end;

class function Activator.CreateInstance(const ARttiObject: TRttiInstanceType): TObject;
var
  LMethod: TRttiMethod;
begin
  if ARttiObject = nil then
    ExceptionHelper.Throw_ArgumentNilError('ARttiObject');

  { Invoke the first parameterless constructor found. }
  for LMethod in ARttiObject.GetMethods() do
    if LMethod.HasExtendedInfo and LMethod.IsConstructor then
      if LMethod.GetParameters() = nil then
        Exit(LMethod.Invoke(ARttiObject.MetaclassType, []).AsObject);

  { Not found ... Use the old fashioned way }
  Result := ARttiObject.MetaclassType.Create();
end;

class function Activator.CreateInstance(const AClassInfo: TClass): TObject;
var
  LCtx: TRttiContext;
begin
  if AClassInfo = nil then
    ExceptionHelper.Throw_ArgumentNilError('AClassInfo');

  { Create an instance }
  Result := CreateInstance(TRttiInstanceType(LCtx.GetType(AClassInfo)));
end;

class function Activator.CreateInstance(const AQualifiedName: String): TObject;
var
  LType: TRttiInstanceType;
  LCtx: TRttiContext;
begin
  { Defaults tp nil }
  LType := nil;

  try
    LType := LCtx.FindType(AQualifiedName) as TRttiInstanceType;
  except // Catch and forget. Will fail later on
  end;

  { Call the other method now }
  Result := CreateInstance(TRttiInstanceType(LType))
end;

initialization
  __Marker := TInterfacedObject.Create();

finalization
  __Marker := nil;

end.
