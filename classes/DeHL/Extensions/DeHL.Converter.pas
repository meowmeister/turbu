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
unit DeHL.Converter;
interface
uses SysUtils,
     TypInfo,
     DeHL.Base,
     DeHL.Types,
     DeHL.StrConsts,
     DeHL.Exceptions;

type
  { Converter interface }
  IConverter<T1, T2> = interface
    function TryConvert(const AFrom: T1; out ATo: T2): Boolean;
    function Convert(const AFrom: T1): T2;
  end;

  { Converter class }
  TConverter<T1, T2> = class sealed(TRefCountedObject, IConverter<T1, T2>)
  private
    FFromComp: IType<T1>;
    FToComp: IType<T2>;

  public
    { Constructor/Destructor }
    constructor Create(const AFromType: IType<T1>; const AToType: IType<T2>); overload;
    constructor Create(); overload;
     destructor Destroy(); override;

    { Conversion methods }
    function TryConvert(const AFrom: T1; out ATo: T2): Boolean;
    function Convert(const AFrom: T1): T2;
  end;

implementation

{ TConverter<T1, T2> }

function TConverter<T1, T2>.Convert(const AFrom: T1): T2;
begin
  if not TryConvert(AFrom, Result) then
    ExceptionHelper.Throw_ConversionNotSupported('?');
end;

constructor TConverter<T1, T2>.Create;
begin
  { Call upper }
  Create(TType<T1>.Default, TType<T2>.Default);
end;

constructor TConverter<T1, T2>.Create(const AFromType: IType<T1>; const AToType: IType<T2>);
begin
  { Check parameters }
  if AFromType = nil then
    ExceptionHelper.Throw_ArgumentNilError('AFromType');

  if AToType = nil then
    ExceptionHelper.Throw_ArgumentNilError('AToType');

  FFromComp := AFromType;
  FToComp := AToType;
end;

destructor TConverter<T1, T2>.Destroy;
begin
  { Nothing }
  inherited;
end;

function TConverter<T1, T2>.TryConvert(const AFrom: T1; out ATo: T2): Boolean;
var
  V: Variant;
begin
  Result := False;

  { Let's try to convert one to another }
  if (not FFromComp.TryConvertToVariant(AFrom, V)) or
      not FToComp.TryConvertFromVariant(V, ATo) then
    Exit;

  Result := true;
end;

end.
