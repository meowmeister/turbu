(*
* Copyright (c) 2009-2010, Ciobanu Alexandru
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
unit DeHL.WideCharSet;
interface
uses SysUtils,
     DeHL.Base,
     DeHL.Types,
     DeHL.Exceptions,
     DeHL.Serialization,
     DeHL.Collections.Base;

type
  { The Wide char set type }
  TWideCharSet = record
  private const
    CCharSets = 256;

  private type
    { Array of pieces }
    TPieceArray = TArray<TSysCharSet>;

    { The enumerator object }
    TEnumerator = class(TEnumerator<Char>)
    private
      FPieces: TPieceArray;
      FCharId, FCharCount: Cardinal;
      FChar: Char;
    public
      { Constructor }
      constructor Create(const APieces: TPieceArray);

      function GetCurrent(): Char; override;
      function MoveNext(): Boolean; override;
    end;

    TEnumerable = class(TEnexCollection<Char>)
    private
      FPieces: TPieceArray;

    public
      { The constructor }
      constructor Create(const APieces: TPieceArray);

      { IEnumerable<T> }
      function GetEnumerator(): IEnumerator<Char>; override;
    end;

  private
    FPieces: TPieceArray;

    { Sets or unsets a char in the array }
    procedure MarkChar(const AChar: Char; const AMark: Boolean); inline;
  public
    { Public constructors }
    constructor Create(const ACharSet: TWideCharSet); overload;
    constructor Create(const ACharSet: TSysCharSet); overload;
    constructor Create(const AChar: Char); overload;
    constructor Create(const AString: String); overload;

    { Enumerability }
    function GetEnumerator(): IEnumerator<Char>;
    function AsCollection(): IEnexCollection<Char>;

    { Equality and inequality }
    class operator Equal(const ALeft, ARight: TWideCharSet): Boolean;
    class operator NotEqual(const ALeft, ARight: TWideCharSet): Boolean; inline;

    { Arithmetic operators }
    class operator Add(const ALeft: TWideCharSet; const AChar: Char): TWideCharSet; inline;
    class operator Add(const ALeft: TWideCharSet; const ARight: TWideCharSet): TWideCharSet;

    class operator Subtract(const ALeft: TWideCharSet; AChar: Char): TWideCharSet; inline;
    class operator Subtract(const ALeft: TWideCharSet; ARight: TWideCharSet): TWideCharSet;

    { Set operators }
    class operator Include(const ALeft: TWideCharSet; const AChar: Char): TWideCharSet; inline;
    class operator Include(const ALeft: TWideCharSet; const AString: String): TWideCharSet; inline;
    class operator Include(const ALeft: TWideCharSet; const ASet: TWideCharSet): TWideCharSet; inline;

    class operator Exclude(const ALeft: TWideCharSet; const AChar: Char): TWideCharSet; inline;
    class operator Exclude(const ALeft: TWideCharSet; const AString: String): TWideCharSet; inline;
    class operator Exclude(const ALeft: TWideCharSet; const ASet: TWideCharSet): TWideCharSet; inline;

    { Warning: Cause the IDE to spew crap }
    class operator In(const AChar: Char; const ARight: TWideCharSet): Boolean;

    class operator Implicit(const ARight: TSysCharSet): TWideCharSet; inline;
    class operator Implicit(const ALeft: TWideCharSet): TSysCharSet; inline;
  end;

  { WideChar set type support }
  TWideCharSetType = class sealed(TRecordType<TWideCharSet>)
  protected
    { Serialization }
    procedure DoSerialize(const AInfo: TValueInfo; const AValue: TWideCharSet; const Acontext: ISerializationContext); override;
    procedure DoDeserialize(const AInfo: TValueInfo; out AValue: TWideCharSet; const AContext: IDeserializationContext); override;

  public
    { Comparator }
    function Compare(const AValue1, AValue2: TWideCharSet): Integer; override;

    { Equality Comparator }
    function AreEqual(const AValue1, AValue2: TWideCharSet): Boolean; override;

    { Hash code provider }
    function GenerateHashCode(const AValue: TWideCharSet): Integer; override;

    { Get String representation }
    function GetString(const AValue: TWideCharSet): String; override;

    { Variant Conversion }
    function TryConvertToVariant(const AValue: TWideCharSet; out ORes: Variant): Boolean; override;
    function TryConvertFromVariant(const AValue: Variant; out ORes: TWideCharSet): Boolean; override;
  end;

  { The global overload for CharInSet that works with Wide char sets }
  function CharInSet(C: WideChar; const CharSet: TWideCharSet): Boolean; overload; inline;

implementation

{ TWideCharSet }

{ The global overload for CharInSet that works with Wide char sets }
function CharInSet(C: WideChar; const CharSet: TWideCharSet): Boolean; overload; inline;
begin
  { Nothing more :) }
  Result := C in CharSet;
end;

class operator TWideCharSet.Add(const ALeft: TWideCharSet; const AChar: Char): TWideCharSet;
begin
  { Copy the input and add the char }
  Result := TWideCharSet.Create(ALeft);
  Result.MarkChar(AChar, true);
end;

constructor TWideCharSet.Create(const ACharSet: TSysCharSet);
begin
  { Grow the length with the necessary pieces to hold the sys set }
  SetLength(FPieces, 1);
  FPieces[0] := ACharSet;
end;

constructor TWideCharSet.Create(const ACharSet: TWideCharSet);
var
  L: Cardinal;
begin
  { Copy the original }
  L := Length(ACharSet.FPieces);
  SetLength(FPieces, L);

  if L > 0 then
    Move(ACharSet.FPieces[0], FPieces[0], L * SizeOf(TSysCharSet));
end;

class operator TWideCharSet.Add(const ALeft: TWideCharSet; const ARight: TWideCharSet): TWideCharSet;
var
  I, L, R: Cardinal;
begin
  L := Length(ALeft.FPieces);
  R := Length(ARight.FPieces);

  { Special cases }
  if L = 0 then
    Exit(ARight);

  if R = 0 then
    Exit(ALeft);

  if L > R then
  begin
    Result := TWideCharSet.Create(ALeft);

    for I := 0 to R - 1 do
      Result.FPieces[I] := Result.FPieces[I] + ARight.FPieces[I];
  end else
  begin
    Result := TWideCharSet.Create(ARight);

    for I := 0 to L - 1 do
      Result.FPieces[I] := Result.FPieces[I] + ALeft.FPieces[I];
  end
end;

function TWideCharSet.AsCollection: IEnexCollection<Char>;
begin
  Result := TEnumerable.Create(FPieces);
end;

constructor TWideCharSet.Create(const AString: String);
var
  LChar: Char;
begin
  { Kill myself }
  FPieces := nil;

  { Simply add all the stuff in }
  for LChar in AString do
    MarkChar(LChar, true);
end;

class operator TWideCharSet.Exclude(const ALeft: TWideCharSet; const AString: string): TWideCharSet;
var
  LChar: Char;
begin
  { Copy the input }
  Result := TWideCharSet.Create(ALeft);

  { Simply remove all the stuff }
  for LChar in AString do
    Result.MarkChar(LChar, false);
end;

constructor TWideCharSet.Create(const AChar: Char);
begin
  { Kill myself }
  FPieces := nil;

  { Mark the char in the set }
  MarkChar(AChar, true);
end;

class operator TWideCharSet.Exclude(const ALeft: TWideCharSet; const AChar: Char): TWideCharSet;
begin
  { Copy the input and remove the char }
  Result := TWideCharSet.Create(ALeft);
  Result.MarkChar(AChar, false);
end;

class operator TWideCharSet.Include(const ALeft: TWideCharSet; const AChar: Char): TWideCharSet;
begin
  { Copy the input and add the char }
  Result := TWideCharSet.Create(ALeft);
  Result.MarkChar(AChar, true);
end;

class operator TWideCharSet.Implicit(const ARight: TSysCharSet): TWideCharSet;
begin
  { Call the constructor }
  Result := TWideCharSet.Create(ARight);
end;

class operator TWideCharSet.Implicit(const ALeft: TWideCharSet): TSysCharSet;
begin
  { Exit on empty set }
  if Length(ALeft.FPieces) = 0 then
    Result := []
  else
    Result := ALeft.FPieces[0];
end;

class operator TWideCharSet.Include(const ALeft: TWideCharSet; const AString: string): TWideCharSet;
var
  LChar: Char;
begin
  { Copy the input }
  Result := TWideCharSet.Create(ALeft);

  { Simply add all the stuff in }
  for LChar in AString do
    Result.MarkChar(LChar, true);
end;

procedure TWideCharSet.MarkChar(const AChar: Char; const AMark: Boolean);
var
  LPieceIndex: Integer;  {REQ}
  LPieceChar: AnsiChar;
begin
  { Calculate piece indexes }
  LPieceIndex := Word(AChar) div CCharSets;
  LPieceChar := AnsiChar(Word(AChar) mod CCharSets);

  { No piece defined just yet }
  if LPieceIndex >= Length(FPieces) then
  begin
    { Exit on un-marking ... nothing to do here actually }
    if not AMark then
      Exit;

    { Increase the length slightly }
    SetLength(FPieces, LPieceIndex + 1);
  end;

  { Set or clear the bit at the specified position }
  if AMark then
    Include(FPieces[LPieceIndex], LPieceChar)
  else
  begin
    Exclude(FPieces[LPieceIndex], LPieceChar);

    { Corner case, need to decrese the size accordingly }
    if (LPieceIndex = Length(FPieces) - 1) then
    begin
      while (LPieceIndex >= 0) and (FPieces[LPieceIndex] = []) do
        Dec(LPieceIndex);

      SetLength(FPieces, LPieceIndex + 1);
    end;
  end;
end;

class operator TWideCharSet.NotEqual(const ALeft, ARight: TWideCharSet): Boolean;
begin
  Result := not (ALeft = ARight);
end;

class operator TWideCharSet.Subtract(const ALeft: TWideCharSet; ARight: TWideCharSet): TWideCharSet;
var
  I, L, R, M: Cardinal;
begin
  L := Length(ALeft.FPieces);
  R := Length(ARight.FPieces);

  { Special cases }
  if L = 0 then
    Exit(ARight);

  if R = 0 then
    Exit(ALeft);

  Result := TWideCharSet.Create(ALeft);

  if L > R then
    M := R
  else
    M := L;

  for I := 0 to M - 1 do
    Result.FPieces[I] := Result.FPieces[I] - ARight.FPieces[I];
end;

class operator TWideCharSet.Subtract(const ALeft: TWideCharSet; AChar: Char): TWideCharSet;
begin
  { Copy the input and remove the char }
  Result := TWideCharSet.Create(ALeft);
  Result.MarkChar(AChar, false);
end;

class operator TWideCharSet.In(const AChar: Char; const ARight: TWideCharSet): Boolean;
var
  LPieceIndex: Cardinal;
  LPieceChar: AnsiChar;
begin
  { Calculate piece indexes }
  LPieceIndex := Word(AChar) div CCharSets;
  LPieceChar := AnsiChar(Word(AChar) mod CCharSets);

  { No piece defined just yet }
  if LPieceIndex >= Cardinal(Length(ARight.FPieces)) then
    Result := false
  else
    Result := CharInSet(LPieceChar, ARight.FPieces[LPieceIndex]);
end;

class operator TWideCharSet.Equal(const ALeft, ARight: TWideCharSet): Boolean;
begin
  { Exit on different lengths }
  if Length(ALeft.FPieces) <> Length(ARight.FPieces) then
    Exit(false);

  Result := CompareMem(@ALeft.FPieces[0], @ARight.FPieces[0], Length(ALeft.FPieces) * SizeOf(TSysCharSet));
end;

class operator TWideCharSet.Exclude(const ALeft, ASet: TWideCharSet): TWideCharSet;
begin
  Result := ALeft - ASet;
end;

function TWideCharSet.GetEnumerator: IEnumerator<Char>;
begin
  { Create the enumerator }
  Result := TEnumerator.Create(FPieces);
end;

class operator TWideCharSet.Include(const ALeft, ASet: TWideCharSet): TWideCharSet;
begin
  Result := ALeft + ASet;
end;

{ TWideCharSet.TEnumerator }

constructor TWideCharSet.TEnumerator.Create(const APieces: TPieceArray);
begin
  inherited Create();

  FPieces := APieces;
  FCharCount := Length(FPieces) * CCharSets;
  FCharId := 0;
end;

function TWideCharSet.TEnumerator.GetCurrent: Char;
begin
  Result := FChar;
end;

function TWideCharSet.TEnumerator.MoveNext: Boolean;
begin
  { Defauls to false }
  Result := false;

  while FCharId < FCharCount do
  begin
    { Check whether the char is in the set specified ANSI set }
    if CharInSet(AnsiChar(FCharId mod CCharSets), FPieces[FCharId div CCharSets]) then
    begin
      FChar := Char(FCharId);

      { Found a char }
      Result := true;
    end;

    Inc(FCharId);

    { Exit on successeful find }
    if Result then
      Exit;
  end;
end;

{ TWideCharSet.TEnumerable }

constructor TWideCharSet.TEnumerable.Create(const APieces: TPieceArray);
begin
  inherited Create();

  { Copy }
  FPieces := APieces;

  { Install the type }
  InstallType(TType<Char>.Default);
end;

function TWideCharSet.TEnumerable.GetEnumerator: IEnumerator<Char>;
begin
  Result := TEnumerator.Create(FPieces);
end;

{ TWideCharSetType }

function TWideCharSetType.AreEqual(const AValue1, AValue2: TWideCharSet): Boolean;
begin
  Result := AValue1 = AValue2;
end;

function TWideCharSetType.Compare(const AValue1, AValue2: TWideCharSet): Integer;
begin
  { No ordering }
  if AValue1 = AValue2 then
    Result := 0
  else
    Result := 1;
end;

procedure TWideCharSetType.DoDeserialize(const AInfo: TValueInfo; out AValue: TWideCharSet; const AContext: IDeserializationContext);
var
  LStr: String;
begin
  AContext.GetValue(AInfo, LStr);
  AValue := TWideCharSet.Create(LStr);
end;

procedure TWideCharSetType.DoSerialize(const AInfo: TValueInfo; const AValue: TWideCharSet; const AContext: ISerializationContext);
begin
  { The value of the charset is a simple string }
  AContext.AddValue(AInfo, GetString(AValue));
end;

function TWideCharSetType.GenerateHashCode(const AValue: TWideCharSet): Integer;
begin
  { Generate the hash }
  Result := BinaryHash(@AValue.FPieces[0], Length(AValue.FPieces) * SizeOf(TSysCharSet));
end;

function TWideCharSetType.GetString(const AValue: TWideCharSet): String;
var
  LChar: Char;
begin
  Result := '';

  { Build up the string }
  for LChar in AValue do
    Result := Result + LChar;
end;

function TWideCharSetType.TryConvertFromVariant(const AValue: Variant; out ORes: TWideCharSet): Boolean;
begin
  try
    ORes := TWideCharSet.Create(String(AValue));
    Result := true;
  except
    Result := false;
  end;
end;

function TWideCharSetType.TryConvertToVariant(const AValue: TWideCharSet; out ORes: Variant): Boolean;
begin
  ORes := GetString(AValue);
  Result := true;
end;

initialization
  { Register custom type }
  TType<TWideCharSet>.Register(TWideCharSetType);

finalization
  { Unregister custom type }
  TType<TWideCharSet>.Unregister();

end.
