unit turbu_classes;
{*****************************************************************************
* The contents of this file are used with permission, subject to
* the Mozilla Public License Version 1.1 (the "License"); you may
* not use this file except in compliance with the License. You may
* obtain a copy of the License at
* http://www.mozilla.org/MPL/MPL-1.1.html
*
* Software distributed under the License is distributed on an
* "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
* implied. See the License for the specific language governing
* rights and limitations under the License.
*
*****************************************************************************
*
* This file was created by Mason Wheeler.  He can be reached for support at
* www.turbu-rpg.com.
*****************************************************************************}

interface
uses
   classes, sysUtils, generics.collections, DB,
   turbu_defs, turbu_constants;

type
   TRpgDatafile = class abstract(TObject)
   protected
      type
      TSetSaver = record
         case boolean of
            true: (aSet: TByteSet);
            false: (aArray: packed array[1..32] of byte);
      end;
   class function getSetLength(mySet: TByteSet): byte;

   var
      FName: string;
      FId: smallint;
      FModified: boolean;

      function getDatasetName: string; virtual;
   public
      constructor Load(savefile: TStream);
      procedure save(savefile: TStream); virtual;
      procedure upload(db: TDataSet); virtual;
      procedure download(db: TDataset); virtual;

      function GetAllEvents: TStringList;

      property name: string read FName write FName;
      property id: smallint read FId write FId;
      property modified: boolean read FModified write FModified;
      property datasetName: string read getDatasetName;
   end;

   TScriptRecord = class abstract(TObject)
   private
      FName: string;
      FDesignName: string;
      function getMethod: TRpgMethod;
   protected
      FMethod: TRpgMethod;
      constructor Create(name, designName: string; script: TRpgMethod);
      procedure setName(const Value: string); virtual;
   public
      procedure upload(db: TDataSet); virtual;
      procedure update(db: TDataSet); virtual;

      property name: string read FName write setName;
      property designName: string read FDesignName write FDesignName;
      property baseMethod: TRpgMethod read getMethod;
   end;

   TColorShift = packed record
   private
      FColorset: array[TColorSet] of single;
      FHue: shortint;

      function getColor(x: TColorSet): single;
      procedure setColor(x: TColorSet; value: single);
      function isClear: boolean;
   public
      property red: single index cs_red read getColor write setColor;
      property green: single index cs_green read getColor write setColor;
      property blue: single index cs_blue read getColor write setColor;
      property sat: single index cs_sat read getColor write setColor;
      property hue: shortint read FHue write FHue;
      property clear: boolean read isClear;
   end;

   TStreamEx = class helper for TStream
   public
      procedure writeString(data: string);
      function readString: string;
      procedure writeChar(data: ansiChar);
      function readChar: ansiChar;
      procedure writeBool(data: boolean);
      function readBool: boolean;
      procedure writeByte(data: byte);
      function readByte: byte;
      procedure writeWord(data: word);
      function readWord: word;
      procedure writeInt(data: integer);
      function readInt: integer;
      function eof: boolean;
   end;

   TRpgList<T> = class(TList<T>)
   private
      function getHigh: integer;
    function getLength: integer;
   public
      function Last: T;
      property High: integer read getHigh;
      property Length: integer read getLength;
   end;

   TRpgObjectList<T: class> = class(TObjectList<T>)
   private
      function getHigh: integer;
      function getLength: integer;
   public
      function Last: T;
      property High: integer read getHigh;
      property Length: integer read getLength;
   end;

   TRpgDataList<T: TRpgDatafile, constructor> = class(TRpgObjectList<TRpgDatafile>)
   public
      constructor Create;
      procedure upload(db: TDataSet);
   end;

   TRpgDecl = class(TEnumerable<TNameType>)
   private
      FName: string;
      FRetval: integer;
      FParams: TRpgList<TNameType>;
   protected
      function DoGetEnumerator: TEnumerator<TNameType>; override;
   public
      constructor Create(aName: string);
      destructor Destroy; override;

      function equals(other: TRpgDecl): boolean; reintroduce;

      property name: string read FName write FName;
      property retval: integer read FRetval write FRetval;
      property params: TRpgList<TNameType> read FParams write FParams;
   end;

   TDeclList = class(TRpgObjectList<TRpgDecl>)
   private
      function getLookup(value: string): TRpgDecl;
   public
      constructor Create;
      function IndexOf(const Value: string): Integer;
      procedure Sort;

      property decl[value: string]: TRpgDecl read getLookup;
   end;

   TFieldExt = class helper for TField
   private
      function getByteSet: TByteSet;
      procedure setByteSet(const Value: TByteSet);
      function getPSMethod: TMethod;
      procedure setPSMethod(const Value: TMethod);
   public
      property asSet: TByteSet read getByteSet write setByteSet;
      property asPSMethod: TMethod read getPSMethod write setPSMethod;
   end;

   TSafeDataset = class helper for TDataset
   public
      procedure postSafe;
   end;

   TRpgDictionary<TKey,TValue> = class(TObjectDictionary<TKey,TValue>)
   public
      destructor Destroy; override;
   end;

   ERpgLoadError = class(Exception);

   procedure lassert(cond: boolean);

implementation
uses
   Generics.Defaults, TypInfo,
   commons,
   uPSRuntime;

threadvar
   currentloader: TStream;

{ TColorShift }

function TColorShift.getColor(x: TColorSet): single;
begin
   result := FColorset[x];
end;

procedure TColorShift.setColor(x: TColorSet; value: single);
begin
   clamp(value, 0, 2);
   FColorset[x] := value;
end;

function TColorShift.isClear: boolean;
begin
   result := (FColorset[cs_red] = 0) and (FColorset[cs_green] = 0) and (FColorset[cs_blue] = 0)
             and (FColorset[cs_sat] = 0) and (FHue = 0);
end;

{ TRpgDatafile }

procedure TRpgDatafile.download(db: TDataset);
begin
   //this procedure intentionally left blank
end;

procedure TRpgDatafile.upload(db: TDataSet);
begin
   if self.datasetName <> '' then
      assert((db.Name = self.datasetName) or (self.datasetName[1] = '_'));
   db.Append;
   db.FieldByName('id').AsInteger := FId;
   db.FieldByName('name').AsString := FName;
   db.FieldByName('modified').AsBoolean := false;
end;

function TRpgDatafile.GetAllEvents: TStringList;
var
   list: PPropList;
   i, count: integer;
begin
   GetMem(list, sizeof(pointer) * 100); //If you've got more than 100 events on
                                        //one object, you're absolutely nuts :P
   try
      count := GetPropList(PTypeInfo(self.ClassInfo), [tkMethod], list, false);
      assert(count <= 100, 'Some insane coder created a class with more than 100 events!');
      result := TStringList.Create;
      for I := 0 to Count - 1 do
      begin
         result.AddObject(string(list[i].Name), GetMethodProp(self, string(list[i].name)).Code);
         assert(GetMethodProp(self, string(list[i].name)).Data = self);
      end;
   finally
      freeMem(list);
   end;
end;

function TRpgDatafile.getDatasetName: string;
begin
   result := '';
end;

class function TRpgDatafile.getSetLength(mySet: TByteSet): byte;
var
   setSaver: TSetSaver;
begin
   if mySet = [] then
      Exit(0);

   setSaver.aSet := mySet;
   result := 32;
   while (result > 0) and (setSaver.aArray[result] = 0) do
      dec(result);
end;

{$Q-}{$R-}
constructor TRpgDatafile.Load(savefile: TStream);
begin
   currentloader := savefile;
   FId := savefile.readWord;
   FName := savefile.readString;
end;

procedure TRpgDatafile.save(savefile: TStream);
begin
   savefile.WriteWord(FId);
   savefile.WriteString(FName);
end;
{$Q+}{$R+}

{ TStreamEx }

function TStreamEx.eof: boolean;
begin
   result := self.Position = self.Size;
end;

{$Q-}{$R-}
function TStreamEx.readBool: boolean;
begin
   self.ReadBuffer(result, 1);
end;

function TStreamEx.readByte: byte;
begin
   self.ReadBuffer(result, 1);
end;

function TStreamEx.readChar: ansiChar;
begin
   self.ReadBuffer(result, 1);
end;

function TStreamEx.readInt: integer;
begin
   self.readBuffer(result, 4);
end;

function TStreamEx.readString: string;
var
   len: integer;
begin
   try
      self.readBuffer(len, 4);
      if len > 0 then
      begin
         setLength(result, len);
         self.ReadBuffer(result[1], len * sizeof(char));
      end;
   except
      on EIntOverflow do
         result := '';
      else raise;
   end;
end;

function TStreamEx.readWord: word;
begin
   self.readBuffer(result, 2);
end;
{$Q+}{$R+}

procedure TStreamEx.writeBool(data: boolean);
begin
   self.WriteBuffer(data, 1);
end;

procedure TStreamEx.writeByte(data: byte);
begin
   self.WriteBuffer(data, 1);
end;

procedure TStreamEx.writeChar(data: ansiChar);
begin
   self.WriteBuffer(data, 1);
end;

procedure TStreamEx.writeInt(data: integer);
begin
   self.WriteBuffer(data, 4);
end;

procedure TStreamEx.writeString(data: string);
var
   len: cardinal;
begin
   len := length(data);
   self.WriteBuffer(len, 4);
   if len > 0 then
      self.WriteBuffer(data[1], len * sizeof(char));
end;

procedure TStreamEx.writeWord(data: word);
begin
   self.WriteBuffer(data, 2);
end;

{ Classless }

procedure lassert(cond: boolean);
begin
   if not cond then
      raise ERpgLoadError.Create('Savefile corrupt at address ' + intToStr(currentloader.Position));
end;

{ TScriptRecord }

constructor TScriptRecord.Create(name, designName: string; script: TRpgMethod);
begin
   FName := name;
   FDesignName := designName;
   FMethod := script;
end;

function TScriptRecord.getMethod: TRpgMethod;
begin
   result := FMethod;
end;

procedure TScriptRecord.setName(const Value: string);
begin
  FName := Value;
end;

procedure TScriptRecord.update(db: TDataSet);
begin
   db.FieldByName('name').AsString := name;
   db.FieldByName('designName').AsString := designName;
   db.FieldByName('address').AsInteger := integer(Self);
   db.FieldByName('baseMethod').asPSMethod := TMethod(FMethod);
end;

procedure TScriptRecord.upload(db: TDataSet);
begin
   db.Append;
   self.update(db);
end;

{ TRpgList<T> }

function TRpgList<T>.GetHigh: integer;
begin
   result := self.Count - 1;
end;

function TRpgList<T>.getLength: integer;
begin
   result := self.High + 1;
end;

function TRpgList<T>.Last: T;
begin
   result := Self[High];
end;

{ TDeclList }

function declComp(const Left, Right: TRpgDecl): Integer;
begin
   result := CompareText(left.name, right.name)
end;

constructor TDeclList.Create;
begin
   inherited Create(TComparer<TRpgDecl>.Construct(declComp));
   Self.OwnsObjects := true;
end;

function TDeclList.getLookup(value: string): TRpgDecl;
begin
   result := self[indexOf(value)];
end;

function TDeclList.IndexOf(const Value: string): Integer;
var
   dummy: TRpgDecl;
begin
   dummy := TRpgDecl.Create(value);
   try
      if not self.BinarySearch(dummy, result) then
         result := -1;
   finally
      dummy.Free;
   end;
end;

procedure TDeclList.Sort;
var
   i: integer;
begin
   inherited Sort;
   i := 1;
   while i < self.Count do
   begin
      if self[i].name = self[i - 1].name then
         self.Delete(i)
      else inc(i);
   end;
end;

{ TRpgDecl }

constructor TRpgDecl.Create(aName: string);
begin
   FName := aName;
   FParams := TRpgList<TNameType>.Create;
end;

destructor TRpgDecl.Destroy;
begin
   FParams.Free;
   inherited Destroy;
end;

function TRpgDecl.DoGetEnumerator: TEnumerator<TNameType>;
begin
   result := FParams.GetEnumerator;
end;

function TRpgDecl.equals(other: TRpgDecl): boolean;
var
   i: integer;
begin
   result := (FParams.Count = other.params.Count) and (Self.retval = other.retval);
   if result then
      for i := 0 to FParams.high do
         result := result and (FParams[i].typeVar = other.params[i].typeVar) and (FParams[i].flags = other.params[i].flags)
end;

{ TRpgObjectList<T> }

function TRpgObjectList<T>.getHigh: integer;
begin
   result := Count - 1;
end;

function TRpgObjectList<T>.getLength: integer;
begin
   result := self.High + 1;
end;

function TRpgObjectList<T>.Last: T;
begin
   result := Self[High];
end;

{ TFieldExt }

function TFieldExt.getByteSet: TByteSet;
begin
   assert(self.DataSize = SizeOf(Result));
   assert(self.GetData(@Result));
end;

procedure TFieldExt.setByteSet(const Value: TByteSet);
begin
   assert(self.DataSize = SizeOf(Value));
   self.SetData(@value);
end;

function TFieldExt.getPSMethod: TMethod;
begin
   result.Code := @uPSRuntime.MyAllMethodsHandler;
   result.Data := pointer(Self.AsInteger);
end;

procedure TFieldExt.setPSMethod(const Value: TMethod);
begin
   assert(value.Code = @uPSRuntime.MyAllMethodsHandler);
   self.asInteger := integer(Value.Data);
end;

{ TSafeDataset }

procedure TSafeDataset.postSafe;
begin
   if self.state in dsEditModes then
      self.Post;
end;

{ TRpgDataList<T> }

constructor TRpgDataList<T>.Create;
begin
   inherited Create(true);
   self.Add(T.Create);
end;

procedure TRpgDataList<T>.upload(db: TDataSet);
var
   iterator: T;
begin
   for iterator in self do
   begin
      if iterator.id = 0 then
         Continue
      else iterator.upload(db);
   end;
   if self.Count > 1 then
      db.postSafe;
end;

{ TRpgDictionary<TKey, TValue> }

destructor TRpgDictionary<TKey, TValue>.Destroy;
begin
   self.Keys.Free;
   self.Values.Free;
   inherited Destroy;
end;

end.
