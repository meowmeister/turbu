unit turbu_serialization;

interface
uses
   TypInfo, RTTI, DB, Generics.Collections,
   RttiHelper;

type
   TInstancePair = TPair<pointer, PTypeInfo>;
   TInstanceStack = class(TStack<TInstancePair>);
   TConstructorDict = TDictionary<TRttiInstanceType, TRttiMethod>;
   TDBSerializerRule = class;
   TRuleDict = TObjectDictionary<TClass, TDBSerializerRule>;

   TSerializer = class abstract(TObject)
   private
      class constructor Create;
      function GetCurrentInstance: pointer;
      function GetTypeInfo: PTypeInfo;
      class destructor Destroy;
   protected

      class var
         FContext: TRttiContext;
         FConstructors: TConstructorDict;
   protected
      FRelationDepth: byte;
      FInstances: TInstanceStack;
      FCurrentField: TRttiField;
      property CurrentInstance: pointer read GetCurrentInstance;
      property CurrentTypeInfo: PTypeInfo read GetTypeInfo;
      function GetConstructor(val: TRttiInstanceType): TRttiMethod;
   public
      constructor Create;
      destructor Destroy; override;
   end;

   TDatasetStack = TStack<TDataset>;
   TKeyFieldStack = TStack<TArray<string>>;

   TDatasetSerializer = class(TSerializer)
   private type
      TReferenceResult = (rrAssigned, rrMustAssign);
   private
      function isKeyField(const fieldname: string): boolean;
   private
      FUploadPrefix: string;
      FRelationDepth: integer;
      FKeyFields: TKeyFieldStack;
      FRules: TRuleDict;

      function handleReferenceManagement(db: TDataset; var value: TValue; const fieldname: string): TReferenceResult;

      function downloadArray(db: TDataset; const value: TValue; zeroOrder: boolean; const fieldname: string): TValue;
      function downloadEnumerable(db: TDataset; const value: TValue;
                                  enumType: TRttiType; const fieldName: string): TValue;
      function downloadRecordRelation(db: TDataset; const fieldname: string): TValue;
      function downloadRecord(db: TDataset; const fieldname: string): TValue;
      function downloadPointer(db: TDataset; var value: TValue; const fieldname: string): TValue;
      function downloadClass(db: TDataset; var value: TValue; const fieldname: string): TValue;
      function downloadValue(db: TDataset; var value: TValue; fieldname: string): TValue;

      procedure uploadArray(db: TDataset; const value: TValue; zeroOrder: boolean; const fieldname: string);
      procedure uploadEnumerable(db: TDataset; enum: TRttiEnumerator; const fieldName: string);
      procedure uploadRecordRelation(db: TDataset; const value: TValue; const fieldname: string);
      procedure uploadRecord(db: TDataset; const value: TValue; const fieldname: string);
      procedure uploadPointer(db: TDataset; const value: TValue; const fieldname: string);
      procedure uploadClass(db: TDataset; const value: TValue; const fieldname: string);
      procedure uploadValue(db: TDataset; const value: TValue; fieldname: string);
    function downloadString(db: TDataset; const fieldname: string): TValue;
   public
      class function getFieldName(field: TRttiField): string; static;
      class function getField(db: TDataset; field: TRttiField): TField; static;

      constructor Create;
      destructor Destroy; override;
      procedure upload(value: TObject; db: TDataset);
      procedure download(value: TObject; db: TDataset);
   end;

   TDBUploadAttribute = class(TCustomAttribute)
   protected
      procedure upload(db: TDataset; field: TRttiField; instance: TObject); virtual; abstract;
      procedure download(db: TDataset; field: TRttiField; instance: TObject); virtual; abstract;
   end;

   NoUploadAttribute = class(TDBUploadAttribute)
   protected
      procedure upload(db: TDataset; field: TRttiField; instance: TObject); override;
      procedure download(db: TDataset; field: TRttiField; instance: TObject); override;
   end;

   TIsUploadableAttribute = class abstract(TCustomAttribute)
   protected
      function IsUploadable(instance: TValue): boolean; virtual; abstract;
   end;

   TDBRelationKeyAttribute = class abstract(TCustomAttribute)
   protected
      procedure SetRelationKey(instance: TValue; DB: TDataSet); virtual; abstract;
      procedure SetRelationFilter(instance: TValue; DB: TDataSet); virtual; abstract;
      function GetRelationFilterFieldNames: TArray<string>; virtual; abstract;
   end;

   TDBIsNullAttribute = class abstract(TCustomAttribute)
   protected
      function IsFieldNull(DB: TDataset; const fieldname: string): boolean; virtual; abstract;
   end;

   TReferenceManagerAttribute = class abstract(TCustomAttribute)
   protected
      function CreateNew(field: TRttiField): TValue; virtual; abstract;
   end;

   TEnumerableManagerAttribute = class abstract(TCustomAttribute)
   protected
      procedure Clear(const instance: TValue); virtual; abstract;
      procedure Add(const instance, value: TValue); virtual; abstract;
      function CreateNew(itemType: TRttiType): TValue; virtual;
   end;

   TDBSerializationOperation = class
   private
      FField: TRttiField;
   public
      constructor Create(field: TRttiField);
      procedure Upload(Value: TObject; db: TDataset); virtual; abstract;
      procedure Download(Value: TObject; db: TDataset); virtual; abstract;
   end;

   TDBAttributeOperation = class(TDBSerializationOperation)
   private
      FAttribute: TDBUploadAttribute;
   public
      constructor Create(field: TRttiField; attr: TDBUploadAttribute);
      procedure Upload(Value: TObject; db: TDataset); override;
      procedure Download(Value: TObject; db: TDataset); override;
   end;

   TDBFieldOperation = class(TDBSerializationOperation)
   private
      FName: string;
      FSerializer: TDatasetSerializer;
   public
      constructor Create(field: TRttiField; const name: string; ser: TDatasetSerializer);
   end;

   TDBCompositeFieldOperation = class(TDBFieldOperation)
   public
      procedure Upload(Value: TObject; db: TDataset); override;
      procedure Download(Value: TObject; db: TDataset); override;
   end;

   TDBSimpleFieldOperation = class(TDBFieldOperation)
   private
      FDBField: TField;
      FDataset: TDataset;
      procedure SetDataset(ds: TDataset);
      procedure CheckDataset(ds: TDataset);
      function GetString: TValue;
      procedure SetString(const value: TValue);
   public
      procedure Upload(Value: TObject; db: TDataset); override;
      procedure Download(Value: TObject; db: TDataset); override;
   end;

   TDBSerializerRule = class
   private
      FSteps: TArray<TDBSerializationOperation>;
      function GetFields(baseType: TRttiType): TArray<TRttiField>;
   public
      constructor Create(cls: TClass; ser: TDatasetSerializer);
      destructor Destroy; override;
      procedure Upload(Value: TObject; db: TDataset);
      procedure Download(Value: TObject; db: TDataset);
   end;

implementation
uses
   SysUtils, Generics.Defaults;

{$OPTIMIZATION ON}

{ TSerializer }

class constructor TSerializer.Create;
begin
   FContext := TRttiContext.Create;
   FConstructors := TConstructorDict.Create;
end;

class destructor TSerializer.Destroy;
begin
   FConstructors.Free;
   FContext.Free;
end;

constructor TSerializer.Create;
begin
   FInstances := TInstanceStack.Create;
end;

destructor TSerializer.Destroy;
begin
   FInstances.Free;
   inherited Destroy;
end;

function TSerializer.GetConstructor(val: TRttiInstanceType): TRttiMethod;
var
   method: TRttiMethod;
begin
   if not FConstructors.TryGetValue(val, result) then
   begin
      for method in val.GetMethods('Create') do
      begin
         if (method.IsConstructor) and (length(method.GetParameters) = 0) then
         begin
            FConstructors.Add(val, method);
            exit(method);
         end;
      end;
      raise EInsufficientRTTI.CreateFmt('No simple constructor available for class %s ',
                                        [val.MetaclassType.ClassName]);
   end;
end;

function TSerializer.GetCurrentInstance: pointer;
begin
   result := FInstances.Peek.Key;
end;

function TSerializer.GetTypeInfo: PTypeInfo;
begin
   result := FInstances.Peek.Value;
end;

{ TDatasetSerializer }

const
  FIELDNAME_STR = '%s_%s';

constructor TDatasetSerializer.Create;
begin
   inherited Create;
   FKeyFields := TKeyFieldStack.Create;
   FRules := TRuleDict.Create([doOwnsValues]);
end;

destructor TDatasetSerializer.Destroy;
begin
   FRules.Free;
   FKeyFields.Free;
   inherited;
end;

class function TDatasetSerializer.getField(db: TDataset;
  field: TRttiField): TField;
begin
   result := db.FieldByName(getFieldName(field));
end;

class function TDatasetSerializer.getFieldName(field: TRttiField): string;
begin
   result := copy(field.Name, 2, maxint);
end;

function TDatasetSerializer.handleReferenceManagement(db: TDataset; var value: TValue; const fieldname: string): TReferenceResult;
var
   manager: TReferenceManagerAttribute;
   isNullAtt:TDBIsNullAttribute;
   isNull: boolean;
begin
   manager := TReferenceManagerAttribute(FCurrentField.GetAttribute(TReferenceManagerAttribute));
   isNullAtt := TDBIsNullAttribute(FCurrentField.GetAttribute(TDBIsNullAttribute));
   if assigned(isNullAtt) then
      isNull := isNullAtt.IsFieldNull(db, fieldname)
   else isNull := {db.FieldByName(fieldname).IsNull;} false;

   result := rrAssigned;
   if not isNull then
   begin
      if value.IsEmpty then
      begin
         if assigned(manager) then
            value := manager.CreateNew(FCurrentField)
         else result := rrMustAssign;
      end
   end;
end;

function TDatasetSerializer.downloadArray(db: TDataset; const value: TValue; zeroOrder: boolean; const fieldname: string): TValue;
var
   i, index: integer;
   subvalue: TValue;
   subfieldName: string;
begin
   inc(self.FRelationDepth);
   try
      result := value;

      for i := 0 to value.GetArrayLength - 1 do
      begin
         subvalue := value.GetArrayElement(i);
         if zeroOrder then
            index := i
         else
            index := i + 1;

         if subvalue.Kind = tkRecord then
            subfieldName := fieldname
         else
            subfieldName := format('%s_%d', [fieldname, index]);
          result.SetArrayElement(i, downloadValue(db, subvalue, subfieldName));
      end;
   finally
      dec(self.FRelationDepth);
   end;
end;

{$WARN USE_BEFORE_DEF OFF}
function TDatasetSerializer.downloadEnumerable(db: TDataset; const value: TValue;
                                               enumType: TRttiType; const fieldName: string): TValue;
var
   enumerableManager: TEnumerableManagerAttribute;
   iType: TRttiInstanceType;

   function downloadNonObjectEnum: TValue;
   begin
      result := downloadValue(db, result, fieldname);
   end;

   function downloadObjectEnum: TValue;
   begin
      if assigned(enumerableManager) then
         result := enumerableManager.CreateNew(enumType)
      else result := GetConstructor(iType).Invoke(iType.MetaclassType, []);
      download(result.AsObject, db);
   end;

var
   relationKey: TDBRelationKeyAttribute;
   clearMethod, addMethod: TRttiMethod;
   parentType: TRttiType;
   keyFields: TArray<string>;
   newVal: TValue;
begin
   parentType := FContext.GetType(GetTypeInfo);
   relationKey := TDBRelationKeyAttribute(parentType.GetAttribute(TDBRelationKeyAttribute));
   enumerableManager := TEnumerableManagerAttribute(FCurrentField.GetAttribute(TEnumerableManagerAttribute));
   if enumType is TRttiInstanceType then
      iType := TRttiInstanceType(enumType)
   else iType := nil;

   db := db.Owner.FindComponent(format('%s_%s', [db.Name, fieldname])) as TDataset;
   if not assigned(db) then
      asm int 3 end;
   if assigned(relationKey) then
   begin
      relationKey.SetRelationFilter(TObject(CurrentInstance), db);
      keyFields := relationKey.GetRelationFilterFieldNames;
   end;
   if assigned(enumerableManager) then
      enumerableManager.Clear(value)
   else begin
      clearMethod := FCurrentField.FieldType.GetMethod('Clear');
      addMethod := FCurrentField.FieldType.GetMethod('Add');
      if not ((assigned(clearMethod)) and (length(clearMethod.GetParameters) = 0) and
              (assigned(addMethod)) and (length(addMethod.GetParameters) = 1)) then
         raise EInsufficientRtti.CreateFmt('Unable to find Add and Clear methods for enumerable %s', [FCurrentField.Name]);
      clearMethod.Invoke(value, []);
   end;
   FKeyFields.Push(keyFields);
   try
      db.First;
      while not db.Eof do
      begin
         if assigned(iType) then
            newval := downloadObjectEnum
         else newval := downloadNonObjectEnum;

         if assigned(enumerableManager) then
            enumerableManager.Add(value, newval)
         else addMethod.Invoke(value, [newval]);
         db.Next;
      end;
   finally
      FKeyFields.Pop;
   end;
   result := value;
end;
{$WARN USE_BEFORE_DEF ON}

function TDatasetSerializer.downloadClass(db: TDataset; var value: TValue; const fieldname: string): TValue;
var
   val: TRttiInstanceType;
   prefix: string;
   enum: TRttiEnumerator;
   rmResult: TReferenceResult;
begin
   assert(value.Kind = tkClass);
   val := FCurrentField.FieldType as TRttiInstanceType;

   rmResult := handleReferenceManagement(db, value, fieldname);
   if rmResult = rrMustAssign then
      value := GetConstructor(val).Invoke(val.MetaclassType, []);

   enum := rttiHelper.GetRttiEnumerator(val, value);

   if assigned(enum) then
   begin
      result := downloadEnumerable(db, value, enum.EnumType, fieldname);
      enum.Free;
   end
   else begin
      prefix := FUploadPrefix;
      try
         if FUploadPrefix = '' then
            FUploadPrefix := fieldname
         else FUploadPrefix := format('%s_%s', [FUploadPrefix, fieldname]);
         self.download(value.AsObject, db);
         result := value;
      finally
         FUploadPrefix := prefix;
      end;
   end
end;

function TDatasetSerializer.downloadPointer(db: TDataset; var value: TValue; const fieldname: string): TValue;
var
   ptr: pointer;
   newval: TValue;
   rmResult: TReferenceResult;
begin
   assert(value.Kind = tkPointer);
   ptr := PPointer(FCurrentField.GetValue(CurrentInstance).GetReferenceToRawData)^;

   rmResult := handleReferenceManagement(db, value, fieldname);
   if rmResult = rrMustAssign then
      GetMem(ptr, (FCurrentField.FieldType as TRttiPointerType).ReferredType.TypeSize);

    TValue.MakeWithoutCopy(ptr, value.TypeInfo, newval);
    result := self.downloadValue(db, newval, fieldname);
end;

function TDatasetSerializer.downloadRecord(db: TDataset; const fieldname: string): TValue;
var
   val: TRttiRecordType;
   recField: TRttiField;
   value: TValue;
   offsetRef: pointer;
begin
   val := FCurrentField.FieldType as TRttiRecordType;
   result := FCurrentField.GetValue(currentInstance);
   offsetRef := result.GetReferenceToRawData;
   for recField in val.GetFields do
   begin
      value := recField.GetValue(offsetRef);
      recField.SetValue(offsetRef,
                        downloadValue(db, value,
                                      format(FIELDNAME_STR, [fieldname, recField.Name])));
   end;
end;

function TDatasetSerializer.downloadRecordRelation(db: TDataset; const fieldname: string): TValue;
var
   val: TRttiRecordType;
   recField: TRttiField;
   datatype: TRttiType;
   relationKey: TDBRelationKeyAttribute;
   value: TValue;
   offsetRef: pointer;
begin
   val := FCurrentField.FieldType as TRttiRecordType;

   db := db.Owner.FindComponent(format('%s_%s', [db.Name, fieldname])) as TDataset;
   if not assigned(db) then
      asm int 3 end;
   dataType := FContext.GetType(CurrentTypeInfo);
   relationKey := TDBRelationKeyAttribute(dataType.GetAttribute(TDBRelationKeyAttribute));
   if assigned(relationKey) then
     relationKey.SetRelationFilter(TObject(CurrentInstance), db);
   result := FCurrentField.GetValue(currentInstance);
   offsetRef := result.GetReferenceToRawData;
   for recField in val.GetFields do
   begin
      value := recField.GetValue(offsetRef);
      recField.SetValue(offsetRef,
                        downloadValue(db, value,
                                      format(FIELDNAME_STR, [fieldname, recField.Name])));
   end;
end;

function TDatasetSerializer.downloadString(db: TDataset; const fieldname: string): TValue;
begin
   result := db.FieldByName(fieldname).AsString;
end;

function TDatasetSerializer.downloadValue(db: TDataset; var value: TValue; fieldname: string): TValue;
begin
   if FUploadPrefix <> '' then
      fieldname := format('%s_%s', [FUploadPrefix, fieldname]);
   case value.Kind of
      tkArray: result := downloadArray(db, value, false, fieldname);
      tkDynArray: result := downloadArray(db, value, true, fieldname);
      tkRecord:
         if FRelationDepth > 0 then
            result := downloadRecordRelation(db, fieldName)
         else result := downloadRecord(db, fieldName);
      tkPointer: result := downloadPointer(db, value, fieldname);
      tkClass: result := downloadClass(db, value, fieldName);
      tkEnumeration:
      begin
         if value.TypeInfo = TypeInfo(Boolean) then
            result := db.FieldByName(fieldname).AsBoolean
         else result := TValue.FromOrdinal(value.TypeInfo, db.FieldByName(fieldname).AsInteger);
      end;
      tkInteger: result := db.FieldByName(fieldname).asInteger;
      tkInt64: result := db.FieldByName(fieldname).AsLargeInt;
      tkUString: result := downloadString(db, fieldname);
      else assert(false);
   end;
end;

function TDatasetSerializer.isKeyField(const fieldname: string): boolean;
var
   keyField: string;
begin
   result := false;
   for keyField in FKeyFields.Peek do
      if keyField = fieldname then
         Exit(true);
end;

procedure TDatasetSerializer.download(value: TObject; db: TDataset);
var
   rule: TDBSerializerRule;
begin
   FInstances.Push(TInstancePair.Create(value, value.ClassInfo));
   try
      if not FRules.TryGetValue(value.ClassType, rule) then
      begin
         rule := TDBSerializerRule.Create(value.ClassType, self);
         FRules.Add(value.ClassType, rule);
      end;
      rule.Download(value, db);
   finally
      FInstances.Pop;
   end;
end;

procedure TDatasetSerializer.upload(value: TObject; db: TDataset);
var
   rule: TDBSerializerRule;
begin
   FInstances.Push(TInstancePair.Create(value, value.ClassInfo));
   try
      if FUploadPrefix = '' then
         db.Append;

      if not FRules.TryGetValue(value.ClassType, rule) then
      begin
         rule := TDBSerializerRule.Create(value.ClassType, self);
         FRules.Add(value.ClassType, rule);
      end;
      rule.Upload(value, db);
   finally
      FInstances.Pop;
   end;
end;

procedure TDatasetSerializer.uploadValue(db: TDataset; const value: TValue;
  fieldname: string);

  procedure uploadVariant;
  begin
      db.FieldByName(fieldname).Value := value.AsVariant
  end;

  procedure uploadString;
  begin
      db.FieldByName(fieldname).AsString := value.AsString
  end;

begin
   if FUploadPrefix <> '' then
      fieldname := format('%s_%s', [FUploadPrefix, fieldname]);
   case value.Kind of
      tkArray: uploadArray(db, value, false, fieldname);
      tkDynArray: uploadArray(db, value, true, fieldname);
      tkRecord:
         if FRelationDepth > 0 then
            uploadRecordRelation(db, value, fieldName)
         else uploadRecord(db, value, fieldName);
      tkPointer: uploadPointer(db, value, fieldname);
      tkClass: uploadClass(db, value, fieldName);
      tkEnumeration: db.FieldByName(fieldname).Value := value.AsOrdinal;
      tkMethod, tkUnknown: ;
      tkInteger: db.FieldByName(fieldname).asInteger := value.AsInteger;
      tkInt64: db.FieldByName(fieldname).AsLargeInt := value.AsInt64;
      tkUString: uploadString;
      else uploadVariant;
   end;
end;

procedure TDatasetSerializer.uploadArray(db: TDataset; const value: TValue;
  zeroOrder: boolean; const fieldname: string);
var
   i, index: integer;
   subvalue: TValue;
   subfieldName: string;
begin
   assert(value.IsArray);
   inc(self.FRelationDepth);
   try
      for i := 0 to value.GetArrayLength - 1 do
      begin
         subvalue := value.GetArrayElement(i);
         if zeroOrder then
            index := i
         else
            index := i + 1;

         if subvalue.Kind = tkRecord then
            subfieldName := fieldname
         else
            subfieldName := format('%s_%d', [fieldname, index]);
         uploadValue(db, subvalue, subfieldName);
      end;
   finally
      dec(self.FRelationDepth);
   end;
end;

procedure TDatasetSerializer.uploadEnumerable(db: TDataset; enum: TRttiEnumerator;
   const fieldname: string);
var
   isUploadable: TIsUploadableAttribute;
   relationKey: TDBRelationKeyAttribute;

   procedure uploadNonObjectEnum;
   var
      enumerator: TValue;
   begin
      for enumerator in enum do
      begin
         if assigned(isUploadable) and (not isUploadable.IsUploadable(enumerator)) then
            Continue;

         db.Append;
         uploadValue(db, enumerator, '');
         if assigned(relationKey) then
           relationKey.SetRelationKey(TObject(CurrentInstance), db);
      end;
   end;

   procedure uploadObjectEnum;
   var
      enumerator: TValue;
   begin
      for enumerator in enum do
      begin
         if assigned(isUploadable) and (not isUploadable.IsUploadable(enumerator)) then
            Continue;

         upload(enumerator.AsObject, db);
         if assigned(relationKey) then
           relationKey.SetRelationKey(TObject(CurrentInstance), db);
      end;
   end;

var
   parentType: TRttiType;
   datafileType: TRttiType;
   newdb: TDataset;
begin
   datafileType := enum.EnumType;
   isUploadable := TIsUploadableAttribute(datafileType.GetAttribute(TIsUploadableAttribute));
   parentType := FContext.GetType(GetTypeInfo);
   relationKey := TDBRelationKeyAttribute(parentType.GetAttribute(TDBRelationKeyAttribute));

   newdb := db.Owner.FindComponent(format('%s_%s', [db.Name, fieldname])) as TDataset;
   if not assigned(newdb) then
      asm int 3 end
   else db := newdb;

   if datafileType.TypeKind = tkClass then
      uploadObjectEnum
   else uploadNonObjectEnum;

   if db.Active and (db.state in [dsEdit, dsInsert]) then
      db.post;
end;

procedure TDatasetSerializer.uploadPointer(db: TDataset; const value: TValue; const fieldname: string);
var
   ptr: pointer;
   newval: TValue;
begin
   assert(value.Kind = tkPointer);
   ptr := PPointer(value.GetReferenceToRawData)^;
   if ptr <> nil then
   begin
      TValue.MakeWithoutCopy(ptr, value.TypeInfo, newval);
      self.uploadValue(db, newval, fieldname);
   end;
end;

procedure TDatasetSerializer.uploadClass(db: TDataset; const value: TValue;
  const fieldname: string);
var
   val: TRttiInstanceType;
   prefix: string;
   enum: TRttiEnumerator;
begin
   assert(value.Kind = tkClass);
   val := FContext.GetType(value.TypeInfo) as TRttiInstanceType;
   if assigned(val.GetMethod('GetEnumerator')) then
      enum := rttiHelper.GetRttiEnumerator(val, value)
   else enum := nil;
   if assigned(enum) then
      uploadEnumerable(db, enum, fieldname)
   else begin
      prefix := FUploadPrefix;
      try
         if FUploadPrefix = '' then
            FUploadPrefix := fieldname
         else FUploadPrefix := format('%s_%s', [FUploadPrefix, fieldname]);
         self.upload(value.AsObject, db);
      finally
         FUploadPrefix := prefix;
      end;
   end
end;

procedure TDatasetSerializer.uploadRecord(db: TDataset; const value: TValue;
  const fieldname: string);
var
   val: TRttiRecordType;
   field: TRttiField;
   newFieldname: string;
begin
   assert(value.Kind = tkRecord);
   val := FContext.GetType(value.TypeInfo) as TRttiRecordType;
   for field in val.GetFields do
   begin
      if fieldname = '' then
         newFieldname := field.name
      else newFieldname := format(FIELDNAME_STR, [fieldname, field.Name]);
      uploadValue(db, field.GetValue(value.GetReferenceToRawData), newFieldname);
   end;
end;

procedure TDatasetSerializer.uploadRecordRelation(db: TDataset; const value: TValue; const fieldname: string);
var
   val: TRttiRecordType;
   field: TRttiField;
   datatype: TRttiType;
   relationKey: TDBRelationKeyAttribute;
begin
   assert(value.Kind = tkRecord);
   val := FContext.GetType(value.TypeInfo) as TRttiRecordType;
   db := db.Owner.FindComponent(format('%s_%s', [db.Name, fieldname])) as TDataset;
   if not assigned(db) then
      asm int 3 end;
   db.Append;
   dataType := FContext.GetType(CurrentTypeInfo);
   relationKey := TDBRelationKeyAttribute(dataType.GetAttribute(TDBRelationKeyAttribute));
   if assigned(relationKey) then
     relationKey.SetRelationKey(TObject(CurrentInstance), db);
   for field in val.GetFields do
      uploadValue(db, field.GetValue(value.GetReferenceToRawData), field.Name);
   db.post;
end;

{ TNoUploadAttribute }

procedure NoUploadAttribute.download(db: TDataset; field: TRttiField;
  instance: TObject);
begin
   // do nothing
end;

procedure NoUploadAttribute.upload(db: TDataset; field: TRttiField;
  instance: TObject);
begin
   // do nothing
end;

{ TEnumerableManagerAttribute }

function TEnumerableManagerAttribute.CreateNew(itemType: TRttiType): TValue;
begin
   // do nothing
end;

{ TSerializationOperation }

constructor TDBSerializationOperation.Create(field: TRttiField);
begin
   FField := field;
end;

{ TDBAttributeOperation }

constructor TDBAttributeOperation.Create(field: TRttiField; attr: TDBUploadAttribute);
begin
   inherited Create(field);
   FAttribute := attr;
end;

procedure TDBAttributeOperation.Download(Value: TObject; db: TDataset);
begin
   FAttribute.download(db, FField, value);
end;

procedure TDBAttributeOperation.Upload(Value: TObject; db: TDataset);
begin
   FAttribute.upload(db, FField, value)
end;

{ TDBFieldOperation }

constructor TDBFieldOperation.Create(field: TRttiField; const name: string; ser: TDatasetSerializer);
begin
   inherited Create(field);
   FName := name;
   FSerializer := ser;
end;

{ TDBCompositeFieldOperation }

procedure TDBCompositeFieldOperation.Download(Value: TObject; db: TDataset);
var
   lValue: TValue;
begin
   if (FSerializer.FKeyFields.Count > 0) and (FSerializer.isKeyField(FName)) then
      Exit;
   FSerializer.FCurrentField := FField;
   lValue := FField.GetValue(value);
   FField.SetValue(value, FSerializer.downloadValue(db, lValue, FName));
end;

procedure TDBCompositeFieldOperation.Upload(Value: TObject; db: TDataset);
begin
   FSerializer.FCurrentField := FField;
   FSerializer.uploadValue(db, FField.GetValue(value), FName)
end;

{ TDBSimpleFieldOperation }

procedure TDBSimpleFieldOperation.SetDataset(ds: TDataset);
var
   fieldname: string;
begin
   FDataset := ds;
   if FSerializer.FUploadPrefix <> '' then
      fieldname := format('%s_%s', [FSerializer.FUploadPrefix, FName])
   else fieldname := FName;
   FDBField := ds.FieldByName(fieldname);
end;

procedure TDBSimpleFieldOperation.CheckDataset(ds: TDataset);
begin
   if (FDataset <> ds) then
      SetDataset(ds);
end;

procedure TDBSimpleFieldOperation.Download(Value: TObject; db: TDataset);
var
   rValue: TValue;
begin
   CheckDataset(db);
   case FField.FieldType.TypeKind of
      tkEnumeration:
      begin
         if FField.FieldType.Handle = TypeInfo(Boolean) then
            rValue := FDbField.AsBoolean
         else rValue := TValue.FromOrdinal(FField.FieldType.Handle, FDbField.AsInteger);
      end;
      tkInteger: rValue := FDbField.asInteger;
      tkInt64: rValue := FDbField.AsLargeInt;
      tkUString: rValue := GetString;
      tkFloat: rValue := FDBField.AsExtended;
      else assert(false);
   end;
   FField.SetValue(value, rValue);
end;

function TDBSimpleFieldOperation.GetString: TValue;
begin
   result := FDbField.AsString;
end;

procedure TDBSimpleFieldOperation.SetString(const value: TValue);
begin
   FDbField.AsString := value.AsString;
end;

procedure TDBSimpleFieldOperation.Upload(Value: TObject; db: TDataset);
var
   lValue: TValue;
begin
   CheckDataset(db);
   lValue := FField.GetValue(value);
   case FField.FieldType.TypeKind of
      tkEnumeration:
      begin
         if FField.FieldType.Handle = TypeInfo(Boolean) then
            FDbField.AsBoolean := lValue.AsBoolean
         else FDbField.AsInteger := lValue.AsOrdinal;
      end;
      tkInteger: FDbField.asInteger := lValue.AsInteger;
      tkInt64: FDbField.AsLargeInt := lValue.AsInt64;
      tkUString: SetString(lValue);
      tkFloat: FDBField.AsExtended := lValue.AsExtended;
      else assert(false);
   end;
end;

{ TDBSerializerRule }

constructor TDBSerializerRule.Create(cls: TClass; ser: TDatasetSerializer);
var
   fields: TArray<TRttiField>;
   field: TRttiField;
   attr: TCustomAttribute;
   counter: integer;
   op: TDBSerializationOperation;
begin
   fields :=  GetFields(TSerializer.FContext.GetType(cls));
   SetLength(FSteps, length(fields));
   counter := 0;
   for field in fields do
   begin
      if not ((field.name <> '') and (field.Name[1] = 'F')) then
         Continue;
      if assigned(field.FieldType) and (field.FieldType.TypeKind in [tkMethod, tkProcedure]) then
         Continue;

      attr := field.GetAttribute(TDBUploadAttribute);
      if assigned(attr) then
         op := TDBAttributeOperation.Create(field, TDBUploadAttribute(attr))
      else if field.FieldType.TypeKind in [tkEnumeration, tkInteger, tkInt64, tkUString, tkFloat] then
         op := TDBSimpleFieldOperation.Create(field, ser.getFieldName(field), ser)
      else op := TDBCompositeFieldOperation.Create(field, ser.getFieldName(field), ser);
      FSteps[counter] := op;
      inc(counter);
   end;
   SetLength(FSteps, counter);
end;

destructor TDBSerializerRule.Destroy;
var
   i: integer;
begin
   for i := 0 to High(FSteps) do
      FSteps[i].Free;
   inherited;
end;

function TDBSerializerRule.GetFields(baseType: TRttiType): TArray<TRttiField>;
begin
   result := baseType.GetFields;
   TArray.Sort<TRttiField>(result, TComparer<TRttiField>.Construct(
      function (const Left, Right: TRttiField): Integer
      begin
         result := left.Offset - right.Offset;
      end));
end;

procedure TDBSerializerRule.Download(Value: TObject; db: TDataset);
var
   i: integer;
begin
   for i := 0 to High(FSteps) do
      FSteps[i].Download(value, db);
end;

procedure TDBSerializerRule.Upload(Value: TObject; db: TDataset);
var
   i: integer;
begin
   for i := 0 to High(FSteps) do
      FSteps[i].Upload(value, db);
end;

end.
