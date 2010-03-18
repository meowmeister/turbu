unit rttiHelper;

interface
uses
   RTTI;

type
   TAttributeClass = class of TCustomAttribute;

   TRttiObjectHelper = class helper for TRttiObject
   protected
      function GetContext: TRttiContext;
   public
      function GetAttribute(const classType: TAttributeClass): TCustomAttribute;
      property Context: TRttiContext read GetContext;
   end;

   TRttiEnumerator = class
   private
      FBase: TValue;
      FCurrent: TRttiProperty;
      FMoveNext: TRttiMethod;
      function GetCurrent: TValue;
      function GetEnumType: TRttiType;
   public
      constructor Create(base: TRttiStructuredType; instance: TValue);
      destructor Destroy; override;
      function GetEnumerator: TRttiEnumerator;

      property Current: TValue read GetCurrent;
      function MoveNext: Boolean;
      property EnumType: TRttiType read GetEnumType;
   end;

   function GetRttiEnumerator(base: TRttiStructuredType; instance: TValue): TRttiEnumerator;

implementation
uses
   SysUtils, TypInfo;

{ TRttiObjectHelper }

function TRttiObjectHelper.GetAttribute(
  const classType: TAttributeClass): TCustomAttribute;
var
   enumerator: TCustomAttribute;
begin
   for enumerator in self.GetAttributes do
      if enumerator is classType then
         Exit(enumerator);
   result := nil;
end;

function TRttiObjectHelper.GetContext: TRttiContext;
begin
   result := TRttiContext.Create;
end;

{ TRttiEnumerator }

function GetRttiEnumerator(base: TRttiStructuredType; instance: TValue): TRttiEnumerator;
begin
   try
      result := TRttiEnumerator.Create(base, instance);
   except
      on EAbort do
         result := nil
   end;
end;

constructor TRttiEnumerator.Create(base: TRttiStructuredType; instance: TValue);

   function IsSimpleFunction(method: TRttiMethod): boolean;
   begin
      result := assigned(method) and (length(method.GetParameters) = 0) and
                assigned(method.ReturnType);
   end;

var
   enumMethod: TRttiMethod;
   enumType: TRttiType;
begin
   enumMethod := base.GetMethod('GetEnumerator');
   if not IsSimpleFunction(enumMethod) then
      Abort;

   FBase := enumMethod.Invoke(instance, []);
   enumType := enumMethod.Context.GetType(FBase.TypeInfo);
   FCurrent := enumType.GetProperty('Current');
   FMoveNext := enumType.GetMethod('MoveNext');
   if not ((assigned(FCurrent)) and (FCurrent.IsReadable)
           and IsSimpleFunction(FMoveNext) and (FMoveNext.ReturnType.Handle = TypeInfo(boolean)) ) then
      Abort;
end;

destructor TRttiEnumerator.Destroy;
begin
   if FBase.Kind = tkClass then
      FBase.AsObject.Free;
   inherited Destroy;
end;

function TRttiEnumerator.GetCurrent: TValue;
begin
   result := FCurrent.GetValue(FBase.GetReferenceToRawData);
end;

function TRttiEnumerator.GetEnumerator: TRttiEnumerator;
begin
   result := self;
end;

function TRttiEnumerator.GetEnumType: TRttiType;
begin
   result := FCurrent.PropertyType;
end;

function TRttiEnumerator.MoveNext: Boolean;
begin
   result := FMoveNext.Invoke(FBase, []).AsBoolean;
end;

end.
