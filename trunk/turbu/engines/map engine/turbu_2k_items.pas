unit turbu_2k_items;

interface
uses
   Generics.collections,
   rsImport,
   turbu_items,
   dwsJSON;

type
   TRpgItem = class(TObject)
   private
      FTemplate: TItemTemplate;
      FQuantity: integer;
      FLevel: integer;

      function getCost: integer;
      function getName: string;
      function getDesc: string;
      function getID: integer;
   protected
      function getUses: integer; virtual;
      procedure setUses(const Value: integer); virtual;
      function getOnField: boolean; virtual; abstract;
      procedure useOnce; inline;
   public
      [NoImport]
      constructor create(const item, quantity: integer); virtual;
      [NoImport]
      class function newItem(const item, quantity: integer): TRpgItem;
      function usableBy(hero: integer): boolean; virtual; abstract;

      [NoImport]
      property template: TItemTemplate read FTemplate;
      property quantity: integer read FQuantity write FQuantity;
      property usesLeft: integer read getUses write setUses;
      property level: integer read FLevel write FLevel;
      property id: integer read getID;
      property desc: string read getDesc;
      property name: string read getName;
      property cost: integer read getCost;
      property usableOnField: boolean read getOnField;
   end;

   TRpgInventory = class(TObject)
   private
      FSorted: boolean;
      FList: TObjectList<TRpgItem>;
      function Count: integer;
   public
      constructor Create;
      destructor Destroy; override;
      [NoImport]
      procedure Serialize(writer: TdwsJSONWriter);

      procedure Add(const id, number: integer);
      procedure AddItem(const value: TRpgItem);
      function indexOf(const id: integer): integer;
      function quantityOf(const id: integer): integer;
      function contains(id: integer): boolean;
      procedure Remove(const id, number: integer);
      procedure sort;
   end;

const
   MAXITEMS = 99;

implementation
uses
   math, Generics.Defaults,
   commons,
   turbu_database, turbu_classes;

type
   TItemClass = class of TRpgItem;

{ TRpgItem }

constructor TRpgItem.create(const item, quantity: integer);
begin
   inherited Create;
   FTemplate := GDatabase.FindItem(item);
   FQuantity := quantity;
end;

function TRpgItem.getCost: integer;
begin
   result := template.cost;
end;

function TRpgItem.getDesc: string;
begin
   result := template.desc;
end;

function TRpgItem.getID: integer;
begin
   result := template.id;
end;

function TRpgItem.getName: string;
begin
   result := template.name;
end;

function TRpgItem.getUses: integer;
begin
   result := FQuantity;
end;

{$WARN USE_BEFORE_DEF OFF}
class function TRpgItem.newItem(const item, quantity: integer): TRpgItem;
var
   subtype: TItemClass;
begin
{$MESSAGE WARN 'Commented out code in live unit'}
{   case GDatabase.findItem(item).itemType of
      it_junk: subtype := TJunkItem;
      weaponItem: subtype := TWeapon;
      shieldItem: subtype := TShield;
      armorItem: subtype := TArmor;
      helmetItem: subtype := THelmet;
      accessoryItem: subtype := TRelic;
      medicineItem: subtype := TRecoveryItem;
      bookItem: subtype := TBookItem;
      materialItem: subtype := TStatItem;
      uniqueItem: subtype := TSkillItem;
      switchItem: subtype := TSwitchItem;
      else} subtype := TRpgItem;
//   end;
   result := subtype.create(item, quantity);
end;
{$WARN USE_BEFORE_DEF ON}

procedure TRpgItem.setUses(const Value: integer);
begin
   //this method deliberately left blank
end;

procedure TRpgItem.useOnce;
begin
   dec(FQuantity);
end;

{ TRpgInventory }

constructor TRpgInventory.Create;
begin
   inherited Create;
   FList := TObjectList<TRpgItem>.Create;
end;

destructor TRpgInventory.Destroy;
begin
   FList.Free;
   inherited Destroy;
end;

procedure TRpgInventory.Serialize(writer: TdwsJSONWriter);
var
   item: TRpgItem;
begin
   Sort;
   writer.BeginArray;
      for item in FList do
      begin
         writer.BeginObject;
            writer.WriteName('ID');
            writer.WriteInteger(item.id);
            writer.WriteName('Quantity');
            writer.WriteInteger(item.FQuantity);
            writer.CheckWrite('Uses', item.getUses, item.FQuantity);
         writer.EndObject;
      end;
   writer.EndArray;
end;

function itemSortCompare(const item1, item2: TRpgItem): integer;
begin
   result := item1.template.id - item2.template.id;
end;

procedure TRpgInventory.Add(const id, number: integer);
var
  i: Integer;
  dummy: TRpgItem;
begin
   if not IsBetween(id, 1, GDatabase.items) then
      Exit;

   dummy := nil;
   i := 0;
   while (dummy = nil) and (i < count) do
   begin
      if FList[i].FTemplate = GDatabase.FindItem(id) then
         dummy := FList[i];
      inc(i);
   end;
   if dummy = nil then
   begin
      FList.Add(TRpgItem.newItem(id, number));
      FSorted := false;
   end
   else
      dummy.FQuantity := min(MAXITEMS, dummy.FQuantity + number);
   //end if
end;

procedure TRpgInventory.AddItem(const value: TRpgItem);
var
  i: Integer;
  dummy: TRpgItem;
  total: integer;
begin
   dummy := nil;
   i := 0;
   while (dummy = nil) and (i < count) do
   begin
      if FList[i].FTemplate = value.FTemplate then
         dummy := FList[i];
      inc(i);
   end;
   if dummy = nil then
   begin
      FList.Add(value);
      FSorted := false;
   end
   else begin
      total := value.quantity + dummy.quantity;
      if total > MAXITEMS then
         dec(value.FQuantity, total - MAXITEMS);
      inc(dummy.FQuantity, value.FQuantity);
   end;
end;

function TRpgInventory.contains(id: integer): boolean;
begin
   result := indexOf(id) <> -1;
end;

function TRpgInventory.Count: integer;
begin
   result := FList.Count;
end;

function TRpgInventory.indexOf(const id: integer): integer;
var
  i: Integer;
begin
   result := -1;
   if not IsBetween(id, 1, GDatabase.items) then
      Exit;

   for i := 0 to self.count - 1 do
   begin
      if FList[i].FTemplate.id = id then
         Exit(i);
   end;
end;

function TRpgInventory.quantityOf(const id: integer): integer;
var dummy: integer;
begin
   dummy := indexOf(id);
   if dummy = -1 then
      result := 0
   else result := FList[dummy].quantity;
end;

procedure TRpgInventory.Remove(const id, number: integer);
var
  i: Integer;
  dummy: TRpgItem;
begin
   if (id < 1) or (id > GDatabase.items) then
      Exit;

   dummy := nil;
   i := 0;
   while (dummy = nil) and (i < count) do
   begin
      if FList[i].FTemplate = GDatabase.findItem(id) then
         dummy := FList[i];
      inc(i);
   end;
   if dummy <> nil then
      if dummy.FQuantity <= number then
         FList.remove(dummy)
      else
         dec(dummy.FQuantity, number);
end;

procedure TRpgInventory.sort;
begin
   if not FSorted then
   begin
      FList.sort(TComparer<TRpgItem>.Construct(itemSortCompare));
      FSorted := true;
   end;
end;

end.
