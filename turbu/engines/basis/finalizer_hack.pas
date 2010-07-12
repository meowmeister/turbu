unit finalizer_hack;

interface
type
   EditorCategoryAttribute = class(TCustomAttribute)
   private
      FName: string;
      FOrder: integer;
      FCategory: string;
   public
      constructor Create(const category, name: string; sortOrder: integer);
      property category: string read FCategory;
      property name: string read FName;
      property order: integer read FOrder;
   end;

implementation

{ EditorCategoryAttribute }

constructor EditorCategoryAttribute.Create(const category, name: string; sortOrder: integer);
begin
   FCategory := category;
   FName := name;
   FOrder := sortOrder;
end;

end.
