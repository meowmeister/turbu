unit finalizer_hack;

interface
type
   EditorCategoryAttribute = class(TCustomAttribute)
   private
      FName: string;
      FCategory: string;
   public
      constructor Create(const category, name: string);
      property category: string read FCategory;
      property name: string read FName;
   end;

   EditorContextAttribute = class(TCustomAttribute)
   private
      FName: string;
   public
      constructor Create(const name: string);
      property name: string read FName;
   end;

implementation

{ EditorCategoryAttribute }

constructor EditorCategoryAttribute.Create(const category, name: string);
begin
   FCategory := category;
   FName := name;
end;

{ EditorContextAttribute }

constructor EditorContextAttribute.Create(const name: string);
begin
   FName := name;
end;

end.
