unit EB_Optimizations;

interface
uses
   Generics.Collections,
   EventBuilder, EB_Expressions;

type
   TCollector = class
      class procedure Collect<T: TEBObject>(value: TEBObject; list: TList<T>); static;
      class procedure CollectExact<T: TEBObject>(value: TEBObject; list: TList<T>); static;
      class procedure CollectExactRecursive<T: TEBObject>(value: TEBObject; list: TList<T>); static;
   end;

function NegateCondition(cond: TEBExpression): TEBExpression;
function IsBinListDuplicate(binList: TEBBooleanBinList; expr: TEBObject): boolean;

implementation
uses
   EB_RpgScript;

{ TCollector }

class procedure TCollector.Collect<T>(value: TEBObject; list: TList<T>);
var
   child: TEBObject;
begin
   for child in value do
      if child.InheritsFrom(T) then
         list.Add(child)
      else Collect<T>(child, list);
end;

class procedure TCollector.CollectExact<T>(value: TEBObject; list: TList<T>);
var
   child: TEBObject;
begin
   for child in value do
      if child.ClassType = TClass(T) then
         list.Add(child)
      else CollectExact<T>(child, list);
end;

class procedure TCollector.CollectExactRecursive<T>(value: TEBObject;
  list: TList<T>);
var
   child: TEBObject;
begin
   for child in value do
   begin
      if child.ClassType = TClass(T) then
         list.Add(child);
      CollectExactRecursive<T>(child, list);
   end;
end;

function NegateCondition(cond: TEBExpression): TEBExpression;
var
   tempIf: TEBIf;
begin
   tempIf := TEBIf.Create(nil, cond);
   try
      tempIf.Negate;
      result := tempIf.children[0] as TEBExpression;
      result.Extract;
   finally
      tempIf.Free;
   end;
end;

function IsBinListDuplicate(binList: TEBBooleanBinList; expr: TEBObject): boolean;
var
   child: TEBObject;
   exprScript: string;
begin
   result := false;
   exprScript := expr.GetScript(0);
   for child in binList do
      if child.GetScript(0) = exprScript then
         Exit(true);
end;

end.
