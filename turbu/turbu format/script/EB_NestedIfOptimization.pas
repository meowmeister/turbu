unit EB_NestedIfOptimization;

interface
uses
   EventBuilder;

procedure NestedIfOptimization(value: TEBRoutine);

implementation
uses
   Generics.Collections,
   EB_Optimizations, EB_RpgScript, EB_Expressions;

procedure CoalesceConditions(ifObj, childIf: TEBIf);
var
   andList: TEBAndList;
begin
   if ifObj.children[0].ClassType = TEBAndList then
      andList := TEBAndList(ifObj.children.Extract(ifObj.children[0]))
   else begin
      andList := TEBAndList.Create(nil);
      andList.Add(ifObj.children[0]);
   end;
   if not IsBinListDuplicate(andList, childIf.children[0]) then
      andList.Add(childIf.children[0].clone);
   ifObj.Insert(0, andList);
end;

procedure CoalesceStatements(ifObj, childIf: TEBIf);
var
   i: integer;
begin
   assert(childIf = ifObj.children[1].children[0]);
   assert(ifObj.children[1].ChildCount = 1);
   childIf.Extract;
   for i := 0 to childIf.children[1].ChildCount - 1 do
      ifObj.Add(childIf.children[1].children[0]);
   if childIf.ElseSet then
   begin
      ifObj.SetElse;
      for i := 0 to childIf.children[2].ChildCount - 1 do
         ifObj.Add(childIf.children[2].children[0]);
   end;
end;

procedure TryCoalesce(ifObj: TEBIf; list: TList<TEBIf>);
var
   childIf: TEBIf;
begin
   if ifObj.ElseSet then
      Exit;
   if ifObj.children[1].ChildCount <> 1 then
      Exit;
   if ifObj.children[1].children[0].ClassType <> TEBIf then
      Exit;
   childIf := TEBIf(ifObj.children[1].children[0]);
   list.Extract(childIf);
   try
      CoalesceConditions(ifObj, childIf);
      CoalesceStatements(ifObj, childIf);
   finally
      childIf.Free;
   end;
   TryCoalesce(ifObj, list);
end;

procedure CheckEmptyNegativeIf(ifObj: TEBIf);
var
   i: integer;
   block: TEBCodeBlock;
begin
   if not ifObj.ElseSet then
      Exit;
   if ifObj.children[1].ChildCount > 0 then
      Exit;
   ifObj.Negate;
   block := ifObj.ClearElse;
   try
      for i := 0 to block.ChildCount - 1 do
         ifObj.children[1].Add(block.children[0]);
   finally
      block.Free;
   end;
end;

procedure NestedIfOptimization(value: TEBRoutine);
var
   ifs: TList<TEBIf>;
   i: integer;
begin
   ifs := TList<TEBIf>.Create;
   try
      TCollector.CollectExactRecursive<TEBIf>(value, ifs);
      for i := 0 to ifs.Count - 1 do
         CheckEmptyNegativeIf(ifs[i]);
      i := 0;
      while i < ifs.Count do
      begin
         TryCoalesce(ifs[i], ifs);
         inc(i);
      end;
   finally
      ifs.Free;
   end;
end;

end.
