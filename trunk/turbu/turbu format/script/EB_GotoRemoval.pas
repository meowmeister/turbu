unit EB_GotoRemoval;

interface
uses
   EB_RpgScript, EventBuilder;

procedure GotoRemoval(value: TEBRoutine);

implementation
uses
   SysUtils, Math, Generics.Collections,
   EB_Expressions, EB_Optimizations, turbu_operators;

type TEBIfSpecial = class(TEBIf);

function CollectGotos(value: TEBRoutine): TList<TEBGoto>;
begin
   result := TList<TEBGoto>.Create;
   TCollector.CollectExact<TEBGoto>(value, result);
end;

function CollectLabels(value: TEBRoutine): TList<TEBLabel>;
begin
   result := TList<TEBLabel>.Create;
   TCollector.CollectExact<TEBLabel>(value, result);
end;

function FindLabel(lGoto: TEBGoto; labels: TList<TEBLabel>): TEBLabel;
var
   lbl: integer;
begin
   lbl := lGoto.Values[0];
   for result in labels do
      if result.Values[0] = lbl then
         Exit;
   result := nil;
end;

function GetGotoTarget(parentIf: TEBIf): integer;
var
   child1: TEBObject;
   lGoto: TEBGoto;
begin
   child1 := parentIf.children[1].children[0];
   if child1.ClassType = TEBGoto then
      lGoto := TEBGoto(child1)
   else raise ERPGScriptError.CreateFmt('Expected TEBGoto object but found %s instead.', [child1.ClassName]);
   result := lGoto.values[0];
end;

function GetGotoName(parentIf: TEBIf): string;
begin
   result := 'goto' + IntToStr(GetGotoTarget(parentIf));
end;

function GetParentIf(lGoto: TEBGoto): TEBIf;
var
   parent: TEBObject;
   i: integer;
begin
   parent := lGoto.Owner;
   if ((parent.ClassType = TEBCodeBlock) and (parent.Owner.ClassType = TEBIf) and (parent.ChildCount = 1) and (parent.Owner.ChildCount = 2)) then
      result := TEBIf(parent.Owner)
   else begin
      result := TEBIf.Create(nil, TEBVariableValue.Create('goto' + IntTOStr(lGoto.Values[0])));
      i := parent.children.IndexOf(lGoto);
      parent.children.Extract(lGoto);
      parent.Insert(i, result);
      result.Add(lGoto);
      parent.Insert(i, TEBAssignment.Create(nil, result.children[0].clone as TEBExpression, TEBBooleanValue.Create(true)));
   end;
end;

function GetAncestors(obj: TEBObject; stop: TEBObject = nil): TArray<TEBObject>;
var
   list: TList<TEBObject>;
   current: TEBObject;
begin
   current := obj;
   list := TList<TEBObject>.Create;
   try
      while current <> stop do
      begin
         list.Add(current);
         current := current.Owner;
      end;
      list.Reverse;
      result := list.ToArray;
   finally
      list.Free;
   end;
end;

procedure HandleBreakCapture(rpt: TEBRepeatLoop; brk: TEBBreak);
var
   name: string;
   parent: TEBObject;
   cond: TEBIf;
begin
   //ugly but simple way to create a unique var name
   name := 'break_' + IntToStr(brk.GetHashCode);
   rpt.Insert(0, TEBAssignment.Create(nil, TEBVariableValue.Create(name), TEBBooleanValue.Create(false)));

   parent := brk.Owner;
   parent.Insert(parent.children.IndexOf(brk), TEBAssignment.Create(nil, TEBVariableValue.Create(name), TEBBooleanValue.Create(true)));

   parent := rpt.Owner;
   cond := TEBIf.Create(nil, TEBVariableValue.Create(name));
   cond.Add(TEBAssignment.Create(nil, TEBVariableValue.Create(name), TEBBooleanValue.Create(false)));
   cond.Add(brk.Clone);
   parent.Insert(parent.children.IndexOf(rpt) + 1, cond);
end;

procedure CheckForBreakCapture(rpt: TEBRepeatLoop);
var
   breaks: TList<TEBBreak>;
   brk: TEBBreak;
   ancestor: TEBObject;
   safe: boolean;
begin
   breaks := TList<TEBBreak>.Create;
   try
      TCollector.Collect<TEBBreak>(rpt, breaks);
      for brk in breaks do
      begin
         safe := false;
         for ancestor in GetAncestors(brk.owner, rpt) do
            if ancestor is TEBLoop then
            begin
               safe := true;
               break;
            end;
            if not safe then
               HandleBreakCapture(rpt, brk);
      end;
   finally
      breaks.Free;
   end;
end;

function CloneIfCondition(parentIf: TEBIf; parentBlock: TEBObject; idx: integer): TEBExpression;
var
   cond, gVar: TEBExpression;
   asn: TEBAssignment;
begin
   cond := parentIf.children[0] as TEBExpression;
   if (cond is TEBVariableValue) and (cond.Text = GetGotoName(ParentIf)) then
      result := cond.Clone as TEBExpression
   else begin
      gVar := TEBVariableValue.Create(GetGotoName(parentIf));
      asn := TEBAssignment.Create(nil, gVar, cond);
      parentBlock.Insert(idx, asn);
      parentIf.Insert(0, gVar.Clone);
      result := gVar.Clone as TEBExpression;
   end;
end;

function CloneIfConditionNeg(parentIf: TEBIf; parentBlock: TEBObject; idx: integer): TEBExpression;
begin
   result := CloneIfCondition(parentIf, parentBlock, idx);
   result := negateCondition(result);
end;

procedure ApplyMoveTransformation(parentIf: TEBIf; dest: TEBObject); forward;

function TryOptimizeLifting(parentIf: TEBIf; dest, block: TEBObject): boolean;
var
   cond: TEBExpression;
   gVar: TEBVariableValue;
   asn: TEBAssignment;
begin
   result := false;
   if (block.ClassType = TEBRepeatLoop) and (block.children[0].ClassType = TEBNotExpr)
     and (block.children[0].children[0].classType = TEBVariableValue) then
   begin
      //avoid creating duplicate nested loops
      if ((block.children[0].children[0].GetScript(0) = parentIf.children[0].GetScript(0))) then
      begin
         block.Insert(1, parentIf);
         ApplyMoveTransformation(parentIf, dest);
         Exit(true);
      end;
      if (block.ChildCount - 1 > block.children.IndexOf(parentIf)) and
        (block.children[block.children.IndexOf(parentIf) + 1].ClassType = TEBAssignment) then
      begin
         cond := parentIf.children[0].Clone as TEBExpression;
         gVar := TEBVariableValue.Create(GetGotoName(parentIf));
         asn := TEBAssignment.Create(nil, gVar, cond);
         try
            if block.children[block.children.IndexOf(parentIf) + 1].GetScript(0) = asn.GetScript(0) then
            begin
               parentIf.Free;
               Abort;
            end;
         finally
            asn.Free;
         end;
      end;
   end;
end;

procedure TryEngulfSpecialIf(parentRpt: TEBRepeatLoop; block: TEBObject);
var
   idx: integer;
   sibling: TEBObject;
begin
   while true do
   begin
      assert(parentRpt.Owner = block);
      idx := block.children.IndexOf(parentRpt);
      if idx = 0 then
         Exit;
      sibling := block.children[idx - 1];
      if sibling.ClassType = TEBIfSpecial then
         parentRpt.Insert(1, sibling)
      else Exit;
   end;
end;

procedure ApplyLiftingTransformation(parentIf: TEBIf; dest, block: TEBObject; parentIdx: integer);
var
   parentRpt: TEBRepeatLoop;
   i, gIndex, lIndex: integer;
begin
   if TryOptimizeLifting(parentIf, dest, block) then
      Exit;
   parentRpt := TEBRepeatLoop.Create(nil);
   block.Insert(parentIdx + 1, parentRpt);
   parentRpt.Add(CloneIfConditionNeg(parentIf, block, parentIdx));
   gIndex := block.children.IndexOf(parentIf);
   lIndex := block.children.IndexOf(dest);
   for i := lIndex to gIndex do
      parentRpt.Add(block.children[lIndex]);
   if parentRpt.ChildCount = 1 then
      ParentRpt.Free
   else begin
      CheckForBreakCapture(parentRpt);
      TryEngulfSpecialIf(parentRpt, block);
   end;
end;

procedure ApplyLoweringTransformation(parentIf: TEBIf; dest, block: TEBObject; lIndex, gIndex: integer);
var
   newIf: TEBIf;
   i: integer;
begin
   newIf := TEBIf.Create(nil, CloneIfConditionNeg(parentIf, block, gIndex));
   if (lIndex - gIndex = 2) and (block.Children[gIndex + 1].classtype = TEBIf) then
   begin
      if (newIf.GetScriptText = block.Children[gIndex + 1].GetScriptText) then
      begin
         //avoid creating duplicate nested IF blocks
         newIf.Free;
         block.insert(lIndex, parentIf);
         Exit;
      end
      else begin
         if parentIf.children[0].GetScript(0) = block.children[gIndex + 1].children[0].GetScript(0) then
         begin
            //avoid creating redundant sequential IF blocks
            newIf.Free;
            parentIf.free;
            Abort
         end;
      end;
   end;
   gIndex := block.children.IndexOf(parentIf);
   lIndex := block.children.IndexOf(dest);
   for i := gIndex + 1 to lIndex - 1 do
      newIf.Add(block.children[gIndex + 1]);
   block.Insert(gIndex + 1, newIf);
end;

procedure ApplyMoveTransformation(parentIf: TEBIf; dest: TEBObject);
var
   gIndex, lIndex: integer;
   block: TEBObject;
begin
   block := parentIf.Owner;
   gIndex := block.children.IndexOf(parentIf);
   lIndex := block.children.IndexOf(dest);
   if gIndex < lIndex then
   begin
      if lIndex - gIndex = 1 then
         Exit
      else ApplyLoweringTransformation(parentIf, dest, block, lIndex, gIndex);
   end
   else ApplyLiftingTransformation(parentIf, dest, block, gIndex);
end;

procedure ApplyElimTransformation(parentIf: TEBIf; lLabel: TEBLabel);
begin
   assert(parentIf.Owner = lLabel.Owner);
   ApplyMoveTransformation(parentIf, lLabel);
   parentIf.Free;
end;

function GetCommonAncestor(o1, o2: TEBObject): TEBObject;
var
   l1, l2: TArray<TEBObject>;
   i: integer;
begin
   l1 := GetAncestors(o1);
   l2 := GetAncestors(o2);
   assert(l1[0] = l2[0]);
   result := l1[0];
   for i := 1 to min(High(L1), High(L2)) do
      if l1[i] = l2[i] then
         result := l1[i]
      else Exit;
end;

procedure CreateBlockBreak(parentIf: TEBIf; parentBlock: TEBObject; idx: integer);
var
   i: integer;
   blockBreak: TEBIf;
begin
   if (parentBlock.children[idx + 1].ClassType = TEBGoto) and
      (parentBlock.children[idx + 1].values[0] = GetGotoTarget(parentIf)) then
   begin
      parentIf.Free;
      Abort;
   end;

   blockBreak := TEBIf.Create(nil, CloneIfCondition(parentIf, parentBlock, idx));
   blockBreak.Negate;

   for i := idx + 1 to parentBlock.ChildCount - 1 do
      blockBreak.Add(parentBlock.children[idx + 1]);
   parentBlock.add(blockBreak);
end;

function MoveGotoUpBlock(parentIf: TEBIf; parentBlock: TEBObject): TEBObject;
var
   ifParent, blockParent: TEBObject;
   idx: integer;
begin
   ifParent := parentIf.Owner;
   idx := ifParent.children.IndexOf(parentIf);
   if idx < ifParent.ChildCount - 1 then
      CreateBlockBreak(parentIf, ifParent, idx);
   blockParent := parentBlock.Owner;
   idx := blockParent.children.IndexOf(parentBlock);
   blockParent.Insert(idx + 1, parentIf);
   result := blockParent;
end;

function MoveGotoUpLoop(parentIf: TEBIf; parentBlock: TEBLoop): TEBObject;
var
   ifParent: TEBObject;
   newIf: TEBIf;
   idx: integer;
begin
   ifParent := parentIf.Owner;
   idx := ifParent.children.IndexOf(parentIf);
   if idx < ifParent.ChildCount - 1 then
   begin
      newIf := TEBIf.Create(nil, parentIf.children[0].clone as TEBExpression);
      newIf.add(TEBBreak.Create(nil));
      ifParent.Insert(idx, newIf);
   end;

   result := parentBlock.Owner;
   idx := result.children.IndexOf(parentBlock);
   result.Insert(idx + 1, parentIf);
end;

function MoveGotoUp(parentIf: TEBIf): TEBObject;
var
   parentBlock, grandparentBlock: TEBObject;
begin
   parentBlock := parentIf.Owner;
   if parentBlock is TEBLoop then
      result := MoveGotoUpLoop(parentIf, TEBLoop(parentBlock))
   else begin
      if not (parentBlock is TEBBlock) then
         assert(false);
      grandparentBlock := parentBlock.Owner;
      if (grandparentBlock is TEBIf) or (grandparentBlock is TEBCase) then
         result := MoveGotoUpBlock(parentIf, TEBBlock(grandparentBlock))
      else result := MoveGotoUpBlock(parentIf, TEBBlock(parentBlock));
   end;
end;

procedure MergeIfCriteria(sourceIf, destIf: TEBIf);
var
   orList: TEBOrList;
begin
   if destIf.children[0].ClassType = TEBOrList then
      orList := TEBOrList(destIf.children.Extract(destIf.children[0]))
   else begin
      orList := TEBOrList.Create(nil);
      orList.Add(destIf.children[0]);
   end;
   if not IsBinListDuplicate(orList, sourceIf.children[0]) then
      orList.Insert(0, sourceIf.children[0]);
   destIf.Insert(0, orList);
end;

procedure MoveIntoIf(parentIf, destIf: TEBIf; destBlock: TEBObject);
var
   destParent: TEBObject;
   cloneIf: TEBIf;
   idx: integer;
begin
   destParent := destIf.Owner;
   idx := destParent.children.IndexOf(destIf);
   cloneIf := TEBIf.Create(nil, CloneIfCondition(parentIf, destParent, idx));
   try
      if destBlock <> destIf.children[1] then
      begin
         if not (destIf.ElseSet and (destBlock = destIf.children[2])) then
            assert(false);
         cloneIf.Negate;
      end;
      MergeIfCriteria(cloneIf, destIf);
      destBlock.Insert(0, parentIf);
   finally
      cloneIf.Free;
   end;
end;

function GetCaseName(target: TEBCase): string;
begin
   result := 'case' + IntToStr(target.GetHashCode);
end;

function GetElseBlockValue(destCase: TEBCase): TEBExpression;
begin
   //hack: nothing in RPG Maker will generate code that matches this, so should always default to Else
   result := TEBIntegerValue.Create(-1);
end;

type PClass = ^TClass;

procedure TransformCase(aCase: TEBCase);
var
   caseType: TClass;
begin
   caseType := aCase.ClassType;
   assert(caseType.InstanceSize = TEBCase.InstanceSize);
   assert(aCase.children[0] is TEBExpression);
   PClass(aCase)^ := caseType.ClassParent;
end;

procedure TryMergeUp(newIf: TEBIf; parent: TEBObject);
var
   idx: integer;
   candidate: TEBIfSpecial;
   casevar, casevar2: string;
begin
   assert(parent = newIf.owner);
   idx := parent.children.IndexOf(newIf);
   if idx = 0 then
      Exit;
   if parent.children[idx - 1].ClassType <> TEBIfSpecial then
      Exit;
   candidate := TEBIfSpecial(parent.children[idx - 1]);
   casevar := (newIf.children[1].children[0] as TEBAssignment).children[0].Name;
   casevar2 := (candidate.children[1].children[0] as TEBAssignment).children[0].Name;
   if casevar <> casevar2 then
      Exit;
   casevar := (newIf.children[1].children[0] as TEBAssignment).children[1].GetScriptText;
   casevar2 := (candidate.children[1].children[0] as TEBAssignment).children[1].GetScriptText;
   if casevar <> casevar2 then
      Exit;

   MergeIfCriteria(newIf, candidate);
   newIf.Free;
end;

procedure MoveIntoCase(parentIf: TEBIf; destCase: TEBCase; destBlock: TEBObject);
var
   destParent: TEBObject;
   caseValue: TEBExpression;
   idx: integer;
   cloneIf: TEBIf;
   caseName: string;
begin
   destParent := destCase.Owner;
   idx := destParent.children.IndexOf(destCase);
   cloneIf := TEBIfSpecial.Create(nil, CloneIfCondition(parentIf, destParent, idx));
   idx := destParent.children.IndexOf(destCase);
   destParent.Insert(idx, cloneIf);
   caseName := GetCaseName(destCase);
   if destBlock is TEBEnumCaseBlock then
      caseValue := TEBEnumValue.Create(destBlock.Text)
   else if destBlock is TEBCaseBlock then
      caseValue := TEBIntegerValue.Create(destBlock.Values[0])
   else begin
      assert(destBlock is TEBElseBlock);
      caseValue := GetElseBlockValue(destCase);
   end;
   cloneIf.add(TEBAssignment.Create(nil, TEBVariableValue.Create(caseName), caseValue));
   cloneIf.SetElse;
   if not ((destCase.classtype = TEBCase) or (destCase.classtype = TEBMaybeCase)) then
      TransformCase(destCase);
   cloneIf.Add(TEBAssignment.Create(nil, TEBVariableValue.Create(caseName), destCase.children[0] as TEBExpression));
   destCase.Insert(0, TEBVariableValue.Create(caseName));
   destBlock.Insert(0, parentIf);
   if destCase.children[0].text = caseName then
      TryMergeUp(cloneIf, destparent);
end;

procedure MoveIntoWhileLoop(parentIf: TEBIf; destLoop: TEBWhileLoop);
var
   destParent: TEBObject;
   idx: integer;
   orList: TEBOrList;
begin
   destParent := destLoop.Owner;
   idx := destParent.children.IndexOf(destLoop);
   if destLoop.children[0].ClassType = TEBOrList then
      orList := TEBOrList(destLoop.children.Extract(destLoop.children[0]))
   else begin
      orList := TEBOrList.Create(nil);
      orList.Add(destLoop.children[0]);
   end;
   orList.Insert(0, CloneIfCondition(parentIf, destParent, idx));
   destLoop.Insert(0, orList);
   destLoop.Insert(1, parentIf);
end;

procedure MoveIntoRepeatLoop(parentIf: TEBIf; destLoop: TEBRepeatLoop);
begin
   if destLoop.children[0] is TEBExpression then
      destLoop.Insert(1, parentIf)
   else destLoop.Insert(0, parentIf);
end;

function MoveGotoDown(routine: TEBRoutine; parentIf: TEBIf; labelParent, gotoParent: TEBObject): TEBObject;
var
   ancestors: TArray<TEBObject>;
   anc1, anc2: TEBObject;
begin
   ancestors := GetAncestors(labelParent.Owner, gotoParent);
   anc1 := ancestors[0];
   if (anc1 is TEBIf) or (anc1 is TEBCase) then
      anc2 := ancestors[1]
   else anc2 := anc1;
   assert(anc1.Owner = gotoParent);
   ApplyMoveTransformation(parentIf, anc1);
   if anc1 is TEBIf then
      MoveIntoIf(parentIf, TEBIf(anc1), anc2)
   else if anc1 is TEBCase then
      MoveIntoCase(parentIf, TEBCase(anc1), anc2)
   else if anc1 is TEBWhileLoop then
      MoveIntoWhileLoop(parentIf, TEBWhileLoop(anc1))
   else if anc1 is TEBRepeatLoop then
      MoveIntoRepeatLoop(parentIf, TEBRepeatLoop(anc1))
   else anc1.Insert(0, parentIf);
   result := anc2;
end;

procedure ApplyLevelTransformation(routine: TEBRoutine; parentIf: TEBIf;
  gotoParent, labelParent: TEBObject; lbl: TEBLabel);
var
   ancestor: TEBObject;
begin
   ancestor := GetCommonAncestor(gotoParent, labelParent);
   if (ancestor is TEBIf) or (ancestor is TEBCase) then
      ancestor := ancestor.Owner;
   while gotoParent <> ancestor do
      gotoParent := MoveGotoUp(parentIf);
   while gotoParent <> labelParent do
      gotoParent := MoveGotoDown(routine, parentIf, lbl, gotoParent);
end;

function TrySimplifyGotoRemoval(routine: TEBRoutine; lGoto: TEBGoto; lLabel: TEBLabel;
  labelParent: TEBObject): boolean;
var
   idx: integer;
   replacement: TEBObject;
begin
   idx := labelParent.children.IndexOf(lLabel);
   if idx <> labelParent.ChildCount - 1 then
      replacement := nil
   else if labelParent = routine then
      replacement := TEBExit.Create(nil)
   else if labelParent is TEBLoop then
      replacement := TEBContinue.Create(nil)
   else replacement := nil;

   if assigned(replacement) then
   begin
      idx := lGoto.Owner.children.IndexOf(lGoto);
      lGoto.Owner.Insert(idx, replacement);
      lGoto.Free;
      result := true;
   end
   else result := false;
end;

procedure DoGotoRemoval(value: TEBRoutine; lGoto: TEBGoto; lLabel: TEBLabel);
var
   parentIf: TEBIf;
   gotoParent, labelParent: TEBObject;
begin
   labelParent := lLabel.Owner;
   if TrySimplifyGotoRemoval(value, lGoto, lLabel, labelParent) then
      Exit;
   parentIf := GetParentIf(lGoto);
   gotoParent := parentIf.Owner;
   try
      if gotoParent <> labelParent then
         ApplyLevelTransformation(value, parentIf, gotoParent, labelParent, lLabel);
      ApplyElimTransformation(parentIf, lLabel);
   except
      on EAbort do ;
   end;
end;

function LoopsInAncestry(lLabel: TEBLabel): boolean;
var
   obj: TEBObject;
begin
   for obj in GetAncestors(lLabel) do
      if obj is TEBLoop then
         Exit(true);
   result := false;
end;

procedure ClearLabel(routine: TEBRoutine; lLabel: TEBLabel);
var
   gotoName: string;
   labelParent: TEBObject;
   idx: integer;
begin
   gotoName := 'goto' + IntToStr(lLabel.Values[0]);
   if (pos(gotoName, routine.GetVarBlock.Text) > 0) and (LoopsInAncestry(lLabel)) then
   begin
      labelParent := lLabel.Owner;
      idx := labelParent.children.IndexOf(lLabel);
      labelParent.Insert(idx, TEBAssignment.Create(nil, TEBVariableValue.Create(gotoName), TEBBooleanValue.Create(false)));
   end;
   lLabel.Free;
end;

procedure TryMoveLabelDown(lLabel: TEBLabel);
var
   parent, grandparent: TEBObject;
   idx: integer;
begin
   parent := lLabel.Owner;
   idx := parent.children.IndexOf(lLabel);
   while idx = parent.ChildCount - 1 do
   begin
      if parent is TEBLoop then
         Break;
      if (parent is TEBCodeBlock) then
      begin
         grandparent := parent.Owner;
         if (grandparent is TEBIf) or (grandparent is TEBCase) then
         begin
            parent := grandparent.Owner;
            idx := parent.children.IndexOf(grandparent);
            parent.Insert(idx + 1, lLabel);
            inc(idx);
         end
         else Break;
      end
      else Break;
   end;
end;

function CompactLabels(base, candidate: TEBLabel; gotos: TList<TEBGoto>): boolean;
var
   baseParent, candParent: TEBObject;
   baseIdx, candIdx, baseValue, candValue: integer;
   lGoto: TEBGoto;
begin
   result := false;
   baseParent := base.Owner;
   candParent := candidate.Owner;
   if baseParent <> candParent then
      Exit;
   baseIdx := baseParent.children.IndexOf(base);
   candIdx := candParent.children.IndexOf(candidate);
   if abs(baseIdx - candIdx) = 1 then
   begin
      candValue := candidate.Values[0];
      baseValue := base.Values[0];
      for lGoto in gotos do
         if lGoto.Values[0] = candValue then
            lGoto.Values[0] := baseValue;
      candidate.Free;
      result := true;
   end;
end;

procedure DoOptimizeLabelPlacement(gotos: TList<TEBGoto>; labels: TList<TEBLabel>);
var
   i, j: integer;
   lLabel, candidate: TEBLabel;
begin
   for i := 0 to labels.count - 1 do
      TryMoveLabelDown(labels[i]);
   for i := labels.Count - 1 downto 0 do
   begin
      lLabel := labels[i];
      for j := labels.Count - 1 downto 0  do
      begin
         candidate := labels[j];
         if lLabel = candidate then
            Continue;
         if CompactLabels(lLabel, candidate, gotos) then
            labels.Delete(j)
         else if (j > i) and (lLabel.Values[0] = candidate.Values[0]) then
         begin
            //duplicate label; toss second value
            candidate.Free;
            labels.Delete(j);
         end;
      end;
   end;
end;

procedure OptimizeLabelPlacement(routine: TEBRoutine; gotos: TList<TEBGoto>; labels: TList<TEBLabel>);
var
   script, newScript: string;
begin
   newScript := routine.GetScript(0);
   repeat
      script := newScript;
      DoOptimizeLabelPlacement(gotos, labels);
      newScript := routine.GetScript(0);
   until script = newScript;
end;

procedure MergeListElement(expr: TEBExpression; list: TEBAndList);
begin
   if not IsBinListDuplicate(list, expr) then
      list.Add(expr.clone);
end;

procedure MergeLists(source, dest: TEBAndList);
var
   i: integer;
begin
   for i := 0 to source.ChildCount - 1 do
      MergeListElement(source.children[i] as TEBExpression, dest);
end;

procedure MergeRepeatCriteria(outer, inner: TEBRepeatLoop);
var
   base: TEBExpression;
   baseList: TEBAndList;
begin
   base := outer.children[0] as TEBExpression;
   if base.ClassType = TEBAndList then
   begin
      base.Extract;
      baseList := TEBAndList(base);
   end
   else begin
      baseList := TEBAndList.Create(nil);
      baseList.Add(base);
   end;

   base := inner.children[0] as TEBExpression;
   if base.ClassType = TEBAndList then
      MergeLists(TEBAndList(base), baseList)
   else MergeListElement(base as TEBExpression, baseList);
   outer.Insert(0, baseList);
end;

procedure TryCollapseNestedRepeat(rpt: TEBRepeatLoop; repeats: TList<TEBRepeatLoop>);
var
   i: integer;
   nested: TEBRepeatLoop;
begin
   while true do
   begin
      if rpt.ChildCount <> 2 then
         Exit;
      if rpt.children[1].ClassType <> TEBRepeatLoop then
         Exit;
      nested := TEBRepeatLoop(rpt.children[1]);
      for i := 1 to nested.ChildCount - 1 do
         rpt.Add(nested.children[1]);
      MergeRepeatCriteria(rpt, nested);
      repeats.Extract(nested);
      nested.Free;
   end;
end;

procedure CollapseNestedRepeats(routine: TEBRoutine);
var
   repeats: TList<TEBRepeatLoop>;
   rpt: TEBRepeatLoop;
   i: integer;
begin
   repeats := TList<TEBRepeatLoop>.Create;
   try
      TCollector.CollectExactRecursive<TEBRepeatLoop>(routine, repeats);
      i := 0;
      while i < repeats.Count do
      begin
         rpt := repeats[i];
         TryCollapseNestedRepeat(rpt, repeats);
         inc(i);
      end;
   finally
      repeats.Free;
   end;
end;

procedure RevertSpecialIfs(routine: TEBRoutine);
var
   ifs: TList<TEBIfSpecial>;
   ifObj: TEBIfSpecial;
begin
   ifs := TList<TEBIfSpecial>.Create;
   try
      TCollector.CollectExactRecursive<TEBIfSpecial>(routine, ifs);
      for ifObj in ifs do
         PClass(ifObj)^ := TEBIf;
   finally
      ifs.Free;
   end;
end;

procedure GotoRemoval(value: TEBRoutine);
var
   gotos: TList<TEBGoto>;
   labels: TList<TEBLabel>;
   lGoto: TEBGoto;
   lLabel: TEBLabel;
   i: integer;
begin
   gotos := CollectGotos(value);
   labels := CollectLabels(value);
   try
      OptimizeLabelPlacement(value, gotos, labels);
      for i := 0 to gotos.Count - 1 do
      begin
         lGoto := gotos[i];
         lLabel := FindLabel(lGoto, labels);
         if assigned(lLabel) then
            DoGotoRemoval(value, lGoto, lLabel)
         else lGoto.Free;
      end;
      for lLabel in labels do
         ClearLabel(value, lLabel);
      CollapseNestedRepeats(value);
      RevertSpecialIfs(value);
   finally
      gotos.Free;
      labels.Free;
   end;
end;

end.
