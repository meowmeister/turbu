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

unit EB_MessagePromptCollapser;

interface
uses
   EventBuilder;

procedure ChoiceMerge(value: TEBRoutine);
procedure InputMerge(value: TEBRoutine);

implementation
uses
   Generics.Collections,
   EB_Optimizations, EB_Messages;

procedure TryMerge(obj, idxParent: TEBObject; linecount: integer);
var
   idx, i: integer;
   msg: TEBShowMessage;
begin
   idx := idxParent.Owner.children.IndexOf(idxParent);
   if (idx > 0) and (idxParent.Owner.children[idx - 1].classtype = TEBShowMessage) then
   begin
      msg := TEBShowMessage(idxParent.Owner.children[idx - 1]);
      inc(linecount); //1 line for the message
      inc(linecount, msg.ChildCount); //additional lines
      if linecount <= 4 then
      begin
         obj.Text := msg.Text;
         for i := 0 to msg.ChildCount - 1 do
            obj.Text := obj.Text + #13#10 + msg.children[i].Text;
         msg.Free;
      end;
   end;
end;

procedure TryMergeChoice(value: TEBChoiceMessage);
var
   linecount: integer;
   expr: TEBChoiceExpr;
begin
   expr := (value.children[0] as TEBChoiceExpr);
   linecount := length(expr.Choices);
   if linecount = 4 then
      Exit;
   tryMerge(expr, value, linecount);
end;

procedure ChoiceMerge(value: TEBRoutine);
var
   choices: TList<TEBChoiceMessage>;
   i: integer;
begin
   choices := TList<TEBChoiceMessage>.Create;
   try
      TCollector.CollectExact<TEBChoiceMessage>(value, choices);
      for i := 0 to choices.Count - 1 do
         TryMergeChoice(choices[i]);
   finally
      choices.Free;
   end;
end;

procedure TryMergeInputs(value: TEBInputNumber);
begin
   tryMerge(value, value, 1);
end;

procedure InputMerge(value: TEBRoutine);
var
   inputs: TList<TEBInputNumber>;
   i: integer;
begin
   inputs := TList<TEBInputNumber>.Create;
   try
      TCollector.CollectExact<TEBInputNumber>(value, inputs);
      for i := 0 to inputs.Count - 1 do
         TryMergeInputs(inputs[i]);
   finally
      inputs.Free;
   end;
end;

end.
