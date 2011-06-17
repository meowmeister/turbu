unit StringListComp;
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
* tech.turbu-rpg.com.
*****************************************************************************}

interface
uses
   Classes;

type
  TStringCompareProc = procedure(const value: string; const data: TObject) of object;
  TStringEqualProc = procedure(const value: string; const data1, data2: TObject) of object;

  (* Higher-order function to compare two lists of strings.  The three
   * callbacks allow you to define what happens when a match is found between
   * the two lists, when the first list contains a string not in the second
   * list, and when the second list contains a string not in the first, in that
   * order. *)
  procedure StringListCompare(List1, List2: TStringList; matchProc: TStringEqualProc;
                              list1Proc, list2Proc: TStringCompareProc; presorted: boolean = false);

  (* Takes two TStringLists and merges them using the StringListCompare algorithm,
   * producing a new TStringList containing all of the unique strings from both
   * lists.  Associated objects, if any, are ignored. *)
  function StringListMerge(List1, List2: TStringList; presorted: boolean = false): TStringList;

  (* Takes two TStringLists and finds the intersection (overlap) using the
   * StringListCompare algorithm, producing a new TStringList containing all of
   * the strings found in both lists.  Associated objects, if any, are ignored. *)
  function StringListIntersection(List1, List2: TStringList; presorted: boolean = false): TStringList;

  (* Takes two TStringLists and finds all strings that are in one list but not
   * the other, using the StringListCompare algorithm, producing a new
   * TStringList containing all of the strings found in the result set.
   * Associated objects, if any, are ignored. *)
  function StringListMismatch(List1, List2: TStringList; presorted: boolean = false): TStringList;

implementation
uses
   Math, SysUtils;

//------------------------------------------------------------------------------
procedure StringListCompare(List1, List2: TStringList; matchProc: TStringEqualProc;
                            list1Proc, list2Proc: TStringCompareProc; presorted: boolean);
var
  i, j: integer;
begin
   if not presorted then
   begin
      List1.sort;
      List2.sort;
   end;

   i := 0;
   j := 0;

   //handle overlaps
   while (i < List1.Count) and (j < List2.Count) do
      case sign(strComp(PChar(List1[i]), PChar(List2[i]))) of
         0:
         begin
            if assigned(matchProc) then
               matchProc(List1[i], List1.Objects[i], List2.Objects[j]);
            inc(i);
            inc(j);
         end;
         NegativeValue:
         begin
            if assigned(List1Proc) then
               List1Proc(List1[i], List1.Objects[i]);
            inc(i);
         end;
         PositiveValue:
         begin
            if assigned(List2Proc) then
               List2Proc(List2[i], List2.Objects[i]);
            inc(j);
         end;
      end;

  //clean up whatever's left
  if assigned(List1Proc) then
    for i := i to List1.Count - 1 do
      List1Proc(List1[i], List1.Objects[i]);

  if assigned(List2Proc) then
    for j := j to List2.Count - 1 do
      List2Proc(List2[j], List2.Objects[j]);
end;

type
   TSlFunctor = class
   private
      FOutputList: TStringList;
   public
      constructor Create;
      procedure AddSingle(const value: string; const data: TObject);
      procedure AddEqual(const value: string; const data1, data2: TObject);
   end;

//------------------------------------------------------------------------------
function StringListMerge(List1, List2: TStringList; presorted: boolean): TStringList;
var
   functor: TSlFunctor;
begin
   functor := TSlFunctor.Create;
   try
      StringListCompare(list1, list2, functor.AddEqual, functor.AddSingle, functor.AddSingle, presorted);
   except
      functor.FOutputList.Free;
      functor.Free;
      raise;
   end;
   result := functor.FOutputList;
   functor.Free;
end;

function StringListIntersection(List1, List2: TStringList; presorted: boolean = false): TStringList;
var
   functor: TSlFunctor;
begin
   functor := TSlFunctor.Create;
   try
      StringListCompare(list1, list2, functor.AddEqual, nil, nil, presorted);
   except
      functor.FOutputList.Free;
      functor.Free;
      raise;
   end;
   result := functor.FOutputList;
   functor.Free;
end;

function StringListMismatch(List1, List2: TStringList; presorted: boolean = false): TStringList;
var
   functor: TSlFunctor;
begin
   functor := TSlFunctor.Create;
   try
      StringListCompare(list1, list2, nil, functor.AddSingle, functor.AddSingle, presorted);
   except
      functor.FOutputList.Free;
      functor.Free;
      raise;
   end;
   result := functor.FOutputList;
   functor.Free;
end;

{ TSlFunctor }

//------------------------------------------------------------------------------
constructor TSlFunctor.Create;
begin
   FOutputList := TStringList.Create;
end;

//------------------------------------------------------------------------------
procedure TSlFunctor.AddEqual(const value: string; const data1,
  data2: TObject);
begin
   FOutputList.Add(value);
end;

//------------------------------------------------------------------------------
procedure TSlFunctor.AddSingle(const value: string; const data: TObject);
begin
   FOutputList.Add(value);
end;

end.
