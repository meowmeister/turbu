{ *****************************************************************************
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
  ***************************************************************************** }

unit fr_EditCondition;

interface

uses
  Forms, StdCtrls, Classes, Controls, ComCtrls,
  EBEdit, EBListView, EventBuilder;

type
   TfrEditCondition = class(TFrame, IContextualEditor)
      trvCondition: TEBTreeView;
      StaticText1: TStaticText;
   private //IContextualEditor implementation
      procedure SetContext(const context, suffix: string);
   public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      procedure UploadObject(obj: TEbObject);
      function DownloadObject: string;
   end;

implementation
uses
   EB_RpgScript, EB_Expressions;

{$R *.dfm}

type
   THolderProc = class(TEBProcedure)
   protected
      function ExprNodeTree: boolean; override;
   end;

{ TfrEditCondition }

constructor TfrEditCondition.Create(AOwner: TComponent);
begin
   inherited Create(AOwner);
   trvCondition.proc := THolderProc.Create(nil);
end;

destructor TfrEditCondition.Destroy;
begin
   trvCondition.proc.Free;
   inherited Destroy;
end;

function TfrEditCondition.DownloadObject: string;
var
   andObj: TEBAndList;
   i: integer;
begin
   if trvCondition.proc.ChildCount = 1 then
      result := trvCondition.proc.children[0].Serialize
   else begin
      andObj := TEBAndList.Create(nil);
      try
         for i := 0 to trvCondition.proc.ChildCount - 1 do
            andObj.add(trvCondition.proc.children[i].Clone);
         result := andObj.Serialize;
      finally
         andObj.free;
      end;
   end;
end;

procedure TfrEditCondition.SetContext(const context, suffix: string);
begin
   trvCondition.EditorCategory := context;
   trvCondition.EditorSuffix := suffix;
end;

procedure TfrEditCondition.UploadObject(obj: TEbObject);
var
   andObj: TEBAndList;
   i: integer;
begin
   if obj.ClassType = TEBAndList then
   begin
      andObj := obj as TEBAndList;
      for i := 0 to andObj.ChildCount do
         trvCondition.proc.Add(TEBObject.Load(obj.children[i].Serialize));
   end else trvCondition.proc.Add(TEBObject.Load(obj.Serialize));
   trvCondition.proc := trvCondition.proc; //ugly hack, I know
end;

{ THolderProc }

function THolderProc.ExprNodeTree: boolean;
begin
   result := true;
end;

end.
