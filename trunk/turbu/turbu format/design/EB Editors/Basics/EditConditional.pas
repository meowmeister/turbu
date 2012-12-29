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

unit EditConditional;

interface

uses
   Controls, Forms, StdCtrls, Classes, ExtCtrls,
   EbEdit, fr_EditCondition;

type
   TfrmConditionEdit = class(TfrmEbEditBase, IContextualEditor)
      frEditCondition: TfrEditCondition;
   private
      procedure SetContext(const context, suffix: string);
   public
      class function Edit(var data: string; const context, suffix: string): boolean;
   end;

implementation
uses
   EventBuilder;

{$R *.dfm}

{ TfrmConditionEdit }

class function TfrmConditionEdit.Edit(var data: string; const context, suffix: string): boolean;
var
   form: TfrmConditionEdit;
   obj: TEBObject;
begin
   form := TfrmConditionEdit.Create(nil);
   try
      if data <> '' then
      begin
         obj := TEBObject.Load(data);
         try
            form.frEditCondition.UploadObject(obj);
         finally
            obj.Free;
         end;
      end;
      form.SetContext(context, suffix);
      result := form.ShowModal = mrOk;
      if result then
         data := form.frEditCondition.DownloadObject;
   finally
      form.free;
   end;
end;

procedure TfrmConditionEdit.SetContext(const context, suffix: string);
begin
   (frEditCondition as IContextualEditor).SetContext(context, suffix);
end;

end.
