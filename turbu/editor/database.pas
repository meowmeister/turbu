unit database;
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

interface

uses
   Classes, Controls, Forms, StdCtrls, ExtCtrls, ComCtrls,
   turbu_database, events, frame_commands, frame_class,
   dm_database, frame_items, DB, JvMemoryDataset;

type
   TfrmDatabase = class(TForm)
      btnOK: TButton;
      btnCancel: TButton;
      btnApply: TButton;
      btnHelp: TButton;
      tabPages: TPageControl;
      tshClass: TTabSheet;
      lblGlobalEvents: TLabel;
      lstEvents: TListBox;
      pnlEvents: TPanel;
      grpName: TGroupBox;
      txtName: TEdit;
      grpStartCondition: TGroupBox;
      cbxStartCondition: TComboBox;
      grpConditionSwitch: TGroupBox;
      chkHasSwitch: TCheckBox;
      txtCondSwitch: TEdit;
      btnCondSwitch: TButton;
      grpEventCommands: TGroupBox;
      txtEventScript: TMemo;
      tshHero: TTabSheet;
      btnCodeView: TButton;
      frmClass: TframeClass;
      tshItems: TTabSheet;
      frameItems1: TframeItems;
      procedure lstEventsClick(Sender: TObject);
      procedure lstEventsKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
      procedure btnCodeViewClick(Sender: TObject);
      procedure chkHasSwitchClick(Sender: TObject);
      procedure tshClassShow(Sender: TObject);
      procedure FormClose(Sender: TObject; var Action: TCloseAction);
      procedure FormShow(Sender: TObject);
      procedure applyChanges(Sender: TObject);
      procedure btnCancelClick(Sender: TObject);
   private
      { Private declarations }
      FId: word;
      FEvent: TEvent;
      FDatabase: TRpgDatabase;
      FEventPage: TEventPage;
      FViewingCode: boolean;
      FWasInit: boolean;
      FUploaded: boolean;
      procedure loadEvent(data: word);
      procedure initGlobalEvents;
   public
      { Public declarations }
      function init(const database: TRpgDatabase): boolean;
      procedure reset;
   end;

var
   frmDatabase: TfrmDatabase;

implementation
{$R *.dfm}

uses
   commons, sysUtils, DBClient,
   turbu_battle_engine;

procedure TfrmDatabase.applyChanges(Sender: TObject);
var
   iterator: TClientDataset;
begin
   for iterator in dmDatabase.datasets do
      iterator.MergeChangeLog;
end;

procedure TfrmDatabase.btnCancelClick(Sender: TObject);
var
   iterator: TClientDataSet;
begin
   for iterator in dmDatabase.datasets do
      iterator.CancelUpdates;
end;

procedure TfrmDatabase.btnCodeViewClick(Sender: TObject);
begin
   FViewingCode := not FViewingCode;
{   case FViewingCode of
      false: txtEventScript.Text := FEventPage.eventText;
      true: txtEventScript.Text := FEventPage.eventScript;
   end;}
  txtEventScript.Text := unicodeString(FEventPage.eventScript);
end;

procedure TfrmDatabase.chkHasSwitchClick(Sender: TObject);
begin
   txtCondSwitch.Enabled := chkHasSwitch.Checked;
   btnCondSwitch.Enabled := chkHasSwitch.Checked;
end;

procedure TfrmDatabase.FormShow(Sender: TObject);
var
   iterator: TClientDataset;
begin
   if not FUploaded then
   begin
      GDatabase.copyToDB(dm_database.dmDatabase);
      for iterator in dmDatabase.datasets do
      begin
         if assigned(iterator.CloneSource) and (iterator.RecordCount <> iterator.CloneSource.RecordCount) then
            iterator.CloneCursor(iterator.CloneSource, false, true);
         if iterator.Filtered then
         begin
            iterator.Filtered := false;
            iterator.Filtered := true;
         end;
      end;
      FUploaded := true;
   end;
   frmClass.onShow;
   frameItems1.onShow;
end;

procedure TfrmDatabase.FormClose(Sender: TObject; var Action: TCloseAction);
var
   iterator: TClientDataSet;
begin
   frmClass.onHide;
   for iterator in dmDatabase.datasets do
      iterator.LogChanges := false;
end;

function TfrmDatabase.init(const database: TRpgDatabase): boolean;
begin
   result := true;
   if FWasInit then
      Exit;

   FDatabase := database;
   initGlobalEvents;
   frmClass.initClasses;
   FWasInit := true;
end;

procedure TfrmDatabase.initGlobalEvents;
var
   I: Integer;
   j, k: integer;
   header: string;
   eventBlock: TEventBlock;
begin
   eventBlock := FDatabase.globalEventBlock as TEventBlock;
   if not assigned(eventBlock) then
      Exit;

   for I := 0 to eventBlock.len - 1 do
   begin
      header := '';
      j := i + 1;
      k := 0;
      repeat
         j := j div 10;
         inc(k)
      until j = 0;
      for j := 1 to 4 - k do
         header := header + '0';
      header := header + intToStr(i) + ': ';
      lstEvents.AddItem(header + unicodeString(eventBlock[i].name), eventBlock[i]);
   end;
   if eventBlock.len > 0 then
      loadEvent(0);
end;

procedure TfrmDatabase.loadEvent(data: word);
var
   dummy: word;
begin
   FId := data;
   FEvent := lstEvents.items.Objects[data] as TEvent;
   FEventPage := FEvent.page[0];
   txtName.Text := unicodeString(FEvent.name);
   FViewingCode := false;
   if lstEvents.itemIndex <> data then
      lstEvents.ItemIndex := data;
{   if FViewingCode then
      txtEventScript.Text := FEventPage.eventScript
   else txtEventScript.Text := FEventPage.eventText;}
   txtEventScript.Text := unicodeString(FEventPage.eventScript);

   case FEventPage.startCondition of
      automatic: cbxStartCondition.ItemIndex := 1;
      parallel: cbxStartCondition.ItemIndex := 2;
      on_call: cbxStartCondition.ItemIndex := 0;
      else assert(false);
   end;
   chkHasSwitch.Checked := FEventPage.conditionBlock.conditions[switch1];
   dummy := FEventPage.conditionBlock.switch1set;
   if chkHasSwitch.Checked then
      txtCondSwitch.Text := intToStr(dummy) + ': ' + FDatabase.switch.name[dummy]
   else
      txtCondSwitch.Text := '';
   //end if
   chkHasSwitchClick(self);
end;

procedure TfrmDatabase.lstEventsClick(Sender: TObject);
begin
   if lstEvents.ItemIndex <> FId then
      loadEvent(lstEvents.ItemIndex);
end;

procedure TfrmDatabase.lstEventsKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
   lstEventsClick(sender);
end;

procedure TfrmDatabase.reset;
begin
   FWasInit := false;
   lstEvents.Clear;
//   lstClasses.Clear;
end;

procedure TfrmDatabase.tshClassShow(Sender: TObject);
var
   needs3: boolean;
   i: integer;
begin
   if not assigned(GDatabase) then
      Exit;

   needs3 := false;
   for I := 0 to high(GDatabase.battleStyle) do
      needs3 := (needs3) or (GDatabase.battleStyle[i].view in NEED_BATTLE_SPRITES);
   if (needs3) and (frmClass.tabGraphics.Tabs.Count = 2) then
      frmClass.tabGraphics.Tabs.Add('Battle Sprite')
   else if (not needs3) and (frmClass.tabGraphics.Tabs.Count = 3) then
      frmClass.tabGraphics.Tabs.Delete(2);
end;

end.
