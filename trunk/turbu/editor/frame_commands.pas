unit frame_commands;
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
   Windows, SysUtils, Classes, Controls, StdCtrls, Forms, JvSpin, Mask, JvExMask,
   DB,
   dm_database, turbu_classes, DBCtrls;

type
   TframeHeroCommands = class(TFrame)
      lblNumber: TLabel;
      spnCount: TJvSpinEdit;
      DataSource: TDataSource;
      dsCommands: TDataSource;
      cbxCommand1: TDBLookupComboBox;
      procedure spnCountChange(Sender: TObject);
   private
      FCommandsAvailable: TStringList;
      FCommandSet: TRpgObjectList<TDBLookupComboBox>;
      function getSize: byte;
      procedure setSize(Value: byte);
      procedure addBox;
      procedure deleteBox;
      function getBox(x: byte): TDBLookupComboBox;
      function getDataset: TDataSet; inline;
      procedure setDataset(const Value: TDataSet);
   public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;

      property size: byte read getSize write setSize;
      property commands[x: byte]: TDBLookupComboBox read getBox;
      property dataSet: TDataSet read getDataset write setDataset;
   end;

implementation

{$R *.dfm}

const
   BAR_WIDTH = 129;

{ TframeHeroCommands }

constructor TframeHeroCommands.Create(AOwner: TComponent);
begin
   inherited Create(AOwner);
   FCommandSet := TRpgObjectList<TDBLookupComboBox>.Create(false);
   FCommandSet.add(cbxCommand1);
end;

destructor TframeHeroCommands.Destroy;
begin
   setSize(1);
   FCommandSet.Free;
   FCommandsAvailable.Free;
   inherited Destroy;
end;

procedure TframeHeroCommands.addBox;
var
   last, new: TDBLookupComboBox;
begin
   last := FCommandSet.Last;
   new := TDBLookupComboBox.Create(nil);
   new.Parent := self;
   new.Top := last.Top + last.Height + 8;
   new.Left := last.Left;
   new.Height := last.Height;
   new.Width := last.Width;
   new.DataSource := DataSource;
   new.DataField := 'command[' + intToStr(FCommandSet.Count) + ']';
   new.ListSource := dsCommands;
   new.ListField := 'name';
   new.KeyField := 'id';
   FCommandSet.add(new);
end;

procedure TframeHeroCommands.deleteBox;
begin
   FCommandSet.OwnsObjects := true;
   FCommandSet.Delete(FCommandSet.High);
   FCommandSet.OwnsObjects := false;
end;

function TframeHeroCommands.getBox(x: byte): TDBLookupComboBox;
begin
   assert(x in [1..FCommandSet.Count]);
   result := FCommandSet[x - 1] as TDBLookupComboBox;
end;

procedure TframeHeroCommands.setDataset(const Value: TDataSet);
begin
   DataSource.DataSet := value;
end;

function TframeHeroCommands.getDataset: TDataSet;
begin
   result := DataSource.DataSet;
end;

function TframeHeroCommands.getSize: byte;
begin
   result := FCommandSet.Count;
end;

procedure TframeHeroCommands.setSize(Value: byte);
var
   i: integer;
   zf: boolean; //zero flag
begin
//   assert(value > 0);
   zf := value = 0;
   if zf then
      inc(value);
   if value = self.size then
      Exit;
   if value > self.size then
      for I := size + 1 to value do
         self.addBox
   else begin
      for I := self.size downto value + 1 do
         self.deleteBox;
   end;
   cbxCommand1.visible := not zf;
   spnCount.Value := value;
   Application.ProcessMessages;
end;

procedure TframeHeroCommands.spnCountChange(Sender: TObject);
begin
   self.setSize(trunc(spnCount.Value));
end;

end.
