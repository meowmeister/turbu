unit turbu_resists;
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
   classes, DB,
   turbu_defs, turbu_classes;

type
   TAttackLimitation = (al_none, al_paralyze, al_berserk, al_charm);

   TConditionTemplate = class(TRpgDatafile)
   private
      FOutOfBattle: boolean;
      FColor: byte;
      FPriority: byte;
      FAttackLimit: TAttackLimitation;
      FTag: T4IntArray;
   protected
      class function keyChar: ansiChar; override;
   public
      constructor Load(savefile: TStream);
      procedure save(savefile: TStream); override;
      procedure upload(db: TDataset); override;

      property outOfBattle: boolean read FOutOfBattle write FOutOfBattle;
      property color: byte read FColor write FColor;
      property priority: byte read FPriority write FPriority;
      property attackLimit: TAttackLimitation read FAttackLimit write FAttackLimit;
      property tag: T4IntArray read FTag write FTag;
   end;

   TAttributeTemplate = class(TRpgDatafile)
   private
      FRequiredForSkills: boolean;
      FStandard: smallint;
   protected
      class function keyChar: ansiChar; override;
   public
      constructor Load(savefile: TStream);
      procedure save(savefile: TStream); override;
      procedure upload(db: TDataset); override;

      property requiredForSkills: boolean read FRequiredForSkills write FRequiredForSkills;
      property standard: smallint read FStandard write FStandard;
   end;

implementation

{ TConditionTemplate }

class function TConditionTemplate.keyChar: ansiChar;
begin
   result := 'c';
end;

constructor TConditionTemplate.Load(savefile: TStream);
begin
   inherited Load(savefile);
   FOutOfBattle := savefile.readBool;
   FColor := savefile.readByte;
   FPriority := savefile.readByte;
   savefile.readBuffer(FAttackLimit, sizeof(TAttackLimitation));
   savefile.readBuffer(FTag, sizeof(T4IntArray));
   lassert(savefile.readChar = 'C');
end;

procedure TConditionTemplate.save(savefile: TStream);
begin
   inherited save(savefile);
   savefile.writeBool(FOutOfBattle);
   savefile.writeByte(FColor);
   savefile.writeByte(FPriority);
   savefile.WriteBuffer(FAttackLimit, sizeof(TAttackLimitation));
   savefile.WriteBuffer(FTag, sizeof(T4IntArray));
   savefile.writeChar('C');
end;

procedure TConditionTemplate.upload(db: TDataset);
var
  i: Integer;
begin
   inherited upload(db);
   db.FieldByName('outOfBattle').AsBoolean := FOutOfBattle;
   db.FieldByName('color').AsInteger := FColor;
   db.FieldByName('priority').AsInteger := FPriority;
   db.FieldByName('attackLimit').AsInteger := integer(FAttackLimit);
   for i := 1 to 4 do
      TArrayField(db.FieldByName('tag'))[i - 1] := i;
end;

{ TAttributeTemplate }

class function TAttributeTemplate.keyChar: ansiChar;
begin
   result := 'a';
end;

constructor TAttributeTemplate.Load(savefile: TStream);
begin
   inherited Load(savefile);
   FRequiredForSkills := savefile.readBool;
   FStandard := savefile.readWord;
   lassert(savefile.readChar = 'A');
end;

procedure TAttributeTemplate.save(savefile: TStream);
begin
   inherited save(savefile);
   savefile.writeBool(FRequiredForSkills);
   savefile.writeWord(FStandard);
   savefile.writeChar('A');
end;

procedure TAttributeTemplate.upload(db: TDataset);
begin
   inherited upload(db);
   db.FieldByName('requiredForSkills').AsBoolean := requiredForSkills;
   db.FieldByName('standard').AsInteger := standard;
end;

end.
