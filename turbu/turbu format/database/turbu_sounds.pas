unit turbu_sounds;
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
   classes,
   turbu_classes;

type
   TSoundTemplate = class abstract(TRpgDatafile)
   private
      FFadeIn: integer;
      FTempo: integer;
      FVolume: integer;
      FBalance: integer;

      procedure setFilename(value: string); virtual; abstract;
   protected
      class function keyChar: ansiChar; override;
   public
      constructor Load(savefile: TStream); override;
      procedure save(savefile: TStream); override;
      constructor assign(const source: TSoundTemplate);

      property filename: string read FName write setFilename;
      property fadeIn: integer read FFadeIn write FFadeIn;
      property tempo: integer read FTempo write FTempo;
      property balance: integer read FBalance write FBalance;
      property volume: integer read FVolume write FVolume;
   end;

   TRpgSound = class(TSoundTemplate)
   private
      procedure setFilename(value: string); override;
   end;

   TRpgMusic = class(TSoundTemplate)
   private
      procedure setFilename(value: string); override;
   end;

implementation
uses
   sysUtils;

{ TSoundTemplate }

constructor TSoundTemplate.assign(const source: TSoundTemplate);
begin
   inherited create;
   FName := source.FName;
   FFadein := source.FFadein;
   FTempo := source.FTempo;
   FBalance := source.FBalance;
end;

class function TSoundTemplate.keyChar: ansiChar;
begin
   result := 'o';
end;

constructor TSoundTemplate.Load(savefile: TStream);
begin
   inherited Load(savefile);
   FFadeIn := savefile.readInt;
   FTempo := savefile.readInt;
   FVolume := savefile.readInt;
   FBalance := savefile.readInt;
   readEnd(savefile);
end;

procedure TSoundTemplate.save(savefile: TStream);
begin
   inherited save(savefile);
   savefile.writeInt(FFadeIn);
   savefile.writeInt(FTempo);
   savefile.writeInt(FVolume);
   savefile.writeInt(FBalance);
   writeEnd(savefile);
end;

{ TRpgSound }

procedure TRpgSound.setFilename(value: string);
begin
   FName := value;
   //fix this later
end;

{ TRpgMusic }

procedure TRpgMusic.setFilename(value: string);
begin
   FName := value;
   //fix this later
end;

end.
