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

unit turbu_terrain;

interface
uses
   DB, RTTI,
   turbu_classes, turbu_sounds, turbu_defs, turbu_serialization;

type
   VehiclePassUploadAttribute = class(TDBUploadAttribute)
   protected
      procedure upload(db: TDataset; field: TRttiField; instance: TObject); override;
      procedure download(db: TDataset; field: TRttiField; instance: TObject); override;
   end;

   TRpgTerrain = class(TRpgDatafile)
   protected
      FDamage: integer;
      FEncounterMultiplier: integer;
      FBattleBg: string;
      [VehiclePassUpload]
      FVehiclePass: TBoolArray;
      FAirshipLanding: boolean;
      FConcealment: TConcealmentFactor;
      FSoundEffect: TRpgSound;
      FFrame: string;
   public
      destructor Destroy; override;
      property damage: integer read FDamage;
      property encounterMultiplier: integer read FEncounterMultiplier;
      property battleBg: string read FBattleBG;
      property vehiclePass: TBoolArray read FVehiclePass;
      property airshipLanding: boolean read FAirshipLanding;
      property concealment: TConcealmentFactor read FConcealment;
      property soundEffect: TRpgSound read FSoundEffect;
      property frame: string read FFrame;
   end;

implementation
uses
   Classes;

{ VehiclePassUploadAttribute }

procedure VehiclePassUploadAttribute.download(db: TDataset; field: TRttiField;
  instance: TObject);
var
   list: TBoolArray;
   stream: TBytesStream;
   i: integer;
begin
   stream := TBytesStream.Create((db.FieldByName('VehiclePass') as TBlobField).AsBytes);
   try
      list := (instance as TRpgTerrain).FVehiclePass;
      setLength(list, stream.Size);
      if length(list) > 0 then
         for i := 0 to High(list) do
            stream.Read(list[i], 1);
   finally
      stream.Free;
   end;
end;

procedure VehiclePassUploadAttribute.upload(db: TDataset; field: TRttiField;
  instance: TObject);
var
   list: TBoolArray;
   stream: TBytesStream;
   i: integer;
begin
   stream := TBytesStream.Create;
   try
      list := (instance as TRpgTerrain).FVehiclePass;
      if length(list) > 0 then
         for i := 0 to High(list) do
            stream.Write(list[i], 1);
      (db.FieldByName('VehiclePass') as TBlobField).AsBytes := stream.Bytes;
   finally
      stream.Free;
   end;
end;

{ TRpgTerrain }

destructor TRpgTerrain.Destroy;
begin
   FSoundEffect.Free;
   inherited;
end;

end.
