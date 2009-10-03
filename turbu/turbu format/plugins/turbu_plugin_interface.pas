unit turbu_plugin_interface;
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
   SysUtils, Generics.Collections,
   turbu_versioning,
   sdl_13;

type
   TEngineStyle = (et_map, et_battle, et_menu, et_minigame);

   TRpgPlugBase = class(TInterfacedObject)
   public
      constructor Create; virtual;
   end;

   TPlugClass = class of TRpgPlugBase;

   TRpgMetadata = class(TObject)
   private
      FName: string;
      FVersion: TVersion;
   public
      constructor Create(name: string; version: TVersion);
      property name: string read FName;
      property version: TVersion read FVersion;
   end;

   TEngineData = class(TObject)
   private
      FStyle: TEngineStyle;
      FEngine: TPlugClass;
   public
      constructor Create(style: TEngineStyle; engine: TPlugClass);
      destructor Destroy; override;
      property style: TEngineStyle read FStyle;
      property engine: TPlugClass read FEngine;
   end;

   TEngineDataList = class(TObjectList<TEngineData>);

   ITurbuPlugin = interface(IInterface)
   ['{EC78AE5D-1B52-4982-9AC7-19D95D67A26E}']
      function listPlugins: TEngineDataList;
   end;

   ERpgPlugin = class(Exception);

implementation

{ TEngineData }

constructor TEngineData.Create(style: TEngineStyle; engine: TPlugClass);
begin
   inherited Create;
   FStyle := style;
   FEngine := engine;
end;

destructor TEngineData.Destroy;
begin
   inherited Destroy;
end;

{ TRpgPlugBase }

constructor TRpgPlugBase.Create;
begin
   inherited Create;
end;

{ TRpgMetadata }

constructor TRpgMetadata.Create(name: string; version: TVersion);
begin
   inherited Create;
   FName := name;
   FVersion := version;
end;

end.
