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

unit EditNull_RM;

interface
uses
   EBEdit, EditNull, EB_System, EB_Maps;
type
   [EditorCategory('Basics', 'Game Over')]
   [EditorContext('RM2K')]
   TFrmGameOver = class(TfrmEBEditNull<TEBGameOver>);

   [EditorCategory('Basics', 'Title Screen')]
   [EditorContext('RM2K')]
   TFrmTitleScreen = class(TfrmEBEditNull<TEBTitleScreen>);

   [EditorCategory('Map', 'Ride Vehicle')]
   [EditorContext('RM2K')]
   TFrmRideVehicle = class(TfrmEBEditNull<TEBRideVehicle>);

   [EditorCategory('Map', 'End Flash Screen')]
   [EditorContext('RM2K')]
   TFrmEndFlashScreen = class(TfrmEBEditNull<TEBEndFlash>);

   [EditorCategory('Map', 'End Shake Screen')]
   [EditorContext('RM2K')]
   TFrmEndShakeScreen = class(TfrmEBEditNull<TEBEndShake>);

implementation

initialization
   RegisterEbEditor(TEBGameOver, TFrmGameOver);
   RegisterEbEditor(TEBTitleScreen, TFrmTitleScreen);
   RegisterEbEditor(TEBRideVehicle, TFrmRideVehicle);
   RegisterEBEditor(TEBEndShake, TFrmEndShakeScreen);
finalization
   UnRegisterEbEditor(TEBGameOver);
   UnRegisterEbEditor(TEBTitleScreen);
   UnRegisterEbEditor(TEBRideVehicle);
   UnRegisterEBEditor(TEBEndFlash);
   UnRegisterEBEditor(TEBEndShake);
end.
