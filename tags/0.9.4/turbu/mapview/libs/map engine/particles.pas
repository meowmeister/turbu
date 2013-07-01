unit particles;
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
   AsphyreSprite;

type
 TParticleSprite = class(TAnimatedSprite)
 private
      FAccelX: Real;
      FAccelY: Real;
      FVelocityX: Real;
      FVelocityY: Real;
      FUpdateSpeed : Single;
      FDecay: Real;
      FLifeTime: Real;
 public
      constructor Create(const AParent: TSprite); override;
      procedure DoMove(const MoveCount: Single); override;
      property AccelX: Real read FAccelX write FAccelX;
      property AccelY: Real read FAccelY write FAccelY;
      property VelocityX: Real read FVelocityX write FVelocityX;
      property VelocityY: Real read FVelocityY write FVelocityY;
      property UpdateSpeed : Single read FUpdateSpeed write FUpdateSpeed;
      property Decay: Real read FDecay write FDecay;
      property LifeTime: Real read FLifeTime write FLifeTime;
 end;

implementation

end.
