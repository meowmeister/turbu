//---------------------------------------------------------------------------
// SDL_Sprite.pas                                        Modified: 5-Apr-2007
// SDL Sprite Engine
//---------------------------------------------------------------------------
// The contents of this file are subject to the Mozilla Public License
// Version 1.1 (the "License"); you may not use this file except in
// compliance with the License. You may obtain a copy of the License at
// http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS"
// basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
// License for the specific language governing rights and limitations
// under the License.
//---------------------------------------------------------------------------
unit sdl_sprite;

interface
uses
   Types, SysUtils, Generics.Collections,
   SDL_ImageManager, SDL_canvas, SG_defs;

type
   TImageType = (itSingleImage, itSpriteSheet, itRectSet);
   TAnimPlayMode = (pmForward, pmBackward, pmPingPong);

   ESpriteError = class(Exception);

   TSpriteEngine = class;
   TParentSprite = class;
   TSprite = class;

   TSpriteList = class(TObjectList<TSprite>);

   {***************************************************************************
   * Custom list optimized for fast access to TSprite objects.
   ***************************************************************************}
   TFastSpriteList = class(TObjectList<TSprite>)
   private
      FSprites: array of TSpriteList;
      FSorted: boolean;
   public
      constructor Create;
      destructor Destroy; override;
      function Add(ASprite: TSprite): Integer;
      function Remove(ASprite: TSprite): Integer;
      function IndexOf(const Value: TSprite): Integer;
   end;

   TSprite = class(TObject)
   private
      FDead: Boolean;
      FWidth: Integer;
      FHeight: Integer;
      FOrigin: TSgPoint;
      FName: string;
      FX, FY: Single;
      FZ: Cardinal;
      FZset: Boolean;
      FVisible: Boolean;
      FImageName: string;
      FImageIndex: Integer;
      FPatternIndex: Integer;
      FMoves: boolean;
      FTag: Integer;
      FImage: TSdlImage;
      FMirrorX, FMirrorY: Boolean;
      FRed: Byte;
      FGreen: Byte;
      FBlue: Byte;
      FAlpha: Byte;
      FAngle: Single;
      FDrawFX: TDrawFX;
      FScaleX, FScaleY: Single;
      FOffsetX, FOffsetY: Single;
      FImageType: TImageType;
      FPinned: boolean;
      FVisibleArea: TRect;

      function GetPatternWidth: Integer; inline;
      function GetPatternHeight: Integer; inline;
      function GetPatternCount: Integer; inline;
      function GetBoundsRect: TRect; inline;
   protected
      FEngine: TSpriteEngine;
      FParent: TParentSprite;

      function GetDrawRect: TRect; virtual;
      procedure SetDrawRect(const Value: TRect); virtual;
      procedure DoDraw; virtual;
      procedure DoMove(const MoveCount: Single); virtual;
      procedure SetImageName(const Value: string); virtual;
      procedure SetX(const Value: Single); virtual;
      procedure SetY(const Value: Single); virtual;
      procedure SetZ(const Value: Cardinal); virtual;
      procedure SetPatternIndex(const Value: Integer); virtual;
      function InVisibleRect: boolean; virtual;
   public
      constructor Create(const AParent: TParentSprite); virtual;
      destructor Destroy; override;
      procedure Assign(const Value: TSprite); virtual;
      procedure Move(const MoveCount: single); virtual;
      procedure SetPos(X, Y: Single); overload;
      procedure SetPos(X, Y: Single; Z: Integer); overload;
      procedure Draw; virtual;
      procedure Dead; virtual;

      property Visible: Boolean read FVisible write FVisible;
      property X: Single read FX write SetX;
      property Y: Single read FY write SetY;
      property Z: Cardinal read FZ write SetZ;
      property ImageName: string read FImageName write SetImageName;
      property Image: TSdlImage read FImage write FImage;
      property ImageIndex : Integer read FPatternIndex write SetPatternIndex;
      property PatternWidth: Integer read GetPatternWidth;
      property PatternHeight: Integer read GetPatternHeight;
      property Width: Integer read FWidth write FWidth;
      property Height:Integer read FHeight write FHeight ;
      property PatternCount: Integer read GetPatternCount;
      property Name: string read FName write FName;
      property Moves: Boolean read FMoves write FMoves;
      property Engine: TSpriteEngine read FEngine write FEngine;
      property Parent: TParentSprite read FParent;
      property Tag: Integer read FTag write FTag;
      property Red: Byte read FRed write FRed;
      property Green: Byte read FGreen write FGreen;
      property Blue: Byte read FBlue write FBlue;
      property Alpha: Byte read FAlpha write FAlpha;
      property DrawFx: TDrawFX read FDrawFX write FDrawFX;
      property Angle: Single read FAngle write FAngle;
      property ScaleX: Single read FScaleX write FScaleX;
      property ScaleY: Single read FScaleY write FScaleY;
      property OffsetX: Single read FOffsetX write FOffsetX;
      property OffsetY: Single read FOffsetY write FOffsetY;
      property MirrorX: Boolean read FMirrorX write FMirrorX;
      property MirrorY: Boolean read FMirrorY write FMirrorY;
      property ImageType: TImageType read FImageType write FImageType;
      property VisibleArea: TRect read FVisibleArea write FVisibleArea;
      property Pinned: boolean read FPinned write FPinned;
      property BoundsRect: TRect read GetBoundsRect;
      property DrawRect: TRect read GetDrawRect write SetDrawRect;
   end;

   TParentSprite = class(TSprite)
   private
      procedure UnDraw(sprite: TSprite);
   protected
      FList: TSpriteList;
      FSpriteList: TFastSpriteList;
      function GetCount: Integer;
      function GetItem(Index: Integer): TSprite;
      procedure AddDrawList(Sprite: TSprite); inline;
   public
      destructor Destroy; override;
      procedure Move(const movecount: single); override;
      procedure Clear;
      procedure Draw; override;
      procedure Add(Sprite: TSprite);
      procedure Remove(Sprite: TSprite);

      property Items[Index: Integer]: TSprite read GetItem; default;
      property Count: Integer read GetCount;
      property SpriteList: TFastSpriteList read FSpriteList;
   end;

   TAnimatedSprite = class(TParentSprite)
   private
      FDoAnimate: Boolean;
      FAnimLooped: Boolean;
      FAnimStart: Integer;
      FAnimCount: Integer;
      FAnimSpeed: Single;
      FAnimPos: Single;
      FAnimEnded: Boolean;
      FDoFlag1, FDoFlag2: Boolean;
      FAnimPlayMode: TAnimPlayMode;
      procedure SetAnimStart(Value: Integer);
   protected
      procedure DoMove(const MoveCount: Single); override;
   public
      constructor Create(const AParent: TParentSprite); override;
      procedure Assign(const Value: TSprite); override;
      procedure SetAnim(AniImageName: string; AniStart, AniCount: Integer; AniSpeed: Single; AniLooped, DoMirror, DoAnimate: Boolean; PlayMode: TAnimPlayMode=pmForward); overload; virtual;
      procedure SetAnim(AniImageName: string; AniStart, AniCount: Integer; AniSpeed: Single; AniLooped: Boolean;  PlayMode: TAnimPlayMode=pmForward); overload; virtual;
      procedure OnAnimStart; virtual;
      procedure OnAnimEnd; virtual;
      property AnimPos: Single read FAnimPos write FAnimPos;
      property AnimStart: Integer read FAnimStart write SetAnimStart;
      property AnimCount: Integer read FAnimCount write FAnimCount;
      property AnimSpeed: Single read FAnimSpeed write FAnimSpeed;
      property AnimLooped: Boolean read FAnimLooped write FAnimLooped;
      property DoAnimate: Boolean read FDoAnimate write FDoAnimate;
      property AnimEnded: Boolean read FAnimEnded;
      property AnimPlayMode: TAnimPlayMode read FAnimPlayMode write FAnimPlayMode;
   end;

   TAnimatedRectSprite = class(TAnimatedSprite)
   private
      FStartingPoint: TSgPoint;
      FDisplacement: TSgPoint;
      FSeriesLength: Integer;
      FAnimPos: Integer;
   protected
      procedure SetDrawRect(const Value: TRect); override;
      procedure DoMove(const MoveCount: Single); override;
   public
      constructor Create(parent: TParentSprite; region: TRect; displacement: TSgPoint; length: integer); reintroduce; overload;
      procedure Assign(const Value: TSprite); override;

      property StartingPoint: TSgPoint read FStartingPoint write FStartingPoint;
      property Displacement: TSgPoint read FDisplacement write FDisplacement;
      property SeriesLength: integer read FSeriesLength write FSeriesLength;
   end;

   TTiledAreaSprite = class(TAnimatedRectSprite)
   private
      FFillArea: TRect;
   protected
      procedure DoDraw; override;
   public
      constructor Create(parent: TParentSprite; region: TRect; displacement: TSgPoint; length: integer);
      property FillArea: TRect read FFillArea write FFillArea;
   end;

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
      constructor Create(const AParent: TParentSprite); override;
      procedure DoMove(const MoveCount: Single); override;
      property AccelX: Real read FAccelX write FAccelX;
      property AccelY: Real read FAccelY write FAccelY;
      property VelocityX: Real read FVelocityX write FVelocityX;
      property VelocityY: Real read FVelocityY write FVelocityY;
      property UpdateSpeed : Single read FUpdateSpeed write FUpdateSpeed;
      property Decay: Real read FDecay write FDecay;
      property LifeTime: Real read FLifeTime write FLifeTime;
   end;

   TSpriteEngine = class(TParentSprite)
   private
      FAllCount: Integer;
      FDeadList: TSpriteList;
      FDrawCount: Integer;
      FWorldX, FWorldY: Single;
      FVisibleWidth: Integer;
      FVisibleHeight: Integer;
      FImages: TSdlImages;
      FCanvas: TSdlCanvas;

   public
      constructor Create(const AParent: TSpriteEngine; const ACanvas: TSdlCanvas); reintroduce;
      destructor Destroy; override;
      procedure Draw; override;
      procedure Dead; reintroduce;
      property AllCount: Integer read FAllCount;
      property DrawCount: Integer read FDrawCount;
      property VisibleWidth:Integer read FVisibleWidth write FVisibleWidth;
      property VisibleHeight: Integer read FVisibleHeight write FVisibleHeight;
      property WorldX: Single read FWorldX write FWorldX;
      property WorldY: Single read FWorldY write FWorldY;
      property Images: TSdlImages read FImages write FImages;
      property Canvas: TSdlCanvas read FCanvas;
   end;

implementation
uses
   Generics.Defaults;

{  TSprite }

constructor TSprite.Create(const AParent: TParentSprite);
begin
   inherited Create;
   if assigned(AParent) then
   begin
      FParent := AParent;
      FParent.Add(Self);
      if FParent is TSpriteEngine then
         FEngine := TSpriteEngine(FParent)
      else
         FEngine := FParent.Engine;
      Inc(FEngine.FAllCount);
     end;
   FVisible := True;
end;

destructor TSprite.Destroy;
begin
   if assigned(FParent) then
   begin
      dec(FEngine.FAllCount);
      FParent.Remove(Self);
      FEngine.FDeadList.Remove(Self);
   end;
   inherited Destroy;
end;

procedure TSprite.Assign(const Value: TSprite);
begin
   FName := Value.Name;
   FImageName := Value.ImageName;
   FX  := Value.X;
   FY  := Value.Y;
   FZ  := Value.Z;
   FPatternIndex := Value.FPatternIndex;
   FVisible := Value.Visible;
   FTag := Value.Tag;
end;

procedure TSprite.DoMove;
begin
end;

procedure TSprite.Move(const MoveCount: Single);
begin
   if FMoves then
      DoMove(MoveCount);
end;

function TSprite.InVisibleRect: boolean;
begin
   result := (X > FEngine.WorldX - Width ) and
   (Y > FEngine.WorldY - Height)    and
   (X < FEngine.WorldX + FEngine.VisibleWidth)  and
   (Y < FEngine.WorldY + FEngine.VisibleHeight);
end;

procedure TSprite.Draw;
begin
   if FVisible then
   begin
      if FEngine <> nil then
      begin
         if self.InVisibleRect then
         begin
            DoDraw;
            Inc(FEngine.FDrawCount);
         end;
      end;
   end;
end;

function TSprite.GetPatternWidth: Integer;
begin
   if assigned(FImage) then
      Result := FImage.TextureSize.X
   else
      Result := 0
end;

function TSprite.GetPatternHeight: Integer;
begin
   if assigned(FImage) then
      Result := FImage.TextureSize.Y
   else
      Result := 0;
end;

function TSprite.GetPatternCount: Integer;
begin
   Result := FImage.count;
end;

procedure TSprite.DoDraw;
var
   followX, followY: single;
   topleft: TSgPoint;
begin
   if (not FVisible) or (FImage = nil) then
      Exit;
   if FEngine.Images[FImageIndex].name <> FImageName then
   begin
      setImageName(FImageName);
      if FImage = nil then
         Exit;
   end;

   if FPinned then
   begin
      followX := 0;
      followY := 0;
   end else begin
      followX := FEngine.WorldX;
      followY := FEngine.worldY;
   end;
   topleft := point(trunc(FX + FOffsetX - followX), trunc(FY + FOffsetY - followY));

   case FImageType of
      itSingleImage: FImage.Draw(topleft);
      itSpriteSheet: FImage.DrawSprite(topleft, FPatternIndex);
      itRectSet: FImage.DrawRect(topleft, Self.DrawRect);
   end;
end;

procedure TSprite.SetPos(X, Y: Single);
begin
   FX := X;
   FY := Y;
end;

procedure TSprite.SetPatternIndex(const Value: Integer);
begin
   FPatternIndex := Value;
end;

procedure TSprite.SetPos(X, Y: Single; Z: Integer);
begin
   FX := X;
   FY := Y;
   FZ := Z;
end;

procedure TSprite.SetImageName(const Value: string);
begin
   if FImageName <> Value then
   begin
      FImageName := Value;
      if assigned(FEngine) then
         FImage := FEngine.FImages.Image[FImageName];
      if assigned(FImage) then
      begin
         if FImageType <> itRectSet then
            if FImage.count > 1 then
               FImageType := itSpriteSheet
            else FImageType := itSingleImage;
         FImageIndex := FEngine.FImages.IndexOf(FImageName);
      end;
   end;
end;

procedure TSprite.SetX(const Value: Single);
begin
   Self.FX := Value;
end;

procedure TSprite.SetY(const Value: Single);
begin
   Self.FY := Value;
end;

procedure TSprite.SetZ(const Value: Cardinal);
begin
   if (FZ <> Value) or (not FZset) then
   begin
      FZ := Value;
      if assigned(FParent) then
      begin
         FParent.UnDraw(Self);
         FParent.AddDrawList(Self);
      end;
      FZset := true;
   end;
end;

procedure TSprite.Dead;
begin
   if assigned(FEngine) and (not FDead) then
   begin
      FDead := True;
      FEngine.FDeadList.Add(Self);
   end;
end;

function TSprite.GetBoundsRect: TRect;
begin
   Result := Bounds(Round(FX), Round(FY), Round(FX + Width), Round(FY + Height));
end;

function TSprite.GetDrawRect: TRect;
begin
   result := rect(FOrigin.X, FOrigin.Y, FWidth, FHeight);
end;

procedure TSprite.SetDrawRect(const Value: TRect);
begin
   FOrigin := Value.TopLeft;
   FWidth := Value.Right;
   FHeight := Value.Bottom;
   FImageType := itRectSet;
end;

{TParentSprite}

destructor TParentSprite.Destroy;
begin
   Self.Clear;
   FList.Free;
   FSpriteList.Free;
   inherited Destroy;
end;

procedure TParentSprite.Add(Sprite: TSprite);
begin
   if FList = nil then
      FList := TSpriteList.Create(false);
   FList.Add(Sprite);
   if Sprite.Z <> 0 then
   begin
      if FSpriteList = nil then
         FSpriteList := TFastSpriteList.Create;
      FSpriteList.Add(Sprite);
   end;
end;

procedure TParentSprite.Remove(Sprite: TSprite);
begin
   if assigned(FList) then
   begin
      FList.Remove(Sprite);
      if assigned(FSpriteList) then
      begin
         FSpriteList.Remove(Sprite);
         if FSpriteList.Count = 0 then
            freeAndNil(FSpriteList);
      end;
   end;
end;

procedure TParentSprite.UnDraw(sprite: TSprite);
begin
   if assigned(FSpriteList) then
      FSpriteList.Remove(sprite);
end;

procedure TParentSprite.AddDrawList(Sprite: TSprite);
begin
   if not assigned(FSpriteList) then
      FSpriteList := TFastSpriteList.Create;
   FSpriteList.Add(Sprite);
end;

procedure TParentSprite.Clear;
var
   i: integer;
begin
   i := count;
   while i > 0 do
   begin
      dec(i);
      Items[i].Free;
      i := count;
//      FList.Delete(i);
   end;
   if assigned(FSpriteList) then
      FSpriteList.Clear;
end;

procedure TParentSprite.Move(const movecount: single);
var
   i: integer;
begin
   inherited Move(movecount);
   for i := 0 to Count - 1 do
      Items[i].Move(MoveCount);
end;

function TParentSprite.GetCount: Integer;
begin
   if assigned(FList) then
      result := FList.Count
   else
      result := 0;
end;

function TParentSprite.GetItem(Index: Integer): TSprite;
begin
   if assigned(FList) then
      Result := TSprite(FList[Index])
   else
      raise ESpriteError.CreateFmt('Index of the list exceeds the range. (%d)', [Index]);
end;

procedure TParentSprite.Draw;
var
   i: integer;
begin
   inherited Draw;
   if self.visible and assigned(FSpriteList) then
   begin
      for i := 0 to FSpriteList.Count - 1 do
         TSprite(FSpriteList[i]).Draw;
   end;
end;

{  TAnimatedSprite  }

constructor TAnimatedSprite.Create(const AParent: TParentSprite);
begin
   inherited Create(AParent);
   FAnimLooped := True;
end;

procedure TAnimatedSprite.Assign(const Value: TSprite);
begin
   if (Value is TAnimatedSprite) then
   begin
      DoAnimate := TAnimatedSprite(Value).DoAnimate;
      AnimStart := TAnimatedSprite(Value).AnimStart;
      AnimCount := TAnimatedSprite(Value).AnimCount;
      AnimSpeed := TAnimatedSprite(Value).AnimSpeed;
      AnimLooped := TAnimatedSprite(Value).AnimLooped;
   end;
   inherited Assign(Value);
end;

procedure TAnimatedSprite.SetAnimStart(Value: Integer);
begin
   if FAnimStart <> Value then
   begin
      FAnimStart := Value;
      FAnimPos := Value;
   end;
end;

procedure TAnimatedSprite.DoMove(const MoveCount: Single);
begin
   if not FDoAnimate then
      Exit;

   case FAnimPlayMode of
      pmForward: //12345 12345  12345
      begin
         FAnimPos := FAnimPos + FAnimSpeed * MoveCount;
         if (FAnimPos >= FAnimStart + FAnimCount ) then
         begin
            if (Trunc(FAnimPos))= FAnimStart then
               OnAnimStart;
            if (Trunc(FAnimPos)) = FAnimStart + FAnimCount then
            begin
               FAnimEnded := True;
               OnAnimEnd;
            end;

            if FAnimLooped then
               FAnimPos := FAnimStart
            else begin
               FAnimPos := FAnimStart + FAnimCount-1 ;
               FDoAnimate := False;
            end;
         end;
      end;
      pmBackward: //54321 54321 54321
      begin
         FAnimPos := FAnimPos - FAnimSpeed * MoveCount;
         if (FAnimPos < FAnimStart) then
         begin
            if FAnimLooped then
               FAnimPos := FAnimStart + FAnimCount
            else FDoAnimate := False;
         end;
      end;
      pmPingPong: // 12345432123454321
      begin
         FAnimPos := FAnimPos + FAnimSpeed * MoveCount;
         if FAnimLooped then
         begin
            if (FAnimPos > FAnimStart + FAnimCount - 1) or (FAnimPos < FAnimStart) then
               FAnimSpeed := -FAnimSpeed
         end
         else begin
            if (FAnimPos > FAnimStart + FAnimCount) or (FAnimPos < FAnimStart) then
               FAnimSpeed := -FAnimSpeed;
            if (Trunc(FAnimPos)) = (FAnimStart + FAnimCount) then
               FDoFlag1 := True;
            if (Trunc(FAnimPos) = FAnimStart) and (FDoFlag1) then
               FDoFlag2 := True;
            if (FDoFlag1) and (FDoFlag2) then
            begin
               FDoAnimate := False;
               FDoFlag1 := False;
               FDoFlag2 := False;
            end;
         end;
      end;
   end; //end of case block
   FPatternIndex := Trunc(FAnimPos);
end;

procedure TAnimatedSprite.SetAnim(AniImageName: string; AniStart, AniCount: Integer; AniSpeed: Single; AniLooped, DoMirror, DoAnimate: Boolean;
                  PlayMode: TAnimPlayMode = pmForward);
begin
   ImageName := AniImageName;
   FAnimStart := AniStart;
   FAnimCount := AniCount;
   FAnimSpeed := AniSpeed;
   FAnimLooped:= AniLooped;
   MirrorX := DoMirror;
   FDoAnimate := DoAnimate;
   FAnimPlayMode := PlayMode;
   if (FPatternIndex < FAnimStart) or (FPatternIndex >= FAnimCount + FAnimStart) then
   begin
      FPatternIndex := FAnimStart mod FAnimCount;
      FAnimPos := FAnimStart;
   end;
end;

procedure TAnimatedSprite.SetAnim(AniImageName: string; AniStart, AniCount: Integer; AniSpeed: Single; AniLooped: Boolean;
                  PlayMode: TAnimPlayMode = pmForward);
begin
   ImageName := AniImageName;
   FAnimStart := AniStart;
   FAnimCount := AniCount;
   FAnimSpeed := AniSpeed;
   FAnimLooped:= AniLooped;
   FAnimPlayMode := PlayMode;
   if (FPatternIndex < FAnimStart) or (FPatternIndex >= FAnimCount + FAnimStart) then
   begin
      FPatternIndex := FAnimStart mod fanimcount;
      FAnimPos := FAnimStart;
   end;
end;

procedure TAnimatedSprite.OnAnimStart;
begin
end;

procedure TAnimatedSprite.OnAnimEnd;
begin
end;

{ TParticleSprite}
constructor TParticleSprite.Create(const AParent: TParentSprite);
begin
   inherited Create(AParent);
   FLifeTime := 1;
end;

procedure TParticleSprite.DoMove(const MoveCount: Single);
begin
   inherited DoMove(MoveCount);
   X := X + FVelocityX * UpdateSpeed;
   Y := Y + FVelocityY * UpdateSpeed;
   FVelocityX := FVelocityX + FAccelX * UpdateSpeed;
   FVelocityY := FVelocityY + FAccelY * UpdateSpeed;
   FLifeTime := FLifeTime - FDecay;
   if FLifeTime <= 0 then
      Self.Dead;
end;

{ TSpriteEngine }

constructor TSpriteEngine.Create(const AParent: TSpriteEngine; const ACanvas: TSdlCanvas);
begin
   inherited Create(AParent);
   FDeadList := TSpriteList.Create;
   FVisibleWidth := 800;
   FVisibleHeight := 600;
   FCanvas := ACanvas;
end;

destructor TSpriteEngine.Destroy;
begin
   FDeadList.Free;
   inherited Destroy;
end;

procedure TSpriteEngine.Dead;
begin
   while FDeadList.Count > 0 do
      TSprite(FDeadList[FDeadList.Count - 1]).Free;
end;

procedure TSpriteEngine.Draw;
begin
   FDrawCount := 0;
   inherited Draw;
end;

{ TSpriteComparer }

type
  TSpriteComparer = class(TInterfacedObject, IComparer<TSprite>)
    function Compare(const Left, Right: TSprite): Integer;
  end;

{$Q-}{$R-}
function TSpriteComparer.Compare(const Left, Right: TSprite): Integer;
begin
   result := Left.Z - Right.Z;
   if result = 0 then
      result := cardinal(left) - cardinal(right);
end;
{$Q+}{$R+}

{ TSpriteList }

constructor TFastSpriteList.Create;
var
   i: integer;
begin
   inherited Create(TSpriteComparer.Create, false);
   SetLength(FSprites, 11);
   for I := 0 to 10 do
      FSprites[i] := TSpriteList.Create(false);
end;

destructor TFastSpriteList.Destroy;
var
   i: integer;
begin
   for I := 0 to high(FSprites) do
      FSprites[i].Free;
   inherited;
end;

function TFastSpriteList.IndexOf(const Value: TSprite): Integer;
begin
   if not FSorted then
   begin
      self.Sort;
      FSorted := true;
   end;

   if not self.BinarySearch(value, result) then
      result := -1;
end;

function TFastSpriteList.Add(ASprite: TSprite): Integer;
var
   z: integer;
begin
   result := inherited Add(ASprite);

   z := ASprite.z;
   if z > high(FSprites) then
      SetLength(FSprites, z + 1);
   if FSprites[z] = nil then
      FSprites[z] := TSpriteList.Create(false);

   FSprites[z].Add(ASprite);
   FSorted := false;
end;

function TFastSpriteList.Remove(ASprite: TSprite): Integer;
begin
   Assert(assigned(FSprites[ASprite.Z]));

   FSprites[ASprite.Z].Remove(ASprite);
   Result := inherited Remove(ASprite);
end;

{ TAnimatedRectSprite }

procedure TAnimatedRectSprite.Assign(const Value: TSprite);
begin
   inherited Assign(value);
   if Value is TAnimatedRectSprite then
   begin
      FStartingPoint := TAnimatedRectSprite(value).FStartingPoint;
      FDisplacement := TAnimatedRectSprite(value).FDisplacement;
      FSeriesLength := TAnimatedRectSprite(value).FSeriesLength;
      FAnimPos := TAnimatedRectSprite(value).FAnimPos;
   end;
end;

constructor TAnimatedRectSprite.Create(parent: TParentSprite; region: TRect; displacement: TSgPoint; length: integer);
begin
   inherited Create(parent);
   self.DrawRect := region;
   FDisplacement := displacement;
   FSeriesLength := length - 1;
   FStartingPoint := region.TopLeft;
end;

procedure TAnimatedRectSprite.DoMove(const MoveCount: Single);
var
   dummy: integer;
begin
   if not FDoAnimate then
      Exit;

   case FAnimPlayMode of
      pmForward: //12345 12345 12345
      begin
         if FAnimPos = FSeriesLength then
         begin
            if FAnimLooped then
               FAnimPos := 0
            else FDoAnimate := false;
         end
         else inc(FAnimPos);
      end;
      pmBackward: //54321 54321 54321
      begin
         if FAnimPos = 0 then
         begin
            if FAnimLooped then
               FAnimPos := FSeriesLength
            else FDoAnimate := false;
         end
         else dec(FAnimPos);
      end;
      pmPingPong: // 12345432123454321
      begin
         dummy := FAnimPos + trunc(FAnimSpeed);
         if (dummy > FSeriesLength) or (dummy < 0) then
            FAnimSpeed := -FAnimSpeed;
         inc(FAnimPos, trunc(FAnimSpeed));
         if not FAnimLooped then
         begin
            if (FAnimPos) >= (FSeriesLength) then
               FDoFlag1 := True;
            if (FAnimPos <= 0) and (FDoFlag1) then
               FDoFlag2 := True;
            if (FDoFlag1) and (FDoFlag2) then
            begin
               FDoAnimate := False;
               FDoFlag1 := False;
               FDoFlag2 := False;
            end;
         end;
      end;
   end; //end of case block
   FOrigin := FStartingPoint + (FDisplacement * FAnimPos);
end;

procedure TAnimatedRectSprite.SetDrawRect(const Value: TRect);
begin
   FStartingPoint := Value.TopLeft;
   FAnimPos := 0;
   inherited SetDrawRect(Value);
end;

{ TTiledAreaSprite }

constructor TTiledAreaSprite.Create(parent: TParentSprite; region: TRect;
  displacement: TSgPoint; length: integer);
begin
   FFillArea := rect(0, 0, 0, 0);
   inherited Create(parent, region, displacement, length);
end;

procedure TTiledAreaSprite.DoDraw;
var
   drawpoint, endpoint: TSgPoint;
begin
   if (FFillArea.Bottom = 0) or (FFillArea.Right = 0) then
      inherited Draw
   else begin
      drawpoint := FFillArea.TopLeft;
      endpoint := drawpoint + FFillArea.BottomRight;
      repeat
         drawpoint.x := FFillArea.Left;
         repeat
            self.x := drawpoint.x;
            self.y := drawpoint.y;
            inherited Draw;
            inc(drawpoint.x, FWidth);
         until drawpoint.x >= endpoint.x;
         inc(drawpoint.y, FHeight);
      until drawpoint.y >= endpoint.y;
      self.x := FFillArea.Left;
      self.y := FFillArea.Top;
   end;
end;

end.
