unit turbu_2k_image_engine;

interface
uses
   sdl_sprite, sdl_canvas, sdl_ImageManager;

type
   TImageEngine = class(TSpriteEngine)
   private
      FParentEngine: TSpriteEngine;
   public
      constructor Create(const AParent: TSpriteEngine; const ACanvas: TSdlCanvas; images: TSdlImages);
      procedure Draw; override;
      property ParentEngine: TSpriteEngine read FParentEngine write FParentEngine;
   end;

implementation

{ TImageEngine }

constructor TImageEngine.Create(const AParent: TSpriteEngine; const ACanvas: TSdlCanvas; images: TSdlImages);
begin
   inherited Create(nil, ACanvas);
   FParentEngine := AParent;
   self.Images := images;
end;

procedure TImageEngine.Draw;
begin
   WorldX := FParentEngine.WorldX;
   WorldY := FParentEngine.WorldY;
   inherited Draw;
   self.Dead;
end;

end.
