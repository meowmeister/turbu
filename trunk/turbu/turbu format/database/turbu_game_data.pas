unit turbu_game_data;

interface
uses
   classes,
   turbu_classes;

type
   TGameLayout = class(TRpgDatafile)
   private
      //add more to this when I have more to add
      FWidth: integer;
      FHeight: integer;
      FPWidth: integer;
      FPHeight: integer;
   protected
      class function keyChar: ansiChar; override;
   public
      constructor Load(savefile: TStream);
      procedure save(savefile: TStream); override;

      property width: integer read FWidth write FWidth;
      property height: integer read FHeight write FHeight;
      property physWidth: integer read FPWidth write FPWidth;
      property physHeight: integer read FPHeight write FPHeight;
   end;

implementation

{ TGameLayout }

class function TGameLayout.keyChar: ansiChar;
begin
   result := 'l';
end;

constructor TGameLayout.Load(savefile: TStream);
begin
   inherited Load(savefile);
   FWidth := savefile.readInt;
   FHeight := savefile.readInt;
   FPWidth := savefile.readInt;
   FPHeight := savefile.readInt;
end;

procedure TGameLayout.save(savefile: TStream);
begin
   inherited save(savefile);
   savefile.writeInt(FWidth);
   savefile.writeInt(FHeight);
   savefile.writeInt(FPWidth);
   savefile.writeInt(FPHeight);
end;

end.
