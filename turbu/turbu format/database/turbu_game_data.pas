unit turbu_game_data;

interface
uses
   classes,
   turbu_defs, turbu_classes;

type
   TTransitionArray = array[TTransitionTypes] of byte;
   TWord7Array = array[1..7] of word;

   TGameLayout = class(TRpgDatafile)
   protected
      FWidth: integer;
      FHeight: integer;
      FPWidth: integer;
      FPHeight: integer;

      FTitleScreen: string;
      FGameOverScreen: string;
      FSysGraphic: string;
      FBattleSysGraphic: string;
      FEditorBattleBG: string;
      FWallpaperStretch: boolean;
      FWhichFont: byte;
      FStartingHeroes: word;
      FStartingHero: T4IntArray;
      FTransition: TTransitionArray;
      FCommands: TWord7Array;
      FUsesFrame: boolean;
      FFrame: string;
      FReverseGraphics: boolean;
      function getStartingHero(which: word): integer;
      function getTransition(which: TTransitionTypes): byte;
  private
    function GetComand(which: word): word;
   protected
      class function keyChar: ansiChar; override;
   public
      property width: integer read FWidth;
      property height: integer read FHeight write FHeight;
      property physWidth: integer read FPWidth;
      property physHeight: integer read FPHeight;

      property systemGraphic: string read FSysGraphic;
      property wallpaperStretch: boolean read FWallpaperStretch;
      property battleSysGraphic: string read FBattleSysGraphic;
      property editorBattleBG: string read FEditorBattleBG;

      property transition[which: TTransitionTypes]: byte read getTransition;
      property startingHeroes: word read FStartingHeroes;
      property startingHero[which: word]: integer read getStartingHero;
      property titleScreen: string read FTitleScreen;
      property gameOverScreen: string read FGameOverScreen;
      property defaultCommands[which: word]: word read GetComand;
      property usesFrame: boolean read FUsesFrame;
      property frame: string read FFrame;
      property reverseGraphics: boolean read FReverseGraphics;
   end;


implementation

{ TGameLayout }

class function TGameLayout.keyChar: ansiChar;
begin
   result := 'l';
end;

function TGameLayout.GetComand(which: word): word;
begin
   assert(which in [1..7]);
   result := FCommands[which];
end;

function TGameLayout.getStartingHero(which: word): integer;
begin
   assert(which in [1..4]);
   result := FStartingHero[which];
end;

function TGameLayout.getTransition(which: TTransitionTypes): byte;
begin
   result := FTransition[which];
end;

end.
