unit turbu_2k_sprite_list;

interface
uses
   turbu_multimaps, SG_Defs, turbu_map_sprites;

type
   TSpriteLocations = class(TMultiMap<TSgPoint, TMapSprite>);

implementation

{initialization
finalization
   TSpriteLocations.CleanupTypes; }

end.
