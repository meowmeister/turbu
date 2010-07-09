unit turbu_2k_sprite_list;

interface
uses
   DeHL.Collections.DistinctMultimap, SG_Defs, turbu_map_sprites;

type TSpriteLocations = class(TDistinctMultiMap<TSgPoint, TMapSprite>);

implementation

initialization
finalization
   TSpriteLocations.CleanupTypes;
end.
