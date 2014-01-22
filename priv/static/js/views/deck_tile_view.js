var TileView = require('./tile_view');
var Droppable = require('../mixins/droppable');
var DeckTileView = TileView.extend(Droppable, {
    drop: function(ev) {
        var tiles = this.get('controller').get('tiles');
        var source = tiles.find(function(item) { return JSON.stringify(item) == ev.dataTransfer.getData('source'); });
        var destination = this.get('source');
        
        var from = tiles.indexOf(source);
        var to = tiles.indexOf(destination);
        
        tiles.replace(from, 1);
        tiles.replace(to, 0, [source]);
        this.get('controller').set('backupTiles',tiles.slice(0));
        
        return this._super(event);
    }
});

module.exports = DeckTileView;

