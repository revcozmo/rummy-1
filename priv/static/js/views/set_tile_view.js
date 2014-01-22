var TileView = require('./tile_view');
var Droppable = require('../mixins/droppable');
var SetTileView = TileView.extend(Droppable, {
    drop: function(ev) {
        var tiles = this.get('controller').get('tiles');
        var sets = this.get('controller').get('sets');
        var source;
        var destination = this.get('source');
        var from;
        var destinationSet = this.get('fromset');
        var to = destinationSet.indexOf(destination);
        
        if(ev.dataTransfer.getData("set")) {
            var sets = this.get('controller').get('sets');
            var set = sets.find(function(item) { return JSON.stringify(item) == ev.dataTransfer.getData('set'); });
            source = set.find(function(item) { return JSON.stringify(item) == ev.dataTransfer.getData('source'); });
            from = set.indexOf(source);
            set.replace(from, 1);
            destinationSet.replace(to+1, 0, [source]);
            if(set.length == 0) {
                sets.replace(sets.indexOf(set), 1);
            }
        }
        else {
            source = tiles.find(function(item) { return JSON.stringify(item) == ev.dataTransfer.getData('source'); });
            from = tiles.indexOf(source);
            tiles.replace(from, 1);
            destinationSet.replace(to+1, 0, [source]);
        }
        return this._super(event);
    }
});

module.exports = SetTileView;

