var Droppable = require('../mixins/droppable');
var NewTileView = Ember.View.extend(Droppable, {
    tagName: 'div',
    classNames: ['set', 'set-new'],
    
    drop: function(ev) {
        var sets = this.get('controller').get('sets');
        var tiles = this.get('controller').get('tiles');
        var source;
        var from;
        
        if(ev.dataTransfer.getData("set")) {
            var set = sets.find(function(item) { return JSON.stringify(item) == ev.dataTransfer.getData('set'); });
            source = set.find(function(item) { return JSON.stringify(item) == ev.dataTransfer.getData('source'); });
            from = set.indexOf(source);
            set.replace(from, 1);
            sets.replace(0, 0, [[source]]);
            if(set.length == 0) {
                sets.replace(sets.indexOf(set), 1);
            }
        }
        else {
            source = tiles.find(function(item) { return JSON.stringify(item) == ev.dataTransfer.getData('source'); });
            from = tiles.indexOf(source);
            tiles.replace(from, 1);
            sets.replace(0, 0, [[source]]);   
        }
        return this._super(event);
    }
});

module.exports = NewTileView;

