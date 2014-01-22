var Draggable = require('../mixins/draggable');
var TileView = Ember.View.extend(Draggable, {
    tagName: 'div',
    classNameBindings: ['color', 'tile'],
    
    tile: function() {
        if(this.get('source').number == 0) {
            return 'joker';
        }
        else {
            return 'tile';
        }
    }.property('source')
});

module.exports = TileView;

