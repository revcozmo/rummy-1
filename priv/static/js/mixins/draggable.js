var Draggable = Ember.Mixin.create({
    attributeBindings: 'draggable',
    
    draggable: 'true',
    
    dragStart: function(ev) {
        ev.dataTransfer.setData("source", JSON.stringify(this.get('source')));
        if(this.get('fromset')) {
            ev.dataTransfer.setData('set', JSON.stringify(this.get('fromset')));
        }
    }
});

module.exports = Draggable;

