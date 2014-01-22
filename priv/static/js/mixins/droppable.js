var Droppable = Ember.Mixin.create({
    dragEnter: function(event) {
        event.preventDefault();
    },
    
    dragOver: function(event) {
        event.preventDefault();
    },
    
    drop: function(event) {
        event.preventDefault();
        return false;
    }
});

module.exports = Droppable;

