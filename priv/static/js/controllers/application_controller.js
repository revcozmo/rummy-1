var ApplicationController = Ember.ObjectController.extend({
    content: {},
    
    actions: {
        dismissError: function(ref) {
            if(this.get('error') && this.get('error').ref == ref) {
                this.set('error', undefined);
            }
        }
    }
});

module.exports = ApplicationController;

