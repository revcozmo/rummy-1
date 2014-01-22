var IndexController = Ember.ObjectController.extend({
    needs: ['application'],
    content: {},
    
    actions: {
        login: function() {
            var self = this;
        
            App.Call(Bert.tuple(Bert.atom('login'),
                                Bert.binary(this.get('nickname')),
                                Bert.binary(this.get('password')))).then(function() {
                self.get('controllers.application').set('nickname', self.get('nickname'));
                self.transitionToRoute('lobby');
            });
        },
        
        register: function() {
            var self = this;
            App.Call(Bert.tuple(Bert.atom('register'),
                                Bert.binary(this.get('nickname')),
                                Bert.binary(this.get('password')))).then(function() {
                                    self.get('controllers.application').set('nickname', self.get('nickname'));
                                    self.transitionToRoute('lobby');
                                })
        }
    }
});

module.exports = IndexController;

