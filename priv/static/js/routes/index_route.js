var IndexRoute = Ember.Route.extend({
    redirect: function() {
        if(!this.controllerFor('application').get('nickname')) {
            this.transitionTo('index');
        }
    }
});

module.exports = IndexRoute;

