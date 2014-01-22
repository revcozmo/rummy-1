var IndexRoute = require('./index_route');
var LobbyRoute = IndexRoute.extend({
    setupController: function(controller) {
        App.Call(Bert.atom('get_rooms')).then(function(reply) {
            controller.set('rooms', reply);
        });
        App.Call(Bert.atom('get_users')).then(function(reply) {
            controller.set('users', reply);
        });
    }
});

module.exports = LobbyRoute;

