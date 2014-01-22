var App = require('./app');

App.Router.map(function() {
    this.route('lobby');
    this.resource('table', {path: '/table/:table_id'});
});

