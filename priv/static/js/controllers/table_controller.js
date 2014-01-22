var TableController = Ember.ObjectController.extend({
    needs: ['application'],
    
    content: {sets: [], backupSets:[], tiles:[], backupTiles: [], myturn: false},
    
    actions: {
        ready: function() {
            App.Call(Bert.atom('start'));
        },
        
        quit: function() {
            var self = this;
            App.Call(Bert.atom('quit_room')).then(function() {
               self.set('users', []);
               self.transitionToRoute('lobby');
            });
        },
        
        peek: function() {
            App.Call(Bert.atom('peek'));
        },
        
        send: function() {
            var self = this;
            App.Call(Bert.tuple(Bert.atom('put'), this.get('sets'))).then(function() {
                self.set('backupTiles', self.get('tiles').map(function(r) { return r }));
            });
        },
        
        reset: function() {
            this.set('sets', this.get('backupSets').map(function(r) { return r.slice(0); }));
            this.set('tiles', this.get('backupTiles').map(function(r) { return r; }));
        },
        
        message: function(data) {
            var self = this;
            if(data.value && data.value == 'exit') {
                this.transitionToRoute('lobby');
            }
            else if(data.length == 2) {
                if(data[0].value == 'join') {
                    var user = data[1];
                    this.get('users').pushObject(user);
                    Ember.set(user, 'class', 'success');
                    Q.delay(2000).then(function() {
                        Ember.set(user, 'class', undefined);
                    });
                }
                else if(data[0].value == 'exit') {
                    var ref = new Date().getTime();
                    self.controllerFor('application').set('error', {message: data[1].value + ' HAS LEFT THE GAME!', ref: ref});
                    Q.delay(2500).then(function() {
                        self.controllerFor('application').send('dismissError', ref);
                    });
                    var user = this.get('users').find(function(item) { return item.username.value == data[1].value} );
                    Ember.set(user, 'class', 'danger');
                    Q.delay(2000).then(function() {
                        Ember.set(user, 'class', undefined);
                        var newUsers = this.get('users').filter(function(item) { return item.username.value != data[1].value});
                        self.set('users', newUsers);
                    });
                }
                else if(data[0].value == 'ready') {
                    var user = this.get('users').find(function(item) { return item.username.value == data[1].value} );
                    Ember.set(user, 'time', "READY");
                }
                else if(data[0].value == 'start') {
                    this.set('tiles', data[1]);
                    this.set('backupTiles', data[1].slice(0));
                    this.get('users').forEach(function(user) {
                        Ember.set(user, 'time', '');
                    });
                }
                else if(data[0].value == 'tile') {
                    this.set('tiles', this.get('backupTiles').map(function(r) { return r; }));
                    this.get('tiles').pushObject(data[1]);
                    this.set('sets', this.get('backupSets').slice(0));
                    this.set('backupTiles', this.get('tiles').slice(0));
                }
                else if(data[0].value == 'move') {
                    this.get('users').forEach(function(user) {
                        Ember.set(user, 'time', '');
                    });
                    var user = this.get('users').find(function(item) { return item.username.value == data[1].value} );
                    Ember.set(user, 'time', 60);
                    var oldUser = this.get('currentUser');
                    this.set('currentUser', user);
                    this.set('myturn', '' + user.value == this.get('controllers.application').get('nickname'));
                    this.set('timer', 60);
                    clearInterval(this.get('interval'));
                    this.set('interval', setInterval(function() {
                        self.set('timer', self.get('timer')-1);
                        Ember.set(user, 'time', self.get('timer'));
                    }, 1000));
                    if(oldUser && oldUser.username.value != this.get('controllers.application').get('nickname')) {
                        this.set('sets', this.get('backupSets').map(function(r) { return r.slice(0); }));
                        this.set('tiles', this.get('backupTiles').slice(0));
                    }
                }
                else if(data[0].value == 'set') {
                    this.set('sets', data[1]);
                    this.set('backupSets', this.get('sets').map(function(r) { return r.slice(0); }));
                }
            }
            else if(data.length == 3) {
                if(data[0].value == 'winner') {
                    console.log(data);
                    var ref = new Date().getTime();
                    self.get('controllers.application').set('error', {message: data[1].value + ' HAS WON THE GAME!', ref: ref});
                    this.set('users', data[2]);
                    Q.delay(2500).then(function() {
                        self.get('controllers.application').send('dismissError', ref);
                    });
                    this.set('tiles', []);
                    this.set('backupTiles', []);
                    this.set('sets', []);
                    this.set('backupSets', []);
                    clearInterval(this.get('interval'));
                }
            }
        }
    }
});

module.exports = TableController;

