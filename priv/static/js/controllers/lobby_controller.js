var LobbyController = Ember.ArrayController.extend({
    needs: ['application','table'],
    actions: {
        logout: function() {
            this.get('controllers.application').set('nickname', undefined);
            this.transitionToRoute('index');
        },
        
        create: function() {
            var self = this;
            App.Call(Bert.atom('create_room')).then(function(data) {
                var username = self.get('controllers.application').get('nickname');
                var user = self.get('users').find(function(item) { return item.username.value == username; });
                self.get('controllers.table').set('users',[user]);
                self.transitionToRoute('table', {id: data.value});
            });
        },
        
        join: function(room) {
            var self = this;
            App.Call(Bert.tuple(Bert.atom('join_room'), room.id)).then(function(members) {
                var users = members.map(function(user) { return user; });
                self.get('controllers.table').set('users',users);
                self.get('controllers.table').set('tiles', []);
                self.get('controllers.table').set('backupTiles', []);
                self.get('controllers.table').set('sets', []);
                self.get('controllers.table').set('backupSets', []);
                clearInterval(self.get('controllers.table').get('interval'));
                self.transitionToRoute('table', {id: room.id.value});
            });
        },
        
        message: function(data) {
            if(data.length == 2) {
                if(data[0].value == 'add_user') {
                    this.get('users').pushObject(data[1]);
                    Ember.set(data[1], 'class', 'success');
                    Q.delay(2000).then(function() {
                        Ember.set(data[1], 'class', undefined);
                    });
                }
                else if(data[0].value == 'delete_user') {
                    var self = this;
                    var user = this.get('users').find(function(item) { return item.username.value == data[1].username.value; });
                    Ember.set(user, 'class', 'danger')
                    Q.delay(2000).then(function() {
                        Ember.set(user, 'class', undefined);
                        var newUsers = self.get('users').filter(function(item) { return item.username.value != data[1].username.value; });
                        self.set('users', newUsers);
                    });
                }
                else if(data[0].value == 'delete_room') {
                    var self = this;
                    var room = this.get('rooms').find(function(item) { return item.id.value == data[1].value; });
                    Ember.set(room, 'class', 'danger');
                    Q.delay(2000).then(function() {
                        Ember.set(room, 'class', undefined);
                        var newRooms = self.get('rooms').filter(function(item) { return item.id.value != data[1].value; });
                        self.set('rooms', newRooms);
                    });
                }
                else if(data[0].value == 'users') {
                    console.log(data[1]);
                    this.set('users', data[1]);
                }
            }
            else if(data.length == 3) {
                if(data[0].value == 'create_room') {
                    var room = {id: data[1], members: data[2]};
                    Ember.set(room, 'class', 'success');
                    this.get('rooms').pushObject(room);
                    Q.delay(2000).then(function() {
                        Ember.set(room, 'class', undefined);
                    })
                }
                else if(data[0].value == 'join') {
                    var room = this.get('rooms').find(function(item) { return item.id.value == data[1].value; });
                    Ember.get(room, 'members').pushObject(data[2]);
                }
                else if(data[0].value == 'exit') {
                    var room = this.get('rooms').find(function(item) { return item.id.value == data[1].value; });
                    var newMembers = Ember.get(room, 'members').filter(function(m) { return m.value != data[2].value});
                    Ember.set(room, 'members', newMembers);
                }
            }
        }
    }
});

module.exports = LobbyController;

