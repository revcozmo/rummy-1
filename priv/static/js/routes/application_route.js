var ApplicationRoute = Ember.Route.extend({
    beforeModel: function() {
        var url = 'ws://'+window.location.hostname+':'+window.location.port+'/rummy';
        var conn = $.bert(url);
        var self = this;
        var applicationController = this.controllerFor('application');
        
        App.Connection = Ember.Deferred.create();
        App.Call = function(term) {
            return App.Connection.then(function(conn) {
                return conn.call(term).then(function(reply) {
                    if(reply[0].value == 'error') {
                        throw reply[1];
                    }
                    else if(reply[0].value == 'ok') {
                        return reply[1];
                    }
                    }).
                    catch(function(exception) {
                        var ref = new Date().getTime();
                        self.controllerFor('application').set('error', {message: exception.value, ref: ref});
                        Q.delay(2500).then(function() {
                            self.controllerFor('application').send('dismissError', ref);
                        });
                        throw exception; 
                    });
                }); 
        };
        
        App.Serialize = function(data) {
            var term = Bert.encode(data);
            var len = term.length;
            var byteArray = new Uint8Array(len);
            for (var i=0; i<len; ++i) {
                byteArray[i] = term.charCodeAt(i);
            }
            return Base64Binary.encode(byteArray.buffer);  
        };
        App.Deserialize = function(data) {
            var byteArray = Base64Binary.decode(data);
            var byteString = Bert.bytes_to_string(byteArray);
            var term = Bert.decode(byteString);
            return term;
        };
        
        conn.onopen = function() {
            App.Connection.resolve(conn);
        };
        
        conn.onmessage = function(data) {
          var namespace = data[0];
          if(data[0].value == 'lobby') {
              self.controllerFor('lobby').send('message', data[1]);
          }
          else if(data[0].value == 'room') {
              self.controllerFor('table').send('message', data[1]);
          }
        };
    
    }
});

module.exports = ApplicationRoute;

