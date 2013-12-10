$(document).ready(function() {
    var url = 'ws://'+window.location.hostname+':'+window.location.port+'/rummy';
    var conn = $.bert(url);
    var lobbyUsers = [];
    var call = function(term) {
        return conn.call(term).then(function(reply) {
            if(reply[0].value == 'error') {
                throw reply[1];
            }
            else if(reply[0].value == 'ok') {
                return reply[1];
            }
        }).
        catch(function(exception) {
            $("#alert").text(exception.value);
            $("#alert").show();
            Q.delay(2000).then(function() {
                $("#alert").hide();
            })
           throw exception; 
        });
    }
    
    var lobbyMessage = function(data) {
      if(data.length == 2) {
          if(data[0].value == 'add_user') {
              lobbyUsers.push(data[1]);
              $("#players-list").trigger('render', [lobbyUsers]);
              var selector = "#players-list tr."+data[1].value;
                            console.log($(selector));
              $(selector).addClass("success");
              Q.delay(2000).then(function() {
                  $(selector).removeClass("success"); 
              });
          }
          else if(data[0].value == 'delete_user') {
              var selector = "#players-list tr."+data[1].value;
              $(selector).addClass("danger");
              Q.delay(2000).then(function() {
                  $(selector).removeClass("danger"); 
                  lobbyUsers = lobbyUsers.filter(function(u) { return u.value != data[1].value; });
                  $("#players-list").trigger('render', [lobbyUsers]);
              });  
          }
      }
    };
    
    conn.onmessage = function(data) {
      var namespace = data[0];
      if(data[0].value == 'lobby') {
          lobbyMessage(data[1]);
      }
    };
    
    // pretty basic templating "engine" :)
    var loadTemplate = function(name) {
        var deferred = Q.defer();
        $.get("templates/"+name+".html", function(data) {
            var template = '<div class="alert alert-danger" id="alert"></div>' + data;
            $(".container").html(template);
            deferred.resolve(true);
        });
        return deferred.promise;
    };
    
    // lobby initialization function
    var loadLobby = function() {
        return loadTemplate('lobby')
        .then(function() {
            $("#players-list").bind('render', function(event, users) {
                var rows = users.map(function(u) {
                        return '<tr class="'+u.value+'"><td>' + u.value + '</td></tr>'
                    }).join('');
                $(this).html(rows);
            })
        })
        .then(function() {
            return call(Bert.atom('get_users'));
        })
        .then(function(users) {
            lobbyUsers = users;
            $("#players-list").trigger('render', [users]);
        })
        .then(function(val) {
            console.log(val);
        });
    };
    
    // load sign-in format template
    loadTemplate('login').then(function() {
        $("#form-signin").submit(function(event) {
            var join = Bert.atom('join');
            var user = Bert.binary($("#form-signin-nickname").val());
            call(Bert.tuple(join, user))
            .then(function(reply) {
                return loadLobby();
            });
            event.preventDefault();
        });
    });

});