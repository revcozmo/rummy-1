$(document).ready(function() {
    var url = 'ws://'+window.location.hostname+':'+window.location.port+'/rummy';
    var conn = $.bert(url);
    var lobbyUsers = [];
    var roomUsers = [];
    var rooms = [];
    var username;
    var room;
    
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
        console.log(data);
      if(data.length == 2) {
          if(data[0].value == 'add_user') {
              lobbyUsers.push(data[1]);
              $("#players-list").trigger('render', [lobbyUsers]);
              var selector = "#players-list tr."+data[1].value;
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
          else if(data[0].value == 'delete_room') {
              var selector = "#rooms-list tr."+data[1].value;
              $(selector).addClass("danger");
              Q.delay(2000).then(function() {
                  $(selector).removeClass("danger"); 
                  rooms = rooms.filter(function(r) { return r.id.value != data[1].value; });
                  $("#rooms-list").trigger('render', [rooms]);
              });
          }
      }
      else if(data.length == 3) {
          if(data[0].value == 'create_room') {
              rooms.push({id: data[1], members: data[2]});
              $("#rooms-list").trigger('render', [rooms]);
              var selector = "#rooms-list tr."+data[1].value;
              $(selector).addClass("success");
              Q.delay(2000).then(function() {
                  $(selector).removeClass("success"); 
              });
          }
          else if(data[0].value == 'join') {
              rooms = rooms.map(function(r) {
                  if(r.id.value == data[1].value) {
                      r.members.push(data[2]);
                  }
                  return r;
              });
              $("#rooms-list").trigger('render', [rooms]);
              
          }
          else if(data[0].value == 'exit') {
              rooms = rooms.map(function(r) {
                  if(r.id.value == data[1].value) {
                      r.members = r.members.filter(function(m) { return m.value != data[2].value });
                  }
                  return r;
              });
              $("#rooms-list").trigger('render', [rooms]);
              console.log(rooms);
          }
      }
    };
    
    var roomMessage = function(data) {
        console.log(data);
        if(data.length == 2) {
            if(data[0].value == 'join') {
                roomUsers.push(data[1]);
                $("#room-players-list").trigger('render', [roomUsers]);
                var selector = "#room-players-list tr."+data[1].value;
                  $(selector).addClass("success");
                  Q.delay(2000).then(function() {
                      $(selector).removeClass("success"); 
                  });
            }
            else if(data[0].value == 'exit') {
                var selector = "#room-players-list tr."+data[1].value;
                $(selector).addClass("danger");
                Q.delay(2000).then(function() {
                    $(selector).removeClass("danger"); 
                    roomUsers = roomUsers.filter(function(u) { return u.value != data[1].value; });
                    $("#room-players-list").trigger('render', [roomUsers]);
                });
            }
        }
    };
    
    conn.onmessage = function(data) {
      var namespace = data[0];
      if(data[0].value == 'lobby') {
          lobbyMessage(data[1]);
      }
      else if(data[0].value == 'room') {
          roomMessage(data[1]);
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
    
    var loadLogin;
    
    // lobby initialization function
    var loadLobby = function() {
        return loadTemplate('lobby')
        .then(function() {
            $("#players-list").bind('render', function(event, users) {
                var rows = users.map(function(u) {
                        return '<tr class="'+u.value+'"><td>' + u.value + '</td></tr>'
                    }).join('');
                $(this).html(rows);
            });
        })
        .then(function() {
            $("#rooms-list").bind('render', function(event, rooms) {
               var rows = rooms.map(function(r) {
                   var members = r.members.map(function(m) {
                       return '<button type="button" class="btn btn-default">'+m.value+'</button>';
                   }).join('');
                   
                   return '<tr class="' + r.id.value + '"><td><em><small>' + r.id.value + '</small></em><br/>'+
                   '<div class="btn-group">'+members+'</div>'+
                   '</td></tr>';
               }).join('');
               $(this).html(rows);
               rooms.forEach(function(r) {
                   var selector = "#rooms-list tr."+r.id.value;
                   $(selector).click(function() {
                       call(Bert.tuple(Bert.atom('join_room'), r.id)).then(function(members) {
                           room = r.id;
                           roomUsers = members;
                           return loadTable();
                       });
                   })
               });
            });
        })
        .then(function() {
            return call(Bert.atom('get_users'));
        })
        .then(function(users) {
            lobbyUsers = users;
            $("#players-list").trigger('render', [users]);
            $("#logout").click(function() {
               call(Bert.atom('logout')).then(function() {
                  return loadLogin(); 
               });
            });
            $("#create-room").click(function() {
               call(Bert.atom('create_room')).then(function(data) {
                   room = data[1];
                   roomUsers = [username];
                   loadTable();
               });
            });
        })
        .then(function() {
            return call(Bert.atom('get_rooms'));
        })
        .then(function(tables) {
            rooms = tables;
            $("#rooms-list").trigger('render', [rooms]);
        });
    };
    
    loadLogin = function() {
        return loadTemplate('login').then(function() {
            $("#form-signin").submit(function(event) {
                var join = Bert.atom('join');
                var user = Bert.binary($("#form-signin-nickname").val());
                username = user;
                call(Bert.tuple(join, user))
                .then(function(reply) {
                    return loadLobby();
                });
                event.preventDefault();
            });
        });
    };
    
    var loadTable = function() {
        return loadTemplate('table').then(function() {
           $("#quit").click(function() {
              call(Bert.atom('quit_room')).then(function() {
                  roomUsers = [];
                  return loadLobby();
              })
           });
        })
        .then(function() {
            $("#room-players-list").bind('render', function(event, users) {
                var rows = users.map(function(u) {
                    return '<tr class="'+u.value+'"><td>' + u.value + '</td></tr>'
                    }).join('');
                $(this).html(rows);
            });
        })
        .then(function() {
            console.log(roomUsers);
            $("#room-players-list").trigger('render', [roomUsers]);
        });
    }
    
    loadLogin();

});