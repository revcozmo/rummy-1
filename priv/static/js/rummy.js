var sets = [];
var backupSets = [];
var backupTiles = [];
var tiles = [];

var renderTiles = function() {
    $("#tiles").trigger('render', [tiles]);
}

var renderSets = function() {
    $("#sets").trigger('render', [sets]);
}

var allowDrop = function (ev)
{
    ev.preventDefault();
}

var idx = function(v) {
    return parseInt(v.split(" ")[2].split("-")[1]);
}

var drag = function(ev)
{
    ev.dataTransfer.setData("source",ev.target.className);
    if(ev.target.parentElement.className.indexOf("set")==0) {
         ev.dataTransfer.setData("set", idx(ev.target.parentElement.className));  
    }
}

var drop = function(ev)
{   
    var source=ev.dataTransfer.getData("source");
    var destination=ev.target.className;
    var from = idx(source);
    var to = idx(destination); 
    var fromObj = tiles[from];
    tiles.splice(from, 1);
    tiles.splice(to, 0, fromObj);
    backupTiles = tiles.slice(0);
    renderTiles();
    ev.preventDefault();
}

var dropNew = function(ev) {
    var source=ev.dataTransfer.getData("source");
    var from = idx(source);
    if(ev.dataTransfer.getData("set")) {
        var set = sets[ev.dataTransfer.getData("set")];
        var fromObj = set[from];
        set.splice(from, 1);
        sets.splice(0, 0, [fromObj]);
        if(set.length == 0) {
            sets.splice(ev.dataTransfer.getData("set")+1, 1);
        }
    }
    else {
        var fromObj = tiles[from];
        tiles.splice(from, 1);
        sets.splice(0, 0, [fromObj]);   
    }
    renderTiles();
    renderSets();
}

var dropExisting = function(ev) {
    var source = ev.dataTransfer.getData("source");
    var destination = ev.target.className;
    var from = idx(source);
    var to = idx(destination);
    

    if(ev.dataTransfer.getData("set")) {
        var set = sets[ev.dataTransfer.getData("set")];
        var fromObj = set[from];
        set.splice(from, 1);
        sets[idx(ev.target.parentElement.className)].splice(to+1, 0, fromObj);
        if(set.length == 0) {
            sets.splice(ev.dataTransfer.getData("set"), 1);
        }
    }
    else {
        var fromObj = tiles[from];
        tiles.splice(from, 1);
        sets[idx(ev.target.parentElement.className)].splice(to+1, 0, fromObj);
    }
    renderTiles();
    renderSets();
}

$(document).ready(function() {
    var url = 'ws://'+window.location.hostname+':'+window.location.port+'/rummy';
    var conn = $.bert(url);
    var lobbyUsers = [];
    var roomUsers = [];
    var rooms = [];
    var username;
    var room;
    var timer;
    var currentUser;
    var interval;
    
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
        //console.log(data);
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
          }
      }
    };
    
    var roomMessage = function(data) {
        //console.log(data);
        if(data.value && data.value == 'exit') {
            return loadLobby();
        }
        else if(data.length == 2) {
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
                $("#alert").text(data[1].value + ' HAS LEFT THE GAME!');
                $("#alert").show();
                Q.delay(5000).then(function() {
                    $("#alert").hide();
                })
                var selector = "#room-players-list tr."+data[1].value;
                $(selector).addClass("danger");
                Q.delay(2000).then(function() {
                    $(selector).removeClass("danger"); 
                    roomUsers = roomUsers.filter(function(u) { return u.value != data[1].value; });
                    $("#room-players-list").trigger('render', [roomUsers]);
                });
            }
            else if(data[0].value == 'ready') {
                var selector = "#room-players-list tr."+data[1].value +" td.time";
                $(selector).html("READY");
            }
            else if(data[0].value == 'start') {
                tiles = data[1];
                backupTiles = tiles.slice(0);
                renderTiles();
                $("#room-players-list td.time").html("");
            }
            else if(data[0].value == 'tile') {
                tiles = backupTiles.map(function(r) { return r; });
                tiles.push(data[1]);
                sets = backupSets.slice(0);
                backupTiles = tiles.slice(0);
                renderSets();
                renderTiles();
            }
            else if(data[0].value == 'move') {
                $("#room-players-list td.time").html("");
                var selector = "#room-players-list tr."+data[1].value +" td.time";
                var oldUser = currentUser;
                currentUser = data[1];
                timer = 60;
                $(selector).html(timer);
                clearInterval(interval);
                interval = setInterval(function() {
                    timer--;
                    $(selector).html(timer);
                }, 1000);
                if(oldUser && oldUser.value != username.value) {
                    sets = backupSets.map(function(r) { return r.slice(0); });
                    tiles = backupTiles.slice(0);
                }
                renderTiles();
                renderSets();
            }
            else if(data[0].value == 'set') {
                sets = data[1];
                backupSets = sets.map(function(r) { return r.slice(0); });
                renderTiles();
                renderSets();
            }
            else if(data[0].value == 'winner') {
                $("#alert").text(data[1].value + ' HAS WON THE GAME!');
                $("#alert").show();
                Q.delay(5000).then(function() {
                    $("#alert").hide();
                });
                tiles = [];
                backupTiles = [];
                sets = [];
                backupSets = [];
                renderTiles();
                renderSets();
                clearInterval(interval);
                
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
            $("#players-list").unbind('render').bind('render', function(event, users) {
                var rows = users.map(function(u) {
                        return '<tr class="'+u.value+'"><td>' + u.value + '</td></tr>'
                    }).join('');
                $(this).html(rows);
            });
        })
        .then(function() {
            $("#rooms-list").unbind('render').bind('render', function(event, rooms) {
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
                   $(selector).unbind('click').click(function() {
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
            $("#logout").unbind('click').click(function() {
               call(Bert.atom('logout')).then(function() {
                  return loadLogin(); 
               });
            });
            $("#create-room").unbind('click').click(function() {
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
           $("#quit").unbind('click').click(function() {
              call(Bert.atom('quit_room')).then(function() {
                  roomUsers = [];
                  return loadLobby();
              })
           });
        })
        .then(function() {
            $("#room-players-list").unbind('render').bind('render', function(event, users) {
                var rows = users.map(function(u) {
                    return '<tr class="'+u.value+'"><td>' + u.value + '</td><td class="time"></td></tr>'
                    }).join('');
                $(this).html(rows);
            });
        })
        .then(function() {
            $("#tiles").unbind('render').bind('render', function(event, tiles) {
                var index = 0;
                var tiles = tiles.map(function(t) {
                    return '<div class="tile '+t.color+' idx-'+(index++)+'">'+t.number+'</div>';
                }).join('');
                $(this).html(tiles);
                $("#tiles div").attr("draggable", true)
                                .attr("ondragover", "allowDrop(event)")
                                .attr("ondrop","drop(event)")
                                .attr("ondragstart", "drag(event)");
            });
        })
        .then(function() {
            $("#sets").unbind('render').bind('render', function(event, localsets) {
                var i = 0;
 
                var localsets = localsets.map(function(s) {
                    var idx = 0;
                    return '<div class="set dummy set-'+(i++)+'">' +
                    s.map(function(t) {
                        return '<div class="tile '+t.color+' idx-'+(idx++)+'">'+t.number+'</div>';
                        }).join('') +
                        '</div>';
                    }).join('');
                    $(this).html('<div class="set set-new">NEW SET</div>'+localsets);
                    if(currentUser && currentUser.value == username.value) {
                        $("#sets div.tile").attr("draggable", true)
                        .attr("ondragover", "allowDrop(event)")
                        .attr("ondrop","dropExisting(event)")
                        .attr("ondragstart", "drag(event)");
                        $("div.set-new").attr("ondragover", "allowDrop(event)")
                        .attr("ondrop","dropNew(event)");
                    }
                    else {
                        $("#sets div.tile").removeAttr("draggable")
                        .removeAttr("ondragover")
                        .removeAttr("ondrop")
                        .removeAttr("ondragstart");
                        $("div.set-new").removeAttr("ondragover")
                        .removeAttr("ondrop");      
                    }
                });
        })
        .then(function() {
            $("#room-players-list").trigger('render', [roomUsers]);
            renderSets();
            $("#ready").unbind('click').click(function() {
               call(Bert.atom('start')); 
            });
            $("#peek").unbind('click').click(function() {
                call(Bert.atom('peek'));
            });
            $("#send").unbind('click').click(function() {
                call(Bert.tuple(Bert.atom('put'), sets)).then(function() {
                    backupTiles = tiles.map(function(r) { return r; });
                });
            });
            $("#reset").unbind('click').click(function() {
                sets = backupSets.map(function(r) { return r.slice(0); });
                tiles = backupTiles.map(function(r) { return r; });
                renderTiles();
                renderSets();
            });
        }).
        then(function() {
            sets = [];
            backupSets = [];
            tiles = [];
            backupTiles = [];
            clearInterval(interval);
            $("#room-players-list td.time").html("");
        })
    }
    
    loadLogin();
});