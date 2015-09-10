///<reference path="Sketch.ts"/>
///<reference path="Connection.ts"/>
///<reference path="lib/jquery.d.ts"/>

class Corridor implements PageLoader{

    private roomList : JQuery;
    private joinedRoom;
    private chat : JQuery;

    getTitle(){return "Corridor";}
    getBackButton(){
	return {
	    text:"Log out",
	    click:function(){
		connection.send({cmd:"logOut",data:""});
	    }
	};
    }
    getForwardButton(){
	return {
	    text:"Create room",
	    click:function(){ loadDialog("create");}
	}
    }
    onUnload(){}
    
    getResponseHandler() : ResponseHandler {
        var self = this;
        return function(data : ServerResponse){
	    if(data.error && data.cmd != "setName" && data.cmd != "createRoom"){
		self.chat.append(data.error + "<br/>");
            }else if(data.cmd == "enterCorridor"){
                self.roomList.html("");
                for(var i = 0; i < data.data.length ; i++){
                    self.addRoom(<RoomElement>data.data[i]);
                }
            }else if(data.cmd == "joinRoom"){
                if(data.error){
                    console.error(data.error);
                }else{
                    loadPage("room",data.data);
		}
            }else if(data.cmd == "newRoom"){
                self.addRoom(data.data);
            }else if(data.cmd == "roomDeleted"){
		self.removeRoom(data.data);
	    }else if(data.cmd == "chat"){
		var chatData = <ChatMessage>data.data;
		self.addMessage(chatData.sender,chatData.message);
	    }else if(data.cmd == "logOut"){
		username = null;
		loadDialog("login");
	    }
        };
    }

    removeRoom(room){
	this.roomList.find("#r-" + room).remove();
    }

    addMessage(sender,msg){
	this.chat.append("<div class='chatMessage'><span class='sender'>" + sender + ":</span> " + msg + "<br /></div>");
	this.chat.prop("scrollTop",this.chat.prop("scrollHeight"));
    }

    addRoom(room : RoomElement){
        var self = this;
        var roomElem = $("<div id='r-" + room.name + "' class='shadow roomElem'><div class='roomName'>" + room.name + "</div><div class='roomUsers'>" + room.users + "/" + room.maxUsers + "</div></div>");
        roomElem.click(function(){self.enterRoom(room.name,room.pass)});
        this.roomList.append(roomElem);
    }

    enterRoom(room : string,pass : boolean){
	if(pass){
	    var password = prompt("Enter password");
	    if(password !== null)
		connection.send({cmd:"joinRoom",data:{room:room,pass:password}});
	}else
            connection.send({cmd:"joinRoom",data:{room:room}});
    }

    onLoad() : void{
	this.chat = $("#chatMessages");
        connection.send({cmd:"enterCorridor", data:""});
        this.roomList = $("#rooms");
	var toSendInput = $("#toSend");
	var height = Math.max(toSendInput.outerHeight(),$("#send").outerHeight());
	this.chat.css("height","calc(100% - " + height + "px)");
	toSendInput.width($(".chat").innerWidth()-$("#send").outerWidth()-10); //10 = padding + border
	var self = this;
	$("#toSend").keydown(function(e){
	    if(e.which == 13){
		if(toSendInput.val().length > 0){
		    self.addMessage("Me",toSendInput.val());
		    connection.send({cmd:"chat",data:toSendInput.val()});
		    toSendInput.val("");
		}
	    }
	});
    }

    getUrl(){
        return "corridor.html";
    }
}
