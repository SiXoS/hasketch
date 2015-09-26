///<reference path="Sketch.ts"/>
///<reference path="Types.ts"/>
///<reference path="lib/jquery.d.ts"/>
///<reference path="lib/spectrum.d.ts"/>
///<reference path="CanvasHandler.ts"/>
class Room implements PageLoader{

    private pressed : boolean;
    private chatField : JQuery;
    private chatMessages : JQuery;
    private userList : JQuery;
    private wordField : JQuery;
    private poss : number[];
    private presentator : string;
    private loader : JQuery;
    private border : JQuery;
    private timer : number;
    private pieTimer = null;
    private ranOutTimer = null;
    private hasGuessedRight : number = 0;
    private canvasHandler : CanvasHandler;
    private paintingTool : string;
    private finTimer : number;
    private users : User[];
    private roundsField : JQuery;
    private round : number;
    private maxRound : number;
    private name : string;
    
    getTitle(){ return "Room";}
    getBackButton(){
	var self = this;
	return {
	    text:"Leave room",
	    click:function(){
		connection.send({cmd:"leaveRoom",data:"",room:self.name});
	    }
	}
    }
    getForwardButton(){return null;}
    onUnload(){
	this.clearTimers();
    }

    clearTimers(){
	if(this.pieTimer != null){
	    clearTimeout(this.pieTimer);
	    this.pieTimer = null;
	}
	if(this.ranOutTimer != null){
	    clearTimeout(this.ranOutTimer);
	    this.ranOutTimer = null;
	}
    }
    
    getResponseHandler(){
	var self = this;
	return function(data : ServerResponse){
	    if(data.error){
		self.chatReceived("@Server",data.error);
		return;
	    }
	    if(data.cmd == "draw"){
		if(data.error){
		    console.log(data.error);
		}else{
		    if(data.data.tool == "trash"){
			self.canvasHandler.clear();
			return;
		    }else if(data.data.tool == "undo"){
                        self.canvasHandler.undo();
                        return;
                    }else if(data.data.tool == "redo"){
                        self.canvasHandler.redo();
                        return;
                    }
		    var tool = data.data.tool;
		    var res = data.data.resolution;
		    var scale = self.canvasHandler.getScale(res.w,res.h)
		    var info = data.data
		    var f = function(x){return Math.round(x*scale)};
		    if(tool == "pencil"){
			var newPoss = [];
			for(var i = 0 ; i<info.poss.length ; i++)
			    newPoss.push(f(info.poss[i]));
			self.canvasHandler.drawPoints(newPoss,info.color,f(info.size));
		    }else if(tool == "bucket"){
			self.canvasHandler.pourBucket(f(info.x),f(info.y),
						      info.r,info.g,info.b,info.tolerance);
		    
		    }else if(tool == "circle"){
			var pos = info.pos;
			self.canvasHandler.drawEllipse(f(pos.sx),f(pos.sy),f(pos.ex),f(pos.ey)
						       ,info.color);
		    }else if(tool == "square"){
			var pos = info.pos;
			self.canvasHandler.drawSquare(f(pos.sx),f(pos.sy),f(pos.ex),f(pos.ey)
						      ,info.color);
		    }
		}
	    }else if(data.cmd == "roundChange"){
		self.roundsField.html("round " + ++self.round + "/" + self.maxRound)
	    }else if(data.cmd == "gameOver"){
		self.gameOver();
	    }else if(data.cmd == "hint"){
		if(self.presentator == username){
		    var w = self.wordField.html().split("<br>");
		    self.wordField.html(w[0] + "<br>" + data.data);
		}else{
		    self.wordField.html("<br/>" + data.data);
		}
		var presS = $("li#u-" + self.presentator).find(".score");
		presS.html("" + (parseInt(presS.html()) - 1));
	    }else if(data.cmd == "newUser"){
		self.addUser(data.data,0);
	    }else if(data.cmd == "presentatorChange"){
		self.clearTimers();
		self.setPresentator(data.data);
		self.drawBlue(0);
		self.canvasHandler.clear();
	    }else if(data.cmd == "userLeave"){
		self.removeUser(data.data);
	    }else if(data.cmd == "chat"){
		var chat : ChatMessage = data.data;
		self.chatReceived(chat.sender,chat.message);
	    }else if(data.cmd == "becamePresentator"){
		self.clearTimers();
		self.becamePresentator(data.data);
		self.drawBlue(0);
		self.canvasHandler.clear();
	    }else if(data.cmd == "guessedCorrect"){
		self.guessedCorrect(data.data.user,data.data.score);
	    }else if(data.cmd == "leaveRoom"){
		loadPage("corridor");
	    }else if(data.cmd == "wordWas"){
		self.chatReceived("@Server","The word was " + data.data + ".");
	    }
	}
    }

    
    onLoad(loadedRoom : RoomInfo){
	this.name = loadedRoom.name;
        $("#title").html("Room " + this.name);
	this.timer = loadedRoom.timer;
	this.finTimer = loadedRoom.finTimer;
	this.roundsField = $("#round");
	this.round = loadedRoom.round;
	this.maxRound = loadedRoom.maxRound;
	this.roundsField.html("round " + this.round + "/" + this.maxRound);
	this.wordField = $("#word");
	$("#colorPickerInput").spectrum({
	    clickOutFiresChange : true,
	    color: "#000",
	    change: function(color) {
		self.canvasHandler.setColor(color);
	    }
	});
	var db = $("#drawboard");
	db.attr("width","" + (db.parent().innerWidth()-20));
	var dBarHeights = 0;
	$(".drawBar").each(function(){
	    dBarHeights += $(this).outerHeight(true);
	});
	db.attr("height","" + (db.parent().innerHeight()-dBarHeights-20));
	var self = this;
	this.canvasHandler = new CanvasHandler($("#drawboard"),loadedRoom.presentator,function(d){ self.onDraw(d); });
	this.border = $("#border");
	this.loader = $("#loader");
	this.chatMessages = $("#chatMessages");
	var presentator = loadedRoom.presentator;
	var chatField = $("#toSend");
	var height = Math.max(chatField.outerHeight(), $("#send").outerHeight());
	this.chatMessages.css("height","calc(100% - " + height + "px)");
	chatField.css("width","calc(100% - " + $("#send").outerWidth() + "px)");
	this.chatField = chatField;
	this.paintingTool = "pencil";
	$("#toolPicker > img.button").click(function(){
            if(self.presentator != username) return;
	    var tool = $(this).attr("alt");
	    if(tool == "trash"){
		self.canvasHandler.clear();
		connection.send({cmd:"draw",data:{tool:"trash"},room:self.name});
	    }else if(tool == "undo"){
                self.canvasHandler.undo();
                connection.send({cmd:"draw",data:{tool:"undo"},room:self.name});
            }else if(tool == "redo"){
                self.canvasHandler.redo();
                connection.send({cmd:"draw",data:{tool:"redo"},room:self.name});
            }else{
		$("#toolPicker > img.button").removeClass("hover");
		$(this).addClass("hover");
		self.canvasHandler.setTool(tool);
	    }
	});
	$("#send").click(function(){
	    if(chatField.val().length > 0){
		self.chatReceived("Me",chatField.val());
		connection.send({cmd:"chat",data:chatField.val(),room:self.name});
	    }
	    chatField.val("");
	});
	this.userList = $("#usersList");
	var users : User[] = <User[]>loadedRoom.users;
	this.users = [];
	for(var i:number = 0; i<users.length ; i++){
	    this.addUser(users[i].user,users[i].score);
	    if(users[i].guessedRight)
		this.hasGuessedRight++;
	}
	if(!loadedRoom.created){
	    this.setPresentator(presentator);
	}else
	    this.presentator = null;
	if(users.length > 1){
	    if(loadedRoom.timerType == "normal"){
		this.drawBlue(loadedRoom.timePassed);
	    }else{
		this.drawRed(loadedRoom.timePassed);
	    }
	}
	$("#hint").click(function(){
	    connection.send({"cmd":"hint",data:"",room:self.name});
	});
	$("#skip").click(function(){
	    $("#dialog").html("If you skipped this word because it can't be drawn or it is very dificult to draw, you should report the word.");
	    $("#dialog").dialog({
		modal:true,
		closeOnEscape:true,
		title:"Report word",
		dialogClass:"",
		buttons:{
		    "Report":function(){
			connection.send({cmd:"newPresentator",data:"report",room:self.name});
			$("#dialog").dialog("close");},
		    "Just skip":function(){
			connection.send({cmd:"newPresentator",data:"skip",room:self.name});
			$("#dialog").dialog("close");},
		    "Cancel":function(){
			$("#dialog").dialog("close");}
		}
	    });
	});

    }

    onDraw(data){
	connection.send({cmd:"draw",room:this.name,data:data});
    }

    drawRed(passedTime:number){
	this.loader.css("fill","#880000");
	this.border.css("fill","#510000");
	var msPerDegree = this.finTimer*1000/360;
	var degrees = 360-Math.round(passedTime/msPerDegree);
	var self = this;
	this.ranOutTimer = setTimeout(
	    function(){ self.timerRanOut(); },
	    this.finTimer*1000-passedTime);
	this.draw(degrees,Math.round(msPerDegree));
    }

    drawBlue(passedTime:number){
	this.loader.css("fill","#0088cc");
	this.border.css("fill","#00517a");
	var msPerDegree = this.timer*1000/360;
	var degrees = 360-Math.round(passedTime/msPerDegree);
	var self = this;
	this.ranOutTimer = setTimeout(
	    function(){ self.timerRanOut(); },
	    this.timer*1000-passedTime);
	this.draw(degrees,Math.round(msPerDegree));
    }

    draw(a : number, t : number) {
	var radius = 18;
	a--;
	if(a < 0){
	    this.pieTimer = null;
	    return;
	}
	var pi = Math.PI;
	var r = ( a * pi / 180 )
	, x = Math.sin( r ) * radius
	, y = Math.cos( r ) * - radius
	, mid = ( a > 180 ) ? 1 : 0
	, anim = 'M 0 0 v -' + radius + ' A ' + radius + ' ' + radius + ' 1 ' 
            + mid + ' 1 ' 
            +  x  + ' ' 
            +  y  + ' z';
	//[x,y].forEach(function( d ){
	//  d = Math.round( d * 1e3 ) / 1e3;
	//});
	
	this.loader.attr( 'd', anim );
	this.border.attr( 'd', anim );
	var self = this;
	this.pieTimer = setTimeout(function(){self.draw(a,t)}, t); // Redraw
    }

    timerRanOut(){
	if(this.presentator == username){
	    connection.send({cmd:"newPresentator",data:"",room:this.name});
	}
    }

    gameOver(){

	var won = [];
	var maxScore = 0;
	for(var i = 0 ; i<this.users.length ; i++)
	    maxScore = Math.max(maxScore,this.users[i].score);
	for(var i = 0 ; i<this.users.length ; i++)
	    if(this.users[i].score == maxScore)
		won.push(this.users[i].user);
	var j = won.indexOf(username);
	if(j !== -1){
	    won.splice(j,1);
	    won.unshift("You");
	}
	var message = "";
	if(won.length == 1){
	    message = won[0] + " won!";
	}else{
	    var last = won.pop();
	    message = won.join(", ") + " and " + last + " won!";
	}
	$("#dialog").html(message);
	$("#dialog").dialog({
	    dialogClass:"",
	    closeOnEscape:true,
	    modal:true,
	    buttons:[],
	    title:message});
	this.updateUsers();
	this.round = 1;
	this.roundsField.html("round 1/" + this.maxRound);
	for(var i:number = 0 ; i<this.users.length; i++)
	    this.users[i].score = 0;
	var nm = this.name;
	setTimeout(function(){connection.send({cmd:"newPresentator",data:"",room:nm})},20000);
    }

    guessedCorrect(user:string,score:number){
	this.userList.find("#u-" + user + ">.score").html(" " + score);
	var presScore = this.userList.find("#u-" + this.presentator+">.score");
	var incScore = this.hasGuessedRight == 0 ? 4 : 2;
	presScore.html(parseInt(presScore.html())+incScore + "");
	if(user == username){
	    this.chatReceived("@Server","You guessed correct!");
	}else{
	    this.chatReceived("@Server", user + " guessed correct.");
	}
	if(this.hasGuessedRight+1 == this.users.length-1){
	    if(this.presentator == username)
		connection.send({cmd:"newPresentator",data:"",room:this.name});
	    if(this.pieTimer != null){
		this.clearTimers();
		this.wordField.html("");
		this.loader.attr("d","M 0 0");
		this.border.attr("d","M 0 0");
	    }
	    this.hasGuessedRight = 0;
	    return;
	}else if(this.hasGuessedRight == 0){
	    this.clearTimers();
	    this.drawRed(0);
	}
	this.hasGuessedRight++;
    }

    updateUsers(){
	this.userList.html("");
	for(var i:number = 0; i<this.users.length; i++){
	    var user = this.users[i].user, score = this.users[i].score;
	    this.userList.append("<li id='u-" + user + "'>" +
				 "<div class='user'>" + user + "</div>" +
				 "<div class='score'>" + score + "</div>" +
				 "<br class='clearBoth'/></li>");
	}
    }

    becamePresentator(word : string){
	this.setPresentator(username);
	$("#skip,#hint").css("display","");
	this.wordField.html("Your word is: " + word);
    }

    chatReceived(sender : string, message : string){
	var se = sender == username ? "Me" : sender;
	var chat : string = "<div class='chatMessage'><span class='sender'>" + sender + ":</span> " + message + "<br/></div>";
	this.chatMessages.append(chat);
	this.chatMessages.prop("scrollTop",this.chatMessages.prop("scrollHeight"));
    }

    addUser(user,score){
	this.users.push({user:user,score:score,guessedRight:false});
	this.userList.append("<li id='u-" + user + "'>" +
			     "<div class='user'>" + user + "</div>" +
			     "<div class='score'>" + score + "</div>" +
			     "<br class='clearBoth'/></li>");
    }

    removeUser(user){
	var i : number = this.users.indexOf(user);
	this.users.splice(i,1);
	if(this.users.length == 1){
	    if(this.pieTimer){
		this.clearTimers();
		this.loader.attr("d","M 0 0");
		this.border.attr("d","M 0 0");
	    }
	}
	this.userList.find("li#u-" + user).remove();
    }

    setPresentator(user){
	$("#skip,#hint").css("display","none");
	this.canvasHandler.setPresentator(user);
	this.wordField.html("&nbsp;");
	this.presentator = user;
	this.userList.children().removeClass("presentator");
	this.userList.find("li#u-" + user).addClass("presentator");
	this.hasGuessedRight = 0;
    }

    getUrl(){
	return "room.html";
    }

}
