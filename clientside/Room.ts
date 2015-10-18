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
    private ctrlDown : boolean = false;
    private colorPicker : JQuery;
    
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
                for(var i = 0; i<self.users.length; i++)
                    if(self.users[i].user == self.presentator)
                        self.users[i].score--;
		self.updateUsers();
	    }else if(data.cmd == "newUser"){
		self.addUser(data.data,0);
	    }else if(data.cmd == "presentatorChange"){
		self.clearTimers();
		self.setPresentator(data.data);
		self.drawBlue(0);
	    }else if(data.cmd == "userLeave"){
		self.removeUser(data.data);
	    }else if(data.cmd == "chat"){
		var chat : ChatMessage = data.data;
		self.chatReceived(chat.sender,chat.message);
	    }else if(data.cmd == "becamePresentator"){
		self.clearTimers();
		self.becamePresentator(data.data);
		self.drawBlue(0);
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
        this.colorPicker = $("#colorPickerInput");
	this.colorPicker.spectrum({
	    clickOutFiresChange : true,
	    color: "#000",
            showPalette: true,
            palette: [["rgb(0, 0, 0)", "rgb(67, 67, 67)", "rgb(102, 102, 102)", /*"rgb(153, 153, 153)","rgb(183, 183, 183)",*/
                       "rgb(204, 204, 204)", "rgb(217, 217, 217)", /*"rgb(239, 239, 239)", "rgb(243, 243, 243)",*/ "rgb(255, 255, 255)"],
                      ["rgb(152, 0, 0)", "rgb(255, 0, 0)", "rgb(255, 153, 0)", "rgb(255, 255, 0)", "rgb(0, 255, 0)",
                       "rgb(0, 255, 255)", "rgb(74, 134, 232)", "rgb(0, 0, 255)", "rgb(153, 0, 255)", "rgb(255, 0, 255)"],
                      ["rgb(230, 184, 175)", "rgb(244, 204, 204)", "rgb(252, 229, 205)", "rgb(255, 242, 204)", "rgb(217, 234, 211)",
                       "rgb(208, 224, 227)", "rgb(201, 218, 248)", "rgb(207, 226, 243)", "rgb(217, 210, 233)", "rgb(234, 209, 220)"],
                      ["rgb(221, 126, 107)", "rgb(234, 153, 153)", "rgb(249, 203, 156)", "rgb(255, 229, 153)", "rgb(182, 215, 168)",
                       "rgb(162, 196, 201)", "rgb(164, 194, 244)", "rgb(159, 197, 232)", "rgb(180, 167, 214)", "rgb(213, 166, 189)"],
                      ["rgb(204, 65, 37)", "rgb(224, 102, 102)", "rgb(246, 178, 107)", "rgb(255, 217, 102)", "rgb(147, 196, 125)",
                       "rgb(118, 165, 175)", "rgb(109, 158, 235)", "rgb(111, 168, 220)", "rgb(142, 124, 195)", "rgb(194, 123, 160)"],
                      ["rgb(166, 28, 0)", "rgb(204, 0, 0)", "rgb(230, 145, 56)", "rgb(241, 194, 50)", "rgb(106, 168, 79)",
                       "rgb(69, 129, 142)", "rgb(60, 120, 216)", "rgb(61, 133, 198)", "rgb(103, 78, 167)", "rgb(166, 77, 121)"],
                      ["rgb(91, 15, 0)", "rgb(102, 0, 0)", "rgb(120, 63, 4)", "rgb(127, 96, 0)", "rgb(39, 78, 19)",
                       "rgb(12, 52, 61)", "rgb(28, 69, 135)", "rgb(7, 55, 99)", "rgb(32, 18, 77)", "rgb(76, 17, 48)"]],
            move: function(color){
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
        $(document).keydown(function(e){
            if(e.which == 17)
                self.ctrlDown = true;
            else if(self.ctrlDown && e.which == 90)
                $("#toolPicker .button[alt='undo']").click();
            else if(self.ctrlDown && e.which == 89)
                $("#toolPicker .button[alt='redo']").click();
        });
        $(document).keyup(function(e){ if(e.which == 17) self.ctrlDown = false; });
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
	$("#toolPicker > img.button").click(function(){self.chooseTool(this);});
	$("#send").click(function(){
	    if(chatField.val().length > 0){
                if(chatField.val().length < 150){
		    self.chatReceived("Me",chatField.val());
		    connection.send({cmd:"chat",data:chatField.val(),room:self.name});
                }else
                    self.chatReceived("@Server","You can't send more than 150 characters.");
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

    chooseTool(toolElem : HTMLElement){
        if(this.presentator != username || $(toolElem).hasClass("disabled")) return;
        var tool = $(toolElem).attr("alt");
        if(tool == "undo" || tool == "redo"){
            var other = tool == "undo" ? "redo" : "undo";
            $("#toolPicker .button[alt='" + other + "']").removeClass("disabled");
            if(tool == "undo" && !this.canvasHandler.undo() || tool == "redo" && !this.canvasHandler.redo())
                $(toolElem).addClass("disabled");
            connection.send({cmd:"draw",data:{tool:tool},room:this.name});
            return;
        }
        $("#toolPicker .button[alt='undo']").removeClass("disabled");
        $("#toolPicker .button[alt='redo']").addClass("disabled");
        if(tool == "trash"){
            var dia = $("#dialog");
            dia.html("Are you sure you want to delete the entire picture?");
            var self = this;
            dia.dialog({
                width:700,
                modal:true,
                title:"Confirm clear picture",
                buttons:{
                    "Clear picture":function(){
                        self.canvasHandler.clear();
                        connection.send({cmd:"draw",data:{tool:"trash"},room:self.name});
                        dia.dialog("close");
                    },
                    "Cancel":function(){ dia.dialog("close"); }
                }
            });
        }else{
            $("#toolPicker > img.button").removeClass("hover");
            $(toolElem).addClass("hover");
            this.canvasHandler.setTool(tool);
        }
    }

    onDraw(data){
        $("#toolPicker .button[alt='undo']").removeClass("disabled");
        $("#toolPicker .button[alt='redo']").addClass("disabled");
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
	var incScore = this.hasGuessedRight == 0 ? 4 : 2;
        for(var i = 0; i<this.users.length; i++){
            if(this.users[i].user == this.presentator)
                this.users[i].score += incScore;
            else if(this.users[i].user == user){
                this.users[i].score = score;
                this.users[i].guessedRight = true;
            }
        }
        this.updateUsers();
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
        this.users.sort(function(a,b){return b.score - a.score;});
	for(var i:number = 0; i<this.users.length; i++){
	    var user = this.users[i].user, score = this.users[i].score;
            var guessedRightClass = this.users[i].guessedRight ? " class='guessedCorrect'" : "";
	    var u = $("<li id='u-" + user + "'>" +
		      "<div class='user'>" + user + "</div>" +
		      "<div class='score'>" + score + "</div>" +
		      "<br class='clearBoth'/></li>");
            if(this.users[i].guessedRight)
                u.addClass("guessedCorrect");
            if(this.users[i].user == this.presentator)
                u.addClass("presentator");
            this.userList.append(u);
	}
    }

    becamePresentator(word : string){
	this.setPresentator(username);
        this.colorPicker.spectrum("enable");
        this.colorPicker.spectrum("set","black");
        $("#toolPicker .button:not([alt='redo'])").removeClass("disabled");
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
	var i : number = -1;
	for(var j : number = 0; j<this.users.length ; j++){
	    if(this.users[j].user == user){
                i = j;
                break;
            }
	}
        if(i == -1) return;
	this.users.splice(i,1);
	if(this.users.length == 1){
	    if(this.pieTimer){
		this.clearTimers();
		this.loader.attr("d","M 0 0");
		this.border.attr("d","M 0 0");
	    }
	}
	this.updateUsers();
    }

    setPresentator(user){
        this.colorPicker.spectrum("disable");
        $("#toolPicker .button").addClass("disabled");
	$("#skip,#hint").css("display","none");
        this.canvasHandler.clear();
	this.canvasHandler.setPresentator(user);
	this.wordField.html("&nbsp;");
	this.presentator = user;
        for(var i=0 ; i<this.users.length ; i++)
            this.users[i].guessedRight = false;
        this.updateUsers();
	this.hasGuessedRight = 0;
    }

    getUrl(){
	return "room.html";
    }

}
