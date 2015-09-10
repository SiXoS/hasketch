///<reference path="Sketch.ts"/>
///<reference path="lib/jquery.d.ts"/>
///<reference path="Types.ts"/>

class Create implements DialogLoader{

    private name : string;
    private timer : number;
    private finTimer : number;
    private errorField : JQuery;
    private rounds : number;
    private wordList : JQuery = null;
    private words : string[];
    
    getTitle(){ return "Create room";}
    closeable(){ return true;}
    onUnload(){ }
    getDialogTitle(){return "Create room";}
    
    getResponseHandler() : ResponseHandler{

	var self = this;
	return function(data : ServerResponse){
	    
	    if(data.cmd == "createRoom" || data.cmd == "getWordlists"){
		if(data.error){
		    self.errorField.addClass("ui-state-error");
		    self.errorField.html(data.error + "<br/>");
		}else if(data.cmd == "createRoom" && self.name){
		    closeDialog();
		    loadPage("room",<RoomInfo>{
			users:[{user:username,score:0,guessedRight:false}],
			presentator:null,
			created:true,
			finTimer:self.finTimer,
			timerType:"normal",
			timePassed:0,
			round:1,
			maxRound:self.rounds,
			timer:self.timer,
			name:self.name});
		    
		}else if(data.cmd == "getWordlists"){
		    var select = $("<select id='wordList'></select>");
		    self.words = data.data;
		    for(var i = 0; i<data.data.length; i++)
			select.append("<option>" + data.data[i] + "</option>");
		    self.wordList = select;
		    var wl = $("#wordListContainer");
		    wl.html("");
		    wl.append(select);
		}
	    }
	}
	
    }

    onLoad(){
	this.errorField = $("#error");
	connection.send({cmd:"getWordlists",data:""});
    }

    getButtons() : Button[]{
	var nameField = $("#roomName");
	var timerField = $("#timer");
	var usersField = $("#maxUsers");
	var finTimerField = $("#finishingTimer");
	var passField = $("#password");
	var roundsField = $("#rounds");
	var self = this;
	return [{text:"Create room",click:function(){
	    
	    self.errorField.html("");
	    self.errorField.removeClass("ui-state-error");
	    if(self.wordList == null){
		self.errorField.addClass("ui-state-error");
		self.errorField.html("Wait for the wordlist to get updated.");
		return;
	    }

	    if(self.words.indexOf(self.wordList.val()) == -1){
		self.errorField.addClass("ui-state-error");
		self.errorField.html("Wait for the wordlist to get updated.");
		return;
	    }
	    
	    var all = [timerField,usersField,finTimerField,roundsField];
	    var lengthOk = nameField.val().length > 0,regexOk = true, regex = /^\d*$/;
	    for(var i=0 ; i<all.length ; i++)
		lengthOk = lengthOk && all[i].val().length > 0;
	    for(var i=0 ; i<all.length ; i++)
		regexOk = regexOk && regex.test(all[i].val());
	    if(!lengthOk || !regexOk){
		self.errorField.addClass("ui-state-error");
		if(!lengthOk)
		    self.errorField.append("Some fields were empty.");
		if(!lengthOk && !regexOk)
		    self.errorField.append("<br />");
		if(!regexOk)
		    self.errorField.append("The round length, max users, finishing timer, and number of rounds must be numbers.")
	    }else{
		self.name = nameField.val();
		self.timer = parseInt(timerField.val());
		self.finTimer = parseInt(finTimerField.val());
		self.rounds = parseInt(roundsField.val());
		var toSend = {cmd:"createRoom",data:{name:nameField.val(),
						     timer:self.timer,
						     maxUsers:parseInt(usersField.val()),
						     finishingTimer:self.finTimer,
						     maxRounds:parseInt(roundsField.val()),
						     wordList:self.wordList.val()}}
		if(passField.val().length > 0)
		    toSend.data.password = passField.val();
		connection.send(toSend);
	    }
	    
	}}];
    }

    getUrl(){
	return "create.html";
    }
    
}
