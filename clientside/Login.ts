///<reference path="Sketch.ts"/>
///<reference path="Types.ts"/>

class Login implements DialogLoader{

    private sentRequest : boolean = false;
    private requestedName : string = "";
    private user : JQuery;
    private error : JQuery;
    private pressed : boolean = false;
    
    getTitle(){ return "Login"; }
    getDialogTitle(){ return "Welcome to 5Sketch!"; }
    onUnload(){}
    
    getResponseHandler() : ResponseHandler {
        var self = this;
        return function(data : ServerResponse){
            if(self.sentRequest && self.requestedName.length > 0){
                if(data.cmd == "setName"){
                    if(data.error){
			this.pressed = false;
                        $("#error").html(data.error + "<br/>");
                    }else{
                        setUsername(self.requestedName);
                        closeDialog();
                    }
                }
            }
            self.sentRequest = false;
            self.requestedName = "";
        };
    }

    getButtons() : Button[]{
	var self = this;
	return [{text:"Login",click:function(){self.login();}}];
    }

    closeable(){return false;}

    login(){
	var val = this.user.val();
        if(val.match(/^(\w|\d){4,12}$/i)){
	    this.error.css("display:none;")
            this.sentRequest = true;
            this.requestedName = val;
            connection.send({cmd:"setName",data:val});
        }else{
	    this.pressed = false;
	    this.error.css("display","");
	    this.error.css({"margin-bottom":"15px",padding:"3px"});
            this.error.html("The username has to be between 4 and 12 characters inclusive and may only contain digits and numbers.");
        }
    }

    onLoad() : void{
        this.user = $("#username");
        this.error = $("#error");
	var self = this;
	this.user.keypress(function(e){
	    if(!self.pressed && e.which == 13){
		self.pressed = true;
		self.login();
	    }
	});
    }

    getUrl() : string{
        return "login.html";
    }

}
