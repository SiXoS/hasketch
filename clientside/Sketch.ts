///<reference path="Connection.ts"/>
///<reference path="Login.ts"/>
///<reference path="Corridor.ts"/>
///<reference path="Create.ts"/>
///<reference path="lib/jquery.d.ts"/>
///<reference path="Room.ts"/>
///<reference path="Types.ts"/>

var currentHandler : ResponseHandler;
var dialogHandler : ResponseHandler = null;
var username : string;
var content;
var connection : Connection.Wrapper;
var bButton : JQuery;
var fButton : JQuery;
var dialogContainer : JQuery;
var title : JQuery;
var currentLoader : PageLoader = null;
var currentDialog : DialogLoader = null;

$(function(){

    connection = Connection.connect(handleResponse,function(){
	loadPage("corridor");
	loadDialog("login");
    });
    content = $("#content");
    bButton = $("#backButtonContainer");
    fButton = $("#forwardButtonContainer");
    dialogContainer = $("#dialog");
    title = $("#title");
    $(window).bind('beforeunload', function(){
        return 'Are you sure you want to leave? All your points will be lost.';
    });
    
});

function connectListeners(){
    $("input").keypress(function(data){ 
	if(data.which == 13){ //Enter key
	    var buttons : JQuery = $(this).siblings("button");
	    if(buttons.length > 0){
		buttons.click();
	    }
	}
    });
}

function handleResponse(data){
    currentHandler(data);
    if(dialogHandler)
	dialogHandler(data);
}

function setUsername(user : string){
    username = user;
}

function loadDialog(dialog : string, arg? : any){
    var dia : DialogLoader;
    if(dialog == "login"){
	dia = new Login;
    }else if(dialog == "create"){
	dia = new Create;
    }else{
	console.error(dialog + " is not a valid dialog.");
	return;
    }
    if(currentDialog != null)
	currentDialog.onUnload();
    currentDialog = dia;
    $.ajax("pages/" + dia.getUrl(),{
	dataType:"html",
	success:function(html){
	    dialogContainer.html(html);
	    $("#title").html(dia.getTitle());
	    dia.onLoad();
	    dialogHandler = dia.getResponseHandler();
	    var diaClass = dia.closeable() ? "" : "no-close";
	    dialogContainer.dialog({
		modal:true,
		title:dia.getDialogTitle(),
		buttons:dia.getButtons(),
		closeOnEscape:dia.closeable(),
		dialogClass:diaClass,
		width:"750px"
	    });
            dialogContainer.find("input[autofocus]").focus();
	}
    })
}

function closeDialog(){
    dialogContainer.dialog("close");
    dialogHandler = null;
    currentDialog.onUnload();
    currentDialog = null;
    title.html(currentLoader.getTitle());
}

function loadPage(page : string, arg? : any){
    var loader : PageLoader;
    if(page == "corridor"){
        loader = new Corridor;
    }else if(page == "room"){
	loader = new Room;
    }else{
        console.error("Page " + page + " is not valid.");
	return;
    }
    if(currentLoader != null){
	currentLoader.onUnload();
    }
    currentLoader = loader;
    $.ajax("pages/" + loader.getUrl(),{
        dataType:"html",
        success:function(html){
            content.html(html);
	    connectListeners();
            $("input[autofocus]").focus();
	    $("#title").html(loader.getTitle());
	    bButton.html("");
	    fButton.html("");
	    if(loader.getBackButton()){
		populateButton(bButton,loader.getBackButton());
	    }if(loader.getForwardButton()){
		populateButton(fButton,loader.getForwardButton());
	    }
            loader.onLoad(arg);
            currentHandler = loader.getResponseHandler();
        }
        });

}

function populateButton(container : JQuery, button : Button){

    var bButt = $("<button>" + button.text + "</button>");
    bButt.click(function(){
	button.click();
    });
    container.append(bButt);

}
