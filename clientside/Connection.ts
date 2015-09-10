//<reference path="Types.ts"/>

module Connection{

    export function connect(responseHandler : ResponseHandler,onLoaded) : Wrapper{

        var conn = new WebSocket('localhost:8181');
        conn.onopen = function(e) {
	    console.log("Connection established!");
	    onLoaded();
        };

	conn.onerror = function(e){
	    console.error(e);
	}

	conn.onclose = function(e){
	    console.log("close");
	    console.log(e);
	    
	    var dia = $("#dialog");
	    dia.html("Unfortunately the connection to the server was interrupted. Try to refresh the page. If this doesn't work, come back a bit later and hopefully the server will be up and running then.");
	    dia.dialog({
		title:"Server down",
		modal:true,
		width:700,
		dialogClass:'no-close',
		closeOnEscape:false,
		buttons:{"Refresh": function(){ window.location.reload(); }}
	    });
	}

        conn.onmessage = function(e) {
	    var jData = JSON.parse(e.data);
	    console.log("recv")
	    console.log(jData);
	    responseHandler(jData);
        };
        return new Wrapper(conn);
    }

    export class Wrapper{
        constructor(private conn){}
        send(message : ServerRequest) : void{
	    console.log("send");
	    console.log(message);
            this.conn.send(JSON.stringify(message));
        }

    }

}
    

    
