///<reference path="PaintBucket.ts"/>
///<reference path="lib/jquery.d.ts"/>
///<reference path="lib/jqueryui.d.ts"/>
///<reference path="lib/spectrum.d.ts"/>

interface OnDraw{
    (data) : void;
}

class CanvasHandler{

    private ctx : CanvasRenderingContext2D;
    private bucket : PaintBucket;
    private paintingTool : string;
    private poss : number[];
    private pressed : boolean = false;
    private beforeDraw : ImageData;
    private color : tinycolorInstance = tinycolor("#000000");
    private penSize : number = 2;
    private eraserSize : number = 5;
    private sliderBar : JQuery;
    private sliderDesc : JQuery;
    private sliderVal : JQuery;
    private tolerance : number = 32;
    private onDraw : OnDraw;
    private startPos;
    private cursorStalker : JQuery;
    private radius : number = 0;
    private history : ImageData[] = [];
    private redoHistory : ImageData[] = [];
    
    constructor(private canvas : JQuery,private presentator : string, onDraw : OnDraw){
	this.onDraw = onDraw;
	this.ctx = (<any>this.canvas.get(0)).getContext("2d");
	this.ctx.fillStyle = "#FFFFFF";
	this.ctx.fillRect(0,0,700,700);
	this.ctx.fillStyle = "#000000";
	this.ctx.lineCap = "round";

	var self = this;
	this.sliderBar = $("#sliderBar");
	this.sliderDesc = $("#sliderDesc");
	this.sliderDesc.html("Pixel size: ");
	this.sliderVal = $("#sliderVal");
	this.sliderVal.html("2");
	this.sliderBar.slider({
	    min:1,
	    max:50,
	    value:2,
	    step:1,
	    slide:function(e,ui){self.sliderSlide(ui.value);},
	    change:function(e,ui){
		self.sliderChange(ui.value);
	    }
	});

	this.cursorStalker = $("#cursorStalker");
	this.cursorStalker.css("display","none");

	this.paintingTool = "pencil";
	this.canvas.mouseover(function(e){
	    if(self.paintingTool == "pencil" || self.paintingTool == "erase"){
		var size = self.paintingTool == "pencil" ? self.penSize : self.eraserSize;
		self.radius = (size+4)/2;
		self.cursorStalker.attr({
		    width: size+4,
		    height:size+4
		}).children().attr({
		    cx: self.radius,
		    cy: self.radius,
		    r:  size/2
		});
		self.cursorStalker.css("display","");
		self.canvas.css("cursor","none");
	    }else
		self.canvas.css("cursor","default");
	});
	this.canvas.mouseout(function(e){
	    self.cursorStalker.css("display","none");
	});
	this.canvas.mousedown(function(e){
	    //This is a bit of an ugly fix for an ugly bug.
	    //If the user is changing color and haven't pressed "choose"
	    //We have to wait to let the color-change be done
	    //before we draw anything.
	    setTimeout(function(){self.canvasOnClick(e)},5);
	});
	this.canvas.mousemove(function(e){self.canvasOnMove(e);});
	$(document).mouseup(function(e){self.canvasOnRelease(e);});

	this.bucket = new PaintBucket(this.ctx);
        this.storeHistory();        

     }

    private sliderChange(value:number){
	if(this.paintingTool == "pencil"){
	    this.penSize = value;
	}else if(this.paintingTool == "erase"){
	    this.eraserSize = value;
	}else if(this.paintingTool == "bucket"){
	    this.tolerance = value;
	}
    }

    setColor(color:tinycolorInstance){
	this.color = color;
	this.ctx.fillStyle = color.toHexString();
	this.ctx.strokeStyle = color.toHexString();
    }

    private sliderSlide(value : number){
	this.sliderVal.html("" + value);
    }
    
    setTool(tool :string){
	this.paintingTool = tool;
	var self = this;
	if(tool == "pencil" || tool == "erase"){
	    this.sliderDesc.html("Pixel size: ");
	    var val = tool == "pencil" ? this.penSize : this.eraserSize;
	    this.sliderVal.html("" + val);
	    this.sliderBar.slider({
		max:50,
		min:1,
		value: val,
		step:1,
		disabled:false
	    });
	}else if(tool == "bucket"){
	    this.sliderDesc.html("Tolerance: ")
	    this.sliderVal.html("" + this.tolerance);
	    this.sliderBar.slider({
		max:64,
		min:0,
		step:4,
		disabled:false,
		value: this.tolerance
	    });
	}else{
	    this.sliderDesc.html("");
	    this.sliderVal.html("");
	    this.sliderBar.slider({disabled:true});
	}
    }

    setPresentator(presentator : string){
	if(this.presentator == username && this.pressed)
	    this.pressed = false;
	this.presentator = presentator;
        this.history = [];
        this.redoHistory = [];
        this.storeHistory();
    }

    /**
     * @param points Draws the points specified. The input should be formatted as: [x1,y1,x2,y2,...,xn,yn]
     * @param color Should be formatted as a hex string whit a # prepended
     */
    drawPoints(points:number[],color:string,size:number){

        this.ctx.fillStyle = this.ctx.strokeStyle = color;
	this.ctx.lineWidth = size;
        var minX : number = points[0],
            minY : number = points[1],
            maxX : number = points[0],
            maxY : number = points[1];
	if(points.length == 2){
	    this.ctx.beginPath();
	    this.ctx.arc(points[0], points[1], size/2, 0, 2 * Math.PI);
	    this.ctx.fill();

	}else{
	
	    this.ctx.beginPath();
	    this.ctx.moveTo(points[0],points[1]);

	    for(var i = 2 ; i< points.length ; i+=2){
                minX = Math.min(points[i],minX);
                minY = Math.min(points[i+1],minY);
                maxX = Math.max(points[i],maxX);
                maxY = Math.max(points[i+1],maxY);
	        this.ctx.lineTo(points[i],points[i+1]);
	        this.ctx.stroke();
	        this.ctx.beginPath();
	        this.ctx.moveTo(points[i],points[i+1]);
	    }

        }
        this.storeHistory();

    }

    drawSquare(x1,y1,x2,y2,color,storeHstry? : boolean){

	this.ctx.fillStyle = this.ctx.strokeStyle = color;
	
	var x = Math.min(x1,x2);
	var y = Math.min(y1,y2);
	var width = Math.abs(x1-x2);
	var height = Math.abs(y1-y2);
	
	this.ctx.fillRect(x,y,width,height);
  
        if(typeof storeHstry == 'undefined' || storeHstry)
            this.storeHistory();
       
    }

    drawEllipse(x1, y1, x2, y2, color,storeHstry? : boolean) {

	this.ctx.fillStyle = this.ctx.strokeStyle = color;

	var x = Math.min(x1,x2);
	var y = Math.min(y1,y2);
	var w = Math.abs(x1-x2);
	var h = Math.abs(y1-y2);
	
	var kappa = .5522848,
	ox = (w / 2) * kappa, // control point offset horizontal
	oy = (h / 2) * kappa, // control point offset vertical
	xe = x + w,           // x-end
	ye = y + h,           // y-end
	xm = x + w / 2,       // x-middle
	ym = y + h / 2;       // y-middle


	var ctx = this.ctx;
	ctx.beginPath();
	ctx.moveTo(x, ym);
	ctx.bezierCurveTo(x, ym - oy, xm - ox, y, xm, y);
	ctx.bezierCurveTo(xm + ox, y, xe, ym - oy, xe, ym);
	ctx.bezierCurveTo(xe, ym + oy, xm + ox, ye, xm, ye);
	ctx.bezierCurveTo(xm - ox, ye, x, ym + oy, x, ym);
	ctx.fill();

        if(typeof storeHstry == 'undefined' || storeHstry)
            this.storeHistory();
       
    }

    private somedo(from,to){
        if(from.length < 2) return false;
        var curImg = from.pop();
        if(to.length == 0)
            to.push(curImg);
        var draw = from[from.length-1];
        to.push(draw);
        this.ctx.putImageData(draw,0,0);
        return from.length > 1;
    }

    public undo(){
        return this.somedo(this.history,this.redoHistory);
    }

    public redo(){
        return this.somedo(this.redoHistory,this.history);
    }

    private storeHistory(){
	var dat = this.ctx.getImageData(0,0,parseInt(this.canvas.attr("width")),parseInt(this.canvas.attr("height")));
        if(this.history.length == 5)
            this.history.shift();
        this.history.push(dat);
        this.redoHistory = [];
    }

    private storeCanvas(){
	this.beforeDraw = this.ctx.getImageData(0,0,this.canvas.width(),this.canvas.height());
    }
    private restoreCanvas(){
	this.ctx.putImageData(this.beforeDraw,0,0);
    }

    getScale(w,h){
	var wRatio = parseInt(this.canvas.attr("width"))/w;
	var hRatio = parseInt(this.canvas.attr("height"))/h;
	return Math.min(wRatio,hRatio);
    }

    pourBucket(x:number,y:number,r:number,g:number,b:number,tolerance:number){
	this.bucket.pourBucket(x,y,r,g,b,tolerance);
        this.storeHistory();
    }

    clear(){
	var prevFillStyle = this.ctx.fillStyle;
	this.ctx.fillStyle = "#FFFFFF";
	this.ctx.fillRect(0,0,this.canvas.width(),this.canvas.height());
	this.ctx.fillStyle = prevFillStyle;
        this.storeHistory();
    }
    
    private canvasOnClick(data){
	if(this.presentator != username || data.button !== 0) return;

        var off = this.getLeftAndTopOffset(this.canvas.get(0));
        var x = data.pageX-off.left, y = data.pageY-off.top;
	if(this.paintingTool == "pencil" || this.paintingTool == "erase"){
	    this.pressed = true;
	    this.poss = [x, y];
	    if(this.paintingTool == "erase"){
		this.ctx.fillStyle = this.ctx.strokeStyle = "#FFFFFF";
		this.ctx.lineWidth = this.eraserSize;
	    }else{
		this.ctx.fillStyle = this.ctx.strokeStyle = this.color.toHexString();
		this.ctx.lineWidth = this.penSize;
	    }
	    this.ctx.beginPath();
            this.ctx.moveTo(x,y);
	}else if(this.paintingTool == "bucket"){
	    var rgba = this.color.toRgb();
	    this.bucket.pourBucket(x,y,rgba.r,rgba.g,rgba.b,this.tolerance);
	    this.onDraw({
		tool:"bucket",
		tolerance:this.tolerance,
		x:x,y:y,
		r:rgba.r,g:rgba.g,b:rgba.b,
		resolution:{w:this.canvas.attr("width"),h:this.canvas.attr("height")}});
	}else if(this.paintingTool == "circle" || this.paintingTool == "square"){
	    this.startPos = {x:x,y:y};
	    this.pressed = true;
	    this.storeCanvas();
	}
    }

    private canvasOnRelease(data){
	if(data.button !== 0 || !this.pressed){
	    return;
	}

	this.pressed = false;
	
        var off = this.getLeftAndTopOffset(this.canvas.get(0));
        var x = data.pageX-off.left, y = data.pageY-off.top;
	
	if((this.paintingTool == "pencil" || this.paintingTool == "erase") && this.poss.length == 2){
		this.ctx.beginPath();
		var size = this.paintingTool == "pencil" ? this.penSize : this.eraserSize;
		this.ctx.arc(x, y, size/2, 0, 2 * Math.PI);
		this.ctx.fill();
	}
	if(this.paintingTool == "pencil"){
            var s = this.penSize;
            this.storeHistory();
            this.onDraw({
		tool:"pencil",
		poss:this.poss,
		color:this.color.toHexString(),
		size:s,
		resolution:{w:this.canvas.attr("width"),h:this.canvas.attr("height")}});
	}else if(this.paintingTool == "erase"){
            var s = this.eraserSize;
            this.storeHistory();
            this.onDraw({
		tool:"pencil",
		poss:this.poss,
		color:"#FFFFFF",
		size:this.eraserSize,
		resolution:{w:this.canvas.attr("width"),h:this.canvas.attr("height")}});
	}else if(this.paintingTool == "circle" || this.paintingTool == "square"){
            var sx = this.startPos.x, sy = this.startPos.y;
            this.storeHistory();
	    this.onDraw({
		tool:this.paintingTool,
		color:this.color.toHexString(),
		pos:{sx:sx,sy:sy,ex:x,ey:y},
		resolution:{w:this.canvas.attr("width"),h:this.canvas.attr("height")}});
	}
    }

    private canvasOnMove(data){

	if(this.paintingTool == "pencil" || this.paintingTool == "erase"){
	    this.cursorStalker.css({
		top:data.pageY - 46 - this.radius, //46 because of the css top value on the content
		left:data.pageX - this.radius
	    }); 
	}
	
	if(!this.pressed){return;}
	
        var off = this.getLeftAndTopOffset(this.canvas.get(0));
        var x = data.pageX-off.left, y = data.pageY-off.top;
	if(this.paintingTool == "pencil" || this.paintingTool == "erase"){
            this.poss.push(x);
            this.poss.push(y);
            this.ctx.lineTo(x,y);
            this.ctx.stroke();
	    this.ctx.beginPath();
	    this.ctx.moveTo(x,y);

	}else if(this.paintingTool == "circle" || this.paintingTool == "square"){
	    this.restoreCanvas();
	    var sx, sy;
	    sx = this.startPos.x;
	    sy = this.startPos.y;
	    if(this.paintingTool == "circle"){
		this.drawEllipse(sx,sy,x,y,this.color.toHexString(),false);
	    }else{
		this.drawSquare(sx,sy,x,y,this.color.toHexString(),false);
	    }
	}
    }

    private getLeftAndTopOffset(DOM){
	return {left: DOM.offsetLeft, top: DOM.offsetTop+46}; //The +46 is because of the css top property on #content
    }

}
