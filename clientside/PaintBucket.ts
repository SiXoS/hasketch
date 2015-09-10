class PaintBucket{

    private canvas ;
    private colorLayerData:ImageData;
    private width : number;
    private height : number;
    private curColor;
    private tolerance : number;
    constructor (private ctx : CanvasRenderingContext2D){
	this.canvas = ctx.canvas;
	this.width = parseInt(this.canvas.width);
	this.height = parseInt(this.canvas.height);
    }

    /**
     * Takes the position clicked at and the color of the paint in the bucket.  
     */
    pourBucket(x:number,y:number,r:number,g:number,b:number,tolerance:number){
	this.tolerance = tolerance;
	this.colorLayerData = this.ctx.getImageData(0,0,this.width,this.height);
	this.curColor = {r:r,g:g,b:b};
	var pixelPos = (y*this.width + x)*4;
	var sr = this.colorLayerData.data[pixelPos];
	var sg = this.colorLayerData.data[pixelPos+1];
	var sb = this.colorLayerData.data[pixelPos+2];
	if(Math.abs(r - sr) <= tolerance && Math.abs(g - sg) <= tolerance && Math.abs(b - sg) <= tolerance) return;
	this.ctx.clearRect(0,0,this.width,this.height);
	this.floodFill(x,y,sr,sg,sb);
	this.ctx.putImageData(this.colorLayerData,0,0);
    }

    private matchStartColor(pixelPos : number, startR:number, startG:number, startB:number):boolean {

	var r = this.colorLayerData.data[pixelPos];
	var g = this.colorLayerData.data[pixelPos + 1];
	var b = this.colorLayerData.data[pixelPos + 2];
	var tol = this.tolerance
	
	return Math.abs(r - startR) <= tol && Math.abs(g - startG) <= tol && Math.abs(b - startB) <= tol;
    }

    private colorPixel(pixelPos:number, r:number, g:number, b:number, a?:number) {

	this.colorLayerData.data[pixelPos] = r;
	this.colorLayerData.data[pixelPos + 1] = g;
	this.colorLayerData.data[pixelPos + 2] = b;
	this.colorLayerData.data[pixelPos + 3] = a !== undefined ? a : 255;
    }

    private floodFill(startX:number, startY:number, startR:number, startG:number, startB:number) {

	var newPos,
	x,
	y,
	pixelPos,
	reachLeft,
	reachRight,
	drawingBoundLeft = 0,
	drawingBoundTop = 0,
	drawingBoundRight = this.width-1,
	drawingBoundBottom = this.height-1,
	pixelStack = [[startX, startY]];

	while (pixelStack.length) {

	    newPos = pixelStack.pop();
	    x = newPos[0];
	    y = newPos[1];

	    // Get current pixel position
	    pixelPos = (y * this.width + x) * 4;

	    // Go up as long as the color matches and are inside the canvas
	    while (y >= drawingBoundTop && this.matchStartColor(pixelPos, startR, startG, startB)) {
		y -= 1;
		pixelPos -= this.width * 4;
	    }

	    pixelPos += this.width * 4;
	    y += 1;
	    reachLeft = false;
	    reachRight = false;

	    // Go down as long as the color matches and in inside the canvas
	    while (y <= drawingBoundBottom && this.matchStartColor(pixelPos, startR, startG, startB)) {
		y += 1;

		this.colorPixel(pixelPos, this.curColor.r, this.curColor.g, this.curColor.b);

		if (x > drawingBoundLeft) {
		    if (this.matchStartColor(pixelPos - 4, startR, startG, startB)) {
			if (!reachLeft) {
			    // Add pixel to stack
			    pixelStack.push([x - 1, y]);
			    reachLeft = true;
			}
		    } else if (reachLeft) {
			reachLeft = false;
		    }
		}

		if (x < drawingBoundRight) {
		    if (this.matchStartColor(pixelPos + 4, startR, startG, startB)) {
			if (!reachRight) {
			    // Add pixel to stack
			    pixelStack.push([x + 1, y]);
			    reachRight = true;
			}
		    } else if (reachRight) {
			reachRight = false;
		    }
		}

		pixelPos += this.width * 4;
	    }
	}
    }

}
