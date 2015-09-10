

interface PageLoader{
    getResponseHandler() : ResponseHandler;
    onLoad(arg? : any) : void;
    onUnload() : void;
    getUrl() : string;
    getTitle() : string;
    getBackButton() : Button;
    getForwardButton() : Button;
}

interface DialogLoader{
    getResponseHandler() : ResponseHandler;
    onLoad(arg? : any) : void;
    onUnload() : void;
    getUrl() : string;
    getTitle() : string;
    getButtons() : Button[];
    closeable() : boolean;
    getDialogTitle() : string;
}

interface Button{
    text : string;
    click : ()=>void;
}

interface ResponseHandler{
    (receivedData : ServerResponse) : void;
}

interface ServerRequest{
    cmd : string;
    room? : string;
    data : any;
}

interface ChatMessage{
    sender:string;
    message:string;
}

interface RoomInfo{
    users:User[];
    presentator:string;
    timer:number;
    finTimer:number;
    timePassed:number;
    timerType:string;
    round:number;
    maxRound:number;
    name:string;
    word?:string;
    created?:boolean;
}

interface RoomElement{
    name:string;
    maxUsers:number;
    users:number;
    pass:boolean;
}

interface User{
    user:string;
    score:number;
    guessedRight:boolean;
}

interface ServerResponse{
    cmd: string;
    data?: any;
    error? : string;
}
