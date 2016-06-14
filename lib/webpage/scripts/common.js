var IPADDRESS = "127.0.0.1";
var KEYBOARDPORT = 9161;
var MOUSEPORT = 9162;
var GRAPHICALPORT = 9163;

//Container of dom elements
var CONTAINERNAME = "body";

//Prefix to new canvas element id's
var CANVASPREFIX = "canvas";


//Mouse Element Types
var CANVAS = "canvas";
var SVG = "svg";

//Used to get the height or width property of an element that
//either resides in style or as characteristic
function safeGetProperty(element, property) {
    var result;
    
    if(element.style) {
        if(element.style[property] && element[property]) {
            result = element.style[property];
        } else if (element[property]) {
            result = element[property];
        } else if (element.style[property]) {
            result = element.style[property];
        } else {
            result = "";
        }
    } else {
        if (element[property]) {
            result = element[property];
        } else {
            result = "";
        } 
    }
    
    return result;
}