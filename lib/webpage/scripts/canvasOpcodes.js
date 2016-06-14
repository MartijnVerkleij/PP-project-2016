//Opcodes

//SystemCanvasIn
var SYSTEMMEASUREDTEXT = 2101;

//SystemCanvasOut
var SYSTEMMEASURETEXT = 2001;

//CanvasIn
var MEASUREDTEXT = 101;

//CanvasOut
var SETUPCANVAS = 201;
var TEARDOWNCANVAS = 202;
var CANVASOPERATIONS = 203;
var MEASURETEXT = 204;

//CanvasOperationType
var DRAWPATH = 301;
var DRAWTEXT = 302;
var DOTRANSFORM = 303;
var CLEAR = 304;
var FRAME = 305;

//ScreenPathPart
var MOVETO = 401;
var LINETO = 402;
var BEZIERCURVETO = 403;
var QUADRATICCURVETO = 404;
var ARCTO = 405;
var ARC = 406;
var RECTANGLE = 407;

//PathStroke
var PATHSTROKE = 501;
var NOPATHSTROKE = 502;

//PathFill
var PATHFILL = 601;
var NOPATHFILL = 602;

//RenderStyle
var CANVASCOLOR = 701;
var CANVASGRADIENT = 702;
var CANVASPATTERN = 703;

//CanvasImage
var CANVASELEMENT = 801;
var IMAGEDATA = 802;

//PatternRepetition
var REPEAT = 901;
var REPEATX = 902;
var REPEATY = 903;
var NOREPEAT = 904;

//CanvasGradientType
var RADIALGRADIENT = 1001;
var LINEARGRADIENT = 1002;

//CanvasText
var CANVASTEXT = 1201;

//Font
var FONT = 1301;

//TextStroke
var TEXTSTROKE = 1401;
var NOTEXTSTROKE = 1402;

//TextFill
var TEXTFILL = 2401;
var NOTEXTFILL = 2402;

//Alignment
var ALIGNLEFT = 1501;
var ALIGNRIGHT = 1502;
var ALIGNCENTER = 1503;

//CanvasTransform
var SAVE = 1601;
var RESTORE = 1602;
var TRANSLATE = 1603;
var ROTATE = 1604;
var SCALE = 1605;
var TRANSFORM = 1606;
var SETTRANSFORM = 1607;
var RESETTRANSFORM = 1608;

//CSSUnit
var CSSPIXELS = 1801;
var CSSPERCENTAGE = 1802;

//CSSPosition
var CSSPOSITION = 2201;

//CSSBINDPOINT
var CSSFROMCENTER = 2301;
var CSSFROMDEFAULT = 2302;

//ClearPart
var CLEARRECTANGLE = 1901;
var CLEARCANVAS = 1902;