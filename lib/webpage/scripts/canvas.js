var DODEBUG = true;

function error(errorToLog) {
    console.log("Error: " + errorToLog);
}

function logActivity(activity) {
    if(DODEBUG) {
        console.log("Activity: " + activity);
    }
}

function handleIncomingMessage(event) {
    var jsonMessage = JSON.parse(event.data);
    handleCanvasMessage(jsonMessage);
}


var GRAPHICALWS = new Websocket( "Canvas"
                               , IPADDRESS
                               , GRAPHICALPORT
                               , graphicalConnectionOpen
                               , handleIncomingMessage
                               , reportOnError
                               , graphicalClose
                               );
                               
                               
function graphicalInit() {
    SOCKETSTOCONNECT.addToConnect(GRAPHICALWS);
}

graphicalInit();

function graphicalConnectionOpen(event) {
    //Destroy children
    resetContainer();
    setConnected.call(this, event);
}

function graphicalClose(event) {
    retryOnUncleanCloseSetUnconnected.call(this, event);
}

function resetContainer() {
    var container = getContainer();
    
    while(container.hasChildNodes()) {
        MOUSECAPTUREELEMENTS.removeCaptureElementByDomElement(container.firstChild);
        container.removeChild(container.firstChild);
    }
}

function getContainer() {
    return document.getElementById(CONTAINERNAME);
}

function getCanvas(canvasId) {
    return document.getElementById(CANVASPREFIX + canvasId);
}

function degreesToRads(degrees) {
    return degrees * (Math.PI / 180);
}

function screenColorToCanvasColor(screenColor) {
    return "rgba(" + screenColor[0] + "," + screenColor[1] + "," + screenColor[2] + "," + screenColor[3] + ")";
}

function sendMeasuredTextResponse(route, opCode, text, fontfamily, fontsize, alignment, width, height) {
    var responseObj = { 'r': route
                      , 'o': { 't': opCode
                             , 'canvastext': { 'text': text
                                             , 'font': { 'fontfamily': fontfamily
                                                       , 'fontsize': fontsize
                                                       }
                                             , 'alignment': alignment
                                             }
                             , 'width': width
                             , 'height': height
                             }
                      };
                      
    GRAPHICALWS.sendJSONObject(responseObj);
}

// Top Object
// ___________________________________________________
var SYSTEMROUTE = 's';
var USERROUTE = 'u';

function handleCanvasMessage(message) {
    switch(message.r) {
        case SYSTEMROUTE:
            handleSystemCanvasOut(message.o);
            break;
        case USERROUTE:
            handleCanvasOut(message.o);
            break;
        default:
            error("Undefined case in handleCanvasMessage");
            break;
    }
}

// ___________________________________________________
// ___________________________________________________
// SYSTEM ROUTE
// ___________________________________________________
// ___________________________________________________
// SystemCanvasOut
// ___________________________________________________
function handleSystemCanvasOut(systemCanvasOut) {
    switch(systemCanvasOut.t) {
        case SYSTEMMEASURETEXT:
            performSystemMeasureText(systemCanvasOut.a[0]);
            break;
        default:
            error("Undefined case in handleSystemCanvasOut");
            break;
    }
}

function performSystemMeasureText(canvasText) {
    performMeasureText(SYSTEMROUTE, SYSTEMMEASUREDTEXT, canvasText);
    logActivity("SystemMeasureText");
}


// ___________________________________________________
// ___________________________________________________
// USER ROUTE
// ___________________________________________________
// ___________________________________________________
// CanvasOut
// ___________________________________________________
function handleCanvasOut(canvasOut) {
    switch(canvasOut.t) {
        case SETUPCANVAS:
            performSetupCanvas(canvasOut.a[0], canvasOut.a[1], canvasOut.a[2], canvasOut.a[3]);
            break;
        case TEARDOWNCANVAS:
            performTeardownCanvas(canvasOut.a[0]);
            break;
        case CANVASOPERATIONS:
            performCanvasOperations(canvasOut.a[0], canvasOut.a[1]);
            break;
        case MEASURETEXT:
            performMeasureText(USERROUTE, MEASUREDTEXT, canvasOut.a[0]);
            break;
        default:
            error("Undefined case in handleCanvasOut");
            break;
    }
}

function performSetupCanvas(canvasId, zIndex, canvasDimensions, cssPosition) {
    var canvas = getCanvas(canvasId),
        ctx;

    if(!canvas) {
        canvas = document.createElement('canvas');
        buffer = document.createElement('canvas');

        ctx = canvas.getContext('2d');
        ctx.bufferCanvas = buffer;
        ctx.bufferContext = buffer.getContext('2d');
        ctx.bufferContext.clears = [];

        canvas.id = CANVASPREFIX + canvasId;
        canvas.style.position = "absolute";
        canvas.style.border = "";

        getContainer().appendChild(canvas);
    } else {
        ctx = canvas.getContext('2d');
    }
    
    canvas.width = canvasDimensions[0];
    ctx.bufferCanvas.width = canvasDimensions[0];
    canvas.height = canvasDimensions[1];
    ctx.bufferCanvas.height = canvasDimensions[1];
    canvas.style.zIndex = zIndex;
    canvas.style.left = "0px";
    canvas.style.top = "0px";
    
    handleCSSPosition(canvas, cssPosition);
    
    //If element falls of screen partially, adjust
    var boundingBox = canvas.getBoundingClientRect();
    
    if(boundingBox.left < 0) {
        canvas.style.marginLeft = (parseInt(canvas.style.marginLeft) - boundingBox.left) + "px";
    }
    
    if(boundingBox.top < 0) {
        canvas.style.marginTop = (parseInt(canvas.style.marginTop) - boundingBox.top) + "px";
    }
    

    ctx.textBaseline = "hanging";
    ctx.bufferContext.textBaseline = "hanging";
    logActivity("SetupCanvas");
    
    //Setup at mouse for capture
    MOUSECAPTUREELEMENTS.addCaptureElement(canvasId, CANVAS, canvas);
}

function performTeardownCanvas(canvasId){
    var canvas = getCanvas(canvasId);
    
    //Teardown at mouse for capture
    MOUSECAPTUREELEMENTS.removeCaptureElementByDomElement(canvas);
    
    if(canvas) {
        var container = getContainer();
        container.removeChild(canvas);
    }
    
    logActivity("TeardownCanvas");
}

function performCanvasOperations(canvasId, listCanvasOperation) {
    var ctx = getCanvas(canvasId).getContext('2d');
    
    for (var i = 0; i < listCanvasOperation.length; i++) {
        handleCanvasOperation(ctx.bufferContext, listCanvasOperation[i], ctx);
    }
    
    logActivity("CanvasOperations");
}

var measureTextCanvas = document.createElement('canvas');
function performMeasureText(route, responseOpCode, canvasText) {
    var ctx = measureTextCanvas.getContext('2d'),
        text = handleCanvasText(ctx, canvasText),
        measurements = ctx.measureText(text);
    
    var font = canvasText.a[1];
    var fontfamily = font.a[0];
    var fontsize = font.a[1];
    var alignment = canvasText.a[2];
    var width = Math.round(measurements.width);
    var height = Math.round(fontsize); //Approximation is as good as we are going to get in Canvas
    
    sendMeasuredTextResponse(route, responseOpCode, text, fontfamily, fontsize, alignment, width, height);
    
    logActivity("MeasureText");
}


// CanvasOperation
// ___________________________________________________
function handleCanvasOperation(ctx, canvasOperation, frontCtx) {
    switch(canvasOperation.t) {
        case DRAWPATH:
            performDrawPath( ctx
                           , canvasOperation.a[0]
                           , canvasOperation.a[1]
                           , canvasOperation.a[2]
                           , canvasOperation.a[3]
                           );
            break;
        case DRAWTEXT:
            performDrawText( ctx
                           , canvasOperation.a[0]
                           , canvasOperation.a[1]
                           , canvasOperation.a[2]
                           , canvasOperation.a[3]
                           );
            break;
        case DOTRANSFORM:
            performDoTransform(ctx, canvasOperation.a[0]);
            break;
        case CLEAR:
            performClear(ctx, canvasOperation.a[0]);
            break;
        case FRAME:
            performFrame(frontCtx);
            break;
        default:
            error("Undefined case in handleCanvasOperation");
            break;
    }
}

function performDrawPath(ctx, screenStartingPoint, listPathPart, pathStroke, pathFill) {
    ctx.beginPath();
    ctx.moveTo(screenStartingPoint[0], screenStartingPoint[1]);
    
    for(var i = 0; i < listPathPart.length; i++) {
        handleScreenPathPart(ctx, listPathPart[i]);
    }
    
    handlePathStroke(ctx, pathStroke);
    handlePathFill(ctx, pathFill);
    
    logActivity("DrawPath");
}

function performDrawText(ctx, canvasText, screenPoint, textStroke, textFill) {
    var text = handleCanvasText(ctx, canvasText);
    
    handleTextStroke(ctx, text, screenPoint, textStroke);
    handleTextFill(ctx, text, screenPoint, textFill);
    
    logActivity("DrawText");
}

function performDoTransform(ctx, canvasTransform) {
    handleCanvasTransform(ctx, canvasTransform);
    
    logActivity("DoTransform");
}

function performClear(ctx, clearPart) {
    handleClearPart(ctx, clearPart);
    
    logActivity("Clear");
}

function performFrame(frontCtx) {
    var bufferCtx = frontCtx.bufferContext;

    for(var i = 0; i < bufferCtx.clears.length; i++) {
        var clear = bufferCtx.clears[i];
        frontCtx.clearRect.apply(frontCtx, clear);
    }

    frontCtx.drawImage(frontCtx.bufferCanvas, 0, 0);
    logActivity("Frame");
}

// PathPart
// ___________________________________________________
function handleScreenPathPart(ctx, screenPathPart) {
    switch(screenPathPart.t) {
        case MOVETO:
            performMoveTo(ctx, screenPathPart.a[0]);
            break;
        case LINETO:
            performLineTo(ctx, screenPathPart.a[0]);
            break;
        case BEZIERCURVETO:
            performBezierCurveTo(ctx, screenPathPart.a[0], screenPathPart.a[1], screenPathPart.a[2]);
            break;
        case QUADRATICCURVETO:
            performQuadraticCurveTo(ctx, screenPathPart.a[0], screenPathPart.a[1]);
            break;
        case ARCTO:
            performArcTo(ctx, screenPathPart.a[0], screenPathPart.a[1], screenPathPart.a[2]);
            break;
        case ARC:
            performArc(ctx, screenPathPart.a[0], screenPathPart.a[1], screenPathPart.a[2]);
            break;
        case RECTANGLE:
            performRectangle(ctx, screenPathPart.a[0], screenPathPart.a[1]);
            break;
        default:
            error("Undefined case in handleScreenPathPart");
            break;
    }
}

function performMoveTo(ctx, screenPoint) {
    ctx.moveTo(screenPoint[0], screenPoint[1]);
    
    logActivity("MoveTo");
}

function performLineTo(ctx, screenPoint) {
    ctx.lineTo(screenPoint[0], screenPoint[1]);
    
    logActivity("LineTo");
}

function performBezierCurveTo(ctx, screenControlPoint1, screenControlPoint2, screenEndPoint) {
    ctx.bezierCurveTo( screenControlPoint1[0]
                     , screenControlPoint1[1]
                     , screenControlPoint2[0]
                     , screenControlPoint2[1]
                     , screenEndPoint[0]
                     , screenEndPoint[1]
                     );
                     
    logActivity("BezierCurveTo");
}

function performQuadraticCurveTo(ctx, screenControlPoint, screenEndPoint) {
    ctx.quadratricCurveTo( screenControlPoint[0]
                         , screenControlPoint[1]
                         , screenEndPoint[0]
                         , screenEndPoint[1]
                         );
    logActivity("QuadraticCurveTo");
}

function performArcTo(ctx, screenControlPoint1, screenControlPoint2, screenRadius) {
    ctx.arcTo( screenControlPoint1[0]
             , screenControlPoint1[1]
             , screenControlPoint2[0]
             , screenControlPoint2[1]
             , screenRadius
             );
             
    logActivity("ArcTo");
}

function performArc(ctx, screenCircle, screenStartingAngle, screenEndAngle) {
    var circleScreenPoint = screenCircle[0];
    var circleScreenRadius = screenCircle[1];
    var startingAngleRads = degreesToRads(screenStartingAngle);
    var endAngleRads = degreesToRads(screenEndAngle);
    
    ctx.arc( circleScreenPoint[0]
           , circleScreenPoint[1]
           , circleScreenRadius
           , startingAngleRads
           , endAngleRads
           , true
           );
           
    logActivity("Arc");
}

function performRectangle(ctx, screenPoint, screenDimensions) {
    ctx.rect( screenPoint[0]
            , screenPoint[1]
            , screenDimensions[0]
            , screenDimensions[1]
            );
            
    logActivity("Rectangle");
}


// PathStroke
// ___________________________________________________
function handlePathStroke(ctx, pathStroke) {
    switch(pathStroke.t) {
        case PATHSTROKE:
            performPathStroke(ctx, pathStroke.a[0], pathStroke.a[1]);
            break;
        case NOPATHSTROKE:
            break;
        default:
            error("Undefined case in handlePathStroke");
            break;
    }
}

function performPathStroke(ctx, lineThickness, renderStyle) {
    handleRenderStrokeStyle(ctx, renderStyle);
    ctx.lineWidth = lineThickness;
    ctx.stroke();
    
    logActivity("PathStroke");
}


// PathFill
// ___________________________________________________
function handlePathFill(ctx, pathFill) {
    switch(pathFill.t) {
        case PATHFILL:
            performPathFill(ctx, pathFill.a[0]);
            break;
        case NOPATHFILL:
            break;
        default:
            error("Undefined case in handlePathFill");
            break;
    }
}

function performPathFill(ctx, renderStyle) {
    handleRenderFillStyle(ctx, renderStyle);
    ctx.fill();
    
    logActivity("PathFill");
}


// PathRender(Stroke/Fill)Style
// ___________________________________________________
function handleRenderStrokeStyle(ctx, renderStrokeStyle) {
    var style;
    
    switch(renderStrokeStyle.t) {
        case CANVASCOLOR:
            style = performCanvasColor(ctx, renderStrokeStyle.a[0]);
            break;
        case CANVASGRADIENT:
            style = performCanvasGradient(ctx, renderStrokeStyle.a[0], renderStrokeStyle.a[1]);
            break;
        case CANVASPATTERN:
            style = performCanvasPattern(ctx, renderStrokeStyle.a[0], renderStrokeStyle.a[1]);
            break;
        default:
            error("Undefined case in handleRenderStrokeStyle");
            break;
    }
    
    ctx.strokeStyle = style;
}

function handleRenderFillStyle(ctx, renderFillStyle) {
    var style;
    
    switch(renderFillStyle.t) {
        case CANVASCOLOR:
            style = performCanvasColor(ctx, renderFillStyle.a[0]);
            break;
        case CANVASGRADIENT:
            style = performCanvasGradient(ctx, renderFillStyle.a[0], renderFillStyle.a[1]);
            break;
        case CANVASPATTERN:
            style = performCanvasPattern(ctx, renderFillStyle.a[0], renderFillStyle.a[1]);
            break;
        default:
            error("Undefined case in handlerenderFillStyle");
            break;
    }
    
    ctx.fillStyle = style;
}

function performCanvasColor(ctx, screenColor) {
    logActivity("CanvasColor");
    return screenColorToCanvasColor(screenColor);
}

function performCanvasGradient(ctx, canvasGradientType, listCanvasColorStop) {
    var gradient = handleCanvasGradientType(ctx, canvasGradientType);
    
    for(var i = 0; i < listCanvasColorStop.length; i++) {
        var canvasColorStop = listCanvasColorStop[i];
        
        gradient.addColorStop(canvasColorStop[0], screenColorToCanvasColor(canvasColorStop[1]));
    }
    
    logActivity("CanvasGradient");
    
    return gradient;
}

function performCanvasPattern(ctx, canvasImage, patternRepetition) {
    var image = handleCanvasImage(ctx, canvasImage);
    var repetition = handlePatternRepetition(patternRepetition);

    logActivity("CanvasPattern");
    
    return ctx.createPattern(image, repetition);
}

// CanvasImage
// ___________________________________________________
function handleCanvasImage(ctx, canvasImage) {
    var image;
    
    switch(canvasImage.t) {
        case CANVASELEMENT:
            image = performCanvasElement(canvasImage.a[0], canvasImage.a[1], canvasImage.a[2]);
            break;
        case IMAGEDATA:
            image = performImageData(ctx, canvasImage.a[0], canvasImage.a[1]);
            break;
        default:
            error("Undefined case in handleCanvasImage");
            break;
    }
    
    return image;
}

function performCanvasElement(canvasId, screenPoint, screenDimensions) {
    logActivity("CanvasElement");
    return getCanvas(canvasId).getContext('2d').getImageData(screenPoint[0], screenPoint[1], screenDimensions[0], screenDimensions[1]);
}

function performImageData(ctx, screenDimensions, listScreenPixel) {
    var imageData = ctx.createImageData(screenDimensions[0], screenDimensions[1]);
    imageData.data.set(listScreenPixel);
    
    logActivity("ImageData");
    
    return imageData;
}

// PatternRepetition
// ___________________________________________________
function handlePatternRepetition(patternRepetition) {
    var repetition;
    
    switch(patternRepetition.t) {
        case REPEAT:
            repetition = "repeat";
            break;
        case REPEATX:
            repetition = "repeat-x";
            break;
        case REPEATY:
            repetition = "repeat-y";
            break;
        case NOREPEAT:
            repetition = "no-repeat";
            break;
        default:
            error("Undefined case in handlePatternRepetition");
            break;
    }
    
    return repetition;
}


// CanvasGradientType
// ___________________________________________________
function handleCanvasGradientType(ctx, canvasGradientType) {
    var gradient;
    
    switch(canvasGradientType.t) {
        case RADIALGRADIENT:
            gradient = performRadialGradient(ctx, canvasGradientType.a[0], canvasGradientType.a[1]);
            break;
        case LINEARGRADIENT:
            gradient = performLinearGradient(ctx, canvasGradientType.a[0], canvasGradientType.a[1]);
            break;
        default:
            error("Undefined case in handleCanvasGradient");
            break;
    }
    
    return gradient;
}

function performRadialGradient(ctx, screenCircle1, screenCircle2) {
    var screenPoint1 = screenCircle1[0];
    var screenRadius1 = screenCircle1[1];
    var screenPoint2 = screenCircle2[0];
    var screenRadius2 = screenCircle2[1];
    
    var gradient = ctx.createRadialGradient( screenPoint1[0]
                                           , screenPoint1[1]
                                           , screenRadius1
                                           
                                           , screenPoint2[0]
                                           , screenPoint2[1]
                                           , screenRadius2
                                           );
                                           
    logActivity("RadialGradient");
                            
    return gradient;
}

function performLinearGradient(ctx, screenPoint1, screenPoint2) {
    logActivity("LinearGradient");
    return ctx.createLinearGradient(screenPoint1[0], screenPoint1[1], screenPoint2[0], screenPoint2[1]);
}


// CanvasText
// ___________________________________________________
function handleCanvasText(ctx, canvasText) {
    var text = canvasText.a[0];
    ctx.font = handleFont(ctx, canvasText.a[1]);
    ctx.textAlign = handleAlignment(canvasText.a[2]);
    
    logActivity("handleCanvasText");
    
    return text;
}


// Font
// ___________________________________________________
function handleFont(ctx, font) {
    logActivity("handleFont");

    return font.a[1] + "px " + font.a[0];
}


// TextStroke
// ___________________________________________________
function handleTextStroke(ctx, text, screenPoint, textStroke) {
    switch(textStroke.t) {
        case TEXTSTROKE:
            performTextStroke(ctx, text, screenPoint, textStroke.a[0], textStroke.a[1]);
            break;
        case NOTEXTSTROKE:
            break;
    }
}

function performTextStroke(ctx, text, screenPoint, screenLineThickness, textStrokeRenderStyle) {
    ctx.lineWidth = screenLineThickness + "px";
    handleRenderStrokeStyle(ctx, textStrokeRenderStyle);
    
    ctx.strokeText(text, screenPoint[0], screenPoint[1]);
}

// TextFill
// ___________________________________________________
function handleTextFill(ctx, text, screenPoint, textFill) {
    switch(textFill.t) {
        case TEXTFILL:
            performTextFill(ctx, text, screenPoint, textFill.a[0]);
            break;
        case NOTEXTFILL:
            break;
    }
}

function performTextFill(ctx, text, screenPoint, textFillRenderStyle) {
    handleRenderFillStyle(ctx, textFillRenderStyle);
    
    ctx.fillText(text, screenPoint[0], screenPoint[1]);
}

// Alignment
// ___________________________________________________
function handleAlignment(alignment) {
    var result;
    
    switch(alignment.t) {
        case ALIGNLEFT:
            result = "left";
            break;
        case ALIGNRIGHT:
            result = "right";
            break;
        case ALIGNCENTER:
            result = "center";
            break;
        default:
            error("Undefined case in handleAlignment");
            break;
    }
    
    return result;
}


// CanvasTransform
// ___________________________________________________
function handleCanvasTransform(ctx, canvasTransform) {
    switch(canvasTransform.t) {
        case SAVE:
            performSave(ctx);
            break;
        case RESTORE:
            performRestore(ctx);
            break;
        case TRANSLATE:
            performTranslate(ctx, canvasTransform.a[0]);
            break;
        case ROTATE:
            performRotate(ctx, canvasTransform.a[0]);
            break;
        case SCALE:
            performScale(ctx, canvasTransform.a[0], canvasTransform.a[1]);
            break;
        case TRANSFORM:
            performTransform(ctx, canvasTransform.a[0]);
            break;
        case SETTRANSFORM:
            performSetTransform(ctx, canvasTransform.a[0]);
            break;
        case RESETTRANSFORM:
            performResetTransform(ctx);
            break;
        default:
            error("Undefined case in handleCanvasTransform");
            break;
    }
}

function performSave(ctx) {
    logActivity("Save");
    ctx.save();
}

function performRestore(ctx) {
    logActivity("Restore");
    ctx.restore();
}

function performTranslate(ctx, screenPoint) {
    logActivity("Translate");
    ctx.translate(screenPoint[0], screenPoint[1]);
}

function performRotate(ctx, screenAngle) {
    logActivity("Rotate");
    ctx.rotate(degreesToRads(screenAngle));
}

function performScale(ctx, scaleX, scaleY) {
    logActivity("Scale");
    ctx.scale(scaleX, scaleY);
}

function performTransform(ctx, transformationMatrix) {
    logActivity("Transform");
    ctx.transform( transformationMatrix[0]
                 , transformationMatrix[1]
                 , transformationMatrix[2]
                 , transformationMatrix[3]
                 , transformationMatrix[4]
                 , transformationMatrix[5]
                 );
}

function performSetTransform(ctx, transformationMatrix) {
    logActivity("SetTransform");
    ctx.setTransform( transformationMatrix[0]
                    , transformationMatrix[1]
                    , transformationMatrix[2]
                    , transformationMatrix[3]
                    , transformationMatrix[4]
                    , transformationMatrix[5]
                    );
}

function performResetTransform(ctx) {
    logActivity("ResetTransform");
    ctx.resetTransform();
}


// CSSPosition
// ___________________________________________________
function handleCSSPosition(element, cssPosition) {
    switch(cssPosition.t) {
        case CSSPOSITION:
            performCSSPosition(element, cssPosition.a[0], cssPosition.a[1]);
            break;
        default:
            error("Undefined case in handleCSSPosition");
            break;
    }
}

function performCSSPosition(element, cssBindPoint, cssMeasurements) {
    logActivity("CSSPosition");
    handleCSSBindPoint(element, cssBindPoint);
    handleCSSMeasurements(element, cssMeasurements);
}


// CSSBindPoint
// ___________________________________________________
function handleCSSBindPoint(element, cssBindPoint) {
    switch(cssBindPoint.t) {
        case CSSFROMCENTER:
            performCSSFromCenter(element);
            break;
        case CSSFROMDEFAULT:
            performCSSFromDefault(element);
            break;
        default:
            error("Undefined case in handleCSSBindPoint");
            break;
    }
}

function performCSSFromCenter(element) {
    logActivity("CSSFromCenter");
    var elementWidth = safeGetProperty(element, "width");
    var elementHeight = safeGetProperty(element, "height");
    var elementLeftOffset = Math.round(elementWidth / 2);
    var elementTopOffset = Math.round(elementHeight / 2);
    
    element.style.marginTop = "-" + elementTopOffset + "px";
    element.style.marginRight = "0px";
    element.style.marginBottom = "0px";
    element.style.marginLeft = "-" + elementLeftOffset + "px";
}

function performCSSFromDefault(element) {
    logActivity("CSSFromDefault");
}


// CSSMeasurements
// ___________________________________________________
function handleCSSMeasurements(element, cssMeasurements) {
    logActivity("CSSMeasurements");
    element.style.left = handleCSSUnit(cssMeasurements[0]);
    element.style.top = handleCSSUnit(cssMeasurements[1]);
}


// CSSUnit
// ___________________________________________________
function handleCSSUnit(cssUnit) {
    var result;
    
    switch(cssUnit.t) {
        case CSSPIXELS:
            result = cssUnit.a[0] + "px";
            break;
        case CSSPERCENTAGE:
            result = cssUnit.a[0] + "%";
            break;
        default:
            error("Undefined case in handleCSSUnit");
            break;
    }
    
    return result;
}


// ClearPart
// ___________________________________________________
function handleClearPart(ctx, clearPart) {
    switch(clearPart.t) {
        case CLEARRECTANGLE:
            performClearRectangle(ctx, clearPart.a[0], clearPart.a[1]);
            break;
        case CLEARCANVAS:
            performClearCanvas(ctx);
            break;
        default:
            error("Undefined case in handleClearPart");
            break;
    }
}

function performClearRectangle(ctx, screenPoint, screenDimensions) {
    logActivity("ClearRectangle");
    ctx.clears.push([screenPoint[0], screenPoint[1], screenDimensions[0], screenDimensions[1]]);
    ctx.clearRect(screenPoint[0], screenPoint[1], screenDimensions[0], screenDimensions[1]);
}

function performClearCanvas(ctx) {
    logActivity("ClearCanvas");
    ctx.clears.push([0, 0, ctx.canvas.width, ctx.canvas.height]);
    ctx.clearRect(0, 0, ctx.canvas.width, ctx.canvas.height);
}