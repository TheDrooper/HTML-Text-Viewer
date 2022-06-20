&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
/*------------------------------------------------------------------------ 
  
    File        : htmltextviewer.w
    Description : View a web page's HTML text
    Author      : David Zadrozny
    Created     : 18-Feb-2022
    Notes       :

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Constant Definitions ---                                             */
&Scoped-define CfgFile "htmltextview.cfg"

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE cTxt AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMsgRead AS CHARACTER NO-UNDO.
DEFINE VARIABLE hSocket AS HANDLE NO-UNDO.

/* I/O Stream Definitions ---                                           */
DEFINE STREAM sFileIn.
DEFINE STREAM sFileOut.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fFrame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS cURL cIP iPort iTimeOut btnSend cReply ~
btnExit 
&Scoped-Define DISPLAYED-OBJECTS cURL cIP iPort iTimeOut cReply cLblURL ~
cStat dtTime cLblReply 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD htmlEncode wWin 
FUNCTION htmlEncode RETURNS CHARACTER
  ( INPUT pcURL AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnExit 
     LABEL "E&xit" 
     SIZE 14 BY 1.05.

DEFINE BUTTON btnSend 
     LABEL "&Send Request" 
     SIZE 18 BY 1.05.

DEFINE VARIABLE cReply AS LONGCHAR 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL LARGE
     SIZE 170.2 BY 22.38 NO-UNDO.

DEFINE VARIABLE cURL AS CHARACTER INITIAL "http://" 
     VIEW-AS EDITOR MAX-CHARS 4096 SCROLLBAR-VERTICAL
     SIZE 170 BY 4 NO-UNDO.

DEFINE VARIABLE cIP AS CHARACTER FORMAT "X(15)":U 
     LABEL "IP" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE cLblReply AS CHARACTER FORMAT "X(256)":U INITIAL "Reply:" 
      VIEW-AS TEXT 
     SIZE 6 BY .62 NO-UNDO.

DEFINE VARIABLE cLblURL AS CHARACTER FORMAT "X(256)":U INITIAL "URL:" 
      VIEW-AS TEXT 
     SIZE 5 BY .62 NO-UNDO.

DEFINE VARIABLE cStat AS CHARACTER FORMAT "X(1024)":U INITIAL "Ready." 
     LABEL "Status" 
      VIEW-AS TEXT 
     SIZE 78.2 BY .62 NO-UNDO.

DEFINE VARIABLE dtTime AS DATETIME FORMAT "99/99/9999 HH:MM:SS":U 
     LABEL "Time" 
      VIEW-AS TEXT 
     SIZE 29 BY .62 NO-UNDO.

DEFINE VARIABLE iPort AS INTEGER FORMAT ">>>>>9":U INITIAL 80 
     LABEL "Port" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE iTimeOut AS INTEGER FORMAT ">>9":U INITIAL 60 
     LABEL "Timeout" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fFrame
     cURL AT ROW 1.71 COL 10 NO-LABEL WIDGET-ID 2
     cIP AT ROW 6.24 COL 7.8 COLON-ALIGNED
     iPort AT ROW 6.24 COL 49 COLON-ALIGNED
     iTimeOut AT ROW 6.24 COL 94 COLON-ALIGNED
     btnSend AT ROW 6.24 COL 162
     cReply AT ROW 8.86 COL 9.8 NO-LABEL
     btnExit AT ROW 31.95 COL 166
     cLblURL AT ROW 1.71 COL 4 NO-LABEL WIDGET-ID 4
     cStat AT ROW 7.67 COL 7.8 COLON-ALIGNED
     dtTime AT ROW 7.67 COL 94 COLON-ALIGNED
     cLblReply AT ROW 8.86 COL 3 NO-LABEL
     "sec." VIEW-AS TEXT
          SIZE 6 BY .62 AT ROW 6.38 COL 103.8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 184.2 BY 32.67
         FONT 4.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "HTML Text Viewer"
         HEIGHT             = 32.67
         WIDTH              = 184.2
         MAX-HEIGHT         = 32.67
         MAX-WIDTH          = 212
         VIRTUAL-HEIGHT     = 32.67
         VIRTUAL-WIDTH      = 212
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR FRAME fFrame
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN cLblReply IN FRAME fFrame
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN cLblURL IN FRAME fFrame
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       cReply:RETURN-INSERTED IN FRAME fFrame  = TRUE.

/* SETTINGS FOR FILL-IN cStat IN FRAME fFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN dtTime IN FRAME fFrame
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* HTML Text Viewer */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* HTML Text Viewer */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnExit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnExit wWin
ON CHOOSE OF btnExit IN FRAME fFrame /* Exit */
DO:
    APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSend
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSend wWin
ON CHOOSE OF btnSend IN FRAME fFrame /* Send Request */
DO:
    RUN sendRequest.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  
    RUN readConfigFile.

    RUN enable_UI.
  
    RUN updateStatus (INPUT "Ready", INPUT 0, INPUT 0).

    IF NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE assignFrame wWin 
PROCEDURE assignFrame :
/*------------------------------------------------------------------------------
  Purpose: Assign fields on the frame.
  Notes:      
------------------------------------------------------------------------------*/

    DO WITH FRAME {&FRAME-NAME}:
    
        ASSIGN {&DISPLAYED-OBJECTS}.    
        
    END.  /* DO WITH FRAME */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE connectToSite wWin 
PROCEDURE connectToSite :
/*------------------------------------------------------------------------------
  Purpose: Create a socket connection to the specified IP/port.
  Notes:      
------------------------------------------------------------------------------*/

    DEFINE INPUT PARAMETER pcURL AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER pcHostIP AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER pcPortNum AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER pcMsg AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER piRespTime AS INT64 NO-UNDO.
    DEFINE OUTPUT PARAMETER plTimeOut AS LOGICAL NO-UNDO.

    DEFINE VARIABLE cReqStr AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lWaitLoop AS LOGICAL NO-UNDO INITIAL TRUE.
    DEFINE VARIABLE iStartTime AS INT64 NO-UNDO.
    DEFINE VARIABLE iSendTime AS INT64 NO-UNDO.

    
    RUN updateStatus (INPUT "Send", INPUT 0, INPUT 0).
    cMsgRead = "".
    iSendTime = ETIME.

    /* Open connection */
    CREATE SOCKET hSocket.
    hSocket:SET-READ-RESPONSE-PROCEDURE("readSocketMsg").
    hSocket:CONNECT("-H " + pcHostIP + " -S " + pcPortNum).

    /* Create request string, send message */
    cReqStr = "GET " + pcURL + " HTTP/1.0" + "~n~n~n".
    RUN sendSocketMsg (INPUT hSocket, INPUT cReqStr).

    /* Wait for reply */
    iStartTime = ETIME.
    DO WHILE hSocket:CONNECTED() = YES AND ((ETIME - iStartTime) < (iTimeOut * 1000)):
    
        RUN updateStatus (INPUT "Read", INPUT 0, INPUT 0).
        WAIT-FOR READ-RESPONSE OF hSocket PAUSE 1.
        PROCESS EVENTS.

    END.  /* DO WHILE hSocket:CONNECTED() */

    /* Check to see if the request has timed out */
    IF (ETIME - iStartTime) > (iTimeOut * 1000) THEN
        plTimeOut = TRUE.    

    piRespTime = ETIME - iSendTime.
    pcMsg = cMsgRead.

    /* Close connection */
    hSocket:DISCONNECT().
    hSocket = ?.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI wWin  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
  THEN DELETE WIDGET wWin.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI wWin  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY cURL cIP iPort iTimeOut cReply cLblURL cStat dtTime cLblReply 
      WITH FRAME fFrame IN WINDOW wWin.
  ENABLE cURL cIP iPort iTimeOut btnSend cReply btnExit 
      WITH FRAME fFrame IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-fFrame}
  VIEW wWin.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE readConfigFile wWin 
PROCEDURE readConfigFile :
/*------------------------------------------------------------------------------
  Purpose: Read values from the config file.    
  Notes:       
------------------------------------------------------------------------------*/

    FILE-INFO:FILE-NAME = {&CfgFile}.
    IF FILE-INFO:PATHNAME = ? THEN RETURN.

    INPUT STREAM sFileIn FROM VALUE({&CfgFile}).

    IMPORT STREAM sFileIn
        cURL
        cIP
        iPort
        iTimeOut.

    INPUT STREAM sFileIn CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE readSocketMsg wWin 
PROCEDURE readSocketMsg :
/*------------------------------------------------------------------------------
  Purpose: Read a message coming from the socket.    
  Notes:       
------------------------------------------------------------------------------*/
   
    DEFINE VARIABLE iMsgLength AS INTEGER NO-UNDO.
    DEFINE VARIABLE mMsg AS MEMPTR NO-UNDO.
    DEFINE VARIABLE iBytesAvail AS INTEGER NO-UNDO.
   

    iBytesAvail = hSocket:GET-BYTES-AVAILABLE(). 
    IF iBytesAvail = 0 THEN RETURN.

    SET-SIZE(mMsg) = iBytesAvail + 1.

    hSocket:READ(mMsg, 1, iBytesAvail).
   
    cMsgRead = cMsgRead + GET-STRING(mMsg, 1, iBytesAvail).
   
    SET-SIZE(mMsg) = 0.  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sendRequest wWin 
PROCEDURE sendRequest :
/*------------------------------------------------------------------------------
  Purpose: Send a request using the screen values.    
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE VARIABLE cMsg AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iRespTime AS INTEGER NO-UNDO.
    DEFINE VARIABLE lTimeOut AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lResult AS LOGICAL NO-UNDO.


    RUN assignFrame.
    RUN validateValues (OUTPUT lResult).
    IF NOT lResult THEN RETURN.

    RUN writeConfigFile.

    cURL = htmlEncode(cURL).

    RUN connectToSite (INPUT cURL, INPUT cIP, INPUT iPort, OUTPUT cMsg, OUTPUT iRespTime, OUTPUT lTimeOut).

    IF lTimeout THEN
        RUN updateStatus (INPUT "Timeout", INPUT 0, INPUT 0).
    ELSE
        RUN updateStatus (INPUT "Done", INPUT LENGTH(cMsg), INPUT (iRespTime / 1000)).

    cReply:SCREEN-VALUE IN FRAME {&FRAME-NAME} = cMsg.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sendSocketMsg wWin 
PROCEDURE sendSocketMsg :
/*------------------------------------------------------------------------------
  Purpose: Send a string through the socket.     
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE INPUT PARAMETER phSocket AS HANDLE NO-UNDO.
    DEFINE INPUT PARAMETER pcMsg AS CHARACTER NO-UNDO.

    DEFINE VARIABLE mSendMsg AS MEMPTR NO-UNDO.
                               

    DO:
        SET-SIZE(mSendMsg) =  LENGTH(pcMsg) + 1.
        PUT-STRING(mSendMsg, 1) = pcMsg.
        phSocket:WRITE(mSendMsg, 1, LENGTH(pcMsg)).
        SET-SIZE(mSendMsg) = 0.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE updateStatus wWin 
PROCEDURE updateStatus :
/*------------------------------------------------------------------------------
  Purpose: Display the current status & time.     
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE INPUT PARAMETER pcStat AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER piBytes AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER pdSec AS DECIMAL NO-UNDO.

    DEFINE VARIABLE cMsg AS CHARACTER NO-UNDO.


    /* Convert status code to a description */
    CASE pcStat:
        WHEN "Ready" THEN cMsg = "Ready.".
        WHEN "Send" THEN cMsg = "Sending request...".
        WHEN "Read" THEN cMsg = "Receiving reply...".
        WHEN "Done" THEN cMsg = "Received " + STRING(piBytes) + " bytes in " + STRING(pdSec, ">>>9.9<<") + " seconds.".
        WHEN "Timeout" THEN cMsg = "Request timed out.".
    END CASE. 

    /* Update description on the screen */
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            cStat:SCREEN-VALUE = cMsg
            dtTime:SCREEN-VALUE = STRING(NOW).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validateValues wWin 
PROCEDURE validateValues :
/*------------------------------------------------------------------------------
  Purpose: Validate screen values before sending request.    
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE OUTPUT PARAMETER plPass AS LOGICAL NO-UNDO INITIAL TRUE.

    DEFINE VARIABLE iLoop AS INTEGER NO-UNDO.
    DEFINE VARIABLE iResult AS INTEGER NO-UNDO.

    
    /* Validate IP address */
    IF cIP = "" OR cIP = ? THEN
    DO:
        plPass = FALSE.
        MESSAGE "The IP address is invalid." VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.
    
    DO iLoop = 1 TO 4:
        iResult = INTEGER(ENTRY(iLoop, cIP, ".")) NO-ERROR.

        IF iResult < 0 OR iResult > 255 OR iResult = ? THEN
            plPass = FALSE.
    END.

    IF NOT plPass THEN
    DO:
        MESSAGE "The IP address is invalid." VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.
    
    /* Validate port */
    IF iPort > 65535 THEN
    DO:
        MESSAGE "The port is invalid." VIEW-AS ALERT-BOX ERROR.
        RETURN.    
    END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE writeConfigFile wWin 
PROCEDURE writeConfigFile :
/*------------------------------------------------------------------------------
  Purpose: Write values to the config file.    
  Notes:       
------------------------------------------------------------------------------*/

     OUTPUT STREAM sFileOut TO VALUE({&CfgFile}).

     EXPORT STREAM sFileOut
        cURL
        cIP
        iPort
        iTimeOut
        SKIP.

    OUTPUT STREAM sFileOut CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION htmlEncode wWin 
FUNCTION htmlEncode RETURNS CHARACTER
  ( INPUT pcURL AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose: Converts various ASCII characters to their HTML representation
           to prevent problems with invalid HTML. 
    Notes: Based on cgiutil.i's html-encode function.
------------------------------------------------------------------------------*/
      
    ASSIGN
        pcURL = REPLACE(pcURL, "&":U, "&amp~;":U)       /* ampersand */
        pcURL = REPLACE(pcURL, "~"":U, "&quot~;":U)     /* quote */    
        pcURL = REPLACE(pcURL, "<":U, "&lt~;":U)        /* < */        
        pcURL = REPLACE(pcURL, ">":U, "&gt~;":U)        /* > */        
        pcURL = REPLACE(pcURL, " ":U, "%20":U).         /* space */
    
    RETURN pcURL.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

