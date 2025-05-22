/*
 *  QUERYXML.PRG
 *
 *  Contents:
 *      Example that shows how to set action codeblock to specific
 *      tags in a XML document tree.
 *
 */

#include "asxml.ch"

/*
 * add library request
 */
#pragma library("asxml10.lib")


/*
 * queryxml <cXMLFileName>
 *
 * Instead of opening, reading and parsing the file directly with
 * a call to XMLDocOpenFile() this samples reads the contents of the
 * file into a string and uses function XMLDocOpenString() to build
 * a XML document tree.
 */
PROC Main(cFileName)
    LOCAL nXMLDoc, cString

    IF(cFileName == NIL)
        ? "usage: queryxml.exe <cFileName>"
        QUIT
    ENDIF

    /*
     * test if file exists
     */
    IF !File(cFileName)
        ? "Error: file " + cFileName + " does not exist!"
        QUIT
    ENDIF

    /*
     * read contents of file into a string variable 
     */
    cString := MemoRead(cFileName)

    IF Len(cString) == 0
        ? "Error: unable to read file " + cFileName
        QUIT
    ENDIF

    /*
     * create and open new XML document from the content string
     */
    nXMLDoc := XMLDocOpenString(cString)
    
    IF(nXMLDoc == 0)
        /*
         * print out any errors occured while parsing the document
         */
        PrintErrors(nXMLDoc)
        QUIT
    ELSE
        ? "XML document parsed successfully"
    ENDIF
 
    WHILE .T.
        ?
        ACCEPT "Type '?' for help or 'q' to quit > " TO cmd

        IF Len(cmd) == 0
            EXIT
        ENDIF

        IF cmd == 'q'
            EXIT
        ELSEIF cmd == '?'
            ?
            ? " 'r' - reset all action codeblocks"
            ? " 'e' - execute all action codeblocks"   
            ? " /... set action codeblocks"
        ELSEIF cmd == 'r'
            /*
             * reset all action codeblock to NIL
             */
            XMLDocResetAction(nXMLDoc)
        ELSEIF cmd == 'e'
            /*
             * process all action codeblocks
             */
            XMLDocProcess(nXMLDoc)
            PrintErrors(nXMLDoc)
        ELSE
            /*
             * set the action codeblock to all tags selected by cmd
             */
            nActions := XMLDocSetAction(nXMLDoc, cmd, {|n,c,a,h|DumpTag(n,c,a,h)})

            IF nActions == 0
                ? "no actions set"
            ELSE
                ? AllTrim(Str(nActions))+" actions set successfully"
            ENDIF
        ENDIF
    END

    XMLDocClose(nXMLDoc)
RETURN
    
/*
 * dump a tag of a XML document
 */
FUNCTION DumpTag(cTag, cContent, aAttribs, nHandle)    
    LOCAL n, aMember

    ? "Name   : ", cTag
    ? "Handle : ", nHandle
    ? "Content: ", cContent
    ? "Attrib.: "

    IF aAttribs != NIL
        FOR n := 1 TO Len(aAttribs)
            QOut("Name: ", aAttribs[n, TAGATTR_NAME], " Value: ", ;
                 aAttribs[n, TAGATTR_VALUE])
        NEXT
    ENDIF

RETURN XML_PROCESS_CONTINUE

/*
 * print error messages
 */
PROC PrintErrors(nXmlDoc)
    LOCAL n, aErrors := XMLDocGetErrorList(nXMLDoc)

    IF aErrors == NIL
        ? "No error descriptions available!"
    ENDIF

    FOR n := 1 TO Len(aErrors)
        /*
         * format and print error message
         */
        ? aErrors[n, XML_ERROR_FILE]+;
          "("+AllTrim(Str(aErrors[n, XML_ERROR_LINE]))+":"+;
          AllTrim(Str(aErrors[n, XML_ERROR_COLUMN]))+")"+;
          " error XPL00"+AllTrim(Str(aErrors[n, XML_ERROR_ID]))+": "+;
          GetErrorText(aErrors[n, XML_ERROR_ID])
        IF aErrors[n, XML_ERROR_ADDINFO] != NIL
            ? aErrors[n, XML_ERROR_FILE]+;
              "("+AllTrim(Str(aErrors[n, XML_ERROR_LINE]))+":"+;
              AllTrim(Str(aErrors[n, XML_ERROR_COLUMN]))+")"+;
              " info: "+aErrors[n, XML_ERROR_ADDINFO]
        ENDIF
    NEXT
RETURN

/*
 * array to translate error codes to error messages
 */
FUNCTION GetErrorText(nError)
    STATIC aErrorText := {;
        "out of memory; close some applications and try again",;
        "invalid DTD declaration",;
        "content outside XML tags not allowed",;
        "document has no root tag",;
        "invalid endtag or no endtag found",;
        "expecting string delimiter",;
        "unterminated string",;
        "file not found",;
        "file read error",;
        "missing name of file",;
        "duplicate attribute",;
        "malformed attribute",;
        "invalid name for attribute",;
        "error while processing action codeblock",;
    }
RETURN aErrorText[nError]
    
