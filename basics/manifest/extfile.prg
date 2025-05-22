//////////////////////////////////////////////////////////////////////
//
//  EXTFILE.PRG
//
//  Copyright:
//      Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//      The program illustrates how a manifest is defined as a
//      separate file.
//   
//////////////////////////////////////////////////////////////////////



#include "xbp.ch"
#include "appevent.ch"

#define CRLF   Chr(13) + Chr( 10 )

#define SAMPLE_MSG  "This sample illustrates how a so-called manifest can be"   +;
                    " defined for an Xbase++ application."                      +;
                    CRLF + CRLF                                                 +;
                    "A manifest must be defined for applications that should"   +;
                    " use the special visual styles introduced with Windows XP."+;
                    " If this example is run on such a platform, the group box" +;
                    " is normally displayed with rounded corners. On other"     +;
                    " operating systems, defining a manifest has no effect."    +;
                    CRLF + CRLF                                                 +;
                    "A manifest can be defined in one of two ways. This sample" +;
                    " application uses a manifest that is supplied as an"       +;
                    " external file. In order to be recognized by the operating"+;
                    " system, the file must have same name as the executable"   +;
                    " plus a specific extension (.manifest)."                   +;
                    CRLF + CRLF +                                               +;
                    "A manifest file basically always has the same content,"    +;
                    " independent of the application is used with. If you open" +;
                    " the file extfile.exe.manifest in an editor, you will"     +;
                    " find only three sections that need to be customized: the" +;
                    " target processor architecture (x86/IA64) and the"         +;
                    " application's nam and description ."                      +;
                    CRLF + CRLF +                                               +;
                    "Supplying a manifest as an external file (versus in the"   +;
                    " application's resource has two advantages:"               +;
                    " the manifest file can be added to applications already"   +;
                    " installed at a client site. Furthermore, the manifest"    +;
                    " file just has to be removed if visual styles should"      +;
                    " later be disabled for any reason."

PROCEDURE Main()

 LOCAL oCrt := SetAppWindow()
 LOCAL oGroup
 LOCAL oMLE
 LOCAL nEvent, mp1, mp2, oXbp

   SetColor( "N/W+" )
   CLS

   oCrt:UseShortCuts := .T.

   IF IsThemeActive(.F.) == .F.
      MsgBox( "This example illustrates a feature only available" + CRLF +;
              "on newer operating systems, such as Windows XP." )
   ENDIF

   oGroup = XbpStatic():New()
   oGroup:Type      = XBPSTATIC_TYPE_GROUPBOX
   oGroup:Caption   = "Information"
   oGroup:Create( ,, {20,20}, {590,370} )

   oMLE = XbpMLE():New( oGroup )
   oMLE:Editable    = .F.
   oMLE:HorizScroll = .F.
   oMLE:Create( ,, {10,10}, {560,340} )
   oMLE:SetData( SAMPLE_MSG )

   nEvent = xbeP_None
   DO WHILE nEvent != xbeP_Close
      nEvent = AppEvent( @mp1, @mp2, @oXbp )
      oXbp:HandleEvent( nEvent, mp1, mp2 )
   ENDDO

RETURN
