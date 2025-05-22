//////////////////////////////////////////////////////////////////////
//
//  RESOURCE.PRG
//
//  Copyright:
//      Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//      The program illustrates how a manifest can be defined in
//      the resource of an Xbase++ application.
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
                    " application uses a manifest that is linked into the"      +;
                    " application's resource. The actual manifest information"  +;
                    " is read from the file extfile.exe.manifest. See the file" +;
                    " resource.arc to see how the manifest resource is defined."+;
                    CRLF + CRLF                                                 +;
                    "A manifest basically always has the same content,"         +;
                    " independent of the application it is used with. If you"   +;
                    " open the file extfile.exe.manifest in an editor, you will"+;
                    " find only three sections that need to be customized: the" +;
                    " target processor architecture (x86/IA64) and the"         +;
                    " application's nam and description ."                      +;
                    CRLF + CRLF                                                +;
                    "If the manifest is linked into the application, no"        +;
                    " separate manifest file must be installed at the client"   +;
                    " site. However, to change the application's support for"   +;
                    " the special visual styles, the application's executable"  +;
                    " must be replaced."


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
