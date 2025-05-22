//////////////////////////////////////////////////////////////////////
//
//  MENUTO.CH
//
//  Copyright:
//   Alaska Software, (c) 1997-2025. All rights reserved.         
//  

//  Contents:
//     #command directives for migrating @ PROMPT, MENU TO
//   
//////////////////////////////////////////////////////////////////////


#ifndef  _MENUTO_CH       

#include "Appevent.ch"
#include "Gra.ch"
#include "Xbp.ch"

#command  @ <nRow>, <nCol> PROMPT <prompt> [MESSAGE <message>] ;
      =>  AAdd( promptList, {<nRow>, <nCol>, <prompt>, <message>} )

#command  MENU TO <var> ;
      =>  <var> := ButtonMenuTo(promptList, <var>, UPPER(#<var>))

#define  _MENUTO_CH

#endif

// * EOF *
