#
# Makefile for extend system (C-API) examples 
#
# Requirements:
# - Environment variable INCLUDE contains path for Xbase++ header files
# - Environment variable LIB contains path for xpprt1.lib and C libraries
# - Environment variable XPPRESOURCE contains path for xpprt1.res
#
# C-Compiler: MS Visual C++
#
# Copyright (C) 1995-2000, Alaska Software
# All Rights Reserved
#

XCOMPILE=/b/q
!ifdef WINDIR
CCOMPILE=/Zl /I$(MSDEVDIR)\INCLUDE /c /Tc 
CC=$(MSDEVDIR)\bin\cl /D__WINDOWS__=
LINK=alink
OS_LIBS=user32.lib
!else
CCOMPILE=/c /Tdp 
CC=icc
LINK=ilink
LINK_END=
OS_LIBS=
!endif



all: main.exe sound.exe
   @echo --- $** done ! --- 

main.exe: main.obj call.obj f2bin.obj soundex.obj
   $(LINK) /PM:PM /DE $** $(OS_LIBS)$(LINK_END)

main.obj: main.prg
   xpp $(XCOMPILE) $*.prg

sound.exe: sound.obj soundex.obj
   $(LINK) /PM:VIO /DE sound.obj soundex.obj$(LINK_END)

sound.obj: sound.prg soundex.ch
   xpp $(XCOMPILE) sound.prg


# call Xbase++ functions from C:
call.obj: $*.c
   $(CC) $(CCOMPILE) $*.c

# soundex:
soundex.obj: soundex.c soundex.ch
   $(CC) /O $(CCOMPILE) $*.c
 
f2bin.obj: f2bin.c  
        $(CC) $(CCOMPILE) f2bin.c 














