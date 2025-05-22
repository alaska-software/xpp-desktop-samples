//////////////////////////////////////////////////////////////////////
//
//  README.TXT
//
//  Copyright:
//   Alaska Software, (c) 2002-2025. All rights reserved.         
//  
//////////////////////////////////////////////////////////////////////


____________________________________
1 Contents:

  This Sample implements a simple service. The service consists of two
  executables, SimpleCtr.exe and SimpleService.exe. SimpleService.exe
  is the service and SimpleCtr.exe is a tool to control this service.

  Please note that services are not supported by Windows 95/98 and
  Windows ME.

  For a better understanding please have a look at the source code of this
  sample.


____________________________________
2 Usage:
  Controlling the service is done exclusively by the application
  ServiceCtr.exe. This must be started at a dos prompt with appropriate
  parameters. To obtain an overview, starte  SimpleCtr.exe without
  parameters or double click its icon.

2.1 Installation ( parameter i ):
  For installing the service SimpleCtr.exe must be started according to
  the following line:

   simplectr i ".\AccountName" "Password"

  Pay attention to the second and third parameter. These are the name and the
  password of the account under which the service is executed. This may
  not be the same account under which you enter this line.

  The name of the account must be prefixed with a dot and a backslash
  (".\").

  At this stage there is a potential for violating security rights. When
  entering the line above you must have the right to install services (The
  Administrator account should have these rights). On the other hand the
  account under which the service is started must have the rights to
  log on as service. A violation against the second rule is not visible 
  immediately. It will be a problem as soon as the service is started.

  To grant the right "log on as service" you may proceed as follows after the
  service is installed:

   - Start the "Services" application which can be found at:

       Windows NT:   Start -> Settings -> Control Panel
       Windows 2000: Start -> Settings -> Control Panel -> Administrative Tools
       Windows XP:   Start -> Control Panel -> Administrative Tools

                     or

                     Start -> Control Panel -> Performance and Maintenance ->
                     Administrative Tools

      Windows Vista: Start -> Control Panel -> System Maintenance 
                     -> Administrative Tools -> Services


   - double click the service "Alaska Software Simple Service".
   - Additionally on Windows 2000/XP/VISTA: select the tab page "Log On"
   - The select boxes for AccountName and Password are allready filled according
     the installation.
   - Reenter the password. This will ensure the right "Log on as Service" is granted
     to the account when leaving the dialog.
   - Select "OK".
   - A dialog pops up where you are informed that the account is granted
     permission to log on as service. If this permission has already been
     granted to this account, this dialog is not displayed.

2.2 Starting the service ( parameter s):
  The service is started by entering the following line:

   simplectr s

  As described before, the service account must have the right to log on as 
  service. Furthermore the service executable must be located at a place
  where it can be found under this account. This means that the drive where
  simpleservice.exe resides must exist on this account. Particularly all
  resources, including the Xbase++ runtime, which are required by the
  service must be reachable.

  If you have difficulties here, copy the sample and the Xbase++ runtime to
  a directory of drive C.

  The service is started if
    1. No error message is returned.
    2. You hear a Tone in a 5 seconds interval. Note: this requires that
       your loudspeaker is operational ;-)

2.3 Stopping the service ( parameter x ):
  When the service is running it can be stopped with the following line:

     simplectr x

2.4 Deinstalling the service ( parameter u ):
  If the service is installed it can be deinstalled with the following line:

    simplectr u

  It is no mistake to deinstall the service when it is running. In this case
  it will actually be deinstalled as soon as it is stopped.


____________________________________
3 Debugging:

  The service may be debugged with the Xbase++ debugger. The service may be
  controlled from within the debugger by calling the controll methods of the
  service from the debugger's command line, eg:

    1. Start the debugger

         XppDbg simpleservice.exe

    2. Set a breakpoint on following line:

         DO WHILE ::lRunning

    3. Execute the program with F5

    4. Open the command window

         Menue Commands -> Open Command Window

    5. Call the class method :stop() of the class MyService by entering
       at the prompt of the command window:

         MyService():stop()

       This will assign .F. to the IVar :lRunning

    6. Continue program execution by pressing the F5 key.




