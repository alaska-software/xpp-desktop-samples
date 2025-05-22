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

  This sample implements a simple Http server that runs as a service.
  It consists of two parts, HttpCtr.exe and HttpService.exe. The former
  controls the service while the latter implements the Http server.

  Please note that services are not supported by Windows 95/98 and
  Windows ME.

  It is recommended to study the source code of both applications for
  a thorough understanding.

____________________________________
2 Limitations:

  The Http server uses the socket functions of the ASINet libraries. Since
  ASINet is neither part of Xbase++ nor included in the Foundation
  subscription, we added ASINet1c.dll to this sample so that both executables
  work and can be tested. Modification of the source code and re-building
  the sample, however, requires all ASINet libraries to be installed on
  your computer. They are part of the Professional subscription.

____________________________________
3 Remarks:

  The error handling of the sample is something you should take a close
  look at: in PROCEDURE Main of HttpService.prg, the standard error
  code block is replaced and a runtime error leads to a call of PROCEDURE
  ErrorLog(). It receives the error object when a runtime error occurs,
  and collects error information in a usual way. But instead of writing the
  error information to a file, such as XPPERROR.LOG, the function

    EventLogWriteStr()

  is called. This function writes the error information to the Event log
  of the operating system before the service is terminated. This way,
  error information about a failed service can be obtained from the
  operating system's event log. For more information on EventLogWriteStr()
  refer to the online help.

____________________________________
4 Usage:

  The service is solely controlled by HttpCtr.exe. This controlling
  application must be started on the command line passing appropriate
  parameters on to it. All valid parameters are displayed by invoking
  HttpCtr.exe with no parameter.
  
4.1 Service installation ( Parameter i ):

  To install the service, HttpCrt.exe must be started as follows:

   httpctr i ".\AccountName" "Password"

  The second and third parameter are important for a successfull installation.
  They contain the name of the user account under which the service is
  installed and its password. Note that account for installation may differ
  from the account that runs HttpCtr.exe.

  Don't forget to prefix the account name with a dot and a backslash (".\"),
  both are required.

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


   - double click the service "Alaska Software Http Service".
   - Additionally on Windows 2000/XP: select the tab page "Log On"
   - The select boxes for AccountName and Password are allready filled according
     the installation.
   - Reenter the password. This will ensure the right "Log on as Service" is granted
     to the account when leaving the dialog.
   - Select "OK".
   - A dialog pops up where you are informed that the account is granted
     permission to log on as service. If this permission has already been
     granted to this account, this dialog is not displayed.


4.2 Starting the service ( Parameter s ):

  The service is started using "s" as parameter for HttpCtr:

   httpctr s

  As mentioned before, the service account must have the right to log on as 
  service. Furthermore the service executable must be located at a place
  where it can be found under this account. This means that the drive where
  HttpService.exe resides must exist on this account. In general, all
  resources required by the service must be reachable, including the
  Xbase++ runtime.

  If you encounter problems at this stage, copy the sample and the
  Xbase++ DLLs to a directory on drive C.

  The service is started when:
    1. no error message is created
    2. a Web browser dirplays a Html page when you enter this URL
       in the browser

        http://<your IP address>

4.3 Stopping the service ( Parameter x ):

  When the service is successfully started, it can be stopped like this:

     httpctr x

4.4 Uninstalling a service ( Parameter u ):
  The parameter "u" uninstalls a service:

    httpctr u

  Note that it is possible to execute this line while the service is still
  running. In this case it will be uninstalled when it is stopped.

4.5 Pause a service ( Parameter p ):
  When a service is running, it can be paused:

    httpctr p

4.6 Continue a paused service ( Parameter c):
  A paused service continues when this is executed:

    httpctr c

4.7 Obtaining status information ( Parameter o):
  The status of a service is shown with this command:

    httpctr o

4.8 List all services ( Parameter a ):
  A list of all installed services can be queried like this:
 
    httpctr a


____________________________________
5 Debugging:

  The service can be controlled in the Xbase++ debugger by calling
  class methods from the debugger command line. For example:

    1. Start the debugger

         XppDbg httpservice.exe

    2. Set a break point on this line:

         IF Empty( cRootDir )

    3. Run the program by pressing F5

    4. Open the LOCAL variable monitor and inspect the value of variable
       "cRootDir". It contains the path where HttpService is installed.
       The value NIL indicates that the service is not yet installed,
       otherwise the value shows the root directory of the Http server
       where it creates its log file and looks for Html files requested
       by a browser.
      
    5. Set a break point in file httpserver.prg on this line of class method
       HttpServer:main():

         DO WHILE ::nStatus != NOTRUNNING

       and press F5.

    6. Now you can see that the service basically consists of an endless loop
       that is exited when ::nStatus changes. The status is changed by
       calling the HttpServer:pause() method, for example, which is usually
       done via the control application by entering on the command line:
 
         httpctr p

       Since the service is executed in the debugger at the moment, we have
       to simulate this call. For this, a break point must be set on this
       line of method HttpServer:main():

         Sleep( 10 )

    7. Now you can execute the :pause() method of the HttpServer class
       by entering on the debugger command line:

         HttpServer():pause()

       This call changes the value of :nStatus to PAUSING

    8. Press F5 to resume program execution. The debugger stops the service
       at the break point set before. When you enter in the debugger:

         HttpServer():continue()

       and press F5, the break point is no longer reached.
