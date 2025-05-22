//////////////////////////////////////////////////////////////////////
//
//  COFFEE.PRG
//
//  Copyright:
//      Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//      Example for advanced multi-threaded programming.
//   
//      This example is a real life simulation of a team of programmers
//      and a single coffee machine. It shows how to solve special problems
//      that may arise in multi-threaded programs.
//   
//  Remarks:
//      Watch the program's output before studying the code
//   
//      The program demonstrates two programming techniques for
//      multi-threading using a classical problem: multiple threads
//      access and change shared program resources at the same time and
//      must be coordinated. Thread coordination is accomplished using
//      one SYNC method (serialize program execution in multiple treads)
//      and two Signal objects (halt/restart program flow in one thread).
//   
//      The simulation uses:
//        1) One thread for the coffee machine (C)
//        2) Nine threads for the team of programmers (P)
//   
//      These are the real life assumptions of the program:
//        1) When no coffee is left in the coffee machine, one thread (C)
//           must wait until another thread (P) fills the coffee machine
//        2) While the coffee machine is running, up to nine threads (P)
//           must stop until the coffee is ready
//        3) A programmer (P) works while he has coffee. When his cup is
//           empty, the programmer goes for a refill.
//        4) Only one programmer (P) has access to the coffee machine (C)
//           at any point in time. If other programmers want to refill
//           their cups at the same time, the corresponding threads (P)
//           must stop.
//   
//////////////////////////////////////////////////////////////////////



/*
 * The Main procedure creates the threads, starts them and waits for a
 * key stroke to terminate the program. Ten threads are running while
 * Inkey(0) waits for a key.
 */
PROCEDURE Main
   LOCAL i, oMachine
   LOCAL aTime := { 130, 100, 140, 90, 100, 110, 150, 120, 100 }
   LOCAL aNames:= { "Frank++ ", "Gernot  ", "Andreas ", ;
                    "Joerg   ", "Michael ", "Steffen ", ;
                    "Till    ", "Volker  ", "Wolfgang"  }
   CLS

   // Start first thread (the coffee machine)
   oMachine := CoffeeMachine():new( 20 )
   oMachine:start()

   // Start nine additional threads (the team of programmers)
   FOR i := 1 TO 9
      Programmer():new( aNames[i], aTime[i], oMachine ):start()
   NEXT

   // Wait for a key stroke
   DispOutAt( 3, 0, "Press a key to QUIT" )
   Inkey(0)

RETURN



/*
 * This class simulates a coffee machine. Access to the coffee machine is
 * synchronized by the SYNC method :enter(). Two Signal objects make
 * sure that a programmer can get coffee only if coffee is ready, and
 * that the coffee machine waits to be filled when empty.
 */
CLASS CoffeeMachine FROM Thread
   EXPORTED:
   VAR capacity, coffee, waterIsFilled, coffeeIsReady

   METHOD init, execute
   SYNC METHOD enter
ENDCLASS

METHOD CoffeeMachine:init( nCapacity )
   ::thread:init(, "Coffee machine")
   ::setInterval( 10 )
   ::coffee        := 0
   ::capacity      := nCapacity
   ::waterIsFilled := Signal():new()
   ::coffeeIsReady := Signal():new()
RETURN self



/*
 * Only one object of the Programmer class can execute the :enter() method
 * at any point in time (SYNC method!). The color Red indicates
 * during runtime which thread is waiting to execute this method.
 */
METHOD CoffeeMachine:enter( oProgrammer )
   oProgrammer:takeCoffee()
RETURN self



/*
 * As long as ::coffee >= 1, the CoffeeMachine thread issues the
 * ::coffeeIsReady signal which all Programmer threads are waiting for.
 * If ::coffee is less than 1, the thread waits until a Programmer thread
 * signals ::waterIsFilled. Then, the "coffee machine" enters the
 * DO WHILE loop which delays the ::coffeeIsReady signal.
 */
METHOD CoffeeMachine:execute
   LOCAL n

   IF ::coffee < 1
      DispoutAt( 1, 0, PadR( "Coffee is empty! ",80 ), "W+/B" )

      ::waterIsFilled:wait()

      n := 0
      DO WHILE ++n <= ::capacity
         DispoutAt( 1, 0, PadR( ::name+" is running! "+Replicate(Chr(219),2*n), 80 ), "W+/B" )
         Sleep(35)
      ENDDO
      ::coffee := ::capacity
   ENDIF

   DispoutAt( 1, 0, PadR( "Coffee is ready!           "+Replicate(Chr(176),::coffee*2), 80 ) )
   ::coffeeIsReady:signal()

RETURN self




/*
 * This class simulates a single programmer. A programmer works as long
 * as he has coffee.
 */
CLASS Programmer FROM Thread
   EXPORTED:
   VAR coffee, coffeeMachine
   METHOD init, execute, takeCoffee
ENDCLASS



/*
 * A Programmer thread is re-started each time <nInterval> * 1/100 seconds
 * have elapsed. This is equivalent to the time a programmer needs to
 * drink his cup of coffee.
 */
METHOD Programmer:init( cName, nInterval, oMachine )
   ::thread:init(, cName)
   ::setInterval( nInterval )
   ::coffee        := 0
   ::coffeeMachine := oMachine
RETURN self



/*
 * This method simulates a working programmer. He works only if there is
 * coffee in his cup. If the cup is empty (::coffee == 0), the programmer
 * walks to the coffee machine (:enter()). By this, he restricts all other
 * Programmer threads from accessing the coffee machine.
 */
METHOD Programmer:execute
   LOCAL nRow := ThreadID() * 2

   IF ::coffee == 0
      // Coffee cup is empty
      DispoutAt( nRow, 0, PadR( ::name + " NEEDS coffee", 80), "W+/R" )

      // When a Programmer thread executes the SYNC method :enter(),
      // all other Programmer threads are stopped here which want
      // to execute this method at the same time
      ::coffeeMachine:enter( self )
   ENDIF

   DispoutAt( nRow, 0, PadR( ::name + " is working - " + ;
                       Replicate( ".", 10*::coffee), 80) )

   // A programmer sips coffee
   ::coffee := Max( ::coffee - 0.1, 0 )
RETURN



/*
 * A programer fills his cup in this method. If no coffee is left in the
 * coffee machine (::coffeeMachine:coffee < 1), the CoffeeMachine thread
 * is signaled (programmer fills coffee machine).
 */
METHOD Programmer:takeCoffee
   LOCAL nRow := ThreadID() * 2

   IF ::coffeeMachine:coffee < 1
      // Coffee machine is empty
      DispoutAt( nRow, 0, PadR( ::name + " MAKES coffee", 80 ), "W+/B" )

      // The waiting CoffeeMachine thread resumes after being signaled
      ::coffeeMachine:waterIsFilled:signal()
   ENDIF

   // All Programmer threads wait at this point until the
   // CoffeeMachine thread signals "coffee is ready".
   // The signal is delayed while the coffee machine is running.
   ::coffeeMachine:coffeeIsReady:wait()

   DispoutAt( nRow, 0, PadR( ::name + " takes coffee ", 80 ), "N/G" )

   // A programmer fills his cup with coffee from the coffee machine
   ::coffeeMachine:coffee -= 1

   // Cup is filled again
   ::coffee := 1
   Sleep(100)
RETURN self
