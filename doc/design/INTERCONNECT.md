Interconnect protocol for Client application
=============================================

## server process(PastehubSync):

use unix domain socket or named-pipe(win32)

### getStatus:
format is JSON. client must poll every 0.5 second.

    {
      "online" : <0 or 1>,
      "error"  : <errorCode>,
      "comes"  : <items which came from server in recent minute>
    }

## Sign-in

sign-in can only exec from command line argument.

### command line argument of PastehubSync:
    PastehubSync password [username] [signature]
      0) success to signin then start sync process.
      1) fail    to signin then exit(1) process.
