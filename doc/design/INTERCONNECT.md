Interconnect protocol for Client application
=============================================

## server process(PastehubSync):

use unix domain socket or named-pipe(win32)

### getStatus
 new paste-data comes
+ error occurs
+ online
+ offline

#### format
+ new\n
+ error N\n  (N is a error code)
+ online\n
+ offline\n

### setNotify
+ new paste-data inserted to local DB

## Sign-in

sign-in can only exec from command line argument.

### command line argument of PastehubSync
>    PastehubSync password [username] [signature]
>      0) success to signin and start sync process.
>      1) fail    to signin and exit(1) process.

EOF
