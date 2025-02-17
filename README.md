- [CL-OpenSubsonic](#org1f0c6db)
- [Usage](#org77f8897)


<a id="org1f0c6db"></a>

# CL-OpenSubsonic

This is an implementation of the [OpenSubsonic](https://opensubsonic.netlify.app/) client API in Common Lisp.


<a id="org77f8897"></a>

# Usage

At the most basic level: To use `CL-OpenSubsonic` you have to initialize a `LOGIN` object and optionally bind it to a variable (though this is obviously recommended so you can pass it to the API functions).

In order to verify that the login works, you can use `SYSTEM/PING`. Every API call will either return a hash-table (which is here printed readably using Alexandria) or throw a condition if the server returns an error code.

```common-lisp
CL-USER> (ql:quickload :cl-opensubsonic)
To load "cl-opensubsonic":
  Load 1 ASDF system:
    cl-opensubsonic
[package cl-opensubsonic].........................
[package cl-opensubsonic.client]..
; Loading "cl-opensubsonic"

(:CL-OPENSUBSONIC)
CL-USER> (in-package :sonic.client)
#<PACKAGE "CL-OPENSUBSONIC.CLIENT">
CLIENT> (sonic-init "https://sonicserver.lan" "admin" "hunter2")
#<SONIC-LOGIN-POST {1005684183}>
CLIENT> (defvar *login* *)
*LOGIN*
CLIENT> (ql:quickload :alexandria)
To load "alexandria":
  Load 1 ASDF system:
    alexandria
; Loading "alexandria"

(:ALEXANDRIA)
CLIENT> (alexandria:hash-table-alist (system/ping *login*))
(("openSubsonic" . T) ("serverVersion" . "0.16.4") ("type" . "gonic")
 ("version" . "1.15.0") ("status" . "ok"))
CLIENT> 
```
