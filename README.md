- [CL-OpenSubsonic](#org9e03472)
  - [Status](#org406b33e)
  - [Usage](#org96938e6)


<a id="org9e03472"></a>

# CL-OpenSubsonic

This is an implementation of the [OpenSubsonic](https://opensubsonic.netlify.app/) client API in Common Lisp.


<a id="org406b33e"></a>

## Status

The OpenSubsonic API isn't very complicated to implement, especially thanks to Common Lisp's macro system which is used here to abstract over the mostly boilerplate task of writing HTTP requests. On paper, this implements nearly all of the API (with a few exceptions).

Note that this is currently mostly untested and that each OpenSubsonic implementation is going to likely differ slightly from each other. PRs welcome.


<a id="org96938e6"></a>

## Usage

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

The API is quite straightforward to use: Every function is mapped by its name to the corresponding API call in OpenSubsonic (converting from kebab to camel case), takes at minimum a `LOGIN` object, and potentially some parameters which will be passed as either query parameters in a GET request or POST parameters depending on whether or not the OpenSubsonic server supports the [extension in question](https://opensubsonic.netlify.app/docs/extensions/formpost/).

If an API call takes parameters, they may be required or optional, which is mapped to the parameters of the function itself.
