;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: Fri Aug 19 17:37:22 2022 +0530 <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2022 Madhu.  All Rights Reserved.
;;;

#+nil
(setq SWANK-LOADER::*FASL-DIRECTORY* *SWANK-BINARY-DIR*)

(assert (or (and (featurep :swank) (not (featurep :slynk)))
	    (and (featurep :slynk) (not (featurep :swank)))
	    (and (not (featurep :slynk)) (not (featurep :swank)))))

(when (or (featurep :slynk) (featurep :swank))

(defpackage "SWANK-COMPAT"
  (:use)
  (:export "CREATE-SERVER" "FIND-DEFINITION-FOR-THING"
   "FUNCTION-NAME")
  (:import-from #+slynk "SLYNK" #+swank "SWANK"
		"CREATE-SERVER" "FIND-DEFINITION-FOR-THING"
		"FUNCTION-NAME"))

(defpackage "SWANK-COMPAT-BACKEND"
  (:use)
  (:export "FUNCTION-NAME" "GUESS-EXTERNAL-FORMAT" "FIND-EXTERNAL-FORMAT"
   "ARGLIST" "MAKE-INPUT-STREAM" "MAKE-OUTPUT-STREAM"
   "INSTALL-DEBUGGER-GLOBALLY"
   )
  (:import-from #+slynk "SLYNK" #+swank "SWANK"
   "FUNCTION-NAME" "GUESS-EXTERNAL-FORMAT" "FIND-EXTERNAL-FORMAT"
      "ARGLIST" "MAKE-INPUT-STREAM" "MAKE-OUTPUT-STREAM"
   "INSTALL-DEBUGGER-GLOBALLY"
   ))
)
