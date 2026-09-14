;;; -*- Mode: LISP; Package: (:SLYNK-GENERA :USE :CL) BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: Mon Sep 14 06:41:48 2026 +0530 <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2026 Madhu.  All Rights Reserved.
;;;
(defpackage "SLYNK-GENERA"
  (:use "CL" "SLYNK-BACKEND"))
(in-package "SLYNK-GENERA")

(defimplementation gray-package-name ()
  "GRAY-STREAMS")

;;; Compilation

(defimplementation call-with-compilation-hooks (function)
  (funcall function))

(defimplementation slynk-compile-string
    (string &key buffer position filename line column policy)
  (declare (ignore line column policy))
  (with-input-from-string (stream string)
    (compiler:compile-from-stream strem nil #'compiler::compiler-to-core nil)))
