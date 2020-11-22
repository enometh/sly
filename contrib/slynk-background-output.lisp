(in-package :slynk)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (slynk-require '(:slynk/mrepl)))

(defpackage :slynk-background-output
  (:use :cl :slynk-api)
  (:export #:create-background-output)
  (:import-from "SLYNK-MREPL"
   #:*saved-global-streams*
   #:*standard-output-streams*
   #:prefixed-var
   #:setup-stream-indirection
   #:init-global-stream-redirection))
(in-package :slynk-background-output)

(defun redirect-background-output-to-connection (connection)
  "Set the standard output streams to redirect to CONNECTION.
Assigns *CURRENT-<STREAM>* for standard streams."
  (format slynk:*log-output* "~&; Redirecting Background Output~%")
  (let ((stream (background-output-stream connection)))
    (assert stream nil "Background output stream on channel not set up for connection")
    (dolist (o *standard-output-streams*)
      (set (prefixed-var '#:current o)
	   stream))))

(defun revert-background-output-redirection ()
  "Set *CURRENT-<STREAM>* to *REAL-<STREAM>* for all standard output streams."
  ;; Log to SLYNK:*LOG-OUTPUT* since the standard streams whose
  ;; redirection are about to be reverted might be in an unconsistent
  ;; state after, for instance, restarting an image.
  ;;
  (format slynk:*log-output* "~&; Reverting Background Output redirection~%")
  (dolist (stream-var *standard-output-streams*)
    (set (prefixed-var '#:current stream-var)
         (getf *saved-global-streams* stream-var))))

(defclass background-output-channel (channel)
  ((remote-id :initarg :remote-id)
   (output-stream :initform nil)))

(defun make-background-output-stream (remote-id)
  (slynk-backend:make-output-stream
   (slynk::make-thread-bindings-aware-lambda
    (lambda (string)
      (send-to-remote-channel remote-id `(:write-string ,string))))))

(defmethod initialize-instance :after ((channel background-output-channel) &key)
  (with-slots (remote-id output-stream) channel
    (setq output-stream (make-background-output-stream remote-id))))

(defun find-background-output-channel (connection)
  (find-if (lambda (channel)
	     (typep channel 'background-output-channel))
	   (slynk::connection-channels connection)))

(defun background-output-stream (connection)
  (let ((channel (find-background-output-channel connection)))
    (when channel
      (with-slots (output-stream) channel
	output-stream))))

(defslyfun create-background-output (remote-id)
  (assert (every (lambda (channel) (not (typep channel 'background-output-channel)))
		 (slynk::channels))
      nil "Only one Background Output per connection")
  (unless (null slynk:*globally-redirect-io*)
    (cerror "OK. Whatever"
	    "Background Output will not work with swank-mrepl unless
*GLOBALLY-REDIRECT-IO* is NIL"))
  (let* ((ch (make-instance 'background-output-channel :remote-id remote-id)))
    (assert (find ch (slynk::channels)))
    (unless *saved-global-streams*
      (init-global-stream-redirection))
    (redirect-background-output-to-connection *emacs-connection*)
    (with-slots (output-stream) ch
      (mapc (lambda (var)
	      (set var output-stream))
	    *standard-output-streams*))
    (list (channel-id ch) (channel-thread-id ch))))

(defmethod close-channel :before ((r background-output-channel) &key force)
  (declare (ignore force))
  (revert-background-output-redirection)
  (format slynk:*log-output* "~&; Reverted Background Output redirection~%"))

;;; sledge hammer. interactive-eval sets up the streams from
;;; the default-listener if it exists.
;; this doesn't work withm mkcl - the output goes to the repl
;; stream if it exists
;; with ecl the inferior-lisp standard-output gets bound to the
;; background stream.
(defvar *original-default-listener* #'default-listener)

(defun default-listener (connection)
  (if (find-background-output-channel connection)
      nil
      (funcall *original-default-listener* connection)))

#||
(slynk::channels)
(slynk::listeners)
(list (background-output-stream (car slynk::*connections*))
      *Standard-output*)
(find-background-output-channel *emacs-connection*)
(slynk::with-connection ((car slynk::*connections*))
  (slynk::send-to-emacs `(:channel-send 2 (:write-string "foobar"))))
(default-listener *emacs-connection*)
(format t "foo~&")
(format (cdr (assoc '*standard-output* (slot-value (car (slynk::listeners)) 'slynk::env)))
	"foobar~%")
(format *standard-output* "barf~%")
(format (background-output-stream *emacs-connection*) "foobar~%")
(format (slot-value (find-background-output-channel *emacs-connection*) 'output-stream)
	"xyz~%")
slynk::*connection
#+ecl-console
(setq *standard-output* (getf slynk-mrepl::*saved-global-streams* '*standard-output*))
(slynk-background-output::close-channel (car (slynk::channels)))
||#

(provide :slynk/background-output)
