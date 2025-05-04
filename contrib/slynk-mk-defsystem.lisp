;;; Slynk-mk-defsystem.lisp -- MK-DEFSYSTEM support
;;; Ported from swank-asdf <https://github.com/slime/slime/blob/master/contrib/swank-asdf.lisp>
;; Authors: Daniel Barlow <dan@telent.net>
;;          Marco Baringer <mb@bese.it>
;;          Edi Weitz <edi@agharta.de>
;;          Francois-Rene Rideau <tunes@google.com>
;;          and others
;; License: Public Domain


(defpackage :slynk-mk-defsystem
  (:use :cl :slynk-api :slynk-backend)
  (:import-from :slynk)
  (:export #:make-output-translation-function
           #:while-collecting-notes
           #:*current-system-buffers*))

(in-package :slynk-mk-defsystem)


(defvar *recompile-system* nil)
(defvar *pathname-component* (make-hash-table :test 'equal))
(defvar *pathname-system* (make-hash-table :test #'equal))

(defvar *current-source-file* nil)

(defvar *current-system-buffers*
  :documentation "List of current open sly buffers. We call load-source-op 
on these directly to improve load-time error messages")

;;; Macros

(defmacro while-collecting-notes ((&key interactive) &body body)
  `(collect-notes (lambda () ,@body) ,interactive))

;;; Callable by RPC

(defslyfun who-depends-on (system)
  (let* ((system (mk::ensure-system system))
	 (name (mk::canonicalize-system-name (mk::component-name system)))
	 ret)
    (maphash (lambda (k v)
	       (dolist (dep (mk::component-depends-on v))
		 (when (equal name
			      (mk::canonicalize-system-name dep))
		   (pushnew k ret :test #'equal))))
	     mk::*defined-systems*)
    ret))

#+nil
(who-depends-on "ALEXANDRIA")

(defslyfun operate-on-system-for-emacs (system-name operation &rest args)
  "Compile and load SYSTEM using MK-DEFSYSTEM.
Record compiler notes signalled as `compiler-condition's."
  (slynk-mk-defsystem:while-collecting-notes (:interactive nil)
    ;; (slynk::collect-notes (lambda () ))
    (apply #'operate-on-system system-name operation args)))

(defun unique-string-list (&rest lists)
  (sort (delete-duplicates (apply #'append lists) :test #'string=) #'string<))


(defslyfun list-all-systems-in-central-registry ()
  "Returns a list of all systems in MK-DEFSYSTEM's central registry
AND in its source-registry. IDIOT! This returns the file names - which
do not correspond to the system names."
  (unique-string-list
   (mapcar #'pathname-name
	   (loop for dir in mk:*central-registry*
		 for defaults = (eval dir)
		 when defaults
		 append (directory (merge-pathnames "*.system" defaults)
				   #+clozure :follow-links
				   #+clozure nil)))))

#+nil
(list-all-systems-in-central-registry)

(defslyfun list-all-systems-known-to-mk-defsystem ()
  "Returns a list of all systems MK-DEFSYSTEM knows already."
  (let (ret)
    (maphash (lambda (k v)
	       (declare (ignore k))
	       (push (mk::component-name v) ret))
	     mk::*defined-systems*)
    ret))

(defslyfun list-mk-defsystem-systems ()
  "Returns the systems in MK-DEFSYSTEM's central registry and those
which MK-DEFSYSTEM already knows."
  (unique-string-list
   (list-all-systems-known-to-mk-defsystem)
   (list-all-systems-in-central-registry)))

#+nil
(list-mk-defsystem-systems)

#+nil
(let (ret)
  (mk::system-map-files (mk:find-system :bordeaux-threads)
					(lambda (x) (push x ret))
					:recursively-handle-deps :never
					:type :source) ret)

(defslyfun mk-defsystem-system-files (name)
  (let (ret)
    (mk::system-map-files (mk:find-system name)
			  (lambda (x) (push x ret))
			  :recursively-handle-deps :module
			  :type :source)
    ret))

#+nil
(mk-defsystem-system-files :bordeaux-threads)

(defslyfun reload-system (name)
  (let ((*recompile-system* (mk:find-system name)))
    (operate-on-system-for-emacs name ':load)))

#+nil
(reload-system :defsystem)

(defslyfun mk-defsystem-system-loaded-p (name)
  (find name MK::*MODULES* :test #'equalp))


(defslyfun mk-defsystem-system-directory (name)
  (namestring (mk::component-root-dir (mk:find-system name) :source)))

#+nil
(mk-defsystem-system-directory "LEM")


(defslyfun mk-defsystem-determine-system (file buffer-package-name)
  (or
   (and file
        (pathname-system file))
   (and file
        (progn
          ;; If not found, let's rebuild the table first
          (recompute-pathname-component-table)
          (pathname-system file)))
   ;; If we couldn't find an already defined system,
   ;; try finding a system that's named like BUFFER-PACKAGE-NAME.
   (loop with package = (guess-buffer-package buffer-package-name)
      for name in (slynk::package-names package)
      for system = (mk:find-system name :load-or-nil)
      when (and system
                (or (not file)
                    (pathname-system file)))
      return (mk::component-name system))))


(defslyfun delete-system-fasls (name)
  (mk:clean-system name :propagate nil))

#+nil
(delete-system-fasls :defsystem)

(defun collect-notes (function interactive)
  "Includes additional asdf specific logic from `slynk::collect-notes`"
  (let ((notes '())
        (restart-p nil)) ;; Denotes a critical failure
    ;; Filter unbound-variable errors that have a corresponding undefined-variable warning.
    ;; The undefined-variable warning provides a better warning with a precise source error
    ;; location (at least on SBCL)
    (flet ((redundant-note-p (note)
             (when (eq (getf note :type) 'unbound-variable)
               (let* ((message (getf note :message))
                      (start 13) ;; "The variable "
                      (end (search " is unbound" message))
                      (var-name (subseq message start end)))
                 (find-if (lambda (prev-note)
                            (let ((type (getf prev-note :type))
                                  (message (getf prev-note :message)))
                              (and (eq type 'simple-warning)
                                   (search var-name message))))
                          notes))))
           (error-p (c) (typep (slynk-backend:original-condition c) 'error)))
      (multiple-value-bind (result seconds)
          (handler-bind ((slynk::compiler-condition
                          (lambda (c)
                            (let ((note (make-compiler-note c)))
                              (when note
                                (push note notes)))
                            (when (and (error-p c) (not interactive)) ;; For warnings, we do not need to restart
                              ;; We may be loading a system in which case we would
                              ;; have an accept restart that we can use to continue
                              ;; compilation. Either way, mark has having restarted
                              (setf restart-p t)
                              ;; Recovering from errors can be fairly unintuitive
                              ;; see :cascading-errors system and issue #29. Disable for now
                              ;; (let ((accept (find-restart 'asdf/action:accept)))
                              ;;   (if accept
                              ;;       (invoke-restart accept)
                              ;;       (invoke-restart 'abort)))
                              (invoke-restart 'abort)))))
            (slynk::measure-time-interval
             (lambda ()
               ;; To report location of error-signaling toplevel forms
               ;; for errors in EVAL-WHEN or during macroexpansion.
               (restart-case (multiple-value-list (funcall function))
                 (abort () :report "Abort compilation." (list nil))))))
	(declare (ignore result))
	(let ((notes (remove-if #'redundant-note-p notes)))
          (slynk::make-compilation-result :notes notes
                                          :duration seconds
                                          :successp (not restart-p)
                                          :loadp t
                                          :faslfile nil))))))

(defun make-compiler-note (condition)
  "Make a compiler note data structure from a compiler-condition."
  (unless (member (severity condition) '(:redefinition))
    (list* :message (message condition)
           :severity (severity condition)
	   ;; MG: If we were unable to get the precise source location, replace with
	   ;; *current-source-file* which we dynamically bound around asdf:load*
           :location (replace-location-if-error (location condition))
           :references (references condition)
           :type (type-of (original-condition condition))
           :asdf nil
           (let ((s (source-context condition)))
             (if s (list :source-context s))))))

(defun replace-location-if-error (location)
  (if (and (eq (car location) :error)
           *current-source-file*)
      (slynk-backend:make-location
       `(:file ,(namestring *current-source-file*))
       `(:position 0))
      location))

(defun parse-condition-location (condition)
  "Inspect the class of a given CONDITION. If it includes a slot with the name LOCATION
return it if it is of the form (:file FILENAME :pos NUMBER)"
  (loop for slot in (slynk-mop:class-direct-slots (class-of condition))
     for name = (slynk-mop:slot-definition-name slot)
     when (string-equal name "LOCATION") :do
       (return-from parse-condition-location
         (slynk-mop:slot-value-using-class  (class-of condition) condition slot))))

(defun make-relative-pathname (pathname)
  (make-pathname
   :name (pathname-name pathname)
   :type (pathname-type pathname)
   :directory (cons :relative (cdr (pathname-directory pathname)))))



;;; ----------------------------------------------------------------------
;;;
;;;
;;;

(defun map-system-components (fn system)
  (map-component-subcomponents fn (mk::ensure-system system)))

(defun map-component-subcomponents (fn component)
  (when component
    (funcall fn component)
    (when (member (mk::component-type component) '(:module :defsystem))
      (dolist (c (mk::component-components component))
        (map-component-subcomponents fn c)))))

;;; Maintaining a pathname to component table
;;; Maintaining a pathname to system table

(defun clear-pathname-component-table ()
  (clrhash *pathname-component*))

(defun clear-pathname-system-table ()
  (clrhash *pathname-system*))

(defvar *current-system* nil)
(defun register-system-pathnames (system)
  (let ((*current-system* system))
    (map-system-components 'register-component-pathname system)))

(defun recompute-pathname-component-table ()
  (clear-pathname-component-table)
  (map nil 'register-system-pathnames (mk:defined-systems)))

(defun pathname-component (x)
  (or
   (let ((truename (probe-file x)))
     (or (when truename
	   (gethash (pathname x) *pathname-component*))
	(gethash x *pathname-component*)))))

(defun pathname-system (pathname)
  (or
   (let ((truename (probe-file pathname)))
     (when truename
       (gethash (namestring truename) *pathname-system*)))
   (gethash (namestring pathname) *pathname-system*)))

#||
(pathname-system (probe-file "~/cl/extern/defsystem-3.x/defsystem.lisp"))
(mk::component-full-pathname (mk::find-component "iolib/os"  '("src" "os" "pkgdcl")) :source)
(mk::component-full-pathname (mk::find-component "iolib/pathnames"  '("pkgdcl")) :source)
||#


(defun register-component-pathname (component)
  (when (member (mk::component-type component) '(:file))
    (let ((p (mk::component-full-pathname component :source)))
      (when p
	(let ((truename (probe-file p)))
	  (when truename
	    (let ((path (namestring truename)))
	      (assert (mk::component-p *current-system*))
	      (setf (gethash path *pathname-system*) *current-system*)
	      (setf (gethash path *pathname-component*) component))))))))

(recompute-pathname-component-table)


;;; This is a crude hack, see MK-DEFSYSTEM's LP #481187.

(defmethod xref-doit ((type (eql :depends-on)) thing)
  (when (typep thing '(or string symbol))
    (loop for dependency in (who-depends-on thing)
       for asd-file = (mk::system-definition-pathname dependency)
       when asd-file
       collect (list dependency
                     (slynk-backend:make-location
                      `(:file ,(namestring asd-file))
                      `(:position 1)
                      `(:snippet ,(format nil "(defsystem :~A" dependency)
                                 :align t))))))

(defun operate-on-system (system-name operation-name &rest keyword-args)
  "Perform OPERATION-NAME on SYSTEM-NAME using MK-DEFSYSTEM.
The KEYWORD-ARGS are passed on to the operation.
Example:
\(operate-on-system \"cl-ppcre\" 'compile-op :force t)"
  ;;(handler-case
  (slynk-backend:with-compilation-hooks ()
    (apply #'mk:oos system-name operation-name keyword-args)))
;;((or asdf:compile-error #+asdf3 asdf/lisp-build:compile-file-error)
;; () nil)))


;;; ----------------------------------------------------------------------
;;;
;;;
;;;
(defun jump-to-component-1 (system file-namestring)
  (let (src-root seen)
    (labels ((walk (comp &aux subs)
	       (cond ((find (mk::component-type comp) '(:file))
		      (let ((namestring
			     (namestring
			      (mk::component-full-pathname
			       comp :source))))
			(if (equal namestring file-namestring)
			    (return-from jump-to-component-1
			      comp))))
		     ((setq subs (mk::component-components comp))
		      (mapcar #'walk subs))))
	     (system-subdir-p (system src-root)
	       (user::prefixp (namestring
			       (mk::component-root-dir system :source))
			      src-root))
	     (doit (system)
	       (when (and (not (find system seen))
			  (system-subdir-p system src-root)))
	       (push system seen)
	       (walk system)
	       (dolist (system (mk::component-depends-on system))
		 (doit (mk::ensure-system system)))))
      (setq system (mk::ensure-system system))
      (setq src-root (namestring (mk::component-root-dir system :source)))
      (doit system))))


(provide 'slynk/mk-defsystem)

