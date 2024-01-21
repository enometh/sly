;;; sly-mk-defsystem.el --- MK-DEFSYSTEM system support for SLY -*- lexical-binding: t; -*-
;;
;; Version: 0.1
;; Original-URL: https://github.com/mmgeorge/sly-asdf
;; Keywords: languages, lisp, sly, mk-defsystem
;; Package-Requires: ((emacs "24.3")(sly "1.0.0-beta2")(popup "0.5.3"))
;; Maintainer: Matt George <mmge93@gmail.com>
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; `sly-mk-defsystem` is an external contrib for SLY that provides additional
;; support for working with mk-defsystem projects.
;;
;;; Code:

(require 'sly)
(require 'cl-lib)
(require 'grep)

(defvar sly-mrepl-shortcut-alist) ;; declared in sly-mrepl

(defvar sly-mk-defsystem-find-system-file-max-depth 10
  "Max recursive depth for finding an asd system definition from the current directory.")

(defvar sly-mk-defsystem-shortcut-alist
  '(("load-system" . sly-mk-defsystem-load-system)
    ("reload-system" . sly-mk-defsystem-reload-system)
    ("test-system" . sly-mk-defsystem-test-system)
    ("browse-system" . sly-mk-defsystem-browse-system)
    ("open-system" . sly-mk-defsystem-open-system)
    ("save-system" . sly-mk-defsystem-save-system)))


(define-sly-contrib sly-mk-defsystem
  "MK-DEFSYSTEM system support"
  (:authors "Daniel Barlow       <dan@telent.net>"
            "Marco Baringer      <mb@bese.it>"
            "Edi Weitz           <edi@agharta.de>"
            "Stas Boukarev       <stassats@gmail.com>"
            "Tobias C Rittweiler <tcr@freebits.de>")
  (:license "GPL")
  (:slynk-dependencies slynk/mk-defsystem)
  (:on-load
   (when (boundp 'sly-mrepl-shortcut-alist)
     (setq sly-mrepl-shortcut-alist
           (append sly-mrepl-shortcut-alist sly-mk-defsystem-shortcut-alist)))))


(defvar *sly-mk-defsystem-lisp-extensions* (list "lisp" "system" "l" "cl")
  "File extensions to look for when finding open Lisp files.")

(defun sly-mk-defsystem--lisp-buffer-p (buffer)
  "Check whether BUFFER refers to a Lisp buffer."
  (member (file-name-extension (buffer-name buffer)) *sly-mk-defsystem-lisp-extensions*))


(defun sly-mk-defsystem--current-lisp-buffers ()
  "Traverses the current `buffer-list`, returning those buffers with a .lisp extension."
  (cl-remove-if-not #'sly-mk-defsystem--lisp-buffer-p (buffer-list)))


;;; Interactive functions

(defun sly-mk-defsystem-load-system (&optional system)
  "Compile and load an MK-DEFSYSTEM SYSTEM.
Default system name is taken from first file matching *.asd in current
buffer's working directory"
  (interactive (list (sly-mk-defsystem-read-system-name)))
  (sly-mk-defsystem-oos system :load :force t))


(defun sly-mk-defsystem-reload-system (system)
  "Compile and load an MK-DEFSYSTEM SYSTEM without reloading dependencies.
Default system name is taken from first file matching *.asd in current
buffer's working directory"
  (interactive (list (sly-mk-defsystem-read-system-name)))
  (sly-mk-defsystem-save-some-lisp-buffers)
  ;;(sly-mk-defsystem-display-output-buffer)
  (message "Performing MK-DEFSYSTEM LOAD on system %S" system)
  (sly-eval-async
      `(slynk-mk-defsystem:reload-system ,system)
    #'(lambda (result)
        (sly-mk-defsystem-oos-finished result (current-buffer))
        (run-hooks 'sly-mk-defsystem--after-oos-hook))))


(defun sly-mk-defsystem-compile-system (&optional system)
  "Compile and load an MK-DEFSYSTEM SYSTEM.
Default system name is taken from first file matching *.asd in current
buffer's working directory"
  (interactive (list (sly-mk-defsystem-read-system-name)))
  (sly-mk-defsystem-oos system :compile :propagate nil :force t))

(defun sly-mk-defsystem-test-system (&optional system)
  "Compile and test an MK-DEFSYSTEM SYSTEM.
Default system name is taken from first file matching *.asd in current
buffer's working directory"
  (interactive (list (sly-mk-defsystem-read-system-name)))
  (sly-mk-defsystem-oos system :test :force t))


(defun sly-mk-defsystem-save-system (system)
  "Save files belonging to an MK-DEFSYSTEM SYSTEM."
  (interactive (list (sly-mk-defsystem-read-system-name)))
  (sly-eval-async
      `(slynk-mk-defsystem:mk-defsystem-system-files ,system)
    (lambda (files)
      (dolist (file files)
        (let ((buffer (get-file-buffer (sly-from-lisp-filename file))))
          (when buffer
            (with-current-buffer buffer
              (save-buffer buffer)))))
      (message "Done."))))


(defun sly-mk-defsystem-browse-system (name)
  "Browse files in an MK-DEFSYSTEM system NAME using Dired."
  (interactive (list (sly-mk-defsystem-read-system-name)))
  (sly-eval-async `(slynk-mk-defsystem:mk-defsystem-system-directory ,name)
    (lambda (directory)
      (when directory
        (dired (sly-from-lisp-filename directory))))))

(defun sly-mk-defsystem-open-system (name &optional load interactive)
  "Open all files implicated in an MK-DEFSYSTEM system, in separate emacs buffers."
  (interactive (list (sly-mk-defsystem-read-system-name) nil t))
  (when (or load
            (and interactive
                 (not (sly-eval `(slynk-mk-defsystem:mk-defsystem-system-loaded-p ,name)))
                 (y-or-n-p "Load it? ")))
    (sly-mk-defsystem-load-system name))
  (sly-eval-async
      `(slynk-mk-defsystem:mk-defsystem-system-files ,name)
    (lambda (files)
      (when files
        (let ((files (mapcar 'sly-from-lisp-filename
                             (nreverse files))))
          (find-file-other-window (car files))
          (mapc 'find-file (cdr files)))))))

(defun sly-mk-defsystem-rgrep-system (sys-name regexp)
  "Run `rgrep' for REGEXP for SYS-NAME on the base directory of an MK-DEFSYSTEM system."
  (interactive (progn (grep-compute-defaults)
                      (list (sly-mk-defsystem-read-system-name nil nil)
                            (grep-read-regexp))))
  (rgrep regexp "*.lisp"
         (sly-from-lisp-filename
          (sly-eval `(slynk-mk-defsystem:mk-defsystem-system-directory ,sys-name)))))


;; FIXME Invalid read syntax: "#"
;;#in packet:
;;#(:return (:ok (#P"/home/madhu/h/elisp/pkg/slime/swank.lisp" #P"/home/madhu/h/elisp/pkg/slime/swank/rpc.lisp" #P"/home/madhu/h/elisp/pkg/slime/swank/match.lisp" #P"/home/madhu/h/elisp/pkg/slime/swank/gray.lisp" #P"/home/madhu/h/elisp/pkg/slime/swank/ccl.lisp" #P"/home/madhu/h/elisp/pkg/slime/metering.lisp" #P"/home/madhu/h/elisp/pkg/slime/swank/backend.lisp" #P"/home/madhu/h/elisp/pkg/slime/packages.lisp")) 8)
(defun sly-mk-defsystem-isearch-system (sys-name)
  "Run function `isearch-forward' on the files of an MK-DEFSYSTEM system SYS-NAME."
  (interactive (list (sly-mk-defsystem-read-system-name nil nil)))
  (let* ((files (mapcar 'sly-from-lisp-filename
                        (sly-eval `(slynk-mk-defsystem:mk-defsystem-system-files ,sys-name))))
         (multi-isearch-next-buffer-function
          (let*
              ((buffers-forward  (mapcar #'find-file-noselect files))
               (buffers-backward (reverse buffers-forward)))
            #'(lambda (current-buffer wrap)
                ;; Contrarily to the docstring of
                ;; `multi-isearch-next-buffer-function', the first
                ;; arg is not necessarily a buffer. Report sent
                ;; upstream. (2009-11-17)
                (setq current-buffer (or current-buffer (current-buffer)))
                (let* ((buffers (if isearch-forward
                                    buffers-forward
                                  buffers-backward)))
                  (if wrap
                      (car buffers)
                    (cl-second (memq current-buffer buffers))))))))
    (isearch-forward)))


(defun sly-mk-defsystem-query-replace-system (name from to &optional delimited)
  "Query-replace in all files of an MK-DEFYSTEM system.

NAME is the MK-DEFSYSTEM's sytem name, FROM is the string to replace, TO
its replacement, and the optional DELIMITED when true restricts
replacements to word-delimited matches."
  (interactive (let ((system (sly-mk-defsystem-read-system-name)))
                 (cons system (sly-mk-defsystem-read-query-replace-args
                               "Query replace throughout `%s'" system))))
  (fileloop-initialize-replace
   (regexp-quote from) to
   (mapcar #'sly-from-lisp-filename
	   (sly-eval `(slynk-mk-defsystem:mk-defsystem-system-files ,name)))
    'default
   delimited)
  (fileloop-continue))


(defun sly-mk-defsystem-query-replace-system-and-dependents
    (name from to &optional delimited)
  "Run `query-replace' on an MK-DEFSYSTEM system with NAME given FROM and TO.
DELIMITED is optional.  Includes the base system and all other systems it depending on it."
  (interactive (let ((system (sly-mk-defsystem-read-system-name)))
                 (cons system (sly-mk-defsystem-read-query-replace-args
                               "Query replace throughout `%s'+dependencies"
                               system))))
  (sly-mk-defsystem-query-replace-system name from to delimited)
  (dolist (dep (sly-mk-defsystem-who-depends-on-rpc name))
    (when (y-or-n-p (format "Descend into system `%s'? " dep))
      (sly-mk-defsystem-query-replace-system dep from to delimited))))


(defun sly-mk-defsystem-delete-system-fasls (name)
  "Delete FASLs produced by compiling a system with NAME."
  (interactive (list (sly-mk-defsystem-read-system-name)))
  (sly-eval-async
      `(slynk-mk-defsystem:delete-system-fasls ,name)
    'message))


(defun sly-mk-defsystem-who-depends-on (sys-name)
  "Determine who depends on system with SYS-NAME."
  (interactive (list (sly-mk-defsystem-read-system-name)))
  (sly-xref :depends-on sys-name))


;;; Utilities

(defgroup sly-mk-defsystem nil
  "MK-DEFSYSTEM support for Sly."
  :prefix "sly-mk-defsystem-"
  :group 'sly)


(defvar sly-mk-defsystem-system-history nil
  "History list for MK-DEFSYSTEM system names.")


(defun sly-mk-defsystem-bogus-completion-alist (list)
  "Make an alist out of LIST.
The same elements go in the CAR, and nil in the CDR.  To support the
apparently very stupid `try-completions' interface, that wants an
alist but ignores CDRs."
  (mapcar (lambda (x) (cons x nil)) list))


(defun sly-mk-defsystem-save-some-lisp-buffers ()
  "Compatability."
  ;;(if slime-repl-only-save-lisp-buffers
  ;;(save-some-buffers nil (lambda ()
  ;;(and (memq major-mode slime-lisp-modes)
  ;;(not (null buffer-file-name)))))
  (save-some-buffers))


(defun sly-mk-defsystem-read-query-replace-args (format-string &rest format-args)
  "Read query args, displaying FORMAT-STRING with FORMAT-ARGS."
  (let* ((common (query-replace-read-args (apply #'format format-string
                                                 format-args)
                                          t t)))
    (list (nth 0 common) (nth 1 common) (nth 2 common))))


(defun sly-mk-defsystem-read-system-name (&optional prompt default-value)
  "Read a system name from the minibuffer, prompting with PROMPT.
If no DEFAULT-VALUE is given, one is tried to be determined: if
DETERMINE-DEFAULT-ACCURATELY is true, by an RPC request which
grovels through all defined systems; if it's not true, by looking
in the directory of the current buffer."
  (let* ((completion-ignore-case nil)
         (prompt (or prompt "System"))
         (system-names (sly-eval `(slynk-mk-defsystem:list-mk-defsystem-systems)))
         (default-value
           (or default-value (sly-mk-defsystem-find-current-system) (car sly-mk-defsystem-system-history)))
         (prompt (concat prompt (if default-value
                                    (format " (default `%s'): " default-value)
                                  ": "))))
    (let ((history-delete-duplicates t))
      (completing-read prompt (sly-mk-defsystem-bogus-completion-alist system-names)
                       nil nil nil
                       'sly-mk-defsystem-system-history default-value))))


(cl-defun sly-mk-defsystem-find-current-system (&optional buffer)
  "Find the name of the current asd system."
  (setf buffer (or buffer
                   (cl-find-if #'buffer-file-name (sly-mk-defsystem--current-lisp-buffers))
                   (current-buffer)))
  (let* ((buffer-file-name (buffer-file-name buffer))
         (directory (if buffer-file-name
                        (file-name-directory buffer-file-name)
                      default-directory))
         (system-file (sly-mk-defsystem-find-system-file directory)))
    (when system-file
      (file-name-base system-file))))


(cl-defun sly-mk-defsystem-find-system-file (directory &optional (depth sly-mk-defsystem-find-system-file-max-depth))
  "Find the first file in the current DIRECTORY or a parent of DIRECTORY that includes a .system file."
  (let ((fname (directory-file-name directory)))
    (or
     (cl-find-if #'(lambda (file) (string-equal "system" (file-name-extension file)))
                 (directory-files directory))
     (and (> depth 0)
          (file-name-directory fname)
          (sly-mk-defsystem-find-system-file (file-name-directory fname) (1- depth))))))


(defun sly-mk-defsystem-determine-mk-defsystem-system (filename buffer-package)
  "Try to determine the mk-defsystem system provided BUFFER-PACKAGE that FILENAME belongs to."
  (sly-eval
   `(slynk-mk-defsystem:mk-defsystem-determine-system ,(and filename
                                            (sly-to-lisp-filename filename))
                                      ,buffer-package)))


(defun sly-mk-defsystem-who-depends-on-rpc (system)
  "Find who depends on RPC for SYSTEM."
  (sly-eval `(slynk-mk-defsystem:who-depends-on ,system)))


(defun sly-mk-defsystem-oos (system operation &rest keyword-args)
  "Operate On System.  Apply the given OPERATION on SYSTEM provided KEYWORD-ARGS."
  (message "Performing MK-DEFSYSTEM %S%s on system %S"
           operation (if keyword-args (format " %S" keyword-args) "")
           system)
  (sly-eval-async
      `(slynk-mk-defsystem:operate-on-system-for-emacs ,system ',operation ,@keyword-args)
    #'(lambda (result)
        (sly-mk-defsystem-oos-finished result (current-buffer))
        (run-hooks 'sly-mk-defsystem--after-oos-hook))))


(defun sly-mk-defsystem-oos-finished (result buffer &optional message)
  "Called when compilation is finished"
  (let ((notes (sly-compilation-result.notes result))
        (duration (sly-compilation-result.duration result))
        (successp (sly-compilation-result.successp result)))
    (sly-show-note-counts notes duration successp t)
    (setf sly-last-compilation-result result) ;; For interactive use
    (when sly-highlight-compiler-notes
      (sly-highlight-notes notes))
    (when message (message "%s" message))
    ;; Conditionally show compilation log and other options defined in settings
    (run-hook-with-args 'sly-compilation-finished-hook successp notes buffer t)))



;;;###autoload
(with-eval-after-load 'sly
  (add-to-list 'sly-contribs 'sly-mk-defsystem 'append))

(defun get-swank-or-slynk-evaler ()
  (cond ((and (boundp 'sly-current-thread)
	      sly-current-thread)
	 'sly-eval)
	((and (boundp 'slime-current-thread)
	      slime-current-thread)
	 'slime-eval)
	((error "no slime or sly connection"))))

(defun sly-require (pkg)
  (interactive "sSystem name: ")
  (funcall (get-swank-or-slynk-evaler) `(cl:require ',(intern (downcase pkg)))))

(defun find-system (str)
  (interactive "sSystem name: ")
  (let ((evaler (get-swank-or-slynk-evaler))
	(pkg str))
    (if (symbolp pkg) (setq pkg (symbol-name pkg)))
    (setq pkg (downcase pkg))
    (if (string-prefix-p ":" pkg)
	(setq pkg (substring pkg 1)))
    (let ((path (funcall evaler `(cl:namestring (mk::find-system-definition-pathname ,pkg)))))
      (when (file-exists-p path)
	(find-file path)))))

(provide 'sly-mk-defsystem)
;;; sly-mk-defsystem.el ends here


