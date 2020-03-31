;;; slime-undefmethod.el -*- Emacs-Lisp -*-
;;;
;;; Touched: Mon Dec 01 20:01:31 2008 +0530
;;; Bugs-To: Madhu <enometh@net.meer>
;;; License: GNU GPL (same license as Emacs)
;;;

;; (lookup-key  sly-mode-map "\C-c\M-u" 'slime-undefmethod)

(define-sly-contrib sly-undefmethod
  (:slynk-dependencies slynk/undefmethod))

(defun sly-undefmethod ()
  "Remove the method which is defined at point."
  (interactive)
  (let ((form (sly-defun-at-point)))
    (when (string-match "^(defmethod " form)
      (sly-interactive-eval
       (replace-regexp-in-string "^(defmethod "
				 "(slynk::undefmethod " form)))))

(defun sly-undefclass ()
  "Remove the class which is defined at point."
  (interactive)
  (let ((form (sly-defun-at-point)))
    (when (string-match "^(defclass " form)
      (sly-interactive-eval
       (replace-regexp-in-string "^(defclass "
				 "(slynk::undefclass " form)))))

(provide 'sly-undefmethod)
