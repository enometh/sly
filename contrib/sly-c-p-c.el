;;; sly-c-p-c.el

(require 'sly)
(require 'cl-lib)

;; madhu 191109 CHANGES: drop sly-complete-symbol*-fancy
;; sly-c-p-c-unambiguous-prefix-p. use emacs completions-style with
;; partial-completion style instead.
;;
;; Add contextual completion for functions which returns only fboundp
;; symbols.
;;
;; WARNING: remember to (remove-hook 'sly-mode-hook
;; 'sly--setup-completion) as sly-completions uses add-function on the
;; buffer-local completion-in-region-function variable and you can
;; never get rid of that advice.

;; For now we make an effort to work around that with by making
;; completion-at-point-functions, completion-in-region-function and
;; completion-styles local to buffers in sly-mode. This is done in
;; sly-c-p-c-on and undone in sly-c-p-c-off.
;;
;; The contrib load and unload functions defined in define-sly-contrib
;; merely adds and removes sly-c-p-c-on to and from sly-mode-hook.
;; You may have to call sly-c-p-c-off explicitly in individual buffers
;; (this function makes emacs completion variables lose their
;; buffer-local-status)
;;
;; turn-on-sly-c-p-c and turn-off-sly-c-p-c arrange to add and remove
;; sly-c-p-c to sly-contribs.  Actually these ought to be defined
;; before sly-c-p-c is loaded.  One workflow is to sly-c-p-c out of
;; slime-contribs, (require 'sly-c-p-c) and then M-x
;; turn-on-sly-c-p-c.


;; Note: 1. If sly-completion runs after sly-c-p-c-on then we
;; lose. 2. This file has to be "*load*-ed - otherwise sly-c-p-c--path
;; gets set to nil.
;;

(defun sly-c-p-c--setup (&optional foo)
  "Remove our functions from completion-at-point-functions. If ARG is non-NIL
add our functions to completion-at-point-functions."
  (setq-local completion-at-point-functions
	(remove 'sly-maybe-complete-as-filename completion-at-point-functions))
  (setq-local completion-at-point-functions
	(remove 'sly-c-p-c-completion-at-point completion-at-point-functions))
  (cond (foo
	 (push 'sly-c-p-c-completion-at-point completion-at-point-functions)
	 (push 'sly-maybe-complete-as-filename completion-at-point-functions)
	 ;; a completion style of basic contradicts with c-p-c.  make
	 ;; sure partial-completion is first
	 (setq-local completion-styles
		     (cons 'partial-completion
			   (remove 'partial-completion completion-styles)))
	 ;; completion-in-region-function may be advised by sly-completion.el
	 (setq-local completion-in-region-function
		     (default-toplevel-value 'completion-in-region-function))
	 )
	(t (kill-local-variable 'completion-at-point-functions)
	   (kill-local-variable 'completion-styles)
	   (kill-local-variable 'completion-in-region))))

(defun sly-c-p-c-off () (interactive) (sly-c-p-c--setup nil))
(defun sly-c-p-c-on  () (interactive) (sly-c-p-c--setup t))

(defun turn-on-sly-c-p-c ()
  (interactive)
  (sly-setup (adjoin 'sly-c-p-c sly-contribs))
  (sly-c-p-c-init)			;if the above does not do it?
  (with-demoted-errors (sly-contrib--load-slynk-dependencies)) ;not called by sly-setup?
  (if sly-mode (sly-c-p-c-on)))

(defun turn-off-sly-c-p-c ()
  (interactive)
  (sly-setup (remove 'sly-c-p-c sly-contribs))
  (sly-c-p-c-unload)			;uf the above does not do it?
  (with-demoted-errors (sly-contrib--load-slynk-dependencies)) ;not called by sly-setup?
  (if sly-mode (sly-c-p-c-off)))

(define-sly-contrib sly-c-p-c
  "ILISP style Compound Prefix Completion."
  (:authors "Luke Gorrie  <luke@synap.se>"
            "Edi Weitz  <edi@agharta.de>"
            "Matthias Koeppe  <mkoeppe@mail.math.uni-magdeburg.de>"
            "Tobias C. Rittweiler <tcr@freebits.de>")
  (:license "GPL")
  (:slynk-dependencies slynk/c-p-c)
  ;; if sly--setup-completion runs after we run we lose. arrange to
  ;; run at the end of the hook
  (:on-load (add-hook 'sly-mode-hook 'sly-c-p-c-on 50))
  (:on-unload (remove-hook 'sly-mode-hook 'sly-c-p-c-on)))

(defcustom sly-when-complete-filename-expand nil
  "Use comint-replace-by-expanded-filename instead of
comint-dynamic-complete-as-filename to complete file names"
  :group 'sly-ui
  :type 'boolean)

(defun sly-maybe-complete-as-filename ()
  "If point is at a string starting with \", complete it as filename.
Return nil if point is not at filename."
  (when (save-excursion (re-search-backward "\"[^ \t\n]+\\="
                                            (max (point-min)
                                                 (- (point) 1000))
					    t))
    (let ((comint-completion-addsuffix '("/" . "\"")) partial-filename)
      (if (and sly-when-complete-filename-expand
	       (setq partial-filename (comint-match-partial-filename)))
	  (replace-match (expand-file-name partial-filename) t t))
      (comint--complete-file-name-data))))

(cl-defun sly-c-p-c-completion-at-point ()
  "Complete the symbol at point.
sly-expand-abbreviations-and-complete
Perform completion similar to `elisp-completion-at-point'."
  (lexical-let (beg end)
    (list (setq beg (sly-symbol-start-pos))
	  (setq end (sly-symbol-end-pos))
	  (completion-table-dynamic
	   (lambda (_)
	     (first (sly-contextual-completions beg end)))
	   nil))))

(cl-defun sly-contextual-completions (beg end)
  "Return a list of completions of the token from BEG to END in the
current buffer."
  (when (> end beg)
  (let ((token (buffer-substring-no-properties beg end)))
    (cond
     ((let ((min (point-min)))
	(and (< beg (point-max))
	     (or (and (> (1- beg) min)
		      (string= (buffer-substring-no-properties (1- beg) beg) "("))
		 (and (> (- beg 2) min)
		      (string= (buffer-substring-no-properties (- beg 2) beg) "#'")))))
      ;; Contextual function completion. Just like prefix completions
      ;; but only for fboundp candidates
      (let ((completions
             (sly-completions-for-function token)))
        (when (cl-first completions)
          (cl-return-from sly-contextual-completions completions))
        ;; If no matching function was found, do regular symbol
        ;; completion.
        ))
     ((and (< beg (point-max))
           (string= (buffer-substring-no-properties beg (1+ beg)) ":"))
      ;; Contextual keyword completion
      (let ((completions
             (sly-completions-for-keyword token
					  (save-excursion
					    (goto-char beg)
					    (sly-parse-form-upto-point)))))
        (when (cl-first completions)
          (cl-return-from sly-contextual-completions completions))
        ;; If no matching keyword was found, do regular symbol
        ;; completion.
        ))
     ((and (>= (length token) 2)
           (string= (cl-subseq token 0 2) "#\\"))
      ;; Character name completion
      (cl-return-from slime-contextual-completions
        (sly-completions-for-character token))))
    ;; Regular symbol completion
    (sly-prefix-completions token))))

(defun sly-completions-for-function (prefix)
  (sly-eval `(cl:let ((slynk-c-p-c::*fboundp-test* cl:t))
		     (slynk-c-p-c:completions ,prefix ',(sly-current-package)))))

(defun sly-prefix-completions (prefix)
  (sly-eval `(slynk-c-p-c:completions ,prefix ',(sly-current-package))))

(defun sly-completions-for-keyword (prefix buffer-form)
  (sly-eval `(slynk-c-p-c:completions-for-keyword ,prefix ',buffer-form)))

(defun sly-completions-for-character (prefix)
  (cl-labels ((append-char-syntax (string) (concat "#\\" string)))
    (let ((result (sly-eval `(slynk-c-p-c:completions-for-character
                                ,(cl-subseq prefix 2)))))
      (when (car result)
        (list (mapcar #'append-char-syntax (car result))
              (append-char-syntax (cadr result)))))))

(provide 'sly-c-p-c)
