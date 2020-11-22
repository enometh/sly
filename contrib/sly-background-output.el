;; -*- lexical-binding:t -*-
(require 'sly)
(require 'cl-lib)
(require 'sly-mrepl)

(define-sly-contrib sly-background-output
  "Redirect the output streams of a slynk connection to an emacs  *Background-Output* buffer instead of the inferior lisp buffer. If you have a slynk-mrepl this won't work."
  (:slynk-dependencies slynk/background-output)
  ;; if sly-mrepl-init runs before we run we lose. arrange to
  ;; run at the end of the hook
  (:on-load (add-hook 'sly-connected-hook 'sly-background-output-start 50))
  (:on-unload (remove-hook 'sly-connected-hook 'sly-background-output-start)))

(sly-define-channel-type background-output)

(sly-def-connection-var sly-connection-background-output-buffer nil
  "The buffer for the Connection.  May be nil or a dead buffer.")

(sly-def-connection-var sly-connection-background-output-local-channel nil)

(defvar sly-background-output-buffer-format
   "*sly-background-output for %s*"
"buffer name for the Background Output buffer.
   Hint: You can use `display-buffer-reuse-frames' and
`display-buffer-alist' to customize the frame in which this buffer should appear.")

(defvar sly-background-output-last-output-marker nil)

(defun sly-background-output-buffer (&optional noprompt)
  "Return the background-output buffer, create it if necessary."
  (let ((buffer (sly-connection-background-output-buffer)))
    (or (if (buffer-live-p buffer) buffer)
        (setf (sly-connection-background-output-buffer)
              (let ((connection (sly-connection)))
		(with-current-buffer (get-buffer-create (format sly-background-output-buffer-format (sly-connection-name connection)))
		  (sly-mode 1)
		  (setq sly-buffer-connection connection)
		  (setq-local sly-background-output-last-output-marker
				(make-marker))
		  (set-marker sly-background-output-last-output-marker
			      (point))
		  (current-buffer)))))))

(sly-define-channel-type background-output)

;; hack support log4sly with log4cl:*global-console* bound to
;; *standard-output*
(defvar sly-background-output-write-string-hooks nil
  "Call Hook functions after a writes-string channel method in the background
buffer `sly-background-output-buffer-beg-pos' and
`sly-background-output-buffer-end-pos' bound to the boundaries of the
inserted string" )

(defvar sly-background-output-buffer-beg-pos)
(defvar sly-background-output-buffer-end-pos)

(defvar sly-background-output-scroll-to-bottom t
  "If NIL do not mess with the window point.

If the selected window is displaying the Background Output buffer we do not
touch the mark and point at all.

But if some other window is displaying the Background Output buffer then we
set the window point after output is finished to the beginning the output if
either `sly-background-output-scroll-to-bottom' is non nil, or the point was
visible was the bottom of the screen when output began.")

(defvar sly-background-autofocus nil
  "If non-nil select background output windows on display.")

(sly-define-channel-method background-output :write-string (string)
  (with-current-buffer (sly-background-output-buffer)
    (cl-flet ((doit
	       ()
	       (goto-char sly-background-output-last-output-marker)
	       (let ((sly-background-output-buffer-beg-pos (point))
		     (inhibit-read-only t))
		 (insert string)
		 (set-marker sly-background-output-last-output-marker (point))
		 (let ((sly-background-output-buffer-end-pos (point)))
		   (pulse-momentary-highlight-region
		    sly-background-output-buffer-beg-pos
		    sly-background-output-buffer-end-pos)
		   (run-hooks 'sly-background-output-write-string-hooks)))))
      (let* ((window (get-buffer-window nil 0))
	     (p (point))
	     (restore-p (and window
			     (pos-visible-in-window-p (1+ (buffer-size))
						      window t))))
	(if (eq (selected-window) window)
	    (save-excursion (doit))
	  (doit)
	  (display-buffer (current-buffer)
			  (cons nil (list '(inhibit-same-window . t)
					  `(inhibit-switch-frame . ,(not sly-background-autofocus)))))
	  (warn "%s" (list 'selected (selected-window) 'window window))
	  (when (and (or sly-background-output-scroll-to-bottom
			 restore-p)
		     window
		     (not (eq (selected-window) window)))
	    (when (region-active-p)
	      (pulse-momentary-highlight-region (region-beginning) (region-end))
	      (deactivate-mark t))
	    (let ((transient-mark-mode nil))
	      ;; (1+ (buffer-size))
	      (set-window-point window p))))))))

(defun sly-background-output-start ()
  (interactive)				;?
  (when (null (sly-connection-background-output-local-channel))
    (let* ((ch (sly-make-channel sly-background-output-channel-methods)) ;TODO ch closed
	   (id (sly-channel.id ch)))
      (setf (sly-connection-background-output-local-channel) ch)
      (sly-eval-async
	  `(slynk-background-output:create-background-output ,id)
	(lambda (result)
	  (cl-destructuring-bind (remote thread-id) result
	    (message "set up background output for channel local:%s remote=%s thread-id=%s"
		     id
		     remote
		     thread-id)))))))

(defun sly-background-output-stop ()
  (interactive)				;?
  (let ((ch (sly-connection-background-output-local-channel)))
    (when ch
      (sly-send-to-remote-channel (sly-channel.id ch) `(:teardown))
      (sly-close-channel ch)
      (setf (sly-connection-background-output-local-channel) nil))))

(defun sly-background-output-ansi-color-callback ()
  ;; called from write-string channel method with the current buffer
  ;; set to background-output-buffer and the following special
  ;; variables bound to the boundaries of the inserted-string
  (let ((start sly-background-output-buffer-beg-pos)
	(end sly-background-output-buffer-end-pos))
    (ansi-color-apply-on-region start end)))

(add-hook 'sly-background-output-write-string-hooks
	  'sly-background-output-ansi-color-callback)


(provide 'sly-background-output)
