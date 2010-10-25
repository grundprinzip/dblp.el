;; This is a simple mode for Emacs to support DBLP queries and
;; insertions
;;
;; Copyright (C) 2010, Martin Grund <grundprinzip@gmail.com>

(require 'json)
(require 'widget)

(eval-when-compile
  (require 'wid-edit))

(defvar dblp-last-buffer nil
  "Variable to store the name of the last buffer")

(define-minor-mode dblp-mode
  "Toggle DBLP mode"
  nil
  " dblp"
  '( ("\C-\M-c" . dblp-query-browse))
  :group 'dblp)

(defun dblp-insert-citation (cite)
  (kill-buffer)
  (switch-to-buffer dblp-last-buffer)
  (insert (format "\\cite{%s}" cite))
  )

(defun dblp-query-browse (q)
  (interactive "MDBLP Query: ")
  (setq dblp-last-buffer (buffer-name))
  
  (switch-to-buffer "*DBLP*")
  (kill-all-local-variables)
  (make-local-variable 'widget-example-repeat)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (let ((all (overlay-lists)))
    ;; Delete all the overlays.
    (mapcar 'delete-overlay (car all))
    (mapcar 'delete-overlay (cdr all)))

  (message "Searching for %s" q)
  (widget-insert "=== DBLP Query Result")
  (widget-insert "\n\n")
  
  (setq result (shell-command-to-string (concat "ruby " "query.rb " q)))
  					; Parse the result json and be happy
  (setq citations (json-read-from-string result))

  (loop for elem across citations do
  	(let (cite title)
  	  (setq cite (cdr (assoc 'cite elem)))
  	  (setq title (cdr (assoc 'title elem)))
  	  (widget-create 'link
  			 :tag cite
  			 :format "\t%[%v%]\n\n"
  			 :help-echo "Click to insert citation"
  			 :notify (lambda (widget &rest ignore)
				   (dblp-insert-citation (widget-get widget :tag)))
  			 title)
  	  ))          
  (use-local-map widget-keymap)
  (widget-setup)
  (goto-line 0)
  )

(provide 'dblp)



