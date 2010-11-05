;; This is a simple mode for Emacs to support DBLP queries and
;; insertions
;;
;;  ;; DBLP mode
;;  (add-to-list 'load-path "~/.emacs.d/elisp/dblp.el")
;;  (require 'dblp)
;;  (add-hook 'LaTeX-mode-hook 'dblp-mode)
;;
;; Copyright (C) 2010, Martin Grund <grundprinzip@gmail.com>

(require 'json)
(require 'widget)
(require 'url)
(require 'http-post-simple)

(eval-when-compile
  (require 'wid-edit))

(defvar dblp-last-buffer nil
  "Variable to store the name of the last buffer")

(defvar dblp-basic-query '
  "Basic Query Information")


;; Define the minor mode and the key binding
(define-minor-mode dblp-mode
  "Toggle DBLP mode"
  nil
  " dblp"
  '( ("\C-\M-c" . dblp-query-browse))
  :group 'dblp)


(defun dblp-do-request (q)
  (let (processed result line body body-object)
    (setq result (http-post-simple
  		"http://dblp.mpi-inf.mpg.de/autocomplete-php/autocomplete/ajax.php"
		'((mo . "100")
		 (navigation_mode . "history")
		 (qi . "1")
		 (eph . "1")
		 (path . "/dblp-mirror/")
		 (accc . ":")
		 (language . "en")
		 (fh . "1")
		 (er . "20")
		 (syn . "0")
		 (mcsr . "40")
		 (log . "/var/opt/completesearch/log/completesearch.error_log")
		 (fhs . "1")
		 (dm . "3")
		 (deb . "0")
		 (mcc . "0")
		 (rid . "0")
		 (mcs . "20")
		 (bnm . "R")
		 (page . "index.php")
		 (hrd . "1a")
		 (mcl . "80")
		 (name . "dblpmirror")
		 (qt . "H")
		 (ll . "2")
		 (qid . "6")
		 (query . "Kossm")
		 (hrw . "1d")
		 (hpp . "20"))
		))

    ;; Once we have the result we have to extract the correct line
    (setq line (nth 29 (split-string (nth 0 result) "\n")))
    (setq body (replace-regexp-in-string "'" "\"" (substring line 11 -1)))

    ;; Parse JSON
    (setq body-object (cdr (assoc 'body (json-read-from-string body))))

    (setq processed ())
    (let ((idx 0) (next_m t))
      (while  (not (null next_m))

	; Scan for the table row
      	(setq next_m (string-match "<tr><td.*?><a href=\"http://dblp.uni-trier.de/rec/bibtex/\\(.*?\\)\">.*?</td><td.*?>\\(.*?\\)</td><td.*?>\\(.*?\\)</td></tr>" body-object idx))
	(setq idx (match-end 0))

	; As a last step strip all tags from the cite-label
	(let ((cite-key (match-string 1 body-object))
	      (cite-label (replace-regexp-in-string "<.*?>" "" (match-string 3 body-object)))
	      (tmp-list () ))
	  (setq tmp-list (list cite-key cite-label))
	  (setq processed (cons tmp-list processed)))
      	)
      )
    processed)
)


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

  (setq result (dblp-do-request q))
  
  (loop for elem in result do
	(let (cite title)
	  (setq cite (pop elem))
	  (setq title (pop elem))
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



