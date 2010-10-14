;; This is a simple mode for Emacs to support DBLP queries and
;; insertions
;;
;; Copyright (C) 2010, Martin Grund <grundprinzip@gmail.com>

(require 'json)
(require 'widget)


(defun dblp-query-browse (q)
  "This method is called to display a list of citations that
match the search query."
  (interactive "MDBLP Query: ")
  (let (result citations)
    (switch-to-buffer "*DBLP*")
    (message "Searching for %s" q)
    
    (setq result (shell-command-to-string (concat "ruby " "query.rb " q)))
    (goto-char 1)

    ; Parse the result json and be happy
    (setq citations (json-read-from-string result))

    (loop for elem across citations do
    	  (let (cite title)
    	    (setq cite (cdr (assoc 'key elem)))
    	    (setq title (cdr (assoc 'title elem)))
	    ; Write the widget here
    	  ))
    
    ;;(insert result)
  
    ))


(provide 'dblp)


