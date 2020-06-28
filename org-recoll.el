;; -*- lexical-binding: t; -*-
;;; org-recoll.el --- a simple emacs interface to recoll full-text search that outputs with org-mode links
;;; Commentary:

;;; Code:

(require 'org)
(require 'dired)
(require 'doc-view)

;; Also recommended for best results:
;;(require 'pdf-tools)
;;(require 'org-pdfview)
;;(require 'ereader)
;;(require 'shr)

;;
;; Setup/Init
;;

(defgroup org-recoll nil
  "Recoll text-search integration for Org Mode"
  :group 'org
  :prefix "org-recoll-")

;; Public Variables

(defcustom org-recoll-results-num 10
  "This is the number of results to be displayed per page.
High numbers will reduce responsiveness.  It's set to 10 by
default for convenient paging"
  :group 'org-recoll
  :type 'integer)

(defcustom org-recoll-command-invocation "recoll -t -A"
  "This is the stem of the recoll shell command called by ORG-RECOLL-SEARCH.
Change this if your recoll executable is not in your path.
CAUTION: At present the parsing below expects a specific output
format, so changing the flags will break things."
  :group 'org-recoll
  :type 'string)

(defcustom org-recoll-index-invocation "recollindex"
  "This is the shell command called by ORG-RECOLL-UPDATE-INDEX.
Modify this if your recoll configuration file is not in the
standard location.  Note: do not add an & as the function already
backgrounds the process by default"
  :group 'org-recoll
  :type 'string)

(defcustom org-recoll-file-search-automatically t
  "Toggle whether file-search starts automatically after following a link.
Set to nil to disable.  This is a good idea if you aren't opening
the files in Emacs."
  :group 'org-recoll
  :type 'boolean)

(defcustom org-recoll-file-search-prompt t
  "Toggle prompt for an alternative file-search term after following a link.
If ORG-RECOLL-FILE-SEARCH-AUTOMATICALLY is t and this is nil, then a
file-search for the default term will auto-start on link opening.  If
ORG-RECOLL-FILE-SEARCH-AUTOMATICALLY is nil, then no search is
initated and this variable is not evaluated."
  :group 'org-recoll
  :type 'boolean)

(defcustom org-recoll-search-history nil
  "List to store your recoll search history."
  :group 'org-recoll
  :type 'list
)

(defcustom org-recoll-result-file-read-only t
  "Toggle whether opened search results are read-only.
This setting is intended as a precaution against accidentally
deleting/editing parts of your research library."
  :group 'org-recoll
  :type 'boolean)

(defcustom org-recoll-render-html t
  "Toggle whether opened html search results are automatically rendered."
  :group 'org-recoll
  :type 'boolean)

;; Internal Variables

(defvar org-recoll-end-of-current-page org-recoll-results-num)

(defvar org-recoll-start-of-current-page 0)

(defvar org-recoll-search-query nil)

(defvar org-recoll-filename nil)

(defvar org-recoll-html-file-types '(html xml opf htm))

;; Mode setup

(defvar org-recoll-mode-map
  (let ((kmap (make-sparse-keymap)))
    (define-key kmap (kbd "C-c n") 'org-recoll-next-page)
    (define-key kmap (kbd "C-c p") 'org-recoll-previous-page)
    (define-key kmap (kbd "C-c q") 'delete-window)
    kmap)
  "The keymap used for `org-recoll-mode'.")

(define-minor-mode org-recoll-mode
  "A minor mode to simplify navigation of recoll search results.
\\<org-recoll-mode-map> Some useful keys are:
\n
\\[org-recoll-next-page] - Advance to Next page of search results.
\\[org-recoll-previous-page] - Go back to previous page of search results.
\\[delete-window] - Exit.
\n
\\{org-recoll-mode-map}"
  nil ; default value
  :lighter " org-recoll" ; modeline notice
  :keymap org-recoll-mode-map ; key bindings
  :group 'org-recoll)

;;
;; Internal Functions
;;

(defun org-recoll-compare-string-to-list (string list)
  "Compares STRING to each element of LIST."
  (let ((matched nil))
    (while list
      (if (string= (prin1-to-string (car list)) string)
	  (setq matched t))
      (setq list (cdr list)))
    matched))

(defun org-recoll-fill-region-paragraphs ()
  "Fill region like `org-fill-paragraph' for each para in buffer."
  (interactive "r\nP")
  (goto-char (point-min))
  (save-excursion
    (while (< (point) (point-max))
     (org-fill-paragraph t)
      (forward-paragraph))))


(defun org-recoll-sanitize-single-quote (source-string)
  "Replace all instances of ' in SOURCE-STRING to be shell safe."
  (replace-regexp-in-string (regexp-quote "'") "'\\''" source-string nil 'literal))


(defun org-recoll-reformat-for-file-search (source-string)
  "Strip certain special search language characters in SOURCE-STRING with nil.
This is necessary because isearch has a different search idiom
than recoll, so, for example, a successful \"phrasal search\" in
recoll will yield no results in isearch.  I also strip out result
narrowing features like author: or title: style searches."
  (setq source-string (replace-regexp-in-string (regexp-quote "\"") "" source-string nil 'literal))
  (setq source-string (replace-regexp-in-string "\\(.*\\):.*?\s" "" source-string nil 'literal)))



(defun org-recoll-shr-render-current-buffer()
  (or (fboundp 'libxml-parse-html-region)
      (error "This function requires Emacs to be compiled with libxml2"))
  (let* (
	 (render-buffer (get-buffer-create "*html*"))
	 (intermediate-render (libxml-parse-html-region (point-min) (point-max))))
    (kill-buffer-if-not-modified (current-buffer))
    (with-current-buffer render-buffer
      (erase-buffer)
      (shr-insert-document intermediate-render)
      (goto-char (point-min)))
    (switch-to-buffer render-buffer)))
 
(defun org-recoll-doc-view-search (squery)
  "Jump to the next match of SQUERY in 'doc-view-mode'.
If the current document hasn't been transformed to plain text
till now do that first."
    ;; New search, so forget the old results.
    (setq doc-view--current-search-matches nil)
    (let ((txt (expand-file-name "doc.txt"
				 (doc-view--current-cache-dir))))
      (if (file-readable-p txt)
	  (progn
	    (setq doc-view--current-search-matches
		  (doc-view-search-internal squery txt))
	    (message "DocView: search yielded %d matches."
		     (doc-view-search-no-of-matches
		      doc-view--current-search-matches)))
	;; We must convert to TXT first!
	(if doc-view--current-converter-processes
	    (message "DocView: please wait till conversion finished.")
	  (doc-view-doc->txt txt (lambda () (org-recoll-doc-view-search nil)))))))

(defun org-recoll-post-open-actions (squery)
  "Perform rendering or search actions on opened file.
Prompt to start a file-search for SQUERY in the opened file or to
call pdf-occur for a pdf.  isearch can be a bit slow with pdfs
due to rendering speed.  PDF-OCCUR provides a speedy alternative.
Falls back gracefully to a modified doc-view-search if in
doc-view (where isearch doesn't work."
  (interactive)
  ;;For some reason at the stage in the org hook where this is called,
  ;;the opened file document is the "selected window" but is not the
  ;;"current buffer."  This lead to weird results attempting to start
  ;;searches, and this line fixes it.
  (switch-to-buffer (window-buffer (selected-window)))
  ;;Retrieve the filename from the buffer title.
  (setq org-recoll-filename (prin1-to-string (window-buffer (selected-window))))
  (setq org-recoll-filename (replace-regexp-in-string ">" "" org-recoll-filename))
  (setq org-recoll-filename (replace-regexp-in-string "#<buffer " "" org-recoll-filename))
  ;;If it's html, render it with shr
  (if (and org-recoll-render-html (featurep 'shr) (org-recoll-compare-string-to-list (file-name-extension org-recoll-filename) org-recoll-html-file-types))
      (org-recoll-shr-render-current-buffer))
  ;; Set pdfs and docs to page view by default
  (if (string= major-mode "doc-view-mode")
      (doc-view-fit-page-to-window)
    (if (string= (file-name-extension org-recoll-filename) "pdf") (pdf-view-fit-page-to-window)))
  ;;Search logic
  (if org-recoll-file-search-automatically
      (progn
	(if org-recoll-file-search-prompt (setq squery (read-string (concat "Enter file-search query: (default: " squery ")") nil nil squery)))
	;;If its a pdf, call pdf-occur (if available); otherwise start
	;;an isearch
	(if (string= (file-name-extension org-recoll-filename) "pdf")
	    (if (featurep 'pdf-tools) (pdf-occur squery) (message "Install pdf-tools and org-pdfview for pdf searching"))
	  (if(string= major-mode "doc-view-mode")
	      (org-recoll-doc-view-search squery)
	    (progn
	      (isearch-forward nil 1)
	      (isearch-yank-string squery))))))
    (if org-recoll-result-file-read-only (setq buffer-read-only t)))


(defun org-recoll-split-and-focus ()
  "Split window and focus the recoll results window after an original search."
  (when (= (length (window-list)) 1)
    (split-window-right))
  (other-window 1)
  (switch-to-buffer "*org-recoll-results*"))


(defun org-recoll-reset-result-count ()
  "Reset results count."
  (setq org-recoll-end-of-current-page org-recoll-results-num)
  (setq org-recoll-start-of-current-page 0))


(defun org-recoll-regexp-replace-in-buffer (from to)
  "Non-interactively replace all occurrences of FROM with TO."
  (goto-char (point-min))
  (while (re-search-forward from nil t)
    (replace-match to))
  )


(defun org-recoll-format-results ()
  "Format recoll results in buffer."
  ;; Format results in org format and tidy up
  (org-recoll-regexp-replace-in-buffer "\\[\\(.*\\)" "[[\\1")
  (org-recoll-regexp-replace-in-buffer "\\]\\(.*\\)" "-link")
  (org-recoll-regexp-replace-in-buffer "text\\/" "* ")
  (org-recoll-regexp-replace-in-buffer "inode\\/" "* ")
  (org-recoll-regexp-replace-in-buffer "message\\/rfc822" "* e-mail")
  (org-recoll-regexp-replace-in-buffer "image\\/" "* ")
  (org-recoll-regexp-replace-in-buffer "application\\/" "* ")
  (org-recoll-regexp-replace-in-buffer "\\/ABSTRACT" "")
  (org-recoll-regexp-replace-in-buffer "ABSTRACT" "")
  (org-recoll-regexp-replace-in-buffer "\\/\\([^\\/]*\\)-link" "/\\1][\\1]]")
  ;; Justify results
  (goto-char (point-min))
  (org-recoll-fill-region-paragraphs)
  ;; Add emphasis
  (highlight-phrase (org-recoll-reformat-for-file-search org-recoll-search-query) 'bold-italic))


(defun org-recoll-recollq-to-org (squery &optional paging)
  "Conduct recoll full-text search and format the results in org links.
SQUERY is the query passed in from the wrapper.
If PAGING is t this indicates that the function is being called to page through results."
  ;;If paging through results or starting a new search from the
  ;;buffer, stay in the buffer
  (unless (or paging (string= "#<buffer *org-recoll-results*>" (prin1-to-string (window-buffer (selected-window))))) (org-recoll-split-and-focus))
  ;;If you're not paging through the results, reset the paging
  ;;counters
  (unless paging (org-recoll-reset-result-count))
  ;;Unset read-only-mode to make the buffer temporarily writeable
  (read-only-mode -1)
  ;;Clear the buffer and do some setup
  (erase-buffer)
  (org-mode)
  (make-local-variable 'org-return-follows-link)
  (setq org-return-follows-link t)
  (org-recoll-mode t)
  ;;Should probably remove this or make it conditional as it's just
  ;;for my benefit
  (if (featurep 'flyspell) (flyspell-mode -1))
  ;;Print the query header and result count
  (insert (shell-command-to-string (concat org-recoll-command-invocation " -Q '" (org-recoll-sanitize-single-quote squery) "'")))
     (insert (concat "\n" "Results: " (number-to-string org-recoll-start-of-current-page) " - " (number-to-string org-recoll-end-of-current-page) "\n\n"))
     ;;Print results
     (insert (shell-command-to-string (concat org-recoll-command-invocation " -n '" (number-to-string org-recoll-start-of-current-page) "-" (number-to-string org-recoll-results-num) "' -q " "'" (org-recoll-sanitize-single-quote squery) "'" " | tail -n +3")))
  ;;Format
  (org-recoll-format-results)
  ;;Prevent editing
  (read-only-mode)
  ;;Add post-processing/file-search hook
  (add-hook 'org-follow-link-hook (lambda () (org-recoll-post-open-actions (org-recoll-reformat-for-file-search squery))) nil t))

;;
;; User-Facing Functions
;;


;;;###autoload
(defun org-recoll-update-index ()
  "Invoke the recoll index update command string specified in ORG-RECOLL-INDEX-INVOCATION."
  (interactive)
  (shell-command (concat org-recoll-index-invocation " &") "*org-recoll-index*" "*org-recoll-index*"))

(defun org-recoll-next-page ()
  "Delivers the next page of results."
  (interactive)
  (setq org-recoll-start-of-current-page org-recoll-end-of-current-page)
  (setq org-recoll-end-of-current-page (+ org-recoll-end-of-current-page org-recoll-results-num))
  (org-recoll-recollq-to-org org-recoll-search-query t))

(defun org-recoll-previous-page ()
  "Delivers the previous page of results."
  (interactive)
  (if (eq org-recoll-start-of-current-page 0) (error "You are already at the beginning of the results list"))
  (setq org-recoll-end-of-current-page org-recoll-start-of-current-page)
  (setq org-recoll-start-of-current-page (- org-recoll-start-of-current-page org-recoll-results-num))
  (org-recoll-recollq-to-org org-recoll-search-query t))

(defun org-recoll-completing-read (&optional squery)
  "Read from minibuffer with completion, but allowing spaces.
If SQUERY is passed offer it as a default."
  (define-key minibuffer-local-completion-map
    " " nil)
  (setq squery (completing-read "Enter your query: " org-recoll-search-history nil nil nil 'org-recoll-search-history squery))
  (define-key minibuffer-local-completion-map
    " " 'minibuffer-complete-word)
  squery)

;;;###autoload
(defun org-recoll-search (&optional query)
  "Prompt for a QUERY and search."
  (interactive)
  (if query
      (setq org-recoll-search-query query)
    (setq org-recoll-search-query (org-recoll-completing-read)))
  (org-recoll-recollq-to-org org-recoll-search-query))

(provide 'org-recoll)

;;; org-recoll.el ends here
