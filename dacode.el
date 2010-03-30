;;; dacode.el --- Lecture de news dacode
;;
;; Author: Alexandre Brillant <abrillant@wanadoo.fr>
;; Maintainer: Alexandre Brillant <abrillant@wanadoo.fr>
;; Official site : http://www.djefer.com
;; Created: 18/06/02
;;
;; This program is free software; you can redistribute it and / or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; This is the version 1.3 of 26 june 2002
;;
;; Installation : copier dacode.el dans un path emacs puis ajouter (require `dacode)
;; dans votre .emacs
;; Si les articles n'apparaissent pas suite a la commande dacode-linuxFr-readnews augmenter
;; la valeur de dacode-time-page. Vous pouvez modifier les couleurs grace aux variables 
;; dacode-text-color et dacode-title-color
;;
;; ATTENTION : Si vous passez par un proxy, affecter une valeur aux variables dacode-proxy-host,
;; et dacode-proxy-host.
;;
;; Command : Vous pouvez aussi y accéder par le menu 'Tools'
;; `dacode-linuxFr-readnews' Pour lires les news linuxFr.org
;; `dacode-emacsFr-readnews' Pour lires les news emacsFr.org
;;
;; Commentaires : Merci à Benoit Moussaud pour ces tests.
;;
;; Change:
;; 1.3 : 
;; Correction pour l'accès à linuxFr.org
;; Trace disponible pour HTTP
;; 1.2 :
;; Support des proxys
;; Quelques améliorations : coloration, défilement, césure du texte
;; 1.1 :
;; Support de EmacsFr.org
;; Ajout d'un menu daCode dans Tools
;; Mode majeur daCode avec selection d'article avec les flèches
;;
;;; Code:

;; Proxy
(defvar dacode-proxy-host nil
  "Proxy host for HTTP")
(defvar dacode-proxy-port nil
  "Proxy port for HTTP")

;; LinuxFr site
(defvar dacode-linuxFr-host "linuxFr.org"
  "Location for LinuxFr")
(defvar dacode-linuxFr-port 80
  "HTTP port")
(defvar dacode-linuxFr-index "/index,0,1,0,0.php3"
  "Index for LinuxFr")

;; EmacsFr site
(defvar dacode-emacsFr-host "emacsFr.org"
  "Location for EmacsFr")
(defvar dacode-emacsFr-port 80
  "HTTP port")
(defvar dacode-emacsFr-index "/"
  "Index of emacsFr")

;; Size of each article
(defvar dacode-article-size 70
  "Width of each article in characters")

;; Time for loading the page, may be to increase
(defvar dacode-time-page 10
  "Time for loading the page")

;; Trace HTTP request
(defvar dacode-trace-request nil
  "Trace HTTP request")

;; Trace HTTP response
(defvar dacode-trace-response nil
  "Trace HTTP response")

;; Title color
(defface dacode-title-face
  '((t (:bold t :foreground "White")))
  "Face for the title")

(defface dacode-text-face
  '((t (:bold f :foreground "CadetBlue")))
  "Face for the text")

(defface dacode-code-face
  '((t (:bold t :foreground "light blue")))
  "Face for the code")

(if (require 'easymenu nil t)
    (progn
      (easy-menu-add-item nil '("tools") "--")
      (easy-menu-add-item nil '("tools")
	  '("daCode"
	   [ "LinuxFr" dacode-linuxFr-readnews t ]
	   [ "EmacsFr" dacode-emacsFr-readnews t ]
	   ))))

(defun dacode-mode()
  "Dacode Major mode for french site under daCode."
  (interactive)
  (dacode-mode-init)
  (kill-all-local-variables)
  (use-local-map dacode-mode-map)
  (setq mode-name "Dacode")
  (setq major-mode 'dacode-mode)
  (run-hooks 'dacode-mode-hook))

(defvar dacode-mode-map nil)

(defun dacode-mode-init()
  (when (not dacode-mode-map)
    (progn
      (setq dacode-mode-map (make-sparse-keymap))
      (define-key dacode-mode-map [up] 'dacode-move-up)
      (define-key dacode-mode-map [down] 'dacode-move-down))))

(defun dacode-move-up()
  "Move the cursor on the previous article."
  (interactive)
  (search-backward "+--" nil t 1)
  (beginning-of-line)
  (recenter))

(defun dacode-move-down()
  "Move the cursor on the next article."
  (interactive)
  (goto-char (1+ (point)))
  (search-forward "+--" nil t 1)
  (beginning-of-line)
  (recenter))

;; Download a page and return its buffer
(defun dacode-download-page(http1-1 site port delay page html-flag)
  "Download a page from the site, wait delay for the full download"
  (let ( (temp-buf (get-buffer-create "*Temp/daCode/*"))
	 (id (concat site page)) )
    (set-buffer temp-buf)
    (erase-buffer)
    (goto-char 0)
    (message (concat "Connection to " site " " page " ..."))

    (when dacode-proxy-host
      (setq page (concat "http://www." site page))
      (setq site dacode-proxy-host)
      (when dacode-proxy-port
	(setq port dacode-proxy-port)))
    (setq connection
	  (open-network-stream id temp-buf site port ))
    (setq request (concat "GET " page " HTTP/1.0\n\n"))
    (when http1-1
      (setq request (concat "GET " page " HTTP/1.1\nHost:www." site "\n\n" )))
    (when dacode-trace-request
      (message (concat "Request " request)))
    (process-send-string connection request)
    (while (and (not (memq (process-status connection) '(stop closed nil)))
		(not (re-search-forward "</html>" nil t) ))
      (sit-for delay))
    ;; Get the final page
    (delete-process connection)
    (message "Done")
    temp-buf))

;; Read news for LinuxFr.org
(defun dacode-linuxFr-readnews()
  (interactive)
  "Access to LinuxFr and show all news in the current buffer"
  (dacode-process-news nil dacode-linuxFr-host dacode-linuxFr-port dacode-linuxFr-index))

;; Read news for EmacsFr.org
(defun dacode-emacsFr-readnews()
  (interactive)
  "Access to LinuxFr and show all news in the current buffer"
  (dacode-process-news t dacode-emacsFr-host dacode-emacsFr-port dacode-emacsFr-index)
  (dacode-color-code)
  (goto-char 0))

;; Color any lisp code
(defun dacode-color-code()
  (interactive)
  "Color any supposed lisp code"
  (goto-char 0)
  (while (re-search-forward "(" nil t)
      (let ( (start (match-beginning 0)))
	(if (re-search-forward ")" nil t)
	    (when (> (point) (+ 3 start))
	    (add-text-properties start (point) '(face dacode-code-face)))))))

;; Scan for news and insert it in a buffer "*daCode/...*"
(defun dacode-process-news(http1-1 host port index)
  "Get the final page and extract news"
  (let ( (page-buf (dacode-download-page http1-1 host port dacode-time-page index t))
	 (news-buf (get-buffer-create (concat "*daCode/" host "*" ))))
    (set-buffer news-buf)
    (dacode-mode)
    (erase-buffer)
    (goto-char 0)
    (dacode-show-news page-buf news-buf)
    (when (not dacode-trace-response)
      (kill-buffer page-buf))
    (switch-to-buffer news-buf)
   (goto-char 0)))

;; Show all news
(defun dacode-show-news(page-buf news-buf)
  "Read news from the page-buf and write the result in the news-buf"
  (insert "News " (format-time-string "%d/%m" (current-time)) "\n\n")
  (set-buffer page-buf)
  (goto-char 0)
  (while (re-search-forward "<!-- NEWSBOX" nil t)
    (let ( (domain (dacode-get-article-domain))
	   (title (dacode-get-article-title))
	   (author (dacode-get-article-author))
	   (text (dacode-get-article-text)))
      (set-buffer news-buf)
      (insert "\n+-----------------------------------------------------------------------------------------------\n" )
      (let ((pt (point)))
	(insert domain " | " title "(" author ")\n") 
	(add-text-properties pt (1- (point)) '(face dacode-title-face))
	)
      (insert "------------------------------------------------------------------------------------------------\n" )
      (let ((pt (point)))
	(insert text "\n")
	(add-text-properties pt (1- (point)) '(face dacode-text-face)))
      (insert "\n")
      (set-buffer page-buf)))
  (set-buffer news-buf)
  (insert "\n\n\n\n\n"))

;; Extract the domain
(defun dacode-get-article-domain()
  "Extract the Domain from the current article"
  (if (re-search-forward "\\([^>]*\\)</a>: " nil t)
      (dacode-process-entities (match-string 1))
    "?"))

;; Extract the title
(defun dacode-get-article-title()
  "Extract the Title from the current article"
  (if (re-search-forward ">\\([^>]*\\)</a>" nil t)
      (dacode-process-entities (match-string 1))
    "?"))

;; Extract the author
(defun dacode-get-article-author()
  "Extract the Author from the current article"
  (if (re-search-forward "Posté par \\([^<]*\\)" nil t)
      (match-string 1)
    "?"))

;; Extract the text
(defun dacode-get-article-text()
  "Extract the Text from the current article"
  (if (re-search-forward "newstext\">" nil t)
      (if (re-search-forward "</a>" nil t)
	  (let ( (old-cursor (point)) (text) )
	    (while (not (re-search-forward "</td>" nil t)))
	    (setq text (buffer-substring old-cursor (point)))
	    (clean-html-text (dacode-process-entities text)))
	"?")
    "?"))

;; Clean the HTML
(defun clean-html-text(str)
"Clean the HTML"
    (while (string-match "<[^>]*>" str 0)
      (setq str (replace-match "" t t str)))
    (while (string-match "\\(  \\)\\|\\(\\)" str 0)
      (setq str (replace-match "" t t str)))
     (let ( (i 0) 
	    (newstr "") 
	    (mustcut nil) 
	    (chrcount 0) )
       (while (< i (length str))
 	  (when (equal (char-to-string (aref str i)) " ")
 	    (progn
 	      (if mustcut
 		  (progn 
 		    (setq chrcount 0)
 		    (setq newstr (concat newstr "\n"))
 		    (setq mustcut nil)))))
	  (when (equal (char-to-string (aref str i)) "\n")
	    (setq mustcut nil)
	    (setq chrcount 0))
 	  (setq chrcount ( 1+ chrcount ))
	  (setq newstr (concat newstr (char-to-string (aref str i))))
 	  (when (> chrcount dacode-article-size)
 	    (setq mustcut t))
	 (setq i (1+ i)))
       newstr))

(defvar dacode-html-entities 
  '(
    ("&excl;"        .  33)
    ("&quot;"        .  34)
    ("&num;"         .  35)
    ("&dollar;"      .  36)
    ("&percent;"     .  37)
    ("&amp;"         .  38)
    ("&rsquo;"       .  39)
    ("&apos;"        .  39)
    ("&lpar;"        .  40)
    ("&rpar;"        .  41)
    ("&times;"       .  42)
    ("&ast;"         .  42)
    ("&plus;"        .  43)
    ("&comma;"       .  44)
    ("&period;"      .  46)
    ("&colon;"       .  58)
    ("&semi;"        .  59)
    ("&lt;"          .  60)
    ("&equals;"      .  61)
    ("&gt;"          .  62)
    ("&quest;"       .  63)
    ("&commat;"      .  64)
    ("&lsqb;"        .  91)
    ("&rsqb;"        .  93)
    ("&uarr;"        .  94)
    ("&lowbar;"      .  95)
    ("&lsquo;"       .  96)
    ("&lcub;"        . 123)
    ("&verbar;"      . 124)
    ("&rcub;"        . 125)
    ("&tilde;"       . 126)
    ("&nbsp;"        . 160)
    ("&iexcl;"       . 161)
    ("&cent;"        . 162)
    ("&pound;"       . 163)
    ("&curren;"      . 164)
    ("&yen;"         . 165)
    ("&brvbar;"      . 166)
    ("&sect;"        . 167)
    ("&uml;"         . 168)
    ("&copy;"        . 169)
    ("&ordf;"        . 170)
    ("&laquo;"       . 171)
    ("&not;"         . 172)
    ("&shy;"         . 173)
    ("&reg;"         . 174)
    ("&macr;"        . 175)
    ("&deg;"         . 176)
    ("&plusmn;"      . 177)
    ("&sup2;"        . 178)
    ("&sup3;"        . 179)
    ("&acute;"       . 180)
    ("&micro;"       . 181)
    ("&para;"        . 182)
    ("&middot;"      . 183)
    ("&cedil;"       . 184)
    ("&sup1;"        . 185)
    ("&ordm;"        . 186)
    ("&raquo;"       . 187)
    ("&frac14;"      . 188)
    ("&frac12;"      . 189)
    ("&frac34;"      . 190)
    ("&iquest;"      . 191)
    ("&Agrave;"      . 192)
    ("&Aacute;"      . 193)
    ("&Acirc;"       . 194)
    ("&Atilde;"      . 195)
    ("&Auml;"        . 196)
    ("&Aring;"       . 197)
    ("&AElig;"       . 198)
    ("&Ccedil;"      . 199)
    ("&Egrave;"      . 200)
    ("&Eacute;"      . 201)
    ("&Ecirc;"       . 202)
    ("&Euml;"        . 203)
    ("&Igrave;"      . 204)
    ("&Iacute;"      . 205)
    ("&Icirc;"       . 206)
    ("&Iuml;"        . 207)
    ("&ETH;"         . 208)
    ("&Ntilde;"      . 209)
    ("&Ograve;"      . 210)
    ("&Oacute;"      . 211)
    ("&Ocirc;"       . 212)
    ("&Otilde;"      . 213)
    ("&Ouml;"        . 214)
    ("&times;"       . 215)
    ("&Oslash;"      . 216)
    ("&Ugrave;"      . 217)
    ("&Uacute;"      . 218)
    ("&Ucirc;"       . 219)
    ("&Uuml;"        . 220)
    ("&Yacute;"      . 221)
    ("&THORN;"       . 222)
    ("&szlig;"       . 223)
    ("&agrave;"      . 224)
    ("&aacute;"      . 225)
    ("&acirc;"       . 226)
    ("&atilde;"      . 227)
    ("&auml;"        . 228)
    ("&aring;"       . 229)
    ("&aelig;"       . 230)
    ("&ccedil;"      . 231)
    ("&egrave;"      . 232)
    ("&eacute;"      . 233)
    ("&ecirc;"       . 234)
    ("&euml;"        . 235)
    ("&igrave;"      . 236)
    ("&iacute;"      . 237)
    ("&icirc;"       . 238)
    ("&iuml;"        . 239)
    ("&eth;"         . 240)
    ("&ntilde;"      . 241)
    ("&ograve;"      . 242)
    ("&oacute;"      . 243)
    ("&ocirc;"       . 244)
    ("&otilde;"      . 245)
    ("&ouml;"        . 246)
    ("&divide;"      . 247)
    ("&oslash;"      . 248)
    ("&ugrave;"      . 249)
    ("&uacute;"      . 250)
    ("&ucirc;"       . 251)
    ("&uuml;"        . 252)
    ("&yacute;"      . 253)
    ("&thorn;"       . 254)
    ("&yuml;"        . 255)))

;; Replace entities
(defun dacode-process-entities(str)
  "Replace all entities for str by caracters"
  (let ( (start 0) )
    (while (string-match "\\(&[^ ]*;\\)" str start)
      (let*
	  ( (entity (match-string 1 str))
	    (value (cdr (assoc entity dacode-html-entities)) ) )
	(setq start (match-beginning 0))
	(when (null value)
	  (setq value 32))
	(setq str (replace-match (char-to-string value) t t str)))))
  str)

(provide 'dacode)

;;; dacode.el ends here
