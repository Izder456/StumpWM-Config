;;;
;; Jump Macros
;;;

;; Term Jump commands
(defmacro make-term-jump (name command term)
  `(defcommand ,(intern name) (search)
	       ((:rest ,(concatenate 'string name " termsearch: ")))
	       (nsubstitute #\+ #\Space search)
	       (run-shell-command
		(format nil "~a -e sh -c '~a ~a | less -R'" ,term ,command search))))

;;;
;; Module Settings
;;;

;; Set browser exe
(setf searchengines:*search-browser-executable* "firefox-esr")

;; Macro for search engine defines
(defmacro define-searchengine (selection-name prompt-name url description key-selection key-prompt)
  `(progn
     (searchengines:make-searchengine-selection ,selection-name ,url ,description :map *search-map* :key ,key-selection)
     (searchengines:make-searchengine-prompt ,prompt-name ,description ,url ,description :map *search-map* :key ,key-prompt)))

;; Set Search Engine Params
(defparameter *URL-DDG* "https://duckduckgo.com/?q=~a")
(defparameter *URL-LIBRE* "https://search.ahwx.org/search.php?q=~a")
(defparameter *URL-PORTS* "https://openports.eu/search?q=~a")
(defparameter *URL-WIKIPEDIA* "https://en.wikipedia.org/w/index.php?title=Special:Search&search=~a")
(defparameter *URL-WIKTIONARY* "https://en.wiktionary.org/w/index.php?title=Special:Search&search=~a")
(defparameter *URL-CLEW* "https://clew.se/search?q=~a")

;; Create threads
(define-searchengine "search-wikipedia-selection" "search-wikipedia-prompt" *URL-WIKIPEDIA* "Wikipedia search" "C-w" "w")
(define-searchengine "search-wiktionary-selection" "search-wiktionary-prompt" *URL-WIKTIONARY* "Wiktionary search" "C-d" "d")
(define-searchengine "search-ddg-selection" "search-ddg-prompt" *URL-DDG* "DuckDuckGo search" "C-s" "s")
(define-searchengine "search-libre-selection" "search-libre-prompt" *URL-LIBRE* "LibreY search" "C-l" "l")
(define-searchengine "search-ports-selection" "search-ports-prompt" *URL-PORTS* "Ports Search" "C-o" "o")
(define-searchengine "search-clew-selection" "search-clew-prompt" *URL-CLEW* "Clew Blog Search" "C-c" "c")

;;;
;; Define Jumps
;;;

;; Define Terminal Jumps
(make-term-jump "mansearch" "apropos" "urxvtc")
(make-term-jump "manpage" "man" "urxvtc")
(make-term-jump "pkgname" "pkg_info -Q" "urxvtc")
(make-term-jump "pkgloc" "pkg_locate" "urxvtc")

;;;
;; Bind Jump Defines from Earlier
;;;
;; Keybindings for Terminal Jumps
(define-key *search-map* (kbd "m") "mansearch")
(define-key *search-map* (kbd "M") "manpage")
(define-key *search-map* (kbd "p") "pkgname")
(define-key *search-map* (kbd "P") "pkgloc")
