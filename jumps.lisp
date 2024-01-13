;;;
;; Jump Macros
;;;

;; Term Jump commands
(defmacro make-term-jump (name command term)
 `(defcommand ,(intern name) (search)
    ((:rest ,(concatenate 'string name " termsearch: ")))
    (nsubstitute #\+ #\Space search)
    (run-shell-command (format nil "~a -e sh -c '~a ~a | most'" ,term ,command search))))

;;
; Browser
;;

;; Module Settings
; Set homepage
(setf browse::*homepage* "http://68k.news")

; Set browser exe
(setf searchengines:*search-browser-executable* "firefox-esr")

;; Macro for search engine defines
(defmacro define-searchengine (selection-name prompt-name url description key-selection key-prompt)
 `(progn
    (searchengines:make-searchengine-selection ,selection-name ,url ,description :map *top-map* :key ,key-selection)
    (searchengines:make-searchengine-prompt ,prompt-name ,description ,url ,description :map *top-map* :key ,key-prompt)))

;; Set Search Engine Params
(defparameter *URL-DDG* "https://duckduckgo.com/?q=~a")
(defparameter *URL-LIBRE* "https://search.ahwx.org/search.php?q=~a")
(defparameter *URL-PORTS* "https://openports.eu/search?q=~a")

(define-searchengine "search-ddg-selection" "search-ddg-prompt" *URL-DDG* "DuckDuckGo search" "M-C-s" "M-s")
(define-searchengine "search-libre-selection" "search-libre-prompt" *URL-LIBRE* "LibreY search" "M-C-l" "M-l")
(define-searchengine "search-ports-selection" "search-ports-prompt" *URL-PORTS* "Ports Search" "M-C-o" "M-o")

;;;
;; Define Jumps
;;;

;; Define Terminal Jumps
(make-term-jump "mansearch" "apropos" "st")
(make-term-jump "manpage" "man" "st")
(make-term-jump "pkgname" "pkg_info -Q" "st")
(make-term-jump "pkgloc" "pkg_locate" "st")

;;;
;; Bind Jump Defines from Earlier
;;;

;; Keybindings for Terminal Jumps
(define-key *top-map* (kbd "M-m") "mansearch")
(define-key *top-map* (kbd "M-M") "manpage")
(define-key *top-map* (kbd "M-p") "pkgname")
(define-key *top-map* (kbd "M-P") "pkgloc")
