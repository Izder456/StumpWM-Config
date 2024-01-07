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

;; We need this module
(load-module "browse")
(setf browse::*homepage* "http://68k.news")

;; Web Searches
(load-module "searchengines")
(setf searchengines:*search-browser-executable* "firefox-esr")

;; Set Search Engine Params
(defparameter *URL-DDG* "https://duckduckgo.com/s?q=~a")
(defparameter *URL-LIBRE* "https://search.ahwx.org/search?q=~a")
(defparameter *URL-PORTS* "https://openports.eu/search?q=~a")

;; Set Search Selects
(searchengines:make-searchengine-selection "search-ddg-selection" *URL-DDG* "DuckDuckGo search" :map *top-map* :key "M-C-s")
(searchengines:make-searchengine-prompt "search-ddg-prompt" "DuckDuckGo" *URL-DDG* "DuckDuckGo search" :map *top-map* :key "M-s")
(searchengines:make-searchengine-selection "search-libre-selection" *URL-LIBRE* "LibreY search" :map *top-map* :key "M-C-l")
(searchengines:make-searchengine-prompt "search-libre-prompt" "LibreY" *URL-LIBRE* "LibreY search" :map *top-map* :key "M-l")
(searchengines:make-searchengine-selection "search-ports-prompt" "Ports" *URL-PORTS* "Ports Search" :map *top-map* :key "M-C-o")
(searchengines:make-searchengine-prompt "search-ports-prompt" *URL-PORTS* "Ports Search" :map *top-map* :key "M-o")

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
