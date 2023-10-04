;; StumpWM binds

(in-package :stumpwm)

;; Web Jump commands
(defmacro make-web-jump (name prefix)
  `(defcommand ,(intern name) (search)
     ((:rest ,(concatenate 'string name " search: ")))
     (nsubstitute #\+ #\Space search)
     (run-shell-command (concatenate 'string ,prefix search))))

;; Term Jump commands
(defmacro make-term-jump (name prefix)
  `(defcommand ,(intern name) (search)
     ((:rest ,(concatenate 'string name " termsearch: ")))
     (nsubstitute #\+ #\Space search)
     (run-shell-command (concatenate 'string ,prefix search))))

;; Define Web Jumps
(make-web-jump "eco" "ungoogled-chromium https://ecosia.org/search?q=")
(make-web-jump "ddg" "ungoogled-chromium https://html.duckduckgo.com/html?q=")

;; Define Terminal Jumps
(make-term-jump "mansearch" "alacritty --hold -e apropos ")
(make-term-jump "manpage" "alacritty --hold -e man ")
(make-term-jump "pkgname" "alacritty --hold -e pkg_info -Q ")
(make-term-jump "pkgloc" "alacritty --hold -e pkg_locate ")

;; Keybindings for Web Jumps
(define-key *top-map* (kbd "M-s") "eco")
(define-key *top-map* (kbd "M-d") "ddg")

;; Keybindings for Terminal Jumps
(define-key *top-map* (kbd "M-m") "mansearch")
(define-key *top-map* (kbd "M-M") "manpage")
(define-key *top-map* (kbd "M-p") "pkgname")
(define-key *top-map* (kbd "M-P") "pkgloc")
