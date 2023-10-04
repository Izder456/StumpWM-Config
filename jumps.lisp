;; StumpWM binds

(in-package :stumpwm)

;;;;;;;;;;;;;;;;;;;;;;;
;; DEFINE JUMP MACRO ;;
;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEFINE JUMP COMMANDS ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Define Web Jumps
(make-web-jump "ddg" "ungoogled-chromium https://html.duckduckgo.com/html?q=")
(make-web-jump "lite" "ungoogled-chromium https://lite.duckduckgo.com/lite?q=")

;; Define Terminal Jumps
(make-term-jump "mansearch" "alacritty --hold -e apropos ")
(make-term-jump "manpage" "alacritty --hold -e man ")
(make-term-jump "pkgname" "alacritty --hold -e pkg_info -Q ")
(make-term-jump "pkgloc" "alacritty --hold -e pkg_locate ")

;;;;;;;;;;;;;;;;;;;;;;;;
;; BIND JUMP COMMANDS ;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; Keybindings for Web Jumps
(define-key *top-map* (kbd "M-s") "ddg")
(define-key *top-map* (kbd "M-d") "lite")

;; Keybindings for Terminal Jumps
(define-key *top-map* (kbd "M-m") "mansearch")
(define-key *top-map* (kbd "M-M") "manpage")
(define-key *top-map* (kbd "M-p") "pkgname")
(define-key *top-map* (kbd "M-P") "pkgloc")
