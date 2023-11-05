;;;
;; Jump Macros
;;;

;; Term Jump commands
(defmacro make-term-jump (name prefix)
  `(defcommand ,(intern name) (search)
     ((:rest ,(concatenate 'string name " termsearch: ")))
     (nsubstitute #\+ #\Space search)
     (run-shell-command (concatenate 'string ,prefix search))))

;;;
;; Define Jumps
;;;

;; Define Terminal Jumps
(make-term-jump "mansearch" "xterm -hold -e apropos ")
(make-term-jump "manpage" "xterm -hold -e man ")
(make-term-jump "pkgname" "xterm -hold -e pkg_info -Q ")
(make-term-jump "pkgloc" "xterm -hold -e pkg_locate ")

;;;
;; Bind Jump Defines from Earlier
;;;

;; Keybindings for Web Jumps
(define-key *top-map* (kbd "M-s") "ddg")
(define-key *top-map* (kbd "M-d") "lite")

;; Keybindings for Terminal Jumps
(define-key *top-map* (kbd "M-m") "mansearch")
(define-key *top-map* (kbd "M-M") "manpage")
(define-key *top-map* (kbd "M-p") "pkgname")
(define-key *top-map* (kbd "M-P") "pkgloc")
