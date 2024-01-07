;;;
;; Jump Macros
;;;

;; Term Jump commands
(defmacro make-term-jump (name command term)
 `(defcommand ,(intern name) (search)
    ((:rest ,(concatenate 'string name " termsearch: ")))
    (nsubstitute #\+ #\Space search)
    (run-shell-command (format nil "~a -e sh -c '~a ~a | most'" ,term ,command search))))

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
