;;;
;; Pre-Bindings
;;;

;; Set prefix key
(set-prefix-key (kbd "C-t"))

;; gross binds
(defvar *gross-default-binds*
  (list "c" "C-c" "e" "C-e" "d" "C-d" "SPC"
        "i" "f" "C-k" "w" "C-w" "a" "C-a"
        "C-t" "R" "o" "TAB" "F" "C-h" "v"
        "#" "m" "C-m" "l" "C-l" "G" "C-N"
        "A" "X" "C-SPC" "I" "r" "W" "+"
        "RET" "C-RET" "C-0" "C-1" "C-2"
        "C-3" "C-4" "C-5" "C-6" "C-7"
        "C-8" "C-9" "0" "1" "2" "3" "4"
        "5" "6" "7" "8" "9"))
;; yuck!
(dolist (bind *gross-default-binds*)
  (define-key *root-map* (kbd bind) NIL))

;;;
;; Bare essential binds
;;;

; Terminal
(define-key *root-map* (kbd "c") (run-shell-command "st"))

; Editor
(dolist (key '("e" "C-e"))
	     (define-key *root-map* (kbd key) "swm-emacs"))
