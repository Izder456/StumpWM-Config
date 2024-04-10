;;;
;; Pre-Bindings
;;;

;; Set prefix key
(set-prefix-key (kbd "C-t"))

;; run-or-raise Firefox-ESR
(defcommand firefox-esr () ()
  "Set notice to Firefox ESR Opening"
  (bt:make-thread
   (lambda () (message "Opening Firefox ESR"))))

;; surf
(defcommand surf () ()
  "Set notice to Suckless SURF Opening"
  (bt:make-thread
   (lambda () (message "Opening Suckless SURF"))))

;; Code by Male Display the keysequence in progress
(defun key-press-hook (key key-seq cmd)
  (declare (ignore key))
  (unless (eq *top-map* *resize-map*)
    (let ((*message-window-gravity* :bottom-right))
      (message-no-timeout "Key sequence: ~A"
               (print-key-seq (reverse key-seq))))
    (when (stringp cmd) ;; Give 'em time to read it.
        (sleep 0.05))))

(defmacro replace-hook (hook fn)
  `(remove-hook ,hook ,fn)
  `(add-hook ,hook ,fn))

(replace-hook *key-press-hook* 'key-press-hook)

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
