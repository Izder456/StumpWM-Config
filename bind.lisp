;;;
;; Bindings
;;;

;; Set prefix key
(set-prefix-key (kbd "C-t"))

; gross binds
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
; yuck!
(dolist (bind *gross-default-binds*)
  (define-key *root-map* (kbd bind) NIL))

;;;
;; Make New Keymaps
;;;
(defvar *search-map*
  (let ((map (make-sparse-keymap)))
    map))
(defvar *media-map*
  (let ((map (make-sparse-keymap)))
    map))
(defvar *app-map*
  (let ((map (make-sparse-keymap)))
    map))

(define-key *root-map* (kbd "M-s") *search-map*)
(define-key *top-map* (kbd "M-s") *search-map*)

(define-key *root-map* (kbd "M-m") *media-map*)
(define-key *top-map* (kbd "M-m") *media-map*)

(define-key *root-map* (kbd "M-a") *app-map*)
(define-key *top-map* (kbd "M-a") *app-map*)

;;;
;; Bind Macro
;;;

;; Bind shell command to a specified map (default is *root-map*)
(defmacro bind-shell-to-key (key command &optional (map *root-map*))
  `(define-key ,map (kbd ,key) (concatenate 'string "run-shell-command " ,command)))

;; Bind stumpwm command to a specified map (default is *root-map*)
(defmacro bind-to-key (key command &optional (map *root-map*))
  `(define-key ,map (kbd ,key) ,command))

;;;
;; Loop & Bind Macro
;;;

;; Loop through keybind lists and bind them
(defmacro loop-and-bind (key-cmd-list bind-macro &optional (map *root-map*))
  `(sb-thread:make-thread
    (lambda ()
      (dolist (key-cmd ,key-cmd-list) (,bind-macro (first key-cmd) (second key-cmd) ,map)))))

;; Push/Pop Current Window Into a Floating group
(defcommand toggle-float () ()
  (sb-thread:make-thread
   (lambda ()
     (if (float-window-p (current-window))
         (unfloat-this)
         (float-this)))))

;;;
;; Bind Key Lists
;;;

;; Set Special keys
(defvar *my-special-key-commands*
  '(("Print" "scrot -F ~/Pictures/screenshot-`date +%F`.png")
    ("M-Print" "scrot -s -F ~/Pictures/screenshot-split-`date +%F`.png")
    ("s-Print" "scrot -u -F ~/Pictures/screenshot-activewin-`date +%F`.png")
    ("XF86AudioRaiseVolume" "volume-up")
    ("XF86AudioLowerVolume" "volume-down")
    ("XF86AudioMute" "toggle-mute")))

;; Set Shell Keys
(defvar *my-shell-key-commands*
  '(("c" "st")
    ("C-c" "st")
    ("l" "slock")
    ("M-b" "feh --bg-fill $(shuf -n1 -e /usr/local/share/backgrounds/*)")))

;; Set App Keys
(defvar *my-app-key-commands*
  '(("E" "claws-mail")
    ("F" "pcmanfm")))

;; Set Rofi Keys
(defvar *my-rofi-key-commands*
  '(("SPC" "rofi -i -show-icons -show drun")
    ("RET" "rofi -i -show-icons -show window")))

;; Raw StumpWM Window-managing Commands
(defvar *my-wm-window-commands*
  '(("M-ESC" "mode-line")
    ("M-q" "quit")
    ("m" "mark")
    ("x" "xkill")
    ("M" "gmove-marked")
    ("C-Up" "exchange-direction up")
    ("C-Down" "exchange-direction down")
    ("C-Left" "exchange-direction left")
    ("C-Right" "exchange-direction right")
    ("p" "toggle-float")
    ("M-p" "flatten-floats")))

;; Raw StumpWM Module Commands
(defvar *my-wm-module-commands*
  '(("f" "browse")
    ("s-e" "emacs-daemon-kill-force")
    ("e" "swm-emacs")
    ("C-e" "swm-emacs")))

;;;
;; Loop & Bind with Macros from earlier
;;;
;; Bind shell keys to *app-map*
(defvar *my-shell-key-thread*
  (loop-and-bind *my-shell-key-commands* bind-shell-to-key *app-map*))
;; Bind app keys to *app-map*
(defvar *my-app-key-thread*
  (loop-and-bind *my-app-key-commands* bind-shell-to-key *app-map*))
;; Bind rofi keys to *app-map*
(defvar *my-rofi-key-thread*
  (loop-and-bind *my-rofi-key-commands* bind-shell-to-key *app-map*))
;; Bind module command keys to *app-map*
(defvar *my-wm-module-thread*
  (loop-and-bind *my-wm-module-commands* bind-to-key *app-map*))
;; Bind special keys to *top-map*
(defvar *my-special-key-thread*
  (loop-and-bind *my-special-key-commands* bind-shell-to-key *top-map*))
;; Bind window management command keys to *root-map*
(defvar *my-wm-window-thread*
  (loop-and-bind *my-wm-window-commands* bind-to-key *root-map*))

(define-key *root-map* (kbd "M-s") '*search-map*)
(define-key *root-map* (kbd "M-a") '*app-map*)
