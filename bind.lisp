;;;
;; Bindings
;;;

;; Set prefix key
(set-prefix-key (kbd "C-t"))

;;;
;; Bind Key Macro
;;;

;; Bind to *root-map*
(defmacro bind-shell-to-key (key command &optional (map *root-map*))
  `(define-key ,map (kbd ,key) (concatenate 'string
                                            "run-shell-command "
                                            ,command)))

;; Bind to *root-map*
(defmacro bind-app-to-key (key command &optional (map *root-map*))
  `(define-key ,map (kbd ,key) (concatenate 'string
                                            "run-shell-command "
                                            ,command)))

;; Bind to *top-map*
(defmacro bind-shell-to-topkey (key command &optional (map *top-map*))
  `(define-key ,map (kbd ,key) (concatenate 'string
                                            "run-shell-command "
                                            ,command)))

;; Bind roft command to *root-map*
(defmacro bind-rofi-to-key (key command &optional (map *root-map*))
  `(define-key ,map (kbd ,key) (concatenate 'string
                                            "run-shell-command "
                                            '"rofi -i -show-icons -show "
                                            ,command)))

;;;
;; Bind Key Lists
;;;

;; Set Rofi Keys
(defvar *my-rofi-key-commands*
  '(("SPC" "drun")
    ("RET" "window")))

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
    ("x" "xkill")
    ("l" "slock")
    ("M-b" "feh --bg-fill $(shuf -n1 -e /usr/local/share/backgrounds/*)")))

;; Set App Keys
(defvar *my-app-key-commands*
  '(("E" "claws-mail")
    ("F" "pcmanfm")))

;;;
;; Loop & Bind with Macros from earlier
;;;

;; Loop through keybind lists
(loop for (key cmd) in *my-rofi-key-commands* do
  (bind-rofi-to-key key cmd))

(loop for (key cmd) in *my-shell-key-commands* do
  (bind-shell-to-key key cmd))

(loop for (key cmd) in *my-app-key-commands* do
  (bind-app-to-key key cmd))

(loop for (key cmd) in *my-special-key-commands* do
  (bind-shell-to-topkey key cmd))

;; Global keybindings
(define-key *top-map* (kbd "M-ESC") "mode-line")
(define-key *root-map* (kbd "M-q") "quit")

;; Window movement/swapping
(define-key *root-map* (kbd "m") "mark")
(define-key *root-map* (kbd "M") "gmove-marked")
(define-key *root-map* (kbd "C-Up") "exchange-direction up")
(define-key *root-map* (kbd "C-Down") "exchange-direction down")
(define-key *root-map* (kbd "C-Left") "exchange-direction left")
(define-key *root-map* (kbd "C-Right") "exchange-direction right")

;; Browser
(define-key *root-map* (kbd "f") "browse")

;; Push/Pop Current Window Into a Floating group
(define-key *root-map* (kbd "P") "float-this")
(define-key *root-map* (kbd "p") "unfloat-this")

;; Flatten all flating windows into tiled frames
(define-key *root-map* (kbd "M-p") "flatten-floats")

;; EMACS!!
(define-key *top-map* (kbd "s-e") "emacs-daemon-kill-force")
(define-key *root-map* (kbd "e") "swm-emacs")
(define-key *root-map* (kbd "C-e") "swm-emacs")
