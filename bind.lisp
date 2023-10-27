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
  '(("Print" "scrot -F ~/Pictures/screenshot-`date +%F`.png && notify-send -i camera -u low -a Scrot scrot")
    ("M-Print" "scrot -s -F ~/Pictures/screenshot-split-`date +%F`.png && notify-send -i camera -u normal -a Split scrot")
    ("s-Print" "scrot -u -F ~/Pictures/screenshot-activewin-`date +%F`.png && notify-send -i camera -u critical -a Window scrot")
    ("XF86AudioRaiseVolume" "volume-up")
    ("XF86AudioLowerVolume" "volume-down")
    ("XF86AudioMute" "toggle-mute")))

;; Set Shell Keys
(defvar *my-shell-key-commands*
  '(("c" "st")
    ("C-c" "st")
    ("M-m" "st -e mocp")
    ("i" "st -e htop")
    ("x" "xkill")
    ("l" "slock")
    ("M-b" "feh --bg-fill $(shuf -n1 -e /usr/local/share/backgrounds/*)")))

;; Set App Keys
(defvar *my-app-key-commands*
  '(("f" "ungoogled-chromium")
    ("E" "claws-mail")
    ("F" "rox")))

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

;;;
;; Misc Bindings
;;;

;; Kill/Enable AutoSleep
(defcommand kill-sleep() ()
  (message "Killing Autosleep")
  (run-commands
   "run-shell-command xset s off"
   "run-shell-command xset s noblank"
   "run-shell-command xset -dpms"
   "run-shell-command pkill xidle"))
(defcommand enable-sleep() ()
  (message "Enable Autosleep")
  (run-commands
   "run-shell-command xset s on"
   "run-shell-command xset s blank"
   "run-shell-command xidle -delay 5 -nw -program /usr/local/bin/slock -timeout 1800 &"))
(define-key *root-map* (kbd "Menu") "kill-sleep")
(define-key *root-map* (kbd "C-Menu") "enable-sleep")

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

;; EMACS!!
(define-key *top-map* (kbd "s-e") "emacs-daemon-kill-force")
(define-key *top-map* (kbd "s-E") "run-shell-command emacs --daemon")
(define-key *root-map* (kbd "e") "emacs")
(define-key *root-map* (kbd "C-e") "emacs")
