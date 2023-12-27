;;;
;; Bindings
;;;

;; Set prefix key
(set-prefix-key (kbd "C-t"))

;;;
;; Bind Macro
;;;

;; Bind shell command to a specified map (default is *root-map*)
(defmacro bind-shell-to-key (key command &optional (map *root-map*))
 `(let ((thread (sb-thread:make-thread
                (lambda ()
                  (define-key ,map (kbd ,key) (concatenate 'string "run-shell-command " ,command))))))
   (sb-thread:join-thread thread)))

;; Bind stumpwm command to a specified map (default is *root-map*)
(defmacro bind-to-key (key command &optional (map *root-map*))
 `(let ((thread (sb-thread:make-thread
                (lambda ()
                  (define-key ,map (kbd ,key) ,command)))))
   (sb-thread:join-thread thread)))

;;;
;; Loop & Bind Macro
;;;

;; Loop through keybind lists and bind them
(defmacro loop-and-bind (key-cmd-list bind-macro &optional (map *root-map*))
 `(dolist (key-cmd ,key-cmd-list)
   (let ((thread (sb-thread:make-thread
                  (lambda ()
                    (,bind-macro (first key-cmd) (second key-cmd) ,map)))))
     (sb-thread:join-thread thread))))

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
    ("x" "xkill")
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
;; Bind special keys to *top-map*
(loop-and-bind *my-special-key-commands* bind-shell-to-key *top-map*)

;; Bind shell keys to *root-map*
(loop-and-bind *my-shell-key-commands* bind-shell-to-key)

;; Bind app keys to *root-map*
(loop-and-bind *my-app-key-commands* bind-shell-to-key)

;; Bind rofi keys to *root-map*
(loop-and-bind *my-rofi-key-commands* bind-shell-to-key)

;; Bind window management command keys to *root-map*
(loop-and-bind *my-wm-window-commands* bind-to-key)

;; Bind module command keys to *root-map*
(loop-and-bind *my-wm-module-commands* bind-to-key)
