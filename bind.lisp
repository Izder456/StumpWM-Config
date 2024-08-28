;;;
;; Make New Keymaps
;;;
(defmacro make-keymap (map-name key-binding &optional root top)
  `(progn
     (defvar ,map-name
       (let ((map (make-sparse-keymap)))
	 map))
     (when ,root
       (define-key *root-map* (kbd ,key-binding) ,map-name))
     (when ,top
       (define-key *top-map* (kbd ,key-binding) ,map-name))))

(make-keymap *search-map* "M-s" t t)
(make-keymap *media-map* "M-m" t t)
(make-keymap *app-map* "M-a" t t)

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
  `(bt:make-thread
    (lambda ()
      (dolist (key-cmd ,key-cmd-list)
        (,bind-macro (first key-cmd) (second key-cmd) ,map)))))

;; Push/Pop Current Window Into a Floating group
(defcommand toggle-float () ()
            (if (float-window-p (current-window))
                (unfloat-this)
              (float-this)))

;;;
;; Bind Key Lists
;;;

;; Set Special keys
(defvar *my-special-key-commands*
  '(("Print" "scrot -z -Z 0 -q 100 -e 'oxipng $f' -F ~/Pictures/screenshot-`date +%F`.png")
    ("M-Print" "scrot -s -z -Z 0 -q 100 -f -e 'oxipng $f' -F ~/Pictures/screenshot-split-`date +%F`.png")
    ("s-Print" "scrot -z -Z 0 -q 100 -u -b -e 'oxipng $f' -F ~/Pictures/screenshot-activewin-`date +%F`.png")))

;; Set Shell Keys
(defvar *my-shell-key-commands*
  '(("c" "urxvtc")
    ("C-c" "urxvtc")
    ("l" "slock")
    ("M-b" "feh --bg-fill --randomize /usr/local/share/backgrounds")))

;; Set App Keys
(defvar *my-app-key-commands*
  '(("E" "claws-mail")
    ("d" "deadbeef")
    ("F" "pcmanfm")
    ("f" "firefox-esr")
    ("e" "emacsclient -a '' -c")
    ("C-e" "emacsclient -e '(kill-emacs)' && emacs --daemon")))

;; Set Rofi Keys
(defvar *my-rofi-key-commands*
  '(("SPC" "rofi -i -show-icons -matching fuzzy -show drun")))

;; Set Playerctl Keys
(defvar *my-media-key-commands*
  '(("p" "playerctl play-pause")
    ("s" "playerctl stop")
    ("b" "playerctl previous")
    ("n" "playerctl next")
    ("z" "playerctl shuffle toggle")
    ("XF86AudioRaiseVolume" "playerctl volume 0.05+")
    ("XF86AudioLowerVolume" "playerctl volume 0.05-")))

;; Raw StumpWM Window-managing Commands
(defvar *my-wm-window-commands*
  '(("M-ESC" "mode-line")
    ("M-q" "quit")
    ("m" "mark")
    ("M" "gmove-marked")
    ("x" "xkill")
    ("B" "beckon")
    ("C-b" "banish")
    ("RET" "expose")
    ("C-Up" "exchange-direction up")
    ("C-Down" "exchange-direction down")
    ("C-Left" "exchange-direction left")
    ("C-Right" "exchange-direction right")
    ("p" "toggle-float")
    ("M-p" "flatten-floats")
    ("/" "toggle-gaps")))

;; Raw StumpWM Module Commands
(defvar *my-wm-module-commands*
  '(("/" "swm-ssh-menu")))

;; Unprefixed Module Commands
(defvar *my-unprefixed-module-commands*
  '(("M-Tab" "select-previous-window")
    ("s-Tab" "windowlist-last")
    ("XF86AudioRaiseVolume" "volume-up")
    ("XF86AudioLowerVolume" "volume-down")
    ("XF86AudioMute" "toggle-mute")))

;;;
;; Loop & Bind with Macros from earlier
;;;
;; List of binds
(defparameter *key-bindings*
	      '((*my-shell-key-thread* *my-shell-key-commands* bind-shell-to-key *app-map*)
		(*my-app-key-thread* *my-app-key-commands* bind-shell-to-key *app-map*)
		(*my-rofi-key-thread* *my-rofi-key-commands* bind-shell-to-key *app-map*)
		(*my-wm-module-thread* *my-wm-module-commands* bind-to-key *app-map*)
		(*my-unprefixed-module-thread* *my-unprefixed-module-commands* bind-to-key *top-map*)
		(*my-special-key-thread* *my-special-key-commands* bind-shell-to-key *top-map*)
		(*my-media-key-thread* *my-media-key-commands* bind-shell-to-key *media-map*)
		(*my-wm-window-thread* *my-wm-window-commands* bind-to-key *root-map*)))

;; Loop over list
(dolist (binding *key-bindings*)
  (destructuring-bind (name commands binding-fn map) binding
		      (eval `(defvar ,name
			       (loop-and-bind ,commands ,binding-fn ,map)))))
