;; StumpWM configuration file

(in-package :stumpwm)

;; Set font and colors for the message window
(set-fg-color "#ebdbb2")
(set-bg-color "#282828")
(set-border-color "#ebdbb2")
(set-msg-border-width 4)
(set-font "-misc-spleen-medium-r-normal--16-160-72-72-c-80-iso10646-1")


;; Utility functions for displaying system information

;; Run a shell command and format the output
(defun run-shell-command-and-format (command)
  (substitute #\Space #\Newline (run-shell-command command t)))

;; Show the kernel version
(defun show-kernel ()
  (run-shell-command-and-format "uname -r"))

;; Show the hostname
(defun show-hostname ()
  (run-shell-command-and-format "hostname"))

;; Show battery information
(defun show-battery ()
  (run-shell-command-and-format "battstat -c '++' -d '--' {i} {p}"))

;; Show the temperature
(defun show-temp ()
  (run-shell-command-and-format "sysctl -n hw.sensors.cpu0.temp0"))

;; Show the window title
(defun show-window-title ()
  (substitute #\Space #\Newline (window-title (current-window))))

;; Configure StumpWM appearance settings

;; Mode line settings
(setf stumpwm:*mode-line-background-color* "#282828"
      stumpwm:*mode-line-foreground-color* "#fabd2f"
      stumpwm:*mode-line-border-color* "#ebdbb2"
      stumpwm:*mode-line-border-width* 4
      stumpwm:*mode-line-pad-x* 16
      stumpwm:*mode-line-pad-y* 14
      stumpwm:*mode-line-timeout* 5)

;; Group format
(setf stumpwm:*group-format* "%n %t")

;; Time modeline format
(setf stumpwm:*time-modeline-string* "%a, %b%d @%I:%M%p")

;; Window format
(setf stumpwm:*window-format* "^b^(:fg \"#b8bb26\")<%25t>")
(setf stumpwm:*window-border-style* :tight)
(setf stumpwm:*normal-border-width* 4)

;; Message window settings
(setf stumpwm:*message-window-padding* 16)
(setf stumpwm:*message-window-y-padding* 14)
(setf stumpwm:*message-window-gravity* :bottom-left)

;; Screen mode line format
(setf stumpwm:*screen-mode-line-format*
      (list "%g | %v ^>^7 | "
            '(:eval (show-hostname))
            "| " '(:eval (show-kernel))
            "| " '(:eval (show-battery))
            "| " '(:eval (show-temp))
            "| %d"))

;; Set focus and unfocus colors
(stumpwm:set-focus-color "#ebdbb2")
(stumpwm:set-unfocus-color "#282828")

;; Rename and create new groups
(stumpwm:grename "Ness")
(stumpwm:gnewbg "Paula")
(stumpwm:gnewbg "Jeff")
(stumpwm:gnewbg "Poo")

;; Toggle mode line display
(stumpwm:toggle-mode-line (stumpwm:current-screen) (stumpwm:current-head))

;; Set prefix key
(set-prefix-key (kbd "C-t"))

;; Define Bind Macro
(defmacro bind-shell-to-key (key command &optional (map *root-map*))
  `(define-key ,map (kbd ,key) (concatenate 'string
                                            "run-shell-command "
                                            ,command)))

(defmacro bind-shell-to-topkey (key command &optional (map *top-map*))
  `(define-key ,map (kbd ,key) (concatenate 'string
                                            "run-shell-command "
                                            ,command)))


(defmacro bind-rofi-to-key (key command &optional (map *root-map*))
  `(define-key ,map (kbd ,key) (concatenate 'string
                                            "run-shell-command "
                                            '"rofi -i -show-icons -show "
                                            ,command)))

;; Set Rofi Keys
(defvar *my-rofi-key-commands*
  '(("SPC" "drun")
    ("RET" "window")))

(loop for (key cmd) in *my-rofi-key-commands* do
  (bind-rofi-to-key key cmd))

;; Set Special keys
(defvar *my-special-key-commands*
           '(("Print" "scrot -F ~/Pictures/screenshot-`date +%F`.png")
             ("XF86AudioRaiseVolume" "sndioctl output.level=+0.05")
             ("XF86AudioLowerVolume" "sndioctl output.level=-0.05")
             ("XF86AudioMute" "sndioctl output.level=\!")))

(loop for (key cmd) in *my-special-key-commands* do
  (bind-shell-to-topkey key cmd))

;; Set App Keys
(defvar *my-app-key-commands*
           '(("c" "alacritty")
            ("C-c" "alacritty")
            ("e" "emacsclient --create-frame --alternate-editor='emacs'")
            ("f" "firefox-esr")
            ("E" "thunderbird")
            ("F" "caja")
            ("x" "xkill")
            ("l" "slock")
            ("M-b" "feh --bg-fill $(shuf -n1 -e /usr/local/share/backgrounds/*)")))

(loop for (key cmd) in *my-app-key-commands* do
  (bind-shell-to-key key cmd))

;; Define WM keybindings

;; Global keybindings
(define-key *root-map* (kbd "M-ESC") "mode-line")
(define-key *root-map* (kbd "M-q") "quit")

;; Window movement between groups
(define-key *root-map* (kbd "m") "mark")
(define-key *root-map* (kbd "M") "gmove-marked")

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

;; Define Web Jumps
(make-web-jump "eco" "firefox-esr https://ecosia.org/search?q=")
(make-web-jump "ddg" "firefox-esr https://html.duckduckgo.com/html?q=")

;; Define Terminal Jumps
(make-term-jump "mansearch" "alacritty --hold -e apropos ")
(make-term-jump "manpage" "alacritty --hold -e man ")
(make-term-jump "pkgname" "alacritty --hold -e pkg_info -Q ")
(make-term-jump "pkgloc" "alacritty --hold -e pkg_locate ")

;; Keybindings for Web Jumps
(define-key *top-map* (kbd "M-s") "eco")
(define-key *top-map* (kbd "M-d") "ddg")

;; Keybindings for Terminal Jumps
(define-key *top-map* (kbd "M-m") "mansearch")
(define-key *top-map* (kbd "M-M") "manpage")
(define-key *top-map* (kbd "M-p") "pkgname")
(define-key *top-map* (kbd "M-P") "pkgloc")

(run-shell-command "exec emacs --daemon")
