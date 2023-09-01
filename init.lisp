;; StumpWM configuration file

(in-package :stumpwm)

;; Set font and colors for the message window
(set-font "xft:GohuFont uni14 Nerd Font Mono:style=Regular")
(set-fg-color "#ebdbb2")
(set-bg-color "#282828")
(set-border-color "#ebdbb2")
(set-msg-border-width 4)

;; Define color variables
(defvar color1 "#fabd2f")
(defvar color2 "#282828")

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
(setf stumpwm:*mode-line-background-color* color2
      stumpwm:*mode-line-foreground-color* color1
      stumpwm:*mode-line-border-color* "#ebdbb2"
      stumpwm:*mode-line-border-width* 4
      stumpwm:*mode-line-pad-x* 16
      stumpwm:*mode-line-pad-y* 14
      stumpwm:*mode-line-timeout* 5)

;; Group format
(setf stumpwm:*group-format* "%n %t")

;; Time modeline format
(setf stumpwm:*time-modeline-string* "%a, %b %d @%I:%M%p")

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

;; Define keybindings

;; Global keybindings
(define-key *root-map* (kbd "M-ESC") "mode-line")
(define-key *root-map* (kbd "M-q") "quit")

;; Application and command keybindings
(define-key *root-map* (kbd "f") "exec firefox-esr")
(define-key *root-map* (kbd "c") "exec alacritty")
(define-key *root-map* (kbd "C-c") "exec alacritty")
(define-key *root-map* (kbd "F") "exec caja")
(define-key *root-map* (kbd "space") "exec rofi -i -show drun -modi drun -show-icons")
(define-key *root-map* (kbd "M-b") "exec feh --bg-fill $(shuf -n1 -e /usr/local/share/backgrounds/*)")
(define-key *root-map* (kbd "e") "exec emacsclient --create-frame --alternate-editor='emacs'")
(define-key *root-map* (kbd "P") "exec alacritty -T ncspot -e ncspot")
(define-key *root-map* (kbd "i") "exec alacritty -T htop -e htop")
(define-key *root-map* (kbd "I") "exec alacritty --hold -T fetch -e neofetch")
(define-key *root-map* (kbd "x") "exec xkill")
(define-key *root-map* (kbd "L") "exec slock")

;; Window movement between groups
(define-key *root-map* (kbd "m") "mark")
(define-key *root-map* (kbd "M") "gmove-marked")

;; Special keys
(define-key *top-map* (kbd "Print") "exec scrot -F ~/Pictures/screenshot-`date +%F`.png")
(define-key *top-map* (kbd "XF86AudioRaiseVolume") "exec sndioctl output.level=+0.05")
(define-key *top-map* (kbd "XF86AudioLowerVolume") "exec sndioctl output.level=-0.05")
(define-key *top-map* (kbd "XF86AudioMute") "exec sndioctl output.mute=\!")

;; Web jump commands
(defmacro make-web-jump (name prefix)
  `(defcommand ,(intern name) (search)
     ((:rest ,(concatenate 'string name " search: ")))
     (nsubstitute #\+ #\Space search)
     (run-shell-command (concatenate 'string ,prefix search))))

(make-web-jump "librey" "firefox-esr https://search.ahwx.org/search.php?q=")
(make-web-jump "ddg" "firefox-esr https://lite.duckduckgo.com/lite?q=")

;; Keybindings for web jumps
(define-key *root-map* (kbd "M-s") "librey")
(define-key *root-map* (kbd "M-d") "ddg")

(run-shell-command "exec emacs --daemon && notify-send 'Emacs Init!'")
