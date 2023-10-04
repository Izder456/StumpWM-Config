;; StumpWM configuration file

(in-package :stumpwm)

;;;
;; SET WM SETTINGS
;;;

;; Set font and colors for the message window
(set-fg-color "#ebdbb2")
(set-bg-color "#282828")
(set-border-color "#ebdbb2")
(set-msg-border-width 4)
(set-font "-misc-spleen-medium-r-normal--16-160-72-72-c-80-iso10646-1")
(setf *mouse-focus-policy* :click)
(run-shell-command "xsetroot -cursor_name left_ptr")
(setf (getenv "GDK_CORE_DEVICE_EVENTS") "1")

;; Mode line settings
(setf stumpwm:*mode-line-background-color* "#282828"
      stumpwm:*mode-line-foreground-color* "#fabd2f"
      stumpwm:*mode-line-border-color* "#ebdbb2"
      stumpwm:*mode-line-border-width* 4
      stumpwm:*mode-line-pad-x* 16
      stumpwm:*mode-line-pad-y* 14
      stumpwm:*mode-line-timeout* 5)

;; Set focus and unfocus colors
(stumpwm:set-focus-color "#ebdbb2")
(stumpwm:set-unfocus-color "#282828")

;; Rename and create new groups
(stumpwm:grename "Ness")
(stumpwm:gnewbg "Paula")
(stumpwm:gnewbg "Jeff")
(stumpwm:gnewbg "Poo")

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

;;;
;; UTILIITY FUNCS FOR MODELINE
;;;

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

;; Screen mode line format
(setf stumpwm:*screen-mode-line-format*
      (list "%g | %v ^>^7 | "
            '(:eval (show-hostname))
            "| " '(:eval (show-kernel))
            "| " '(:eval (show-battery))
            "| " '(:eval (show-temp))
            "| %d"))

;; Toggle mode line display
(stumpwm:toggle-mode-line (stumpwm:current-screen) (stumpwm:current-head))

;;;
;; LOAD OTHER FILES
;;;

;; Load BIND file
(load "~/.stumpwm.d/bind.lisp")

;; Load JUMPS file
(load "~/.stumpwm.d/jumps.lisp")
