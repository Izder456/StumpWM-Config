;; Quicklisp Setup
(let ((quicklisp-init (merge-pathnames ".quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;; this automatically prefixes 'stumpwm:' to commands that need it
(in-package :stumpwm)
(setf *default-package* :stumpwm)

;; Set Modules
(set-module-dir "/home/izder456/.stumpwm.d/modules")

;; Colors Gruv Super List
(defvar iz-black "#282828")
(defvar iz-red "#CC241D")
(defvar iz-softred "#FB4934")
(defvar iz-green "#98971A")
(defvar iz-softgreen "#B8BB26")
(defvar iz-yellow "#D79921")
(defvar iz-softyellow "#FABD2F")
(defvar iz-blue "#458588")
(defvar iz-softblue "#83A598")
(defvar iz-purple "#B16286")
(defvar iz-softpurple "#D3869B")
(defvar iz-aqua "#689D6A")
(defvar iz-softaqua "#8EC07C")
(defvar iz-orange "#D65D0E")
(defvar iz-softorange "#FE8019")
(defvar iz-white "#EBDBB2")
(defvar iz-gray "#928374")

;; Set font and colors for the message window
(set-fg-color iz-white)
(set-bg-color iz-black)
(set-border-color iz-white)
(set-msg-border-width 4)
(set-font "-misc-spleen-medium-r-normal--16-160-72-72-c-80-iso10646-1")
(setf *mouse-focus-policy* :click)
(run-shell-command "xsetroot -cursor_name left_ptr")
(setq *startup-message* (format nil "Welcome Izzy!"))

;; Set env vars
(setf (getenv "GDK_CORE_DEVICE_EVENTS") "1")
(setf (getenv "PAGER") "most")
(setf (getenv "TERM") "xterm-256color")
(setf (getenv "PKG_PATH") "https://cdn.OpenBSD.org/pub/OpenBSD/snapshots/packages/amd64")

;; Set Gaps
(load-module "swm-gaps")
(setf swm-gaps:*inner-gaps-size* 10)
(setf swm-gaps:*outer-gaps-size* 12)

;; Turn em onV
(swm-gaps:toggle-gaps-on)

;; Mode line settings
(setf *mode-line-background-color* iz-black
      *mode-line-foreground-color* iz-softyellow
      *mode-line-border-color* iz-white
      *mode-line-border-width* 4
      *mode-line-pad-x* 16
      *mode-line-pad-y* 14
      *mode-line-timeout* 5)

;; Set focus and unfocus colors
(set-focus-color iz-white)
(set-unfocus-color iz-gray)

;; Rename and create new groups
(grename "Ness")
(gnewbg "Paula")
(gnewbg "Jeff")
(gnewbg "Poo")

;; Group format
(setf *group-format* "%n %t")

;; Time modeline format
(setf *time-modeline-string* "%a, %b%d @%I:%M%p")

;; Window format
(setf *window-format* (format NIL "^b^(:fg \"~A\")<%25t>" iz-softgreen))
(setf *window-border-style* :tight)
(setf *normal-border-width* 4)

;; Message window settings
(setf *message-window-padding* 16)
(setf *message-window-y-padding* 14)
(setf *message-window-gravity* :center)

;; Input window settings
(setf *input-window-gravity* :center)

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
(toggle-mode-line (current-screen) (current-head))

;; Load BIND file
(load "~/.stumpwm.d/bind.lisp")

;; Load JUMPS file
(load "~/.stumpwm.d/jumps.lisp")
