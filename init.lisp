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
(set-font "-*-spleen-*-*-*-*-12-*-*-*-*-*-*-*")
(setf *mouse-focus-policy* :click)
(setq *startup-message* (format nil "Welcome Izzy!"))

;; Set env vars
(setf (getenv "PATH") "/home/izder456/.npm-global/bin:/home/izder456/.cargo/bin:/home/izder456/.local/bin:/home/izder456/.emacs.d/bin:/home/izder456/.local/share/pkg/bin:/bin:/usr/bin:/sbin:/usr/sbin:/usr/X11R6/bin:/usr/local/bin:/usr/local/sbin:/usr/local/jdk-17/bin")
(setf (getenv "PAGER") "most")
(setf (getenv "TERM") "xterm-256color")
(setf (getenv "PKG_PATH") "https://cdn.OpenBSD.org/pub/OpenBSD/snapshots/packages/amd64")

;; Set focus and unfocus colors
(set-focus-color iz-white)
(set-unfocus-color iz-gray)

;; GAAAAAPs
(load-module "swm-gaps")
;; Set Gaps
(setf swm-gaps:*inner-gaps-size* 8)
(setf swm-gaps:*outer-gaps-size* 10)
;; Turn em on
(swm-gaps:toggle-gaps-on)

;; Emacs
(load-module "swm-emacs")

;; Hostname
(load-module "hostname")

;; Battery
(load-module "battery-portable")

;; Cleaner SNDIO interface
(load-module "stumpwm-sndioctl")

;; Rename and create new groups
(grename "Ness")
(gnewbg "Paula")
(gnewbg "Jeff")
(gnewbg "Poo")

;; Group format
(setf *group-format* "%n %t")

;; Window format
(setf *window-format* (format NIL "^b^(:fg \"~A\")<%25t>" iz-softgreen))
(setf *window-border-style* :tight)
(setf *normal-border-width* 4)

;; Message window settings
(setf *message-window-padding* 12)
(setf *message-window-y-padding* 10)
(setf *message-window-gravity* :center)

;; Input window settings
(setf *input-window-gravity* :center)

;; Run a shell command and format the output
(defun run-shell-command-and-format (command)
  (substitute #\Space #\Newline (run-shell-command command t)))

;; Show the kernel version
(defun show-kernel ()
  (run-shell-command-and-format "uname -r"))

;; Show the temperature
(defun show-temp ()
  (run-shell-command-and-format "sysctl -n hw.sensors.cpu0.temp0"))

;; Show the window title
(defun show-window-title ()
  (substitute #\Space #\Newline (window-title (current-window))))

;; Screen mode line format
(setf *screen-mode-line-format*
      (list ;; Groups
            "^3( " ;; Yellow
            "^n%g " ;; Default
            ;; Windows
            "^1[ " ;; Red
            "^n%v ^>^7 ";; Default
            "^1] " ;; Red
            ;; Statuses
            "^5[ " ;; Magenta
            "^n" ;; Default
            "| " "%h " ;; Hostname
            "| " "%B " ;; Battery
            "| " '(:eval (show-temp)) ;; Cpu Temp
            "| " "%d |" ;; Date
            "^5 ]" ;; Magenta
            "^3)" ;; Yellow
            ))

;; Format Modeline
(setf *time-modeline-string* "%a, %b %d @%I:%M%p"
      *mode-line-background-color* iz-black
      *mode-line-foreground-color* iz-softyellow
      *mode-line-border-color* iz-white
      *mode-line-border-width* 4
      *mode-line-pad-x* 12
      *mode-line-pad-y* 10
      *mode-line-timeout* 5)

;; Toggle mode line display
(toggle-mode-line (current-screen) (current-head))

;; Load BIND file
(load "~/.stumpwm.d/bind.lisp")

;; Load JUMPS file
(load "~/.stumpwm.d/jumps.lisp")
