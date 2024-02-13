;;;
;; StumpWM Boilerplate
;;;

;; Quicklisp Setup
(let ((quicklisp-init (merge-pathnames ".quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;; no style-warns
(declaim #+sbcl(sb-ext:muffle-conditions style-warning))

;; this automatically prefixes 'stumpwm:' to commands that need it
(in-package :stumpwm)
(setf *default-package* :stumpwm)

;; Set Modules
(set-module-dir "~/.stumpwm.d/modules")

;; Load Slynk Package
(ql:quickload :slynk)

;;;
;; Colors
;;;

;; Colormap
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

;; Color list for `^` formatting
(setf *colors* (list iz-black ;; ^0
                     iz-softred ;; ^1
                     iz-softgreen ;; ^2
                     iz-yellow ;; ^3
                     iz-softblue ;; ^4
                     iz-softpurple ;; ^5
                     iz-softaqua ;; ^6
                     iz-white ;; ^7
                     iz-softorange ;; ^8
                     iz-gray ;; ^9
                     ))
;; Set those colors
(update-color-map (current-screen))

;;;
;; Styling
;;;

;; Set font and colors for the message window
(set-fg-color iz-white)
(set-bg-color iz-black)
(set-border-color iz-white)
(set-msg-border-width 4)
(set-font "-*-spleen-*-*-*-*-12-*-*-*-*-*-*-*")

;; MouseKeys
(setf *mouse-focus-policy* :click
      *float-window-modifier* :SUPER)

;; Welcome
(setq *startup-message* (format nil "^B^8Welcome Izzy!")) ;; Orange

;; Set focus and unfocus colors
(set-focus-color iz-white)
(set-unfocus-color iz-gray)
(set-float-focus-color iz-aqua)
(set-float-unfocus-color iz-softaqua)

;;;
;; Env Vars
;;;

;; Set env vars
(setf (getenv "PATH") "/home/izder456/.npm-global/bin:/home/izder456/.cargo/bin:/home/izder456/.local/bin:/home/izder456/.emacs.d/bin:/home/izder456/.local/share/pkg/bin:/bin:/usr/bin:/sbin:/usr/sbin:/usr/X11R6/bin:/usr/local/bin:/usr/local/sbin:/usr/local/jdk-17/bin")
(setf (getenv "PAGER") "less -R")

;;;
;; Modules & their config
;;;

;; Init modules
(init-load-path *module-dir*)
(add-to-load-path "~/.stumpwm.d/extras/scratchpad")

(defvar *modulenames*
  (list "swm-gaps"
	"swm-emacs"
	"scratchpad"
	"hostname"
	"battery-portable"
	"stumpwm-sndioctl"
	"browse"
	"searchengines"))

(dolist (modulename *modulenames*)
  (load-module modulename))

;;
;; Module Settings
;;
;; swm-gapes
;; Set Gaps
(setf swm-gaps:*inner-gaps-size* 8
      swm-gaps:*outer-gaps-size* 10)
;; Turn em on
(swm-gaps:toggle-gaps-on)

;; scratchpad
;; define default scratchpad term
(defcommand scratchpad-term () ()
            (scratchpad:toggle-floating-scratchpad "term" "st"
                                                   :initial-gravity :center
                                                   :initial-width 720
                                                   :initial-height 480))
;; Bind Scratchpad to Super+t
(define-key *top-map* (kbd "s-t") "scratchpad-term")

;;;
;; Load in other files
;;;

;; binds
(load "~/.stumpwm.d/bind.lisp")

;; jumps
(load "~/.stumpwm.d/jumps.lisp")

;; Rename and create new groups
(when *initializing*
  (grename "Ness")
  (gnewbg "Paula")
  (gnewbg "Jeff")
  (gnewbg "Poo"))

;; Group format
(setf *group-format* "%n %t")

;; Window format
(setf *window-format* (format NIL "^(:fg \"~A\")<%25t>" iz-softgreen)
      *window-border-style* :tight
      *normal-border-width* 4)

;; Time format
(setf *time-modeline-string* "%a, %b %d @%I:%M%p")

;; Message window settings
(setf *message-window-padding* 12
      *message-window-y-padding* 10
      *message-window-gravity* :top)

;; Input window settings
(setf *input-window-gravity* :center)

;;;
;; Define Functions
;;;

;; Run a shell command and format the output
(defun run-shell-command-and-format (command)
  (substitute #\Space #\Newline (run-shell-command command t)))

;; Show the kernel version
(defun show-kernel ()
  (run-shell-command-and-format "uname -r"))

;; Show the temperature
(defun show-temp ()
  (run-shell-command-and-format "sysctl -n hw.sensors.cpu0.temp0"))

;; Show Volume
(defun show-output-vol ()
  (run-shell-command-and-format "sndioctl -n output.level"))
(defun show-input-vol ()
  (run-shell-command-and-format "sndioctl -n input.level"))
  
;; Show the window title
(defun show-window-title ()
  (substitute #\Space #\Newline (window-title (current-window))))

;;;
;; Formatting
;;;

;; Break out modeline formatting
;; Constants
(defvar pipe "|")

;; Format Lists
(defvar group-fmt (list
		   "^n%g" ;; Default
		   ))
(defvar win-fmt (list
		 "^n%v ^>^7" ;; Default -> Right Allign
		 ))
(defvar audio-fmt (list
		   " " '(:eval (show-output-vol))
		   "/"
		   " " '(:eval (show-input-vol))
		   ))
(defvar status-fmt (list
		    "^n" pipe ;; Default
		    " %h " pipe ;; Hostname
		    " %B " pipe ;; Battery
		    " " '(:eval (show-temp)) pipe ;; Cpu Temp
		    " %d " ;; Date
		    ))

;; Screen mode line format
(setf *screen-mode-line-format*
      (list "(" ;; Yellow
	    group-fmt
	    "^1 [ " ;; Red
	    win-fmt
	    "^1] " ;; Red
	    "^6{" ;; Aqua
	    audio-fmt
	    "^6} " ;; Aqua
	    "^5[" ;; Magenta
	    status-fmt
	    "^5]" ;; Magenta
	    "^3)" ;; Yellow
	    ))

;; Format Modeline
(setf *mode-line-background-color* iz-black
      *mode-line-foreground-color* iz-softyellow
      *mode-line-border-color* iz-white
      *mode-line-border-width* 4
      *mode-line-pad-x* 12
      *mode-line-pad-y* 10
      *mode-line-timeout* 5)

;; Toggle mode line display
(toggle-mode-line (current-screen) (current-head))

;; cleanup/autostart
(load "~/.stumpwm.d/autostart.lisp")
