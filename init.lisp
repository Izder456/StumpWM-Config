;;;
;; StumpWM Boilerplate
;;;


;; Quicklisp Setup
(let ((quicklisp-init (merge-pathnames ".quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;; Load Quicklisp Packages
(ql:quickload '("stumpwm"
		"clx"
		"cl-ppcre"
		"alexandria"
		"cl-fad"
		"xembed"
		"anaphora"
		"drakma"
		"slynk"))

;; no style-warns
(declaim #+sbcl(sb-ext:muffle-conditions style-warning))

;; this automatically prefixes 'stumpwm:' to commands that need it
(in-package :stumpwm)

;; Set Modules
; (set-contrib-dir) is deprecated, this is the method now
(set-module-dir "~/.stumpwm.d/modules")

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
                     iz-softyellow ;; ^3
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
(set-msg-border-width 3)
(set-font "-misc-spleen-medium-r-normal--16-160-72-72-c-80-iso10646-1")

;; MouseKeys
(setf *mouse-focus-policy* :click
      *float-window-modifier* :super)

;; Welcome
(setq *startup-message* (format nil "^b^8Welcome Izzy!")) ;; Orange

;; Set focus and unfocus colors
(set-focus-color iz-white)
(set-unfocus-color iz-gray)
(set-float-focus-color iz-softaqua)
(set-float-unfocus-color iz-aqua)

;;;
;; Env Vars
;;;

;; Set env vars
(setf (getenv "PATH") "/home/izder456/.npm-global/bin:/home/izder456/.cargo/bin:/home/izder456/.local/bin:/home/izder456/.emacs.d/bin:/home/izder456/.local/share/pkg/bin:/bin:/usr/bin:/sbin:/usr/sbin:/usr/X11R6/bin:/usr/local/bin:/usr/local/sbin:/usr/local/jdk-17/bin:/home/izder456/.cargo/bin:/home/izder456/.local/bin:/usr/local/jdk-17/bin:/bin:/home/izder456/.go/bin:/home/izder456/.gems/bin:/home/izder456/bin")
(setf (getenv "PAGER") "less -R")
(setf (getenv "BROWSER") "firefox-esr")

;;;
;; Modules & their config
;;;

;; Init modules
(add-to-load-path "~/.stumpwm.d/extras/scratchpad")
(add-to-load-path "~/.stumpwm.d/extras/stumpwm-window-switch")

;; Load that module shizz in
(init-load-path *module-dir*)

(defvar *modulenames*
  (list
   "swm-gaps" ;; gaps
   "swm-emacs" ;; emacs
   "swm-ssh" ;; ssh
   "scratchpad" ;; floating scratchterm
   "window-switch" ;; switch windows 
   "hostname" ;; native hostname
   "battery-portable" ;; battery level
   "stumpwm-sndioctl" ;; sound
   "searchengines" ;; search macros
   "beckon" ;; yank mouse cursor focus
   "globalwindows" ;; navigate windows in all spacs
   "urgentwindows" ;; get urgent windows
   ))

(dolist (modulename *modulenames*)
  (load-module modulename))
;;
;; Module Settings
;;
;; swm-gapes
;; Set Gaps
(setf swm-gaps:*inner-gaps-size* 6
      swm-gaps:*outer-gaps-size* 6)
;; Turn em on
(swm-gaps:toggle-gaps-on)
;; SSH
(setq swm-ssh:*swm-ssh-default-term* "st")

;; urgent window
(setf urgentwindows:*urgent-window-message* "Application ~a has just finished!")

;; Oneko command
(defcommand oneko () ()
  "Oneko Start Command"
  (bt:make-thread
   (lambda ()
     (run-shell-command "oneko -tora -tofocus -name 'neko'"))))
(defcommand kill-oneko () ()
  "Oneko Stop Command"
  (bt:make-thread
   (lambda ()
     (run-shell-command "pkill -9 oneko"))))

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

;; pre-binds
(load "~/.stumpwm.d/pre-bind.lisp")
 
;; binds
(load "~/.stumpwm.d/bind.lisp")

;; jumps
(load "~/.stumpwm.d/jumps.lisp")

;; Rename and create new groups
(when *initializing*
  (grename "Ness")
  (gnewbg "Jeff")
  (gnewbg "Paula")
  (gnewbg "Poo"))

;; Clear rules
(clear-window-placement-rules)

;; Group format
(setf *group-format* "%n")

;; Window format
(setf *window-format* (format NIL "^(:fg \"~A\")<%25t>" iz-softgreen)
      *window-border-style* :tight
      *normal-border-width* 3
      *hidden-window-color* "^**")

;; Time format
(setf *time-modeline-string* "%a, %b %d @%I:%M%p")

;; Message window settings
(setf *message-window-padding* 6
      *message-window-y-padding* 6
      *message-window-gravity* :bottom)

;; Input window settings
(setf *input-window-gravity* :center)

;;;
;; Define Functions
;;;

;; Run a shell command and format the output
(defun run-shell-command-and-format (command)
  (substitute #\Space #\Newline (run-shell-command command t)))

;; Show system information
(defun show-system-info (command)
  (run-shell-command-and-format command))

;; Show the kernel version
(defun show-kernel ()
  (show-system-info "uname -r"))

;; Show the temperature
(defun show-temp ()
  (show-system-info "sysctl -n hw.sensors.cpu0.temp0"))

;; Show Volume
(defun show-volume (type)
  (run-shell-command-and-format (format nil "sndioctl -n ~a.level" type)))

;; Show the current track
(defun show-current-track ()
  (run-shell-command-and-format
   "playerctl metadata --format '| [{{duration(position)}}] @{{trunc(volume, 5)}}|'"))

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
(defvar group-fmt "^n%g") ;; Default
(defvar win-fmt "^n%v ^>^7") ;; Default -> Right Align
(defvar audio-fmt (list
		   " " '(:eval (show-volume "output"))
		   "/"
		   " " '(:eval (show-volume "input"))
		   '(:eval (show-current-track))
		   ))
(defvar status-fmt (list
		    "^n" pipe ;; Default
		    " %B " pipe ;; Battery
		    " " '(:eval (show-temp)) pipe ;; Cpu Temp
		    " %d " pipe ;; Date
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
      *mode-line-border-width* 3
      *mode-line-pad-x* 6
      *mode-line-pad-y* 6
      *mode-line-timeout* 1)

;; Toggle mode line display
(toggle-mode-line (current-screen) (current-head))

;; cleanup/autostart
(load "~/.stumpwm.d/autostart.lisp")
