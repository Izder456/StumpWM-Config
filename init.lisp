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

;; Define a list of color names and their corresponding hex codes
(defvar *color-map*
  '((iz-black . "#282828")
    (iz-red . "#CC241D")
    (iz-softred . "#FB4934")
    (iz-green . "#98971A")
    (iz-softgreen . "#B8BB26")
    (iz-yellow . "#D79921")
    (iz-softyellow . "#FABD2F")
    (iz-blue . "#458588")
    (iz-softblue . "#83A598")
    (iz-purple . "#B16286")
    (iz-softpurple . "#D3869B")
    (iz-aqua . "#689D6A")
    (iz-softaqua . "#8EC07C")
    (iz-orange . "#D65D0E")
    (iz-softorange . "#FE8019")
    (iz-white . "#EBDBB2")
    (iz-gray . "#928374")))

;; Directly create *colors* by selecting the desired colors from *color-map*
(setf *colors*
      (mapcar (lambda (color-name)
		(cdr (assoc color-name *color-map*)))
	      '(iz-black ;; ^0
		iz-softred ;; ^1
		iz-softgreen ;; ^2
		iz-softyellow ;; ^3
		iz-softblue ;; ^4
		iz-softpurple ;; ^5
		iz-softaqua ;; ^6
		iz-white ;; ^7
		iz-softorange ;; ^8
		iz-gray))) ;; ^9

;; Set those colors
(update-color-map (current-screen))

;;;
;; Styling
;;;

;; Set font and colors for the message window
(set-fg-color "#EBDBB2")
(set-bg-color "#282828")
(set-border-color "#EBDBB2")
(set-msg-border-width 1)
(set-font "-misc-spleen-medium-r-normal--16-160-72-72-c-80-iso10646-1")

;; MouseKeys
(setf *mouse-focus-policy* :click
      *float-window-modifier* :super)

;; Welcome
(setq *startup-message* (format nil "^b^8Welcome Izzy!")) ;; Orange

;; Set focus and unfocus colors
(set-focus-color "#EBDBB2")
(set-unfocus-color "#928374")
(set-float-focus-color "#8EC07C")
(set-float-unfocus-color "#689D6A")

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
   "stumptray" ;; tray icons
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
(setf swm-gaps:*inner-gaps-size* 2
      swm-gaps:*outer-gaps-size* 4)
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
(setf *window-format* (format NIL "^(:fg \"~A\")<%25t>" "#B8BB26")
      *window-border-style* :tight
      *normal-border-width* 1
      *hidden-window-color* "^**")

;; Time format
(setf *time-modeline-string* "%I:%M%p")

;; Message window settings
(setf *message-window-padding* 2
      *message-window-y-padding* 2
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
   "playerctl metadata --format '|[{{duration(position)}}] @{{trunc(volume, 5)}}|'"))

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
(defvar group-fmt "^6%g") ;; Default (softaqua)
(defvar win-fmt "^n%v") ;; Default (green)
(defvar status-fmt (list
		    "^n" ;; Default
		    " %B " pipe ;; Battery
		    " " '(:eval (show-temp)) pipe ;; Cpu Temp
		    " %d " ;; Date
		    ))
(defvar audio-fmt (list
		   "^2 " '(:eval (show-volume "output")) ;; Soft Green
		   "^7/" ;; White
		   "^2 " '(:eval (show-volume "input")) ;; Soft Green
		   "^4" '(:eval (show-current-track)) ;; Soft Blue
		   ))

;; Screen mode line format
(setf *screen-mode-line-format*
      (list
	    "^8[" ;; Soft Orange
	    group-fmt
	    "^8]" ;; Soft Orange
	    "^9[" ;; Gray
	    audio-fmt
	    "^9]" ;; Gray
	    "^5[" ;; Magenta
	    status-fmt
	    "^5]"
	    "^1[" ;; Red
	    win-fmt
	    "^1]" ;; Red
	    ))

;; Format Modeline
(setf *mode-line-background-color* "#282828"
      *mode-line-foreground-color* "#FABD2F"
      *mode-line-border-color* "#EBDBB2"
      *mode-line-border-width* 1
      *mode-line-pad-x* 6
      *mode-line-pad-y* 6
      *mode-line-timeout* 1)

;; mode line
(mode-line)

;; Open Stumptray (AFTER modeline)
(stumptray:stumptray)

;; cleanup/autostart
(load "~/.stumpwm.d/autostart.lisp")
