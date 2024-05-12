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

;; Normalize String
(defun normalize-string (string)
  (remove-if #'(lambda (x) (member x '(#\Newline #\Return))) string))

;; Run a shell command and format the output
(defun run-shell-command-and-format (command)
  (let ((output (run-shell-command command t)))
    (if (string= output "")
	"^Rnil^r"
	(normalize-string output))))

;; Show the temperature
(defun show-temp ()
  (run-shell-command-and-format "sysctl -n hw.sensors.cpu0.temp0"))

;; Show Volume
(defun show-volume (type)
  (run-shell-command-and-format (format nil "sndioctl -n ~a.level" type)))

;; Show the current track
(defun show-current-track ()
  (run-shell-command-and-format
   "playerctl metadata --format '|[{{duration(position)}}] @{{trunc(volume, 5)}}|'"))

;; Show the window title
(defun show-window-title ()
  (normalize-string (window-title (current-window))))

;;;
;; Formatting
;;;

;; Constants
(defparameter pipe " | ")
(defparameter group-bracket-color "^8") ;; Soft Orange
(defparameter group-content-color "^6") ;; Soft Aqua
(defparameter audio-bracket-color "^9") ;; Gray
(defparameter audio-content-color "^B^2*^b") ;; Soft Green
(defparameter status-bracket-color "^5") ;; Soft Magenta
(defparameter status-content-color "^3*") ;; Soft Yellow
(defparameter win-bracket-color "^1") ;; Soft Red
(defparameter win-content-color "^2" ) ;; Soft Green

;; Components
(defvar group-fmt "%g")
(defvar win-fmt "%v")
(defvar status-fmt (list "%B" pipe ;; Battery
		   '(:eval (show-temp)) pipe ;; Cpu Temp
		   "%d" ;; Date
		   ))
(defvar audio-fmt (list '(:eval (show-volume "output"))
			" / "
			'(:eval (show-volume "input"))
			" "
			'(:eval (show-current-track))))

;; Generate a Component of a given color
(defun generate-mode-line-component (out-color in-color component)
  (list out-color "[" in-color component out-color "]"))

(setf *screen-mode-line-format*
	   (list
	    (generate-mode-line-component group-bracket-color group-content-color group-fmt)
	    (generate-mode-line-component audio-bracket-color audio-content-color audio-fmt)
	    (generate-mode-line-component status-bracket-color status-content-color status-fmt)
	    (generate-mode-line-component win-bracket-color win-content-color win-fmt)))

;; Format Modeline
(setf *mode-line-background-color* "#282828"
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
