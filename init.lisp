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

;; Optimize
(declaim (optimize (speed 3) (safety 3)))

;; Compile FASL
(setq *block-compile-default* t)

;; this automatically prefixes 'stumpwm:' to commands that need it
(in-package :stumpwm)
(setf *default-package* :stumpwm)

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

;; Ignore Resize Hints
(setq *ignore-wm-inc-hints* t)

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
   "swm-ssh" ;; ssh
   "stumptray" ;; System tray
   "scratchpad" ;; floating scratchterm
   "window-switch" ;; switch windows 
   "hostname" ;; native hostname
   "stumpwm-sndioctl" ;; sound
   "searchengines" ;; search macros
   "beckon" ;; yank mouse cursor focus
   "globalwindows" ;; navigate windows in all spacs
   "urgentwindows" ;; get urgent windows
   ))

;; Return inversed nil if string empty
(defun handle-empty-string (string)
  "return '^Rnil^r' if string id empty, otherwise return the string"
  (if (string= string "")
      "^Rnil^r"
    string))

;; normalize String
(defun normalize-string (string)
  "remove weird whitespace or rubbish in strings"
  (string-trim
   '(#\Space #\Newline #\Backspace #\Tab #\Linefeed #\Page #\Return #\Rubout)
   string))

;; combine
(defun normalize-and-handle-empty (string)
  "normalize string and handle empty result"
  (handle-empty-string (normalize-string string)))

;; there a battery?
(defun battery-present-p ()
  (let ((output (normalize-and-handle-empty (run-shell-command "apm -b" t))))
    (not (string= output "4"))))

;; Conditionally add battery-portable module
(when (battery-present-p)
  (push "battery-portable" *modulenames*))

;; Load modules
(dolist (modulename *modulenames*)
  (load-module modulename))

;;
;; Module Settings
;;
;; swm-gapes
;; Set Gaps
(setf swm-gaps:*inner-gaps-size* 3
      swm-gaps:*outer-gaps-size* 5)
;; Turn em on
(swm-gaps:toggle-gaps-off)
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
(setf *message-window-padding* 1
      *message-window-y-padding* 1
      *message-window-gravity* :bottom)

;; Input window settings
(setf *input-window-gravity* :center)

;;;
;; Define Functions
;;;

;; Run a shell command and format the output
(defun run-shell-command-and-format (command)
  "run a shell command, if output is empty reverse coloring and return string 'nil'"
  (let ((output (run-shell-command command t)))
        (normalize-and-handle-empty output)))

;; Show CPU Temperature
(defun show-temp ()
  "get temp data from sysctl"
  (run-shell-command-and-format 
   "sysctl -n hw.sensors.cpu0.temp0 2>/dev/null || 
    sysctl -n hw.sensors.acpitz0.temp0 2>/dev/null || 
    echo ''"))

;; Show Volume
(defun show-volume (type)
  "show current volume given a (type) argument"
   (format nil "~,1f%" (* 100 (read-from-string (run-shell-command-and-format (format nil "sndioctl -n ~a.level" type))))))

;; Show the window title
(defun show-window-title ()
  "show the title of the active window"
  (normalize-and-handle-empty (window-title (current-window))))

;;;
;; Formatting
;;;

;; Constants
(defparameter pipe " | ")
(defparameter group-bracket-color "^8") ;; Soft Orange
(defparameter group-content-color "^6") ;; Soft Aqua
(defparameter audio-bracket-color "^7") ;; White
(defparameter audio-content-color "^B^2*^b") ;; Soft Green
(defparameter status-bracket-color "^5") ;; Soft Magenta
(defparameter status-content-color "^3*") ;; Soft Yellow
(defparameter win-bracket-color "^1") ;; Soft Red
(defparameter win-content-color "^2" ) ;; Soft Green

;; Components
(defvar group-fmt "%g")
(defvar status-fmt (list
                    '(:eval (if (battery-present-p)
                                (concatenate 'string "%B" pipe)
                                "")) ;; Battery
                    '(:eval (show-temp)) pipe ;; Cpu Temp
                    "%d" ;; Date
                    ))
(defvar audio-fmt (list
                   '(:eval (show-volume "output"))
                   "/"
                   '(:eval (show-volume "input"))))
(defvar win-fmt "%v")

;; Generate a Component of a given color
(defun generate-mode-line-component (out-color in-color component &optional right-alignment)
  "Generate a Component of a given color, by default the component is Left aligned. set right-alignment to not nil for Right alignment"
  (if right-alignment
      (list "^>" out-color "[" in-color component out-color "]")
      (list out-color "[" in-color component out-color "]")))

(defun generate-mode-line ()
  "build a modeline"
  (setf *screen-mode-line-format*
	(list
         (generate-mode-line-component group-bracket-color group-content-color group-fmt)
         (generate-mode-line-component status-bracket-color status-content-color status-fmt)
         (generate-mode-line-component audio-bracket-color audio-content-color audio-fmt)
         (generate-mode-line-component win-bracket-color win-content-color win-fmt))))

;; Actually load my modeline
(generate-mode-line)

;; Format Modeline
(setf *mode-line-background-color* "#282828"
      *mode-line-border-color* "#EBDBB2"
      *mode-line-border-width* 1
      *mode-line-pad-x* 1
      *mode-line-pad-y* 1
      *mode-line-timeout* 1)

;; mode line on all heads
(dolist (h (screen-heads (current-screen)))
  (enable-mode-line (current-screen) h t))

;; Load in StumpTray
(stumptray::stumptray)

;; cleanup/autostart
(load "~/.stumpwm.d/autostart.lisp")
