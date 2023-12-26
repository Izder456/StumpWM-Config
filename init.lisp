;;;
;; StumpWM Boilerplate
;;;

;; Quicklisp Setup
(let ((quicklisp-init (merge-pathnames ".quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;; this automatically prefixes 'stumpwm:' to commands that need it
(in-package :stumpwm)
(setf *default-package* :stumpwm)

;; Set Modules
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
                     iz-red ;; ^1
                     iz-green ;; ^2
                     iz-yellow ;; ^3
                     iz-blue ;; ^4
                     iz-purple ;; ^5
                     iz-aqua ;; ^6
                     iz-white ;; ^7
                     iz-orange ;; ^8
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

;; Click-to-focus
(setf *mouse-focus-policy* :click)

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
(setf (getenv "PAGER") "most")
(setf (getenv "PKG_PATH") "https://cdn.OpenBSD.org/pub/OpenBSD/snapshots/packages/amd64")

;;;
;; Modules & their config
;;;

;; Init modules
(init-load-path *module-dir*)

;; GAAAAAPs
(load-module "swm-gaps")
;; Set Gaps
(setf swm-gaps:*inner-gaps-size* 8)
(setf swm-gaps:*outer-gaps-size* 10)
;; Turn em on
(swm-gaps:toggle-gaps-on)

;;; Moving the mouse for me
;; Used for warping the cursor
(load-module "beckon")
(defmacro with-focus-lost (&body body)
  "Make sure WIN is on the top level while the body is running and
restore it's always-on-top state afterwords"
  `(progn (banish)
          ,@body
          (when (current-window)
            (beckon:beckon))))

;; Emacs
(load-module "swm-emacs")

;; Browser
(load-module "browse")
(setf browse::*homepage* "http://68k.news")

;; Web Searches
(load-module "searchengines")
(setf searchengines:*search-browser-executable* "firefox-esr")
;; Set Search Engine Params
(defparameter *URL-DDG* "https://duckduckgo.com/s?num=100&q=~a")
(defparameter *URL-LIBRE* "https://search.ahwx.org/search?num=100&q=~a")
;; Set Search Selects
(searchengines:make-searchengine-selection "search-ddg-selection" *URL-DDG* "DuckDuckGo search" :map *top-map* :key "M-C-s")
(searchengines:make-searchengine-prompt "search-ddg-prompt" "DuckDuckGo" *URL-DDG* "DuckDuckGo search" :map *top-map* :key "M-s")
(searchengines:make-searchengine-selection "search-libre-selection" *URL-LIBRE* "LibreY search" :map *top-map* :key "M-C-l")
(searchengines:make-searchengine-prompt "search-libre-prompt" "LibreY" *URL-LIBRE* "LibreY search" :map *top-map* :key "M-l")

;; load-path
(add-to-load-path "~/.stumpwm.d/extras/scratchpad")
;; Scratchpad
(load-module "scratchpad")
(defcommand scratchpad-term () ()
  (scratchpad:toggle-floating-scratchpad "term" "st"
                                         :initial-gravity :center
                                         :initial-width 720
                                         :initial-height 480))
;; Bind Scratchpad to Super+t
(define-key *top-map* (kbd "s-t") "scratchpad-term")

;; Hostname
(load-module "hostname")

;; Battery
(load-module "battery-portable")

;; Cleaner SNDIO interface
(load-module "stumpwm-sndioctl")

;; Play Startup sound
(defun play-startup-sound ()
  (run-shell-command "sleep 1 && ffplay -autoexit -nodisp ~/.local/sfx/okdesuka.wav"))

(defun set-default-sounds ()
  (run-shell-command "sndioctl input.level=0.74")
  (run-shell-command "sndioctl output.level=1.00"))

;; Rename and create new groups
(grename "Ness")
(gnewbg "Paula")
(gnewbg "Jeff")
(gnewbg "Poo")

;; Group format
(setf *group-format* "%n %t")

;; Window format
(setf *window-format* (format NIL "^(:fg \"~A\")<%25t>" iz-softgreen))
(setf *window-border-style* :tight)
(setf *normal-border-width* 4)

;; Time format
(setf *time-modeline-string* "%a, %b %d @%I:%M%p")

;; Message window settings
(setf *message-window-padding* 12)
(setf *message-window-y-padding* 10)
(setf *message-window-gravity* :center)

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

;; Show the window title
(defun show-window-title ()
  (substitute #\Space #\Newline (window-title (current-window))))

;;;
;; Formatting
;;;

;; Break out modeline formatting
(defvar group-fmt (list
                   "^n%g " ;; Default
                   ))
(defvar win-fmt (list
                 "^n%v ^>^7 " ;; Default -> Right Allign
                 ))
(defvar status-fmt (list
                    "^n" ;; Default
                    "| " "%h " ;; Hostname
                    "| " "%B " ;; Battery
                    "| " '(:eval (show-temp)) ;; Cpu Temp
                    "| " "%d |" ;; Date
                    ))

;; Screen mode line format
(setf *screen-mode-line-format*
      (list "^b( " ;; Yellow
            group-fmt
            "^1[ " ;; Red
            win-fmt
            "^1 ]" ;; Red
            "^5[ " ;; Magenta
            status-fmt
            "^5 ]" ;; Magenta
            "^3^b)" ;; Yellow
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

;;;
;; Load in other files
;;;

;; binds
(load "~/.stumpwm.d/bind.lisp")

;; jumps
(load "~/.stumpwm.d/jumps.lisp")

;; Startup Sound
(set-default-sounds)
(play-startup-sound)
