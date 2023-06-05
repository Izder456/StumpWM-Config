(in-package :stumpwm)
;; Message window themeing
(set-font "xft:GohuFont Nerd Font Mono:size=14")
(set-fg-color "#ebdbb2")
(set-bg-color "#282828")
(set-border-color "#ebdbb2")
(set-msg-border-width 4)

(defvar color1 "#fabd2f")
(defvar color2 "#282828")

(defun show-kernel ()
  (let ((kernel (run-shell-command "uname -r" t)))
    (substitute #\Space #\Newline kernel)))

(defun show-hostname ()
  (let ((host-name (run-shell-command "hostname" t)))
    (substitute #\Space #\Newline host-name)))

(defun show-battery ()
  (let ((batt-perc (run-shell-command "battstat -c '++' -d '--' {i} {p}" t)))
    (substitute #\Space #\Newline batt-perc)))

(defun show-temp ()
  (let ((temp-celc (run-shell-command "sysctl -n hw.sensors.acpithinkpad0.temp4" t)))
    (substitute #\Space #\Newline temp-celc)))

(setf
  stumpwm:*mode-line-background-color* color2
  stumpwm:*mode-line-foreground-color* color1
  stumpwm:*mode-line-border-color* "#ebdbb2"
  stumpwm:*screen-mode-line-format* (list "%g | %v ^>^7 | " '(:eval (show-hostname)) "| " '(:eval (show-kernel)) "| " '(:eval (show-battery)) "| " '(:eval (show-temp)) "| %d")
  stumpwm:*mode-line-border-width* 4
  stumpwm:*mode-line-pad-x* 16
  stumpwm:*mode-line-pad-y* 14
  stumpwm:*mode-line-timeout* 5
  stumpwm:*group-format*  "%n %t"
  stumpwm:*time-modeline-string* "%a, %B %d %Y @%I:%M%p"
  stumpwm:*window-format* "^b^(:fg \"#b8bb26\")<%25t>"
  stumpwm:*window-border-style* :tight
  stumpwm:*normal-border-width* 4
  stumpwm:*message-window-padding* 16
  stumpwm:*message-window-y-padding* 14
  stumpwm:*message-window-gravity* :bottom-left
  )
(stumpwm:set-focus-color "#ebdbb2")
(stumpwm:set-unfocus-color "#282828")
(stumpwm:grename "Ness")
(stumpwm:gnewbg "Paula")
(stumpwm:gnewbg "Jeff")
(stumpwm:gnewbg "Poo")
(stumpwm:gnewbg "Ninten")
(stumpwm:gnewbg "Ana")
(stumpwm:gnewbg "Loid")
(stumpwm:gnewbg "Teddy")
(stumpwm:toggle-mode-line (stumpwm:current-screen) (stumpwm:current-head))

;; Set Prefix
(set-prefix-key (kbd "C-t"))
(define-key *top-map* (kbd "M-ESC") "mode-line")
(define-key *root-map* (kbd "M-q") "quit")

;; prompt the user for an interactive command. The first arg is an
;; optional initial contents.
(defcommand colon1 (&optional (initial "")) (:rest)
  (let ((cmd (read-one-line (current-screen) ": " :initial-input initial)))
    (when cmd
      (eval-command cmd t))))

;; Browser
(define-key *root-map* (kbd "f") "exec qutebrowser")
;; Term
(define-key *root-map* (kbd "c") "exec st")
(define-key *root-map* (kbd "C-c") "exec st")
;; FileMgr
(define-key *root-map* (kbd "F") "exec caja" )
;; Rofi Menu
(define-key *root-map* (kbd "space") "exec rofi -i -show drun -modi drun -show-icons")
;; BG Set
(define-key *root-map* (kbd "M-b") "exec feh --bg-fill $(shuf -n1 -e /usr/local/share/backgrounds/*)")
;; Spotify
(define-key *root-map* (kbd "P") "exec st -T ncspot -e ncspot")
;; htop
(define-key *root-map* (kbd "i") "exec st -T htop -e htop")
;; neovim
(define-key *root-map* (kbd "e") "exec st -T nvim -e nvim")
;; XKill
(define-key *root-map* (kbd "x") "exec xkill")

;; Handle Window movine between groups)
(define-key *root-map* (kbd "m") "mark")
(define-key *root-map* (kbd "M") "gmove-marked")

;; special-Keys
(define-key *top-map* (kbd "Print") "exec scrot -F ~/Pictures/screenshot-`date +%F`.png")
(define-key *top-map* (kbd "XF86AudioRaiseVolume") "exec sndioctl output.level=+0.05")
(define-key *top-map* (kbd "XF86AudioLowerVolume") "exec sndioctl output.level=-0.05")
(define-key *top-map* (kbd "XF86AudioMute") "exec sndioctl output.mute=!")

;; Web jump (works for DuckDuckGo and Imdb)
(defmacro make-web-jump (name prefix)
  `(defcommand ,(intern name) (search) ((:rest ,(concatenate 'string name " search: ")))
    (nsubstitute #\+ #\Space search)
    (run-shell-command (concatenate 'string ,prefix search))))

(make-web-jump "linxer" "qutebrowser https://linxer.org/search.php?q=")
(make-web-jump "ddg" "qutebrowser https://lite.duckduckgo.com/lite?q=")
(make-web-jump "whoogle" "qutebrowser https://whoogle.click/search?q=")
;; C-t M-s is a terrble binding, but you get the idea.
(define-key *root-map* (kbd "M-s") "linxer")
(define-key *root-map* (kbd "M-d") "ddg")
(define-key *root-map* (kbd "M-w") "whoogle")
