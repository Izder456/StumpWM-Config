;; StumpWM binds

(in-package :stumpwm)

;; Set prefix key
(set-prefix-key (kbd "C-t"))

;;;
;; BIND MACROS
;;;

;; Bind to *root-map*
(defmacro bind-shell-to-key (key command &optional (map *root-map*))
  `(define-key ,map (kbd ,key) (concatenate 'string
                                            "run-shell-command "
                                            ,command)))

;; Bind to *top-map*
(defmacro bind-shell-to-topkey (key command &optional (map *top-map*))
  `(define-key ,map (kbd ,key) (concatenate 'string
                                            "run-shell-command "
                                            ,command)))

;; Bind roft command to *root-map*
(defmacro bind-rofi-to-key (key command &optional (map *root-map*))
  `(define-key ,map (kbd ,key) (concatenate 'string
                                            "run-shell-command "
                                            '"rofi -i -show-icons -show "
                                            ,command)))

;;;
;; DEFINE BINDING LISTS
;;;

;; Set Rofi Keys
(defvar *my-rofi-key-commands*
  '(("SPC" "drun")
    ("RET" "window")))

;; Set Special keys
(defvar *my-special-key-commands*
           '(("Print" "scrot -F ~/Pictures/screenshot-`date +%F`.png")
             ("M-Print" "scrot -s -F ~/Pictures/screenshot-split-`date +%F`.png")
             ("s-Print" "scrot -u -F ~/Pictures/screenshot-activewin-`date +%F`.png")
             ("XF86AudioRaiseVolume" "sndioctl output.level=+0.05")
             ("XF86AudioLowerVolume" "sndioctl output.level=-0.05")
             ("XF86AudioMute" "sndioctl output.level=\!")))

;; Set App Keys
(defvar *my-app-key-commands*
           '(("c" "alacritty")
            ("C-c" "alacritty")
            ("f" "ungoogled-chromium")
            ("E" "thunderbird")
            ("F" "caja")
            ("x" "xkill")
            ("l" "slock")
            ("M-b" "feh --bg-fill $(shuf -n1 -e /usr/local/share/backgrounds/*)")))

;;;
;; LOOP OVER BINDING LISTS
;;;

(loop for (key cmd) in *my-rofi-key-commands* do
  (bind-rofi-to-key key cmd))

(loop for (key cmd) in *my-app-key-commands* do
  (bind-shell-to-key key cmd))

(loop for (key cmd) in *my-special-key-commands* do
  (bind-shell-to-topkey key cmd))

;; Define WM keybindings

;; Global keybindings
(define-key *top-map* (kbd "M-ESC") "mode-line")
(define-key *root-map* (kbd "M-Q") "quit")

;; Window movement between groups
(define-key *root-map* (kbd "m") "mark")
(define-key *root-map* (kbd "M") "gmove-marked")

;; EMACS!!
(define-key *root-map* (kbd "e") "emacs")
