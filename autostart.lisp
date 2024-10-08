;; Play Startup sound
(defun play-startup-sound ()
  (run-shell-command "sleep 1 && ffplay -autoexit -nodisp ~/.local/sfx/okdesuka.wav"))
(defun set-default-sounds ()
  (run-shell-command "sndioctl input.level=0.74")
  (run-shell-command "sndioctl output.level=1.00"))


(defun gnome-keyring ()
  (run-shell-command "gnome-keyring-daemon -r -d -c secrets"))

;; Start
(when *initializing*
  ;; Startup Sound
  (set-default-sounds)
  (play-startup-sound)
  ;; gnome keyring
  (gnome-keyring)
  ;; re/start slynk server
  (slynk:create-server
   :dont-close t))

;; Finish Threads
(defvar *bind-thread-list*
  (list
   *my-special-key-thread*
   *my-wm-window-thread*
   *my-shell-key-thread*
   *my-app-key-thread*
   *my-rofi-key-thread*
   *my-media-key-thread*
   *my-wm-module-thread*
   *my-unprefixed-module-thread*))
(dolist (threadname *bind-thread-list*)
  (bt:join-thread threadname))
