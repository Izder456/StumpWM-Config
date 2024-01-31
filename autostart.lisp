;; Play Startup sound
(defun play-startup-sound ()
  (run-shell-command "sleep 1 && ffplay -autoexit -nodisp ~/.local/sfx/okdesuka.wav"))
(defun set-default-sounds ()
  (run-shell-command "sndioctl input.level=0.74")
  (run-shell-command "sndioctl output.level=1.00"))

(when *initializing*
  ;; Startup Sound
  (set-default-sounds)
  (play-startup-sound)
  ;; which-key interactive
  (which-key-mode))

;; Finish Threads
(defvar *bind-thread-list*
  (list *my-special-key-thread*
        *my-wm-window-thread*
        *my-shell-key-thread*
        *my-app-key-thread*
        *my-rofi-key-thread*
        *my-media-key-thread*
        *my-wm-module-thread*))
(dolist (threadname *bind-thread-list*)
  (sb-thread:join-thread threadname))
