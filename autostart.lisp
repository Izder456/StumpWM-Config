;; Start Slynk Server
(ql:quickload :slynk)
(slynk:create-server :dont-close t)

;; Play Startup sound
(defun play-startup-sound ()
  (run-shell-command "sleep 1 && ffplay -autoexit -nodisp ~/.local/sfx/okdesuka.wav"))

(defun set-default-sounds ()
  (run-shell-command "sndioctl input.level=0.74")
  (run-shell-command "sndioctl output.level=1.00"))

;; Startup Sound
(set-default-sounds)
(play-startup-sound)

;; Finish Threads
(defvar *my-thread-list*
  (list *my-special-key-thread*
	*my-shell-key-thread*
	*my-app-key-thread*
	*my-rofi-key-thread*
	*my-wm-window-thread*
	*my-wm-module-thread*))
(dolist (thread *my-thread-list*)
  (sb-thread:join-thread thread))
