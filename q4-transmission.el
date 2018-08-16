(defvar q4/transmission-username nil
  "user name required to use transmission-remote")

(defvar q4/transmission-password nil
  "password required to use transmission-remote")

(defun q4/start-transmission-daemon nil
  "starts the transmission daemon"
  (unless (= 0 (shell-command "transmission-daemon" nil nil ))
    (error "transmission daemon failed to start")))

(defun q4/add-magnet-link (link &optional args)
  (shell-command (format "transmission-remote -n '%s:%s' -a %s  %s"
			 q4/transmission-username
			 q4/transmission-password
			 link
			 (if args args ""))))

;; TODO text search and fontification(buttons?)

