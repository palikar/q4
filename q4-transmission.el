;; most of the hard part of transmission is setting up the server settingsg
(defvar q4/transmission-username nil
  "user name required to use transmission-remote")

(defvar q4/transmission-password nil
  "password required to use transmission-remote")

(defun q4/start-transmission-daemon nil
  "starts the transmission daemon"
  (unless (= 0 (shell-command "transmission-daemon" nil nil ))
    (error "transmission daemon failed to start")))

(defun q4/add-magnet-link (link &optional args)
  "adds a magnet link to the transmission daemon using transmission-remote
(specifically requires a `transmission-remote` package)"
  (shell-command (format "transmission-remote -n '%s:%s' -a %s  %s"
			 q4/transmission-username
			 q4/transmission-password
			 link
			 (if args args ""))))

(defun q4/buttonize-magnets (text)
  "matches single line magnet links and creates buttons that add them via the transmission"
  (string-match "magnet:.+" text)
  (let ((magnet-link (match-string-no-properties 0 text)))
    (insert-button "magnet" 'action `(lambda (x) (q4/add-magnet-link ,magnet-link)) )))
;; TODO: integrate with actual rendering, place magnet button above actual link


;; test
(let ((test-string "this is a magnet link magnet:?xt=urn:btih:bf524ae47445188adc1d3f58239c2ddfb15bdc33&dn=Glenn%20Gould%20Jacket%20Collection%20%28%21%21%21%29&tr=udp%3A%2F%2Ftracker.leechers-paradise.org%3A6969&tr=udp%3A%2F%2Fzer0day.ch%3A1337&tr=udp%3A%2F%2Fopen.demonii.com%3A1337&tr=udp%3A%2F%2Ftracker.coppersurfer.tk%3A6969&tr=udp%3A%2F%2Fexodus.desync.com%3A6969"))
  (q4/buttonize-magnets test-string))
