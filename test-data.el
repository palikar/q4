;; -*- lexical-binding: t -*-
(defvar boards nil)

(cl-defstruct q4/watched-thread
  "structure for a watched thread
board-name : string ; the name of the board that the thread resides
last-modified : integer ; the time the thread was last posted today
thread-no : integer ; the number of the thread
changed : boolean ; whether the thread has changed since last seen
"
  board-name
  last-modified
  thread-no
  changed)

(cl-defstruct q4/thread-struct
  "structure for a vichan thread
NO : integer
DATE : string 
POSTER-NAME : integer
SUBJECT : string
COMMENT : string
TIM : integer
TIME : integer
FILE-NAME : string
FILE-EXTENSION : string
FILE-WIDTH : integer
FILE-HEIGHT : integer
FILE-MD5 : integer
SEMANTIC-URL : string
REPLIES : integer
IMAGES : integer
LAST-MODIFIED : integer
PAGE : integer
"
  no
  date
  poster-name
  subject
  comment
  tim
  time
  file-name
  file-extension
  file-width
  file-height
  file-md5
  semantic-url
  replies
  images
  last-modified
  page
  comments
)

(cl-defstruct q4/comment-struct
  "structure for a vichan thread
NO : integer
DATE : string 
POSTER-NAME : integer
SUBJECT : string
COMMENT : string
TIM : integer
TIME : integer
FILE-NAME : string
FILE-EXTENSION : string
FILE-WIDTH : integer
FILE-HEIGHT : integer
FILE-MD5 : integer
SEMANTIC-URL : string
REPLIES : integer
IMAGES : integer
LAST-MODIFIED : integer
PAGE : integer
REPLIED-BY : list of threads that reply to the current thread
REPLIED-TO : list of threads that this comment replies to
"
  board
  no
  date
  poster-name
  comment
  tim
  time
  file-name
  file-extension
  file-width
  file-height
  file-md5
  replied-by
  replied-to)



(defmacro alist-get-prop (property)
  `(alist-get ,property alist))

(defun q4/set-thread-properties (alist page-num)
  "sets the properties of the thread struct THREAD-STRUCT
based of alist ALIST
and int PAGE-NUM"
  (make-q4/thread-struct
   :no (alist-get-prop 'no)
   :date (alist-get-prop 'now)
   :poster-name (alist-get-prop 'name)
   :subject (unless (alist-get-prop 'sub) "")
   :comment (unless (alist-get-prop 'com) "")
   :file-name (alist-get-prop 'filename)
   :file-extension (alist-get-prop 'ext)
   :tim (alist-get-prop 'tim)
   :time (alist-get-prop 'time)
   :file-md5 (alist-get-prop 'md5)
   :semantic-url (alist-get-prop 'semantic_url)
   :replies (alist-get-prop 'replies)
   :images (alist-get-prop 'images)
   :page page-num
   :last-modified (alist-get-prop 'last_modified)))
(defun q4/set-comment-properties (alist board )
  "sets the properties of the thread struct THREAD-STRUCT
based of alist ALIST
and int PAGE-NUM"
  (make-q4/comment-struct
   :no (alist-get-prop 'no)
   :date (alist-get-prop 'now)
   :poster-name (alist-get-prop 'name)
   :comment (alist-get-prop 'com)
   :file-name (alist-get-prop 'filename)
   :file-extension (alist-get-prop 'ext)
   :tim (alist-get-prop 'tim)
   :time (alist-get-prop 'time)
   :file-md5 (alist-get-prop 'md5)
   :board board
   :replied-by '()
   :replied-to '()))

(defun q4/transform-board (json board &option set-var)
  "creates the new board for BOARD from the JSON and creates child threads using
q4/board-struct for the board and
q4/thread-struct for the thread"
  (let ((current-board (make-q4/board-struct :name board :threads '()))
	(current-thread nil))
    (dotimes (page (1- q4/catalog-pages))
      (cl-loop for alist across (alist-get 'threads (aref json page))
	       do (progn
		    (setq current-thread 
			  (q4/set-thread-properties alist page))
		    ;; (insert (format "\n %S \n" alist))
		    (setf (q4/board-struct-threads current-board)
			  (cons current-thread (q4/board-struct-threads current-board))))))
    ;; (switch-to-buffer buffer)
    (if set-var
	(setf set-var current-board)
      (setq boards current-board))))



(defun q4/threads-operate-by-property (threads property value)
  "Compares The Property of THREADS by VALUE
and returns a list of threads that match the search
non-destructive"
  (let ((accessor (pcase property ;TODO: finish adding all of these (int, date, file, etc)
		    ('subject #'q4/thread-struct-subject)
		    ('comment #'q4/thread-struct-comment)
		    ('no #'q4/thread-struct-no)))
	(comparator (pcase property
		      ('subject #'string-match-p) 
		      ('comment #'string-match-p)
		      ('no #'=)))
	(search-element nil)) 
    (remove nil
	    (cl-loop for thread in threads ;descriptive
		     collect
		     (if (setq search-element (funcall accessor thread))
			 (when (funcall comparator value search-element)
			   thread))))))

(defalias 'q4/threads-search-by-property 'q4/threads-operate-by-property
  "Search THREADS by VALUE using PROPERTY")

(defun q4/threads-filter-by-property (threads property value)
  "Filters out THREADS by VALUE using PROPERTY
e.g. filters threads where the value of subject is \"emacs\""
  (cl-set-exclusive-or threads (q4/threads-operate-by-property threads property value)))

(defun q4/extract-reply-no (comment)
  ;; (string-match "<a href=\"#p\\([:digit:]+?\\)\" class=\"quotelink\">" comment)
  (let ((replies '())
	(matching-text comment)
	(no nil))
    (cl-loop
     for i = (string-match "<a href=\"#p\\([0-9]+\\)\" class=\"quotelink\">" matching-text)
     while i
     while (setq no (match-string-no-properties 1 matching-text))
     do (progn
	  (push no replies)
	  (setq matching-text (substring matching-text (fourth (match-data))))))
    (mapcar #'string-to-number replies))
  )

(q4/extract-reply-no "<a href=\"#p67297713\" class=\"quotelink\">&gt;&gt;67297713</a><br>like the colour contrast and it looks fairly comfy<br>it does look like you&#039;ve tried to make KDE look like <a href=\"#p67297716\" class=\"quotelink\">&gt;&gt;67297713</a> XFCE")
(setq temp-t nil)

(defun q4/transform-thread (json buffer &rest args)
  "transformes the json thread to a tree of structs
with the top level being the OP and every post that isn't a reply to another post
and the replies being children of other posts"
  (let*  ((board "g")
	  (post-hash-table (make-hash-table))
	  (posts (cl-loop for post across (alist-get 'posts json)
			  collect (q4/set-comment-properties post board))))
    (cl-loop for post in posts ;populates a hash table with key being the post number and the data being the comment struct
  	     do (puthash
		 (q4/comment-struct-no post)
		 post
		 post-hash-table))
    
    (cl-loop for post in posts
	     for comment = (q4/comment-struct-comment post)
	     while comment
	     for replied-to = (q4/extract-reply-no comment)
	     do (cl-loop for i in replied-to
			 do
			 (push
			  (gethash i post-hash-table)
			  (q4/comment-struct-replied-to post))))
    (setq test-posts posts))
  ;; (let ((temp-buffer (get-buffer-create "temp-b")))
  ;;   (with-current-buffer temp-buffer
  ;;     (insert (format "%S" json))))
  )

(defun q4/backtrace-posts (posts)
  "goes through each post, and if it replies to a thread, set the replied by to the current post, essentially converting the singally linked map into a doubly linked map"
  (cl-loop
   for post in posts
   for replied-to = (q4/comment-struct-replied-to post)
   do (cl-loop for reply in replied-to
	       do (push post (q4/comment-struct-replied-by reply)))
   )
  )
(setq test-posts nil)
(setq test-posts2 (q4/backtrace-posts test-posts))
(q4/query "catalog.json" 'q4/transform-board "g")
(let ((no 67300447)
      (board "g")) (q4/query (format "thread/%s.json" no) 'q4/transform-thread board))


(defvar temp-board nil)
(defun q4/check-watched-threads ()
  (loop for i in q4/watched-threads
	
	do ))

(defvar q4/watched-threads
  '()  "a list of the watched threads")

(defun q4/watch-thread (thread board-name)
  (push (make-q4/watched-thread
	 :board-name board-name
	 :last-modified (q4/thread-struct-last-modified thread)
	 :thread-no (q4/thread-struct-no thread)) ))	;since the thread 'no' is unique, it will be a good choice for the watched threads

(defun q4/clear-watched-thread nil
  (setq q4/watched-threads '()))
(car (q4/board-struct-threads boards))


(q4/thread-struct-no (car (q4/threads-search-by-property (q4/board-struct-threads boards) 'comment "Fuck google")))
67187950

