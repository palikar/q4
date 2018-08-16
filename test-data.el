(defvar boards nil)

(cl-defstruct q4/board-struct
  "structure for a vichan board
NAME : string
THREADS : list"
  name
  threads)

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
  page)



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
   :subject (alist-get-prop 'sub)
   :comment (alist-get-prop 'com)
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

(defun q4/transform-board (json board &rest args)
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
    (setq boards current-board)))

(defun q4/threads-operate-by-property (threads property value)
  "Compares The Property of THREADS by VALUE
and returns a list of threads that match the search
non-destructive"
  (let ((accessor (pcase property	;TODO: finish adding all of these (int, date, file, etc)
		    ('subject #'q4/thread-struct-subject)
		    ('comment #'q4/thread-struct-comment)))
	(comparator (pcase property
		      ('subject #'string-match-p) 
		      ('comment #'string-match-p)))
	(search-element nil)) 
    (remove nil
	    (cl-loop for thread in threads	;descriptive
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


(q4/query "catalog.json" 'q4/transform-board "g")
