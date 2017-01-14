;; [Q4 Mode by @desvox (Blake DeMarcy)]
;; [   https://github.com/desvox/q4   ]

;; Commentary:

;; q4.el is a fucking masterpiece. The crude hacks, lack of error handling,
;; verbosity of solutions, and the lack of testing outshine the Mona Lisa.
;; When I write code, I shove it in there, and if it bleeds, I smile and
;; laugh and push it in dee...errr, polish it later :^)

;; Kidding aside: this is not finished software. It wont break your emacs
;; but it may not be what you are expecting yet.

;; The entry point to start browsing is the interactive funtion
;; q4/browse-board. Opening media has preference to use the third party
;; programs feh and mpv, but soon I will implement a fallback to use the
;; built-in image mode, which has gif support but cannot handle webms.

;; Sometimes, 4chan sends back gzipped responses from their API. There is
;; no indication of when or why this happens, and it seems to only happen
;; every few dozen requests. First, Q4 will check if your build of emacs
;; has zlib support compiled in. Most do, including the windows builds
;; linked below. If it doesn't, your system will need to have the the gzip
;; utility installed, but its a very standard util thats most likely
;; installed already if you use a Unix/GNU Linux operating system. You can
;; double check By typing 'gzip' in a command line and it will spit out a
;; message whether or not its installed.

;; If these both fail, the worst case scenario is that sometimes you will
;; have to call an action twice if it errors out.

;; If the colors are ugly, they try to set themselves based on whether
;; your theme is dark or light using emacs' own face system. If this
;; fails for you, please look at the functions q4/set-light-colors
;; and q4/set-dark-colors: they will force the right ones into place.

;; Q4 attempts to bind keys to Evil's normal mode if it is installed. It
;; also attempts to utilize helm or ivy for prompts when they are
;; installed. It will fall back to the built-in ido-mode, and if for some
;; arcane resason that fails, falls back to completing-read which is the
;; same component used by vanilla functions like M-x, switch-buffer, etc.
;; Q4 was built on GNU/Linux, in Spacemacs, but I also do testing on
;; vanilla (unconfigured, standard) emacs installs, and on Virtualbox'd
;; Windows 7 and 8. I have no way to test OSX support at this time.

;; As of this time, the Windows builds found at
;;                 https://sourceforge.net/projects/emacsbinw64/
;; are fully operational (as far as I can tell) except for external media
;; support. Thumbnails work but don't expect the i key to pop open Windows
;; Photo Viewer yet :^)

;; DO NOT USE THE OFFICAL GNU WINDOWS BUILDS. They DO NOT have the xml, html,
;; or image libraries Q4 depends on. Either compile it yourself or use the
;; link above. Emacs 24.x and below are not supported on any platform; the
;; shr library included before 25 lacks functionality that Q4 expects.

;; This file is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 3, or (at your option) any
;; later version.

;; This file is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along
;; with GNU Emacs; see the file COPYING. If not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;; ====================== PROGRESS ======================
;; DONE:
;;   Zero third party dependencies, works on the standard elisp library
;;   Evil (when installed) and vanilla emacs keybinds out of the box.
;;   Fairly robust property-based navigation. (refactoring and optimization still ongoing)
;;   Full thumbnail support, works async in stages to keep browsing snappy
;;   Reply threading support (interface needs work and better keybinds)
;;   Color highlighting for greentext, quotes, IDs, headers and seperators.
;;   Detects quotes that reference to deleted posts, applies a
;;     different face with no navigation callbacks.
;;   Cap and /pol/ flag icon support. Flags are already showing, but I still need to
;;     add display hooks for the caps.
;;   Tripcode and name support.
;;   External media support via feh and mpv
;;   Download full thread/catalog content with wget with interactive directory prompt
;;   Tracked navigation up a reply tree with a buffer local marker stack
;;   Inline quote expansion to read quotes without changing position
;;   Generate permalinks to open threads externally.
;;   Scrape up all URLs from a post or buffer, using helm, ivy, ido, or vanilla
;;     completeing-read to pop one open in an external browser. Super comfy.
;;   Cleans up all of its http request buffers.
;;   In-place thread refreshing, appending new posts at the end of the buffer.
;;     Catalogs need something better than the current "throw it all out" method.
;;   Gzip response support (this may cause a *nix dependency, will investigate for
;;   MS Windows later)
;;
;; ======================== TODO ========================
;; viper-mode support
;; smooth out replies-to-post naviagtion; add backward navigation, handling to close the window, etc
;; Utilize defcustom where it makes sense.
;; switch from the json 'now' property to the epoch timestamp
;; add /t/ magnet support in addition the URLs
;; set up photo download dir to prompt for full, not relative path when var is set to nil
;; add optional faces for tripcodes, names, dubs/trips/quads/etc..
;; MS Windows support for external media
;; more intelligent thumbnail rendering based on buffer position.
;; get /pol/ flags centered in the row instead of at the bottom (looks weird af)
;; /pol/ ID support
;; tree based browsing
;; 8chan support, maybe a few others, when more progress is made for 4chan.
;; ======================================================

(require 'derived)
(require 'json)
(require 'shr)
(require 'url)
(require 'cl)

(when (< emacs-major-version 25)
  (warn "Q4 WILL NOT WORK ON EMACS 24"))

(defvar q4/wrapwidth 80
  "The width, in characters, of post seperators and when post texts will be
word wrapped.")

(defvar q4/keep-point-centered t
  "Keep point position consistent when navigating.")

(defvar q4/show-namefags t
  "Acknowledge attention whores. When non-nil, will show names next to post
IDs if the user has chosen to use one. Also see `q4/show-tripfags'")

(defvar q4/show-tripfags t
  "Acknowledge attention whores. When non-nil, will show tripcodes next to
post IDs if the user has chosen to use one. Also see `q4/show-namefags'")

(defvar q4/photo-download-directory "~/Pictures/q4/"
  ;; TODO: Set this var to nil to prompt for new dir every time
  "The top level folder where thread content can be downloaded to.")

(defvar q4/show-countries t
  "Display country names or flags in supported boards (/pol/). See
`q4/country-type' to choose how this is displayed.")

(defvar q4/country-type 'flag
  "When `q4/show-countries' is non-nil, this symbol determines the
rendering type. You can use the symbols 'flag, 'name, 'abbrev, 'flag/name,
and 'flag/abbrev. Pretend, in this example, that % is a flag icon :^)

'flag: %
'name: Great Britain
'abbrev: GB
'flag/name: % Great Britain
'flag/abbrev: % GB")

(defvar q4/catalog-pages 6
  "number of pages to load from the catalog. Max is 10.")

(defvar q4/dead-quote-string ">>DEAD"
  "String to insert for quotes that refer to deleted posts. The face
`q4/dead-quote-face' is applied as well.")

(defvar q4/thumbnails t
  ;; tfw performance is so good you dont have to warn people about
  ;; this anymore
  "Render thumbnails in the catalog and threads when non nil. Disabling
this speeds up browsing a bit. Use the t key to switch this on the fly. Any
value set here is silently nil'd if you are using a terminal or your emacs
build doesn't have image support.")

(defvar q4/header-indicator "||>"
  "A string to insert at the beginning of each post/thread before any other
info. Must be at least one character, and navigation will be more reliable
if it is fairly unique (though text properties are also checked).")

(defvar q4/spacer "               ")
(defvar q4/centered nil
  "When this is non nil, q4/spacer is inserted at the beginning of each
line. This isn't dynamic atm, you will have to set this to your screen
width manually")

(defvar q4/seperator-char "-"
  "A 1-length string to draw seperators with.")

(defvar q4/discard-request-buffers t
  "Whether HTTP request buffers should be killed after their data is
extracted. This is t by default, but disabling it is useful for debugging.")


;;;;;;;;;;;;;;; blah blah "user servicable parts" blah blah "high quality code" ;;;;;;;;;;;;;;;


(defvar q4/base "http://a.4cdn.org/"
  "Base URL for all requests.")

(defvar q4/icon-base "http://s.4cdn.org/image/"
  "Base URL for cap and country icons.")

(defvar q4/url-regexp
  ;; TODO: Add /t/ magnet support
  "https*://[^ \n\r\t]+")

(defvar q4/icon-cache (make-hash-table :test 'equal :weakness nil)
  ;; https://www.youtube.com/watch?v=hU7EHKFNMQg
  "A hash table containing the gif image data for cap and flag icons, with
their names as keys.")

(defvar q4/all-4chan-boards '()
  "A list of all available boards for 4chan. This variable is initilized
on the first call to `q4/browse-board'.")

;; blah blah (eq '() nil) blah blah type clarity
(make-variable-buffer-local (defvar q4/establish-data t
  "When this is non nil, `q4/render-content' and all of it's worker
functions will do side effects to buffer variables, like pushing new data
to `q4/metadata' and `q4/postnos', etc. This can be bound as nil in a `let'
block to temporatily disable side effects for the renderer."))

(make-variable-buffer-local (defvar q4/metadata '()
 "A buffer local alist with cars for each post number, containing
information about the posts like replies, image data, etc. The keys are
stored as integers and not strings! `q4/get-post-property' is a wrapper to
access data from this list; it will convert input from a string to integer
if needed."))

(make-variable-buffer-local (defvar q4/threadpics '()
  "Buffer-local list containing links to the full-resolution photos in a
thread in the order they were posted."))

(make-variable-buffer-local (defvar q4/reply-ring '()
  ;; UNIMPLEMENTED
  "Buffer-local list which stores post IDs and point positions while browsing
through replies."))

(make-variable-buffer-local (defvar q4/mark-ring '()
  "Buffer-local list which stores navigation marks for quote hopping."))

(make-variable-buffer-local (defvar q4/thumblist '()
  "Buffer local containment list while rendering thumbnails from their urls."))

(make-variable-buffer-local (defvar q4/postnos '()
  "A list of all the thread/reply numbers (as strings) contained in the
current buffer."))

(make-variable-buffer-local (defvar q4/threadno ""
  "Buffer local string that is either 'catalog' or the OPs post number.
Also see `q4/extlink'"))

(make-variable-buffer-local (defvar q4/extlink ""
  "Buffer local string containing the URL for this thread or catalog."))

(make-variable-buffer-local (defvar q4/board ""
  "Buffer local string containing the board this buffer is visting." ))


(defvar q4/icon-path (expand-file-name "q4-icons" user-emacs-directory)
  "Path where cap and flag icons are stored in. This can
be safely changed, the contents will be redownloaded.")


(defface q4/greentext-face
  '((((type graphic) (background dark))
     :background nil :foreground "#90a959")
    (((type graphic) (background light))
     :background nil :foreground "DarkOliveGreen")
    (t :backround nil :foreground "green"))
  "Face for rendering greentexts."
  :group 'q4-mode)


(defface q4/gray-face
  '((((type graphic) (background dark))
     :background nil :foreground "#666")
    (((type graphic) (background light))
     :background nil :foreground "grey60")
    (t :background nil :foreground nil))
  "Face for rendering seperators, timestamps, and other
frilly UI elements."
  :group 'q4-mode)


(defface q4/id-face
  '((((type graphic) (background dark))
     :background nil :foreground "#d28445")
    (((type graphic) (background light))
     :background nil :foreground "IndianRed4")
    (t :background nil :foreground "cyan"))
  "Face for rendering comment and thread ID's."
  :group 'q4-mode)


(defface q4/quote-face
  '((((type graphic) (background dark))
     :background nil :foreground "#aa759f")
    (((type graphic) (background light))
     :background nil :foreground "MediumOrchid4")
    (t :background nil :foreground "magenta"))
  "Face for rendering quotes (ie. >>2903242)"
  :group 'q4-mode)


(defface q4/dead-quote-face
  '((((type graphic))
     :inherit 'error :strike-through t)
    (t (:inherit 'error :underline t)))
  "Face for rendering quotes that refer to
deleted posts."
  :group 'q4-mode)


(defface q4/country-name-face
  '((t :inherit 'q4/id-face))
  "Face for country name and abbreviation texts."
  :group 'q4-mode)


(defun q4/set-light-colors ()
  "Force the built-in faces to use the colors for light themes."
  (interactive)
  (set-face-attribute 'q4/greentext-face nil :foreground "DarkOliveGreen")
  (set-face-attribute 'q4/id-face nil :foreground "IndianRed4")
  (set-face-attribute 'q4/quote-face nil :foreground "MediumOrchid4")
  (set-face-attribute 'q4/gray-face nil :foreground "grey60"))


(defun q4/set-dark-colors ()
  "Force the built-in faces to use the colors for dark themes."
  (interactive)
  (set-face-attribute 'q4/greentext-face nil :foreground "#90a959")
  (set-face-attribute 'q4/id-face nil :foreground "#d28445")
  (set-face-attribute 'q4/quote-face nil :foreground "#aa759f")
  (set-face-attribute 'q4/gray-face nil :foreground "#666"))


(defun q4/recenter ()
  ;; TODO: Add preference var to change from top of screen (default) to center of screen.
  (when q4/keep-point-centered
    (recenter 1)))


(defun q4/next-pos (string &optional regex prop backward group bound)
  "Takes a STRING and returns the char position of the beginning of its
next occurence from point in `current-buffer'. Returns nil if not found.
A simpler way to call this is to use `q4/next-prop'.

When REGEX is non-nil, STRING is interpreted as a regular expression.

PROP, when non-nil, will only return matches if they have the corresponding
value for a property.  This can either be a symbol or a cons cell. If it's
a symbol, the property key used is :q4type. As a cons, The key and expected
value are given, eg '(:q4type . end)

BACKWARD, when non-nil, does what it says on the tin.

When GROUP is non-nil and an integer, returns start pos of that match
group. When PROP is in effect, it checks property at this position instead
of 0.

BOUND can be a buffer position (integer) that the search will not exceed."
  (save-excursion
    (let ((search (if backward (if regex 're-search-backward 'search-backward)
                    (if regex 're-search-forward 'search-forward)))
          (group (or group 0))
          (propkey (if (consp prop) (car prop) :q4type))
          (propval (if (consp prop) (cdr prop) prop))
          found)
      ;; for the unaware: searches through this function,
      ;; when passed t as seen here, will return nil when
      ;; it reaches end of buffer. This loop will not get
      ;; stuck because of this.
      (while (and (not found) (funcall search string bound t))
        (if prop (setq found (eql propval
                                  (get-char-property
                                   (match-beginning group)
                                   propkey)))
          (setq found t)))
      (when found (match-beginning group)))))

;; and lets also add a few shorthand functions for the
;; ones searched the most...

(defun q4/next-prop (prop &optional backward bound)
  "A simpler way to call `q4/next-pos' that only looks for properties and
doesn't match an input string. This is morally equivalent to:
  (q4/next-pos \".\" t PROP BACKWARD nil BOUND)"
  (q4/next-pos  "."  t prop backward nil bound))
  ;; A+ symmetry


(defun q4/head-pos (&optional backward)
  "Return char position of the next header block of a post from point.
BACKWARD, when non nil, goes...uh, backward."
  (q4/next-pos q4/header-indicator nil 'head backward))


(defun q4/sep-pos (&optional backward)
  "Return char position of the next seperator block between
posts. BACKWARD, when non nil, goes...uh, to Vegas."
  (q4/next-prop 'end backward))


(defun q4/assert-post-start ()
  "Makes sure the point is at the head of a post before doing any side
effects."
  (unless (eql 'head (get-char-property (point) :q4type))
    ;; I haven't really found a case where this would be nil,
    ;; and am also not currently sure how that should be handled.
    ;; Eh, fuck it.
    (let ((check (q4/head-pos t))) (when check (goto-char check)))))


(defun q4/current-post (&optional int buffer)
  "Returns the post number point is currently focused on."
  (with-current-buffer (or buffer (current-buffer))
    (let ((no (save-excursion
                (q4/assert-post-start)
                (get-char-property (point) :no))))
      (if int (string-to-int no) no))))


(defun q4/get-post-property (prop &optional post buffer)
  "Consults `q4/metadata' for PROP of POST in BUFFER. POST, if omitted,
uses `q4/current-post'. POST can be provided either as an integer or a
string. BUFFER defaults to `current-buffer', remember that `q4/metadata'
is buffer-local.

Returns either the cdr of PROP (which can be a nil value) or nil if it
isn't in the list."
  (when (stringp (setq post (or post (q4/current-post t))))
    (setq post (string-to-int post)))
  (with-current-buffer (or buffer (current-buffer))
    (alist-get prop (assq post q4/metadata))))


(defun q4/prop-at-point (prop)
  "Gets the property PROP from the character at point. See
`get-char-property'."
  (get-char-property (point) prop))


(defun q4/inboundp (marker &optional endbound)
  "Returns t when a given char position is within the boundaries of a
single post or catalog entry (or ENDBOUND)."
  (> (or endbound (q4/sep-pos)) marker))


(defun q4/boip (&optional marker)
  "Returns t when point is at the beginning of indentation OR at the
beginning of the line at column 0. This is equivalent to `bolp' except it
also checks `back-to-indentation'"
  (let ((point (or marker (point))))
    (save-excursion
      (back-to-indentation)
      (or (= point (point))
          (= point (point-at-bol))))))


(defmacro q4/append (newelt list)
  "Adds NEWELT to the end of LIST in place. LIST may be nil, this
will add its first element if needed."
  ;; blah blah nconc does shit in place blah blah doesnt work when the list is nil
  `(setq ,list (nconc ,list (cons ,newelt nil))))


(defun q4/fuck-whitespace (string &optional newlines-btfo)
  "Trim leading/trailing whitespace, and optionally remove all inner
newlines."
  (while (and newlines-btfo (string-match "[\n\r]+" string))
    (setq string (replace-match "" t t string)))
  (string-trim string))


(defun q4/threadpics-string ()
  "Returns a string with urls of the current buffer's photos, in the order
they were posted. This also works in the catalogs."

  ;; The laziest possible way of turning a list to
  ;; a usable string is to pass it to format, and chop
  ;; the parens off of it's representation :^)

  (when q4/threadpics
    (substring (format "%s" q4/threadpics) 1 -1)))


(defun q4/point-to-post (dir)
  "Move point to the head of next post in DIR. DIR can be one of the symbols
'next and 'prev."
  (let ((check
         (case dir
           ('prev (q4/head-pos t))
           ('next (save-excursion ;; or else point will stick
                    (forward-char (length q4/header-indicator))
                    (q4/head-pos))))))
    (when check (goto-char check) (q4/recenter))))


(defun q4/point-to-next-post ()
  "Feeds starving children in Africa, and does a better job at it then Vim."
  (interactive)
  (q4/point-to-post 'next)
  ;; Whenever using quote-hop-backward, its easy to lose track of what you're
  ;; doing and possibly leave the ring in an unclean state. This clears the
  ;; list if manual navigation exceeds the position of first jump
  (let ((lastmark (car (last q4/mark-ring))))
    (when (and (integerp lastmark) (> (point) lastmark))
      (setq q4/mark-ring nil))))


(defun q4/point-to-previous-post ()
  "Elects Donald Trump for president of the United States."
  (interactive) (q4/point-to-post 'prev))


(defun q4/seek-next-button (&optional goto)
  "Returns buffer position if the next button from POINT. if GOTO is
non-nil, moves point to the button."
  (interactive)
  (let ((pos (overlay-start (next-button (point)))))
    (if goto (goto-char pos) pos)))


(defun q4/seek-post (number &optional mark forward nocenter)
  "Takes a post NUMBER, which is actually a string :^), searches backward
for it unless FORWARD is non-nil, and pushes current cursor position to
`q4/mark-ring' when MARK is non-nil

When NOCENTER is non nil, suppresses calling on `q4/recenter'."
  (let ((search (q4/next-pos (concat q4/header-indicator number)
                             nil 'head (not forward))))
    (if search (progn (when mark (push (point) q4/mark-ring))
                      (goto-char search)
                      (unless nocenter
                        (q4/recenter)))
      (message "Post %s not found" number))))


(defun q4/quote-hop-backward (&optional seek)
  "Stores point position in a buffer-local mark ring, and jumps to the post
number being referenced.

SEEK, if provided as a string, will search for that post number
directly. When nil, it will locate the next quote within the current post
and jump to it, if it exists."
  (interactive)
  (if (or seek ;; if the caller didn't provide a num, find next quote
          (let ((next (q4/next-prop 'quoted nil (q4/sep-pos))))
            (when next (setq seek (get-char-property next :no)))))
      (q4/seek-post seek t)
    (message "No quotes between point and end of post.")))


(defun q4/complete-collection (prompt collection)
  "Prompts the user with the string PROMPT to select an item from
COLLECTION. Will check for the following packages to make this as comfy as
possible:

helm-mode
ivy-mode
ido-mode
vanilla emacs `completing-read'"
  (let ((choice
         (cond
          ((boundp 'helm-mode)
           (require 'helm)
           (helm-comp-read prompt collection :must-match t))
          ((boundp 'ivy-mode)
           (require 'ivy)
           (ivy-read prompt collection :require-match t))
          ((boundp 'ido-mode)
           (require 'ido)
           (ido-completing-read prompt collection nil t))
          (t (completing-read
              "(Use TAB to complete)> "
              collection nil t)))))
    choice))


(defun q4/list-urls (&optional whole-buffer)
  "Collects all urls in the current post and lets you pick one to pass to
to `browse-url'. Searches for the following packages (in the following
order) to provide only The Comfiest Selection Experience™

helm-mode
ivy-mode
ido-mode
vanilla emacs `completing-read'

If WHOLE-BUFFER is non nil, it will also put AMD out of
busi....errr...collect all urls in the buffer."
  ;; TODO: Isolate URLs from parenthesis if needed
  (interactive)
  (save-excursion
    (if whole-buffer (goto-char (point-min)) (q4/assert-post-start))
    (let ((bound (if whole-buffer (point-max) (q4/sep-pos))) collection)
      (save-excursion
        ;; [[ D O U B L E  D E C K E R  E X C U R S I O N  P R O T E C T I O N  S Q U A D ]]
        (while (re-search-forward q4/url-regexp bound t)
          (q4/append (match-string 0) collection)))
      (if collection
          (let ((choice (q4/complete-collection
                         "(Browse URL)> "
                         collection)))
            (if choice (browse-url choice) (message "Nevermind then!")))
        (message "No URLs in this post.")))))


(defun q4/view-content-externally ()
  "Prompts the user to browse either the post or buffer in the default
external browser. In this context, post is either a thread in a catalog, or
a reply in a thread. A buffer is either a catalog or a thread number. If
the current post has an image, includes an option for it as well."
  (interactive)
  (let* ((imglink (q4/get-post-property 'image))
         (postlink (q4/get-post-property 'link))
         (prompt (format "Open [b]uffer, [p]ost, %sor [c]ancel?\n(C-g/q/b/p/c%s)>"
                         (if imglink "[i]mage, " "")
                         (if imglink "/i" "")))
         ;; additionally, allow q, ESC and C-c to bail. read-char handles C-g
         ;; for free.
         (gtfo   `(?q ?Q ?c ?C ?\C-c ?\C-\[))
         (buffer '(?b ?B))
         (post   '(?p ?P))
         (image  '(?i ?I))
         (response (progn ;; workaround for it not always displaying...
                     (message prompt)
                     (read-char prompt))))
    (while (not (member response (concatenate
                                  'list buffer post
                                  gtfo (if imglink image))))
      (setq response (read-char prompt)))
    (cond
     ((member response buffer)
      (browse-url q4/extlink))
     ((member response post)
      (browse-url postlink))
     ((member response image)
      (browse-url imglink))
     ((member response gtfo)
      (message "Nevermind then!")))))


(defun q4/pop-mark ()
  "Flies backward to the last post in the mark ring at about mach 6, and
securely disposes of the previous position."
  (interactive)
  (if q4/mark-ring
      (progn (goto-char (pop q4/mark-ring)) (q4/recenter))
    (message "Stack is empty.")))


(defun q4/ext-program-p (program &optional whine)
  "Returns whether or not PROGRAM is installed.  When WHINE is non-nil,
and a string, will `message' WHINE to the user.

The only programs q4 takes advantage of at the moment is feh and mpv, for
viewing images and vids/gifs respectively."
  ;; TODO: Figure out something for windows users.
  (let ((check (eql 0 (shell-command (format "which %s" program)))))
    (when (and (not check) whine)
      ;; I say "congigure an alt.+" here because I'll add hooks
      ;; for custom functions Soon™. Also this message should
      ;; be less shit...
      (message whine))
    check))


(defun q4/pass-to-feh ()
  "Passes the list of photos in this thread or catalog buffer to the
external program, feh. Doesn't download them though, if you want that, see
`q4/wget-threadpics'

In feh, SPC/DEL and n/p can be used to navigate forward and backward in the
list. Press d to show the number of photos in the list."
  (interactive)
  (let ((urls (q4/threadpics-string)))
    (if urls
        (start-process-shell-command
         "feh" nil (format "feh -FZ %s" urls))
      (message "Photo stack for this thread is empty."))))


(defun q4/open-post-image ()
  "Opens the current post's image in feh."
  (interactive)
  (save-excursion
    (q4/assert-post-start)
    (let ((image (q4/next-prop 'image nil (q4/sep-pos))))
      (if image (push-button image)
        (message "No image in this post.")))))


(defun q4/open-thread ()
  "When in the catalog, this will open the current post in a new
buffer. Will complain otherwise."
  (interactive)
  (save-excursion
    (q4/assert-post-start)
    (let ((button (q4/next-prop 'thread nil (q4/sep-pos))))
      (if button (push-button button)
        (message "Not a catalog entry.")))))


(defun q4/wget-threadpics (&optional name)
  "When called without args/interactively, will prompt for a folder name,
or use the string NAME if you supply it. If you supply an empty string by
just hitting return at the prompt, or passing in an empty string as the
argument, will use the current board and thread number.

The folder is created as a subdirectory of `q4/photo-download-directory'
and wget will populate it with the images of the current buffer (catalog or
thread)."
  (interactive)
  (let ((pics (q4/threadpics-string)))
    (if pics
        (let* ((input
                (or name (read-string
                          (format "(Folder Name)> %s"
                                  q4/photo-download-directory))))
               (path
                (expand-file-name
                 (if (string= input "")
                     (format "%s.%s" q4/board q4/threadno)
                   input)
                 q4/photo-download-directory)))
          (start-process-shell-command
           "q4-wget" nil (format "DEST=%s; mkdir $DEST; cd $DEST; wget %s"
                                 path pics)))
      (message "No photos in this buffer."))))


(defun q4/toggle-thumbnails ()
  "Uh-huh.

For real crowd-pleasing precision, you can also just setq `q4/thumbnails'
yourself :^)"
  (interactive)
  (message
   "Thumbs now %s."
   (setq q4/thumbnails (not q4/thumbnails))))


(when (bound-and-true-p evil-mode)
  (evil-define-key 'normal q4-mode-map
    "j" 'q4/point-to-next-post
    "k" 'q4/point-to-previous-post
    "H" 'evil-backward-char
    "J" 'evil-next-line
    "K" 'evil-previous-line
    "L" 'evil-forward-char
    "u" 'q4/list-urls
    "U" 'q4/view-content-externally
    "q" 'kill-this-buffer
    "Q" 'evil-record-macro
    "d" 'kill-this-buffer
    "]" 'q4/quote-hop-backward
    "[" 'q4/pop-mark
    "a" 'q4/pass-to-feh
    "A" 'q4/wget-threadpics
    "t" 'q4/toggle-thumbnails
    "i" 'q4/open-post-image
    "o" 'q4/open-thread
    "@" 'rename-buffer
    "r" 'q4/show-replies
    "R" 'q4/refresh-page
    "}" 'q4/expand-quotes
    "{" 'q4/unexpand-quotes))


(define-derived-mode q4-mode fundamental-mode "Q4"
  "Mode for browsing 4chan."
  :group 'q4-mode
  (local-set-key (kbd "SPC") 'q4/point-to-next-post)
  (local-set-key (kbd "RET") 'q4/point-to-next-post)
  (local-set-key (kbd "DEL") 'q4/point-to-previous-post)
  (local-set-key (kbd "n") 'q4/point-to-next-post)
  (local-set-key (kbd "p") 'q4/point-to-previous-post)
  (local-set-key (kbd "N") 'scroll-up-line)
  (local-set-key (kbd "P") 'scroll-down-line)
  (local-set-key (kbd "r") 'q4/show-replies)
  (local-set-key (kbd "l") 'q4/recenter)
  (local-set-key (kbd "q") 'kill-this-buffer)
  (local-set-key (kbd "]") 'q4/quote-hop-backward)
  (local-set-key (kbd "[") 'q4/pop-mark)
  (local-set-key (kbd "{") 'q4/unexpand-quotes)
  (local-set-key (kbd "}") 'q4/expand-quotes)
  (local-set-key (kbd "t") 'q4/toggle-thumbnails)
  (local-set-key (kbd "a") 'q4/pass-to-feh)
  (local-set-key (kbd "A") 'q4/wget-threadpics)
  (local-set-key (kbd "i") 'q4/open-post-image)
  (local-set-key (kbd "o") 'q4/open-thread)
  (local-set-key (kbd "u") 'q4/list-urls)
  (local-set-key (kbd "U") 'q4/view-content-externally)
  (local-set-key (kbd "g") 'q4/refresh-page)
  (local-set-key (kbd "<f5>") 'q4/refresh-page)
  (local-set-key (kbd "@") 'rename-buffer)
  (local-set-key (kbd "<tab>") 'forward-button)
  (local-set-key (kbd "<backtab>") 'backward-button))


(defun q4/query (dest callback board &optional buffer &rest cbargs)
  "Call to the mother ship and apply CALLBACK. DEST is a string
representing the resource you're craving. BOARD is also a string,
representing the sorry state of your....errr, the board you want to access.
BUFFER, which is optional or can be nil when passing addional callbacks,
is a the buffer passed to the callback. If you don't give one yourself,
this function will create a new buffer and name it according to DEST.
CBARGS are all passed to the callback in the order provided.

A call to this looks like:
    (q4/query (format \"thread/%s.json\" no) 'q4/thread board nil no)

The callback function recieves the following arguments in this order;
json - the rendered json response
buffer
board
CBARGS"
  (let ((url-request-extra-headers
         '(("Accept-Encoding" . "identity")
           ("Connection" . "close")))
        (endpoint (concat q4/base board "/" dest))
        (url-request-method "GET")
        (buffer (or buffer
                    (generate-new-buffer
                     (format "/%s/%s" board
                             (substring dest 0 -5))))))
    (url-retrieve
     endpoint
     `(lambda (status)
        (if (setq status (plist-get status :error))
            (case (caddr status)
              (404 (message "Thread has 404'd")
                   (kill-buffer (current-buffer))
                   (kill-buffer ,buffer)))
          (apply
           ',callback
           (q4/get-response-data nil t)
           ,buffer ,board ',cbargs))))))


(defmacro q4/@ (key)
  "A dumb macro to fetch KEY from the variable called alist, which is
assumed to already be bound outside of this macro. It keeps code (slightly)
tidier."
  `(alist-get ,key alist))


(defmacro q4/with-new-props (props &rest body)
  "Applies a list of properties to text created within BODY. Leaks the
variable named point."
  `(let ((point (point)))
     ,@body
     (add-text-properties
      point (point) ,props)))


(defmacro q4/with-new-face (face &rest body)
  "Applies FACE (symbol) to text created within BODY.  Leaks the variable
named point."
  `(let ((point (point)))
     ,@body
     (put-text-property
      point (point) 'face ,face)))


(defun q4/render-html-string (string &optional trim newlines-btfo)
  "Takes STRING of html, renders it with shr, strips properties, and
returns the string. TRIM, when non-nil, strips trail/lead-ing
whitespace. NEWLINES-BTFO gets rid of all newlines in the whole body of
text, when non nil"
  (let ((result (with-temp-buffer
                  (insert string)
                  (shr-render-region (point-min) (point-max))
                  (buffer-substring-no-properties (point-min) (point-max)))))
    (if trim (q4/fuck-whitespace result newlines-btfo) result)))


(defmacro q4/render-content ()
  "This does all the work. Implemented as a macro so it can use the same
namespace as other operations.

Not only does this function insert and propertize incoming content, it
also keeps the buffer's variables up to speed on what the buffer contains.
This is required for in-place content refreshing."
  '(progn
     (q4/with-new-face
      'q4/id-face
      (insert
       (propertize
        q4/header-indicator
        :q4type 'head
        :no no))
      (insert no)
      (when title (insert (concat " | " title))))
     (q4/handle-country)
     (q4/with-new-face
      'q4/gray-face
      (insert
       (format
        " %s%s@ %s\n"
        (if (and q4/show-namefags name)
            (format "by %s " name) "")
        (if (and q4/show-tripfags trip)
            (concat trip " ") "")
        when)))
     ;; TODO: use the unix timestamps and make a when-like string out of it
     ;; (local timezone support!)
     (when q4/establish-data (q4/append no q4/postnos))
     (if img
         (progn
           (when q4/establish-data (q4/append img q4/threadpics))
           (insert-button
            (concat file ext)
            :q4type 'image
            'face 'q4/gray-face
            'action `(lambda (b) (q4/load-image ,img)))
           (insert "\n")
           (q4/append thumb q4/thumblist)
           (if (and (display-graphic-p) q4/thumbnails)
               (insert
                (propertize
                 (format "Loading thumbnail... (%s)\n" thumb)
                 :q4type 'pending-thumb
                 :thumb thumb))
             (insert "\n")))
       (insert "\n"))
     (when comment (insert comment))
     (insert "\n\n")
     (when q4/establish-data
       (push
        (cons
         (string-to-int no)
         `((comment . ,comment)
           (apidata . ,alist)
           (title   . ,title)
           (thumb   . ,thumb)
           (time    . ,time)
           (trip    . ,trip)
           (link    . ,link)
           (name    . ,name)
           (file    . ,file)
           (image   . ,img)
           (id      . ,id)
           (replies . ,nil)))
        ;; It looks odd but ,nil is not a typo. Without doing that, all
        ;; the cdrs get the same pointer.
        q4/metadata))))


(defun q4/render-tag-span (dom)
  "A slightly modified version of `shr-tag-span' which colors greentext."
  (let ((class (dom-attr dom 'class)))
    (dolist (sub (dom-children dom))
      (if (stringp sub)
          (cond
           ((equal class "quote")
            (q4/with-new-face 'q4/greentext-face (shr-insert sub)))
           ((equal class "deadlink")
            (q4/with-new-face 'q4/dead-quote-face (insert q4/dead-quote-string)))
           (t (shr-insert sub)))
        (shr-descend sub)))))


(defun q4/render-tag-a (dom)
  "This is a modification to the function `shr-tag-a', which deals
with board/thread crosslinking and quotes."
  (let ((url (dom-attr dom 'href))
        (title (dom-attr dom 'title))
        (class (dom-attr dom 'class))
        (start (point))
        shr-start)
    ;; TODO: this section needs a lot of polish.
    (cond
     ((equal class "quotelink")
      (if (eq ?# (aref url 0))
          ;; check for the #p prefix for in-thread anchors
          (let* ((quoted-num (substring url 2))
                 (quoting (q4/get-post-property 'comment quoted-num q4/parent-buffer))
                 (current-post q4/current-no))
            (when q4/establish-data
              (with-current-buffer q4/parent-buffer
                ;; assign the parent post a reply attribute for this quote
                (unless (member current-post (q4/get-post-property 'replies quoted-num))
                  (setcdr (last (assq 'replies (assq (string-to-int quoted-num) q4/metadata)))
                        (cons current-post nil)))))
            (insert
             (propertize
              (concat
               ">>"
               (if (string= quoted-num (with-current-buffer q4/parent-buffer q4/threadno))
                   "OP" quoted-num))
              :no quoted-num
              :q4type 'quoted
              :quoting (substring-no-properties
                        (q4/fuck-whitespace
                         (or quoting "(no text in this post)")))
              'face 'q4/quote-face)))
        ;; out-of-thread references come in two forms:
        ;;    a board (/pol/, /trash/, etc)
        ;;    a board AND thread (/g/thread/58391828#p58391828)
        ;; They are not distinguished by class or id, as such,
        ;; we must fall back to s\\h\it\\ l\\ik\e t\\his\\.
        ;; FIXME: Needs support for searches and verification that the board exists!
        ;;   this is getting in the way of quality shitposts like >>>/reddit/
        (string-match
         ;; G1: board, mandatory        G2: threadno            G3: postno
         "^/\\([^/]+\\)\\(?:/thread/\\)*\\([0-9]+\\)*\\(?:#p\\)*\\([0-9]+\\)*"
         url)
        (let ((board  (match-string 1 url))
              (thread (match-string 2 url))
              (post   (match-string 3 url)))

          ;; MEGA HAX (+ 27 1990): Its easy to lose context in this mess, but right
          ;; now we are in a child rendering buffer, " *q4/rendering*" as
          ;; defined in q4/render-content. In the process of tranferring
          ;; data from this buffer to the REAL buffer, which is bound here
          ;; as q4/parent-buffer, we lose buttons. We can keep text properties
          ;; and faces, but not actionable buttons. So, here I add the desired
          ;; callback as the 'action property and actually create the button in
          ;; q4/post-processing.
          ;;
          ;; Addionally, the default format 4chan uses for threadlinking is
          ;; /thread/%THREAD#p%THREAD, which is rather ugly since they're
          ;; both the same number. Here I check if they are the same, and
          ;; make changes to the texts and actions as needed.

          (insert
           (propertize
            (concat
             ">>>"
             (if (and (stringp post)
                      (string= thread post))
                 (progn
                   (setq post nil)
                   (format "/%s/thread/%s/" board thread))
               url))
            'face 'q4/id-face
            :q4type 'crosslink
            'action
            `(lambda (button)
               (when (yes-or-no-p
                      ,(concat
                        "Open "
                        (cond
                         ((stringp post)
                          (format "post %s in /%s/thread/%s/" post board thread))
                         ((stringp thread) (format "/%s/thread/%s/" board thread))
                         (t (format "/%s/" board)))
                        " in a new buffer? "))
                 ,(cond
                   ((stringp post)
                    `(q4/query
                      (format "thread/%s.json" ,thread)
                      'q4/subthread ,board nil ,thread ,post))
                   ((stringp thread)
                    `(q4/query
                      (format "thread/%s.json" ,thread) 'q4/thread ,board nil ,thread))
                   (t `(q4/query "catalog.json" 'q4/catalog ,board))))))))))
     ;; comments and code below are ripped from shr-tag-a,
     ;; thanku based stallman
     (t (shr-generic dom)
        (when (and shr-target-id
                   (equal (dom-attr dom 'name) shr-target-id))
          ;; We have a zero-length <a name="foo"> element, so just
          ;; insert...  something.
          (when (= start (point))
            (shr-ensure-newline)
            (insert " "))
          (put-text-property start (1+ start) 'shr-target-id shr-target-id))
        (when url
          (shr-urlify (or shr-start start) (shr-expand-url url) title))))))


(defmacro q4/map-boards (&rest body)
  "Evaluates BODY while iterating over the rendered json response of
boards.json. The data is bound to the variable named alist, which can be
accessed with `q4/@' This does no collection of data on its own; thats left
to the caller."
  `(let ((response (alist-get
                    'boards
                    (q4/get-response-data
                     (url-retrieve-synchronously
                      "http://a.4cdn.org/boards.json" t) t))))
     (cl-loop for alist across response do ,@body)))


(defun q4/seperator ()
  "Returns the seperator string, dynamicly created according to
`q4/wrapwidth' and `q4/seperator-char'"
  (make-string q4/wrapwidth (aref q4/seperator-char 0)))


(defun q4/insert-seperator ()
  "Inserts `q4/seperator' with `q4/gray-face', and an ending property."
  (q4/with-new-face
   'q4/gray-face
   (insert (propertize (format "\n%s\n" (q4/seperator))
                       :q4type 'end))))


(defun q4/get-icon (name &optional type)
 "Returns an image object for a 4chan icon resource. NAME should be a
string indicating the name of the resource on the server. To get the United
States flag, call this as (q4/get-icon \"US\" 'flag). The name is downcased
and the \".gif\" extention is appended to the request.

The icons are downloaded to `q4/icon-path' if they haven't already been.
The first time an item is accesed, it gets stored into the hash table
`q4/icon-cache' for the remainder of your emacs session. Unless you delete
or change the icon path, each icon should only ever be downloaded one
time.

If the request fails or the data can not be decoded for whatever reason,
this function will return nil. A+ error handling."
  (let* ((icon (concat (downcase name) ".gif"))
         (path (expand-file-name icon q4/icon-path)))
    (unless (file-exists-p q4/icon-path) (make-directory q4/icon-path))
    (or (gethash icon q4/icon-cache) ;; https://www.youtube.com/watch?v=hU7EHKFNMQg
        (if (file-exists-p path)
            ;; https://www.youtube.com/watch?v=hU7EHKFNMQg
            (puthash icon (create-image path) q4/icon-cache) ;; https://www.youtube.com/watch?v=hU7EHKFNMQg
          ;; https://www.youtube.com/watch?v=hU7EHKFNMQg
          (message "Indexing new icon: %s (%s)" name path)
          (let ((data (ignore-errors
                        (q4/get-response-data
                         (url-retrieve-synchronously
                          (concat
                           q4/icon-base
                           (if (eql type 'flag) "country/" "")
                           icon) t)))))
            ;; https://www.youtube.com/watch?v=hU7EHKFNMQg
            (let ((rendered (ignore-errors (create-image data nil t))))
              (if (not rendered)
                  (puthash icon nil q4/icon-cache)
                (with-temp-file path (insert data)) ;; https://www.youtube.com/watch?v=hU7EHKFNMQg
                (puthash icon rendered q4/icon-cache))))))))
              ;; https://www.youtube.com/watch?v=hU7EHKFNMQg


(defun q4/insert-icon (icon &optional type)
  "Inserts ICON (see `q4/get-icon') at point, with a trailing space.
If the resource fails to be fetched for any reason, inserts ICON
surrounded in brackets with a trailing space."
  ;; This needs real error handling...
  (let ((image (q4/get-icon icon type)))
    (if (not image)
        (insert (format "[%s] " icon))
      (insert-image image "%")
      (insert " "))))


(defmacro q4/handle-country ()
  "Handles inserting the representation of a post's country as according to
`q4/show-countries' and `q4/country-type'"
  '(when (and (display-graphic-p) q4/show-countries country)
     (q4/with-new-face
      'q4/country-name-face
      (case q4/country-type
        ('abbrev
         (insert (format "%s " country)))
        ('name
         (insert (format "%s " (q4/@ 'country_name))))
        ('flag (q4/insert-icon country 'flag))
        ('flag/name
         (q4/insert-icon country 'flag)
         (insert (format "%s " (q4/@ 'country_name))))
        ('flag/abbrev
         (q4/insert-icon country 'flag)
         (insert (format "%s " country)))))))


(defmacro q4/with-4chan-binds (&rest body)
  "Sets all the variables needed by the rendering and organization of
4chan posts. Also handles some stuff for shr's html rendering."
  `(let* (;; first bind some stuff for shr,
          ;; which renders the html contents
          ;; of all the comments. Binding it
          ;; in let allows these settings to
          ;; never persist outside of q4's
          ;; activities...
          (url-request-extra-headers
           '(("Accept-Encoding" . "identity")
             ("Connection" . "close")))
          (shr-external-rendering-functions
           '((span . q4/render-tag-span)
             (a . q4/render-tag-a)))
          (shr-width q4/wrapwidth)
          (shr-use-fonts nil)
          ;; ...and now the json api properties
          (bumpdeath (equal (q4/@ 'bumplimit) 1))
          (imgdeath (equal (q4/@ 'imagelimit) 1))
          (archived (equal (q4/@ 'archived) 1))
          (no (int-to-string (q4/@ 'no)))
          (replies (q4/@ 'replies))
          (country (q4/@ 'country))
          (images (q4/@ 'images))
          (cap (q4/@ 'capcode))
          (trip (q4/@ 'trip))
          (time (q4/@ 'time))
          (when (q4/@ 'now))
          (tag (q4/@ 'tag))
          (ext (q4/@ 'ext))
          (id (q4/@ 'id))

          (link
           (if (equal q4/threadno "catalog")
               (format "http://boards.4chan.org/%s/thread/%s"
                       q4/board no)
             (format "http://boards.4chan.org/%s/thread/%s#p%s"
                     q4/board q4/threadno no)))

          (file
           (let ((file (q4/@ 'filename)))
             (if file (q4/render-html-string file t t))))

          (img
           (if file
               (concat
                "http://i.4cdn.org/"
                board "/" (int-to-string (q4/@ 'tim)) ext)))

          (thumb
           (if file
               (concat
                "http://i.4cdn.org/"
                board "/" (int-to-string (q4/@ 'tim)) "s.jpg")))

          (comment
           (let ((comment (q4/@ 'com))
                 (parent-buffer (current-buffer)))
             (if comment
                 (with-current-buffer (get-buffer-create " *q4 rendering*")
                   (erase-buffer)
                   (setq-local q4/parent-buffer parent-buffer)
                   (setq-local q4/current-no no)
                   (insert comment)
                   (goto-char (point-min))
                   ;; libxml chokes on these, and shr handles
                   ;; sane word wrapping just fine without them.
                   (while (search-forward "<wbr>" nil t)
                     (replace-match ""))
                   (shr-render-region (point-min) (point-max))
                   (buffer-substring (point-min) (point-max))))))

          (name
           (let ((name (q4/@ 'name)))
             (if (and name (not (member name '("" "Anonymous"))))
                 (q4/render-html-string name t t))))

          (title
           (let ((title (q4/@ 'sub)))
             (if title (q4/render-html-string title t t)))))
     ,@body))


(defun q4/get-response-data (&optional buffer json)
  "Returns the useful stuff from a buffer returned by `url-retrieve', and
deletes the buffer. BUFFER should be a candlelight vigi...err, a buffer
object, otherwise it uses `current-buffer'.

JSON, when non nil, reads the response into `json-read' and returns that
object instead."
  ;; Would it really be so bad to return the headers
  ;; in a better way than just throwing them on top
  ;; in plaintext? Maybe I'm missing something...
  (let (data (buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      ;; say those lines 10 times fast :^)
      (goto-char (point-min))
      (when (re-search-forward "\r?\n\r?\n" nil t)
        (setq data
         (if (not json)
             (buffer-substring-no-properties (point) (point-max))
           (condition-case nil
               (json-read)
             ('json-readtable-error
              (let ((p (point)))
                (if (not (zlib-available-p))
                    (call-process-region (point) (point-max) "gzip" t t nil "-d")
                  (set-buffer-multibyte nil)
                  (zlib-decompress-region (point) (point-max)))
                (goto-char p)
                (json-read)))))))
      (when q4/discard-request-buffers
        (kill-buffer buffer))
      data)))


(defun q4/async-thumbnail-dispatch (buffer thumbs)
  "Semi-asynchonously starts the retrieval of a buffer's thumbnails after
rendering. Loads 10, waits a bit, and re-runs itself in a few seconds.
Rinse, repeat until list is done. User interaction begins very quickly
after this function is first called."
  ;; this looks like a staircase lmao
  (let ((url-request-extra-headers
         '(("Connection" . "close")))
        (count 0) addr)
    (while (and (buffer-live-p buffer)
                thumbs (< count 10))
      (setq addr (pop thumbs)
            count (1+ count))
      (url-retrieve
       addr
       `(lambda (status)
          (goto-char (point-min))
          (let ((data (q4/get-response-data)))
            (when (buffer-live-p ,buffer)
              (with-current-buffer ,buffer
                (save-excursion
                  (goto-char (point-min))
                  (while (search-forward ,(format "Loading thumbnail... (%s)" addr) nil t)
                    (when (equal ,addr (get-char-property (match-beginning 0) :thumb))
                      (delete-region (progn (back-to-indentation) (point)) (point-at-eol))
                      (if data (progn (insert-image (create-image data nil t))
                                      (insert "\n"))
                        (delete-backward-char 1)))))))))
       nil t))
    (if (and thumbs (buffer-live-p buffer))
        (run-at-time
         2 nil 'q4/async-thumbnail-dispatch
         buffer thumbs)
      (setq q4/thumblist nil))))


;; adding site property for when this branches past 4chan only
(defun q4/list-all-boards (&optional cars site)
  "Returns an ordered list of all boards available at SITE. Defaults to
4chan. This has a side effect of blocking execution for a brief moment to
initialize the index if it has not already been set for this session.

When CARS is non-nil, returns only the board names with no additional
details."
  ;; this is temporary and is also flaming trash
  (let ((boards
         (case (or site '4chan)
           ('4chan
            (or q4/all-4chan-boards
                (with-temp-message "Establishing board index..."
                  (q4/map-boards
                   (q4/append
                    (list
                     (q4/@ 'board)
                     (q4/@ 'title)
                     (q4/render-html-string
                      (q4/@ 'meta_description) t t))
                    q4/all-4chan-boards)))
                q4/all-4chan-boards)))))
    (if cars
        (mapcar (lambda (b) (car b)) boards)
      boards)))


(defun q4/browse-board (&optional board)
  "An interactive function which prompts for a board to start
browsing. This is the entry point for q4."
  (interactive)
  (let ((board (or board
                   (q4/complete-collection
                    "(Board)> "
                    (q4/list-all-boards t)))))
    (if (and board (not (string= board "")))
        (q4/query "catalog.json" 'q4/catalog board)
      (message "Nevermind then!"))))


;; (defun q4/list-boards ()
;;   "Pretty-print all boards to a dedicated buffer, with buttons to open them."
;;   (interactive)
;;   )


(defun q4/refresh-page ()
  "An interactive function which refreshes content on the page. This
behaves differently in threads and catalogs.

In threads, point position is preserved and the new posts are appended to
the bottom of the buffer. This is seamless.

In catalogs, the whole buffer is scrapped and point is returned to the
top."
  (interactive)
  (q4/query
   (if (equal q4/threadno "catalog") "catalog.json"
     (format "thread/%s.json" q4/threadno))
   'q4/refresh-callback q4/board (current-buffer) q4/threadno))


(defun q4/refresh-callback (json buffer board thread)
  "See `q4/refresh-page': this is just the callback function it uses for
the URL request."
  (with-current-buffer buffer
    (if (equal thread "catalog")
        (progn
          (erase-buffer)
          (q4/catalog json buffer board))
      (save-excursion
        (message "Parsing new content...")
        (goto-char (point-max))
        (cl-loop for alist across (alist-get 'posts json) do
          (q4/with-4chan-binds
           (unless (member no q4/postnos)
             (q4/render-content)
             (q4/insert-seperator)))))
      (q4/postprocess)
      (when (and q4/thumbnails (display-graphic-p))
        (q4/async-thumbnail-dispatch
         buffer q4/thumblist))
      (message " "))))


(defun q4/expand-quotes ()
  "Insert the parent comments of all quotes in the current post. This is
not recursive; if you want to view the contents of a quote in the parent
text, use `q4/quote-hop-backward' to navigate to it normally.

Inserts with `q4/gray-face' and can be reversed with `q4/unexpand-quotes'"
  (interactive)
  (save-excursion
    (q4/assert-post-start)
    (unless (q4/next-prop 'expanded nil (q4/sep-pos))
      (let ((next (q4/next-prop 'quoted nil (q4/sep-pos)))
             no text)
        (while next
          (setq text (get-char-property next :quoting)
                no (get-char-property next :no))
          (goto-char next)
          (unless (q4/boip)
            (insert (propertize "\n" :q4type 'expanded)))
          (goto-char (next-property-change (point)))
          (insert
           (propertize
            (concat "\n" text "\n")
            :q4type 'expanded
            'face 'q4/gray-face))
          (setq next (q4/next-prop 'quoted nil (q4/sep-pos)))))))
  (q4/recenter))


(defun q4/unexpand-quotes ()
  "Collapses quotes created by `q4/expand-quotes'"
  (interactive)
  (save-excursion
    (q4/assert-post-start)
    (while (q4/inboundp (point) (q4/sep-pos))
      (if (eq 'expanded (get-char-property (point) :q4type))
          (delete-region (point) (next-property-change (point)))
        (goto-char (next-property-change (point)))))))


(defun q4/btfo ()
  ;; DOCME
  ;; Fuck it, how about WRITEME
  (interactive)
  (cond
   ((bound-and-true-p q4/replyview-p)
    (let* ((ring (with-current-buffer q4/parent-buffer
                   (pop q4/reply-ring))))
      (if (not ring)
          (quit-window)
        (q4/show-replies (car ring))
        (goto-char (cdr ring)))))))


(defun q4/show-replies (&optional post)
  ;; DOCME
  ;; this is in a very early state, may be causing some wicked side effects,
  ;; and is a crude hack. Polish comes later.
  (interactive)
  (let* ((buffer (get-buffer-create "*Q4 Replies*"))
         (parent-buffer (current-buffer))
         (post (or post (q4/current-post)))
         (replies (q4/get-post-property 'replies post))
         (descending-p (eq (current-buffer) buffer))
         (parent-thread q4/threadno)
         (parent-data q4/metadata)
         (parent-link q4/extlink)
         (q4/establish-data nil)
         (board q4/board))
    (if (not replies)
        (message "No replies to this post.")
      (when descending-p
        (push (cons post (point)) q4/reply-ring))
      (with-current-buffer buffer
        (erase-buffer)
        (q4-mode)
        (setq-local q4/metadata parent-data)
        (setq-local q4/parent-buffer parent-buffer)
        (setq-local q4/replyview-p t)
        (setq q4/extlink parent-link
              q4/threadno parent-thread
              q4/board board)
        (cl-loop for reply in replies do
          (let ((alist (q4/get-post-property 'apidata reply)))
            (q4/with-4chan-binds
             (q4/render-content)
             (q4/insert-seperator))))
        (goto-char (point-min))
        (q4/postprocess)
        (when q4/thumbnails
          (q4/async-thumbnail-dispatch
           buffer q4/thumblist)))
      (funcall (if descending-p 'switch-to-buffer 'pop-to-buffer)
               buffer))))


(defun q4/load-image (addr)
  "The callback attached to image buttons, which opens the image in feh or
mpv depending on the file type."
  (let ((gif-p (member (substring addr -4) (list ".gif" "webm" ".mp4"))))
    (message "Loading %s..." (if gif-p "video" "image"))
    (if gif-p
        (start-process-shell-command
         "mpv" nil (format "wget -O /tmp/4gif %s;
                          emacsclient -e '(message \" \")';
                          mpv --loop=inf /tmp/4gif" addr))
      (start-process-shell-command
       "feh" nil (format "wget -O - %s | feh -FZ -" addr)))))


(defun q4/postprocess ()
  "Scans forward from current point position and applies cosmetic changes
to the buffer after the initial rendering phase. It's necessary to jump to
`point-min' prior to the first pass so this will also play nice with page
refreshing.

Currently implemented functions are assigning callbacks to board
crosslinks, to make sure no newline padding exceeds two lines, and to
optionally center the buffer when `q4/centered' is non-nil."
  ;; crosslink and quote buttons must be added in postprocessing because
  ;; buttons can not persist across propertized substrings, or some
  ;; bullshit like that. TLDR this shit don't work in `q4/render-content'
  (save-excursion
    (while (re-search-forward ">>>[^ \n\r\t]+" nil t)
      (when (eql 'crosslink (get-char-property (match-beginning 0) :q4type))
        (make-button
         (match-beginning 0) (match-end 0)
         'action (get-char-property (match-beginning 0) 'action)))))
  (save-excursion
    (while (re-search-forward ">>\\([0-9]+\\|OP\\)" nil t)
      (when (eql 'quoted (get-char-property (match-beginning 0) :q4type))
        (make-button
         (match-beginning 0) (match-end 0)
         'face 'q4/quote-face
         'action `(lambda (b)
                    (q4/seek-post
                     ,(get-char-property (match-beginning 0) :no) t))))))
  ;; center the buffer, if enabled
  (save-excursion
    (when q4/centered
      (insert q4/spacer)
      (while (search-forward "\n" nil t)
        (replace-match (concat "\n" q4/spacer)))))
  ;; make whitespace consistent across all posts
  (save-excursion
    (while (re-search-forward "\n\n\n+" nil t)
      (replace-match "\n\n"))))


(defun q4/catalog (json buffer board)
  "Renders the catalog. Must be used as a callback for q4/query."
  ;; AFTER ALL THESE YEARS, THE ACTUAL RENDERING FUNCTION
  (message "Loading /%s/..." board)
  (with-current-buffer buffer
    (q4-mode)
    (setq q4/extlink (format "http://boards.4chan.org/%s/catalog" board)
          q4/threadno "catalog"
          q4/board board)
    ;; given that common lisp looping may be a product of a sentient
    ;; lifeform within the language, these loops could probably be merged
    ;; into one cl-loop clause. However, I can't be arsed. Just Werks™.
    (dotimes (page (1- q4/catalog-pages))
      (cl-loop for alist across (alist-get 'threads (aref json page)) do
        (q4/with-4chan-binds
         (q4/render-content)
         (insert-button
          (format "[r: %d | i: %d]\n" replies images)
          'face 'q4/gray-face
          :q4type 'thread
          'action `(lambda (b)
                     (q4/query ,(format "thread/%s.json" no)
                               'q4/thread
                               ,board nil ,no)))
         (q4/insert-seperator))))
    (goto-char (point-min))
    (q4/postprocess))
  (switch-to-buffer buffer)
  (goto-char (point-min))
  (message " ")
  (when q4/thumbnails
    (q4/async-thumbnail-dispatch
     buffer q4/thumblist)))


(defun q4/thread (json buffer board thread)
  "Renders threads, must be used as a callback for q4/query which has a
thread number."
  (message "Loading /%s/%s..." board thread)
  (with-current-buffer buffer
    (q4-mode)
    (setq q4/extlink
          (format "http://boards.4chan.org/%s/thread/%s"
                  board thread)
          q4/threadno thread
          q4/board board)
    (cl-loop for alist across (alist-get 'posts json) do
      (q4/with-4chan-binds
       (q4/render-content)
       (q4/insert-seperator)))
    (goto-char (point-min))
    ;; (q4/bind-quotes)
    (goto-char (point-min))
    (q4/postprocess))
  (switch-to-buffer buffer)
  (goto-char (point-min))
  (message " ")
  (when q4/thumbnails
    (q4/async-thumbnail-dispatch
     buffer q4/thumblist)))


(defun q4/subthread (json buffer board thread post)
  (q4/thread json buffer board thread)
  (q4/seek-post post nil t))

;; HI IM DAISY
