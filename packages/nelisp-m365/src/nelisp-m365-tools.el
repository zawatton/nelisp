;;; nelisp-m365-tools.el --- MCP tool surface for nelisp-m365  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The read-only tool surface exposed over MCP: sign-in, Outlook mail,
;; calendar, OneDrive (including the Excel workbook API), contacts,
;; Microsoft To Do and OneNote.
;;
;; Every tool that returns account content is marked `:untrusted', which
;; makes the MCP layer wrap the payload in a warning.  Mail bodies, file
;; contents and OneNote pages are attacker-reachable text; a model
;; reading them must not treat them as instructions.
;;
;; Responses are trimmed with `nelisp-m365-graph-pick' rather than
;; passed through whole.  A Graph message row carries about forty fields
;; and several kilobytes of routing metadata that no caller needs.

;;; Code:

(require 'nelisp-m365-compat)
(require 'nelisp-m365-auth)
(require 'nelisp-m365-graph)

(defvar nelisp-m365-tools-max-text-bytes 262144
  "Largest text payload returned by a content-fetching tool.
Longer content is truncated and flagged, so a single oversized document
cannot swamp the caller's context.")

;;; HTML to text --------------------------------------------------------

(defconst nelisp-m365-tools--block-tags
  '("br" "/p" "/div" "/tr" "/li" "/h1" "/h2" "/h3" "/h4" "/h5" "/h6"
    "/blockquote" "/table" "p" "div" "tr" "li")
  "Tags whose boundary becomes a newline in the plain-text rendering.")

(defun nelisp-m365-tools--tag-name (tag)
  "Return the normalised name of raw TAG text, without attributes."
  (let* ((trimmed (string-trim tag))
         (space (nelisp-m365-compat-find " " trimmed))
         (name (downcase (if space (substring trimmed 0 space) trimmed))))
    ;; A self-closing tag arrives as "br/"; drop the slash so it matches.
    (if (string-suffix-p "/" name) (substring name 0 -1) name)))

(defun nelisp-m365-tools--skip-container (html start name)
  "Return the index just past the closing tag for NAME, searching from START.
Falls back to the end of HTML when the container is never closed.

Walks tag to tag rather than searching a downcased copy of the whole
document: `downcase' allocates about 160 MB for a 7 KB input on this
runtime, and the copy existed only to make this one search
case-insensitive."
  (let ((pos start)
        (len (length html))
        (closing (concat "/" name))
        (result nil))
    (while (not result)
      (let ((lt (nelisp-m365-compat-find "<" html pos)))
        (if (not lt)
            (setq result len)
          (let ((gt (nelisp-m365-compat-find ">" html lt)))
            (cond
             ((not gt) (setq result len))
             ((equal (nelisp-m365-tools--tag-name (substring html (1+ lt) gt))
                     closing)
              (setq result (1+ gt)))
             (t (setq pos (1+ gt))))))))
    result))

(defconst nelisp-m365-tools--entities
  '(("&nbsp;" . " ") ("&#160;" . " ")
    ("&lt;" . "<") ("&gt;" . ">")
    ("&quot;" . "\"") ("&#34;" . "\"")
    ("&#39;" . "'") ("&#x27;" . "'") ("&apos;" . "'")
    ("&hellip;" . "...") ("&mdash;" . "--") ("&ndash;" . "-")
    ("&amp;" . "&"))
  "HTML entities decoded in Outlook and OneNote output.")

(defconst nelisp-m365-tools--entity-max-length 8
  "Longest entity in `nelisp-m365-tools--entities', plus a little slack.
Bounds how far past an ampersand the decoder looks for a semicolon, so a
bare `&' in prose does not trigger a scan to the end of the document.")

(defun nelisp-m365-tools--decode-entities (text)
  "Decode HTML entities in TEXT.

One left-to-right pass, and only when an ampersand is present at all.
The obvious implementation -- one `string-replace' per entity -- costs
about 1.3 GB for a 2.6 KB input on this runtime, which is what drove the
server into the OOM killer.  A single pass also removes the ordering
hazard that made `&amp;' have to be decoded last: `&amp;lt;' cannot turn
into a live `<' because the text produced by a replacement is never
re-examined."
  (if (not (nelisp-m365-compat-find "&" text))
      text
    (let ((out nil)
          (pos 0)
          (len (length text)))
      (while (< pos len)
        (let ((amp (nelisp-m365-compat-find "&" text pos)))
          (if (not amp)
              (progn (push (substring text pos) out)
                     (setq pos len))
            (when (> amp pos)
              (push (substring text pos amp) out))
            (let* ((semi (nelisp-m365-compat-find ";" text amp))
                   (end (and semi
                             (<= (- semi amp) nelisp-m365-tools--entity-max-length)
                             semi)))
              (if (not end)
                  (progn (push "&" out)
                         (setq pos (1+ amp)))
                (let* ((entity (substring text amp (1+ end)))
                       (replacement (cdr (assoc entity
                                                nelisp-m365-tools--entities))))
                  (push (or replacement entity) out)
                  (setq pos (1+ end))))))))
      (apply #'concat (nreverse out)))))

(defun nelisp-m365-tools--collapse-blank-lines (text)
  "Collapse runs of blank lines in TEXT down to a single blank line.
A single split and rejoin; the previous `string-replace' convergence
loop rebuilt the whole string on every iteration."
  (let ((keep nil)
        (blank 0))
    (dolist (line (nelisp-m365-compat-split-all text "\n"))
      (if (equal (string-trim line) "")
          (setq blank (1+ blank))
        (when (> blank 0) (push "" keep))
        (setq blank 0)
        (push line keep)))
    (let ((pieces nil)
          (first t))
      (dolist (line (nreverse keep))
        (unless first (push "\n" pieces))
        (setq first nil)
        (push line pieces))
      (apply #'concat (nreverse pieces)))))

(defun nelisp-m365-tools-html-to-text (html)
  "Return HTML rendered as plain text.

Walks tag to tag rather than scanning characters, so cost tracks the
number of tags rather than the document length -- an interpreted
per-character loop is too slow for a 200 KB mail body on this runtime.
Script and style contents are dropped entirely.

Allocation is the binding constraint here, not speed: the first version
of this function allocated 2 GB for a 7 KB input and was what killed the
server mid-session.  Nothing in it may build a whole-document copy, and
every search goes through `nelisp-m365-compat-find' rather than
`string-search' with a START argument."
  (if (or (null html) (equal html ""))
      ""
    (let* ((len (length html))
           (pos 0)
           (out nil))
      (while (< pos len)
        (let ((lt (nelisp-m365-compat-find "<" html pos)))
          (if (not lt)
              (progn (push (substring html pos) out)
                     (setq pos len))
            (when (> lt pos) (push (substring html pos lt) out))
            (let ((gt (nelisp-m365-compat-find ">" html lt)))
              (if (not gt)
                  (setq pos len)
                (let ((name (nelisp-m365-tools--tag-name
                             (substring html (1+ lt) gt))))
                  (cond
                   ((or (equal name "script") (equal name "style"))
                    ;; Emit a break where the block was: dropping it
                    ;; silently would weld the words on either side
                    ;; together into one misleading token.
                    (push "\n" out)
                    (setq pos (nelisp-m365-tools--skip-container
                               html gt name)))
                   (t
                    (when (member name nelisp-m365-tools--block-tags)
                      (push "\n" out))
                    (setq pos (1+ gt))))))))))
      (string-trim
       (nelisp-m365-tools--collapse-blank-lines
        (nelisp-m365-tools--decode-entities
         (apply #'concat (nreverse out))))))))

;;; Argument helpers ----------------------------------------------------

(defun nelisp-m365-tools--arg (args name &optional default)
  "Return argument NAME from the ARGS alist, or DEFAULT when absent."
  (let ((cell (assoc name args)))
    (if (and cell (cdr cell) (not (equal (cdr cell) ""))) (cdr cell) default)))

(defun nelisp-m365-tools--int-arg (args name default min max)
  "Return integer argument NAME from ARGS, clamped to MIN..MAX."
  (let* ((raw (nelisp-m365-tools--arg args name default))
         (n (if (stringp raw) (string-to-number raw) raw)))
    (unless (numberp n) (setq n default))
    (max min (min max (truncate n)))))

(defun nelisp-m365-tools--require-arg (args name)
  "Return argument NAME from ARGS, signalling when it is missing."
  (or (nelisp-m365-tools--arg args name)
      (error "nelisp-m365: required argument %s is missing" name)))

(defun nelisp-m365-tools--truncate (text)
  "Return (TEXT . TRUNCATED-P) with TEXT clipped to the size budget."
  (if (> (length text) nelisp-m365-tools-max-text-bytes)
      (cons (substring text 0 nelisp-m365-tools-max-text-bytes) t)
    (cons text nil)))

(defun nelisp-m365-tools--collection-result (key collected)
  "Shape a `nelisp-m365-graph-collection' result for JSON encoding.
KEY names the item array; COLLECTED is the plist the graph layer built."
  (list (cons key (nelisp-m365-compat-json-array (plist-get collected :items)))
        (cons "count" (length (plist-get collected :items)))
        (cons "truncated"
              (nelisp-m365-compat-json-bool (plist-get collected :truncated)))))

;;; Sign-in and account -------------------------------------------------

(defun nelisp-m365-tools-authenticate (args)
  "Start the device-code sign-in and return the code and URL."
  (nelisp-m365-auth-begin
   (not (equal (nelisp-m365-tools--arg args "openBrowser" t) :json-false))))

(defun nelisp-m365-tools-authenticate-finish (_args)
  "Finish a sign-in started by `nelisp-m365-tools-authenticate'."
  (nelisp-m365-auth-poll))

(defun nelisp-m365-tools-status (_args)
  "Return the local connection status without a network call."
  (let ((status (nelisp-m365-auth-status)))
    (cons (cons "requested_scopes"
                (nelisp-m365-compat-json-array
                 (cdr (assoc "requested_scopes" status))))
          (nelisp-m365-graph-pick
           status '("connected" "tenant" "client_id_configured"
                    "granted_scope" "access_token_expires_at" "token_cache")))))

(defun nelisp-m365-tools-profile (_args)
  "Return the signed-in account's basic profile."
  (nelisp-m365-graph-get
   (concat "/me"
           (nelisp-m365-graph-query
            '(("$select" . "id,displayName,givenName,surname,userPrincipalName,mail"))))))

;;; Mail -----------------------------------------------------------------

(defconst nelisp-m365-tools--message-fields
  (concat "id,conversationId,subject,from,receivedDateTime,bodyPreview,"
          "isRead,hasAttachments,webLink")
  "Fields returned for a message in list results.
`conversationId' is included so a search result can be followed straight
into `m365_get_mail_thread'.")

(defun nelisp-m365-tools-search-mail (args)
  "Search Outlook mail and return matching message summaries."
  (let* ((query (nelisp-m365-tools--require-arg args "query"))
         (limit (nelisp-m365-tools--int-arg args "maxResults" 10 1 50))
         (folder (nelisp-m365-tools--arg args "folder"))
         (base (if folder
                   (concat "/me/mailFolders/"
                           (nelisp-m365-compat-url-encode folder) "/messages")
                 "/me/messages"))
         ;; $search cannot be combined with $orderby on messages, so the
         ;; server's relevance order is what comes back.
         (path (concat base
                       (nelisp-m365-graph-query
                        (list (cons "$search"
                                    (concat "\""
                                            (string-replace "\"" " " query)
                                            "\""))
                              (cons "$select" nelisp-m365-tools--message-fields)
                              (cons "$top" limit)))))
         (collected (nelisp-m365-graph-collection path limit)))
    (nelisp-m365-tools--collection-result "messages" collected)))

(defun nelisp-m365-tools-get-mail (args)
  "Return one message with its body rendered as plain text."
  (let* ((id (nelisp-m365-tools--require-arg args "messageId"))
         (path (concat "/me/messages/" (nelisp-m365-compat-url-encode id)
                       (nelisp-m365-graph-query
                        '(("$select" . "id,subject,from,toRecipients,ccRecipients,receivedDateTime,sentDateTime,isRead,hasAttachments,body,webLink")))))
         (msg (nelisp-m365-graph-get path))
         (body (cdr (assoc "body" msg)))
         (content (or (cdr (assoc "content" body)) ""))
         (kind (downcase (or (cdr (assoc "contentType" body)) "text")))
         (text (if (equal kind "html")
                   (nelisp-m365-tools-html-to-text content)
                 content))
         (clipped (nelisp-m365-tools--truncate text)))
    (append
     (nelisp-m365-graph-pick
      msg '("id" "subject" "receivedDateTime" "sentDateTime"
            "isRead" "hasAttachments" "webLink"))
     (list (cons "from" (nelisp-m365-graph-address (cdr (assoc "from" msg))))
           (cons "to" (nelisp-m365-compat-json-array
                       (nelisp-m365-graph-addresses
                        (cdr (assoc "toRecipients" msg)))))
           (cons "cc" (nelisp-m365-compat-json-array
                       (nelisp-m365-graph-addresses
                        (cdr (assoc "ccRecipients" msg)))))
           (cons "body" (car clipped))
           (cons "body_truncated"
                 (nelisp-m365-compat-json-bool (cdr clipped)))))))

(defun nelisp-m365-tools-list-mail-folders (args)
  "List Outlook mail folders with their unread and total counts."
  (let* ((limit (nelisp-m365-tools--int-arg args "maxResults" 30 1 100))
         (path (concat "/me/mailFolders"
                       (nelisp-m365-graph-query
                        (list (cons "$select" "id,displayName,unreadItemCount,totalItemCount")
                              (cons "$top" limit)))))
         (collected (nelisp-m365-graph-collection path limit)))
    (nelisp-m365-tools--collection-result "folders" collected)))

;;; Calendar --------------------------------------------------------------

(defun nelisp-m365-tools-list-calendar (args)
  "Return calendar events in an ISO 8601 window."
  (let* ((start (nelisp-m365-tools--require-arg args "start"))
         (end (nelisp-m365-tools--require-arg args "end"))
         (limit (nelisp-m365-tools--int-arg args "maxResults" 25 1 100))
         (zone (nelisp-m365-tools--arg args "timeZone" "Tokyo Standard Time"))
         (path (concat "/me/calendarView"
                       (nelisp-m365-graph-query
                        (list (cons "startDateTime" start)
                              (cons "endDateTime" end)
                              (cons "$select" "id,subject,start,end,location,organizer,isAllDay,isCancelled,showAs,webLink,bodyPreview")
                              (cons "$orderby" "start/dateTime")
                              (cons "$top" limit)))))
         (collected (nelisp-m365-graph-collection
                     path limit
                     :headers (list (cons "Prefer"
                                          (concat "outlook.timezone=\""
                                                  (string-replace "\"" "" zone)
                                                  "\""))))))
    (nelisp-m365-tools--collection-result "events" collected)))

;;; OneDrive ---------------------------------------------------------------

(defconst nelisp-m365-tools--item-fields
  "id,name,size,lastModifiedDateTime,webUrl,file,folder,parentReference"
  "Fields returned for a driveItem.")

(defun nelisp-m365-tools-search-onedrive (args)
  "Search OneDrive by file name and content."
  (let* ((query (nelisp-m365-tools--require-arg args "query"))
         (limit (nelisp-m365-tools--int-arg args "maxResults" 20 1 50))
         (path (concat "/me/drive/root/search(q='"
                       (nelisp-m365-compat-url-encode
                        (nelisp-m365-graph-escape-odata query))
                       "')"
                       (nelisp-m365-graph-query
                        (list (cons "$select" nelisp-m365-tools--item-fields)
                              (cons "$top" limit)))))
         (collected (nelisp-m365-graph-collection path limit)))
    (nelisp-m365-tools--collection-result "items" collected)))

(defun nelisp-m365-tools-list-onedrive (args)
  "List the children of a OneDrive folder, defaulting to the drive root."
  (let* ((limit (nelisp-m365-tools--int-arg args "maxResults" 50 1 200))
         (item-id (nelisp-m365-tools--arg args "itemId"))
         (folder-path (nelisp-m365-tools--arg args "path"))
         (base (cond
                (item-id (concat "/me/drive/items/"
                                 (nelisp-m365-compat-url-encode item-id)
                                 "/children"))
                (folder-path (concat "/me/drive/root:/"
                                     (nelisp-m365-compat-url-encode folder-path)
                                     ":/children"))
                (t "/me/drive/root/children")))
         (path (concat base
                       (nelisp-m365-graph-query
                        (list (cons "$select" nelisp-m365-tools--item-fields)
                              (cons "$top" limit)))))
         (collected (nelisp-m365-graph-collection path limit)))
    (nelisp-m365-tools--collection-result "items" collected)))

(defconst nelisp-m365-tools--text-mime-prefixes
  '("text/" "application/json" "application/xml" "application/javascript"
    "application/x-yaml" "application/yaml")
  "MIME prefixes treated as safe to return as text.")

(defconst nelisp-m365-tools--text-extensions
  '(".md" ".org" ".txt" ".csv" ".tsv" ".log" ".ini" ".yaml" ".yml"
    ".json" ".xml" ".el" ".py" ".sh" ".ps1")
  "File extensions treated as text when the MIME type is unhelpful.")

(defun nelisp-m365-tools--text-like-p (name mime)
  "Return non-nil when a file called NAME with MIME type MIME is text."
  (let ((lower (downcase (or name "")))
        (type (downcase (or mime "")))
        (hit nil))
    (dolist (p nelisp-m365-tools--text-mime-prefixes)
      (when (string-prefix-p p type) (setq hit t)))
    (dolist (e nelisp-m365-tools--text-extensions)
      (when (string-suffix-p e lower) (setq hit t)))
    hit))

(defun nelisp-m365-tools-get-onedrive-text (args)
  "Return the text content of a small OneDrive file."
  (let* ((id (nelisp-m365-tools--require-arg args "itemId"))
         (encoded (nelisp-m365-compat-url-encode id))
         (meta (nelisp-m365-graph-get
                (concat "/me/drive/items/" encoded
                        (nelisp-m365-graph-query
                         '(("$select" . "id,name,size,file,webUrl,lastModifiedDateTime"))))))
         (file (cdr (assoc "file" meta)))
         (size (or (cdr (assoc "size" meta)) 0))
         (name (cdr (assoc "name" meta)))
         (mime (cdr (assoc "mimeType" file))))
    (unless file
      (error "nelisp-m365: %s is a folder, not a file" name))
    (when (> size nelisp-m365-tools-max-text-bytes)
      (error "nelisp-m365: %s is %s bytes, over the %s byte limit"
             name size nelisp-m365-tools-max-text-bytes))
    (unless (nelisp-m365-tools--text-like-p name mime)
      (error "nelisp-m365: %s is not a text format (%s); use m365_excel_read_range for workbooks"
             name (or mime "unknown")))
    (let* ((dest (make-temp-file "nelisp-m365-dl-"))
           (status (nelisp-m365-graph-download
                    (concat "/me/drive/items/" encoded "/content") dest))
           (text (or (nelisp-m365-compat-read-file dest) "")))
      (condition-case nil (delete-file dest) (error nil))
      (unless (and (>= status 200) (< status 300))
        (error "nelisp-m365: content download failed with HTTP %s" status))
      (let ((clipped (nelisp-m365-tools--truncate text)))
        (list (cons "metadata" (nelisp-m365-graph-pick
                                meta '("id" "name" "size" "webUrl"
                                       "lastModifiedDateTime")))
              (cons "text" (car clipped))
              (cons "truncated" (nelisp-m365-compat-json-bool (cdr clipped))))))))

;;; Excel -------------------------------------------------------------------

(defun nelisp-m365-tools-excel-worksheets (args)
  "List the worksheets in a OneDrive-hosted workbook."
  (let* ((id (nelisp-m365-tools--require-arg args "itemId"))
         (body (nelisp-m365-graph-get
                (concat "/me/drive/items/" (nelisp-m365-compat-url-encode id)
                        "/workbook/worksheets"
                        (nelisp-m365-graph-query
                         '(("$select" . "id,name,position,visibility")))))))
    (list (cons "worksheets"
                (nelisp-m365-compat-json-array (cdr (assoc "value" body)))))))

(defun nelisp-m365-tools-excel-read-range (args)
  "Read a cell range from a worksheet in a OneDrive-hosted workbook."
  (let* ((id (nelisp-m365-tools--require-arg args "itemId"))
         (sheet (nelisp-m365-tools--require-arg args "worksheet"))
         (range (nelisp-m365-tools--arg args "range"))
         (base (concat "/me/drive/items/" (nelisp-m365-compat-url-encode id)
                       "/workbook/worksheets/"
                       (nelisp-m365-compat-url-encode sheet)))
         (path (if range
                   (concat base "/range(address='"
                           (nelisp-m365-compat-url-encode
                            (nelisp-m365-graph-escape-odata range))
                           "')")
                 (concat base "/usedRange")))
         (body (nelisp-m365-graph-get
                (concat path
                        (nelisp-m365-graph-query
                         '(("$select" . "address,rowCount,columnCount,values,text")))))))
    (list (cons "address" (cdr (assoc "address" body)))
          (cons "rowCount" (cdr (assoc "rowCount" body)))
          (cons "columnCount" (cdr (assoc "columnCount" body)))
          ;; `values' is an array of row arrays; normalise both levels so
          ;; the shape is right whichever way the parser handed it over.
          (cons "values"
                (nelisp-m365-compat-json-array
                 (mapcar #'nelisp-m365-compat-json-array
                         (nelisp-m365-compat-to-list
                          (cdr (assoc "values" body)))))))))

;;; Contacts -----------------------------------------------------------------

(defun nelisp-m365-tools-list-contacts (args)
  "List or search Outlook contacts."
  (let* ((limit (nelisp-m365-tools--int-arg args "maxResults" 25 1 100))
         (query (nelisp-m365-tools--arg args "query"))
         (path (concat "/me/contacts"
                       (nelisp-m365-graph-query
                        (list (cons "$select" "id,displayName,emailAddresses,companyName,jobTitle,mobilePhone")
                              (cons "$top" limit)
                              (cons "$search"
                                    (and query
                                         (concat "\"" (string-replace "\"" " " query) "\"")))))))
         (collected (nelisp-m365-graph-collection path limit)))
    (nelisp-m365-tools--collection-result "contacts" collected)))

;;; Microsoft To Do -----------------------------------------------------------

(defun nelisp-m365-tools-todo-lists (_args)
  "List the Microsoft To Do task lists."
  (let ((body (nelisp-m365-graph-get "/me/todo/lists")))
    (list (cons "lists" (nelisp-m365-compat-json-array
                         (cdr (assoc "value" body)))))))

(defun nelisp-m365-tools-todo-tasks (args)
  "List tasks in one Microsoft To Do list."
  (let* ((list-id (nelisp-m365-tools--require-arg args "listId"))
         (limit (nelisp-m365-tools--int-arg args "maxResults" 50 1 200))
         (include-done (nelisp-m365-tools--arg args "includeCompleted"))
         (path (concat "/me/todo/lists/"
                       (nelisp-m365-compat-url-encode list-id) "/tasks"
                       (nelisp-m365-graph-query
                        (list (cons "$top" limit)
                              (cons "$filter"
                                    (and (not (eq include-done t))
                                         "status ne 'completed'"))))))
         (collected (nelisp-m365-graph-collection path limit)))
    (nelisp-m365-tools--collection-result "tasks" collected)))

;;; OneNote ---------------------------------------------------------------------

(defun nelisp-m365-tools-onenote-pages (args)
  "List or search OneNote pages by title."
  (let* ((limit (nelisp-m365-tools--int-arg args "maxResults" 25 1 100))
         (query (nelisp-m365-tools--arg args "query"))
         (path (concat "/me/onenote/pages"
                       (nelisp-m365-graph-query
                        (list (cons "$select" "id,title,createdDateTime,lastModifiedDateTime,links")
                              (cons "$top" limit)
                              (cons "$orderby" "lastModifiedDateTime desc")
                              (cons "$filter"
                                    (and query
                                         (concat "contains(title,'"
                                                 (nelisp-m365-graph-escape-odata query)
                                                 "')")))))))
         (collected (nelisp-m365-graph-collection path limit)))
    (nelisp-m365-tools--collection-result "pages" collected)))

(defun nelisp-m365-tools-onenote-page (args)
  "Return one OneNote page rendered as plain text."
  (let* ((id (nelisp-m365-tools--require-arg args "pageId"))
         (encoded (nelisp-m365-compat-url-encode id))
         (meta (nelisp-m365-graph-get
                (concat "/me/onenote/pages/" encoded
                        (nelisp-m365-graph-query
                         '(("$select" . "id,title,createdDateTime,lastModifiedDateTime")))))))
    ;; The content endpoint answers text/html, not JSON, so it goes
    ;; through the raw transport rather than `nelisp-m365-graph-get'.
    (let* ((resp (nelisp-m365-curl-request
                  "GET"
                  (concat nelisp-m365-graph-root "/me/onenote/pages/"
                          encoded "/content")
                  :bearer (nelisp-m365-auth-access-token)))
           (status (plist-get resp :status)))
      (unless (and (>= status 200) (< status 300))
        (error "nelisp-m365: OneNote page content failed with HTTP %s" status))
      (let ((clipped (nelisp-m365-tools--truncate
                      (nelisp-m365-tools-html-to-text (plist-get resp :body)))))
        (list (cons "metadata" meta)
              (cons "text" (car clipped))
              (cons "truncated" (nelisp-m365-compat-json-bool (cdr clipped))))))))

;;; Downloads ---------------------------------------------------------------

(defvar nelisp-m365-download-dir nil
  "Directory that binary downloads are written to.
Set from the generated bootstrap file.  Nothing is written outside it.")

(defconst nelisp-m365-tools--unsafe-name-chars
  '("/" "\\" ":" "*" "?" "\"" "<" ">" "|" "\0" "\n" "\r" "\t")
  "Characters removed from a downloaded file's name.")

(defun nelisp-m365-tools--safe-basename (name)
  "Return NAME reduced to a basename that is safe to write.

Attachment and drive item names come from outside the machine, so a name
carrying a path separator, a drive letter or a `..' segment must not be
able to steer a write out of the download directory.  Everything
structural is stripped rather than escaped."
  (let ((out (or name "download")))
    (dolist (ch nelisp-m365-tools--unsafe-name-chars)
      (setq out (string-join (nelisp-m365-compat-split-all out ch) "_")))
    ;; A leading dot would hide the file; a name of only dots would
    ;; resolve to the directory itself.
    (while (string-prefix-p "." out)
      (setq out (substring out 1)))
    (setq out (string-trim out))
    (if (equal out "") "download" out)))

(defun nelisp-m365-tools--download-path (name)
  "Return an absolute path under `nelisp-m365-download-dir' for NAME."
  (unless nelisp-m365-download-dir
    (error "nelisp-m365: no download directory is configured"))
  (unless (nelisp-m365-compat-directory-p nelisp-m365-download-dir)
    (condition-case nil
        (make-directory nelisp-m365-download-dir t)
      (error nil)))
  (concat (directory-file-name nelisp-m365-download-dir) "/"
          (nelisp-m365-tools--safe-basename name)))

;;; Mail attachments and threads ----------------------------------------------

(defun nelisp-m365-tools-list-mail-attachments (args)
  "List the attachments on one message."
  (let* ((id (nelisp-m365-tools--require-arg args "messageId"))
         (body (nelisp-m365-graph-get
                (concat "/me/messages/" (nelisp-m365-compat-url-encode id)
                        "/attachments"
                        (nelisp-m365-graph-query
                         '(("$select" . "id,name,contentType,size,isInline")))))))
    (list (cons "attachments"
                (nelisp-m365-compat-json-array (cdr (assoc "value" body))))
          (cons "count" (length (nelisp-m365-compat-to-list
                                 (cdr (assoc "value" body))))))))

(defun nelisp-m365-tools-save-mail-attachment (args)
  "Save one message attachment into the download directory."
  (let* ((mid (nelisp-m365-tools--require-arg args "messageId"))
         (aid (nelisp-m365-tools--require-arg args "attachmentId"))
         (meta (nelisp-m365-graph-get
                (concat "/me/messages/" (nelisp-m365-compat-url-encode mid)
                        "/attachments/" (nelisp-m365-compat-url-encode aid)
                        (nelisp-m365-graph-query
                         '(("$select" . "id,name,contentType,size"))))))
         (name (cdr (assoc "name" meta)))
         (dest (nelisp-m365-tools--download-path name))
         (status (nelisp-m365-graph-download
                  (concat "/me/messages/" (nelisp-m365-compat-url-encode mid)
                          "/attachments/" (nelisp-m365-compat-url-encode aid)
                          "/$value")
                  dest)))
    (unless (and (>= status 200) (< status 300))
      (error "nelisp-m365: attachment download failed with HTTP %s" status))
    (list (cons "path" dest)
          (cons "name" name)
          (cons "contentType" (cdr (assoc "contentType" meta)))
          (cons "size" (cdr (assoc "size" meta))))))

(defun nelisp-m365-tools--by-received (a b)
  "Return non-nil when message A was received before B.
Graph timestamps are ISO 8601 in UTC and fixed width, so ordering them
as strings orders them as instants."
  (string< (or (cdr (assoc "receivedDateTime" a)) "")
           (or (cdr (assoc "receivedDateTime" b)) "")))

(defun nelisp-m365-tools-get-mail-thread (args)
  "Return every message in one Outlook conversation, oldest first.

Sorted here rather than by the server: Exchange rejects `$orderby'
alongside a `conversationId' filter with `InefficientFilter', because
the restriction and the sort are on different properties."
  (let* ((conversation (nelisp-m365-tools--require-arg args "conversationId"))
         (limit (nelisp-m365-tools--int-arg args "maxResults" 20 1 100))
         (path (concat "/me/messages"
                       (nelisp-m365-graph-query
                        (list (cons "$filter"
                                    (concat "conversationId eq '"
                                            (nelisp-m365-graph-escape-odata
                                             conversation)
                                            "'"))
                              (cons "$select" nelisp-m365-tools--message-fields)
                              (cons "$top" limit)))))
         (collected (nelisp-m365-graph-collection path limit))
         (sorted (sort (plist-get collected :items)
                       #'nelisp-m365-tools--by-received)))
    (nelisp-m365-tools--collection-result
     "messages" (list :items sorted
                      :truncated (plist-get collected :truncated)))))

;;; OneDrive: downloads, recent, shared ----------------------------------------

(defun nelisp-m365-tools-download-onedrive-file (args)
  "Download any OneDrive file into the download directory.
Unlike `m365_get_onedrive_text' this does not decode the content -- it
is for PDFs, workbooks and images, which the model reads with other
tools once they are on disk."
  (let* ((id (nelisp-m365-tools--require-arg args "itemId"))
         (encoded (nelisp-m365-compat-url-encode id))
         (meta (nelisp-m365-graph-get
                (concat "/me/drive/items/" encoded
                        (nelisp-m365-graph-query
                         '(("$select" . "id,name,size,file,webUrl,lastModifiedDateTime"))))))
         (name (cdr (assoc "name" meta))))
    (unless (cdr (assoc "file" meta))
      (error "nelisp-m365: %s is a folder, not a file" name))
    (let* ((dest (nelisp-m365-tools--download-path name))
           (status (nelisp-m365-graph-download
                    (concat "/me/drive/items/" encoded "/content") dest)))
      (unless (and (>= status 200) (< status 300))
        (error "nelisp-m365: download failed with HTTP %s" status))
      (list (cons "path" dest)
            (cons "metadata" (nelisp-m365-graph-pick
                              meta '("id" "name" "size" "webUrl"
                                     "lastModifiedDateTime")))))))

(defun nelisp-m365-tools-recent-files (args)
  "List the OneDrive items this account touched most recently."
  (let* ((limit (nelisp-m365-tools--int-arg args "maxResults" 25 1 100))
         (collected (nelisp-m365-graph-collection
                     (concat "/me/drive/recent"
                             (nelisp-m365-graph-query
                              (list (cons "$top" limit))))
                     limit)))
    (nelisp-m365-tools--collection-result "items" collected)))

(defun nelisp-m365-tools-shared-with-me (args)
  "List OneDrive items other people have shared with this account."
  (let* ((limit (nelisp-m365-tools--int-arg args "maxResults" 25 1 100))
         (collected (nelisp-m365-graph-collection
                     (concat "/me/drive/sharedWithMe"
                             (nelisp-m365-graph-query
                              (list (cons "$top" limit))))
                     limit)))
    (nelisp-m365-tools--collection-result "items" collected)))

(defun nelisp-m365-tools-drive-info (_args)
  "Return the drive's identity and storage quota."
  (let ((body (nelisp-m365-graph-get "/me/drive")))
    (append (nelisp-m365-graph-pick body '("id" "driveType" "name"))
            (list (cons "quota" (cdr (assoc "quota" body)))))))

;;; Calendars -------------------------------------------------------------------

(defun nelisp-m365-tools-list-calendars (args)
  "List the calendars on this account."
  (let* ((limit (nelisp-m365-tools--int-arg args "maxResults" 25 1 100))
         (collected (nelisp-m365-graph-collection
                     (concat "/me/calendars"
                             (nelisp-m365-graph-query
                              (list (cons "$select" "id,name,isDefaultCalendar,canEdit,owner")
                                    (cons "$top" limit))))
                     limit)))
    (nelisp-m365-tools--collection-result "calendars" collected)))

;;; Excel tables -----------------------------------------------------------------

(defun nelisp-m365-tools-excel-tables (args)
  "List the named tables in a OneDrive-hosted workbook."
  (let* ((id (nelisp-m365-tools--require-arg args "itemId"))
         (sheet (nelisp-m365-tools--arg args "worksheet"))
         (base (concat "/me/drive/items/" (nelisp-m365-compat-url-encode id)
                       "/workbook"
                       (if sheet
                           (concat "/worksheets/"
                                   (nelisp-m365-compat-url-encode sheet))
                         "")
                       "/tables"))
         (body (nelisp-m365-graph-get
                (concat base
                        (nelisp-m365-graph-query
                         '(("$select" . "id,name,showHeaders,highlightFirstColumn")))))))
    (list (cons "tables"
                (nelisp-m365-compat-json-array (cdr (assoc "value" body)))))))

(defun nelisp-m365-tools-excel-read-table (args)
  "Read the cells of one named table in a OneDrive-hosted workbook."
  (let* ((id (nelisp-m365-tools--require-arg args "itemId"))
         (table (nelisp-m365-tools--require-arg args "table"))
         (body (nelisp-m365-graph-get
                (concat "/me/drive/items/" (nelisp-m365-compat-url-encode id)
                        "/workbook/tables/"
                        (nelisp-m365-compat-url-encode table)
                        "/range"
                        (nelisp-m365-graph-query
                         '(("$select" . "address,rowCount,columnCount,values")))))))
    (list (cons "address" (cdr (assoc "address" body)))
          (cons "rowCount" (cdr (assoc "rowCount" body)))
          (cons "columnCount" (cdr (assoc "columnCount" body)))
          (cons "values"
                (nelisp-m365-compat-json-array
                 (mapcar #'nelisp-m365-compat-json-array
                         (nelisp-m365-compat-to-list
                          (cdr (assoc "values" body)))))))))

;;; OneNote structure ---------------------------------------------------------------

(defun nelisp-m365-tools-onenote-notebooks (_args)
  "List the OneNote notebooks on this account."
  (let ((body (nelisp-m365-graph-get
               (concat "/me/onenote/notebooks"
                       (nelisp-m365-graph-query
                        '(("$select" . "id,displayName,createdDateTime,lastModifiedDateTime")))))))
    (list (cons "notebooks"
                (nelisp-m365-compat-json-array (cdr (assoc "value" body)))))))

(defun nelisp-m365-tools-onenote-sections (args)
  "List OneNote sections, optionally within one notebook."
  (let* ((notebook (nelisp-m365-tools--arg args "notebookId"))
         (path (concat (if notebook
                           (concat "/me/onenote/notebooks/"
                                   (nelisp-m365-compat-url-encode notebook)
                                   "/sections")
                         "/me/onenote/sections")
                       (nelisp-m365-graph-query
                        '(("$select" . "id,displayName,lastModifiedDateTime")))))
         (body (nelisp-m365-graph-get path)))
    (list (cons "sections"
                (nelisp-m365-compat-json-array (cdr (assoc "value" body)))))))

;;; Schemas ---------------------------------------------------------------------

(defun nelisp-m365-tools--schema (properties required)
  "Build a JSON Schema object from PROPERTIES and the REQUIRED name list."
  (list (cons "type" "object")
        (cons "properties" (or properties (nelisp-m365-compat-json-object)))
        (cons "required" (nelisp-m365-compat-json-array required))))

(defun nelisp-m365-tools--prop (type description &rest extra)
  "Build one JSON Schema property of TYPE with DESCRIPTION.
EXTRA is appended verbatim, for keywords such as minimum or default."
  (append (list (cons "type" type) (cons "description" description)) extra))

(defun nelisp-m365-tools--max-results (default max)
  "Return a maxResults property capped at MAX with DEFAULT."
  (nelisp-m365-tools--prop
   "integer" (format "Maximum rows to return (1-%s)." max)
   (cons "minimum" 1) (cons "maximum" max) (cons "default" default)))

;;; Registry -----------------------------------------------------------------------

(defun nelisp-m365-tools-registry ()
  "Return the tool definitions this server exposes.

Each entry is a plist with :name, :title, :description, :schema,
:handler and :untrusted.  Handlers take the parsed argument alist and
return a value the MCP layer encodes as structured content."
  (list
   (list :name "m365_authenticate"
         :title "Sign in to Microsoft 365 Personal"
         :description "Start device-code sign-in for a personal Microsoft account. Returns a short code and a URL; the user enters the code in a browser, then m365_authenticate_finish completes the sign-in."
         :read-only nil
         :untrusted nil
         :schema (nelisp-m365-tools--schema
                  (list (cons "openBrowser"
                              (nelisp-m365-tools--prop
                               "boolean"
                               "Try to open the verification URL in a browser."
                               (cons "default" t))))
                  nil)
         :handler #'nelisp-m365-tools-authenticate)

   (list :name "m365_authenticate_finish"
         :title "Finish Microsoft 365 sign-in"
         :description "Wait for the user to finish entering the device code, then store the tokens. Returns status \"pending\" if they have not finished yet; call it again in that case."
         :read-only nil
         :untrusted nil
         :schema (nelisp-m365-tools--schema nil nil)
         :handler #'nelisp-m365-tools-authenticate-finish)

   (list :name "m365_status"
         :title "Microsoft 365 connection status"
         :description "Report whether this connector is signed in, which scopes were granted, and where the token cache lives. Makes no network call."
         :read-only t
         :untrusted nil
         :schema (nelisp-m365-tools--schema nil nil)
         :handler #'nelisp-m365-tools-status)

   (list :name "m365_profile"
         :title "Microsoft account profile"
         :description "Return the display name and email address of the signed-in personal Microsoft account."
         :read-only t
         :untrusted nil
         :schema (nelisp-m365-tools--schema nil nil)
         :handler #'nelisp-m365-tools-profile)

   (list :name "m365_search_mail"
         :title "Search Outlook mail"
         :description "Full-text search across Outlook mail. Returns summaries with a body preview only; use m365_get_mail for a full body."
         :read-only t
         :untrusted t
         :schema (nelisp-m365-tools--schema
                  (list (cons "query"
                              (nelisp-m365-tools--prop
                               "string" "Words to search for in subject, sender or body."))
                        (cons "folder"
                              (nelisp-m365-tools--prop
                               "string" "Restrict to one mail folder id or well-known name such as inbox or sentitems."))
                        (cons "maxResults" (nelisp-m365-tools--max-results 10 50)))
                  '("query"))
         :handler #'nelisp-m365-tools-search-mail)

   (list :name "m365_get_mail"
         :title "Read one Outlook message"
         :description "Return one message with recipients and the body rendered as plain text."
         :read-only t
         :untrusted t
         :schema (nelisp-m365-tools--schema
                  (list (cons "messageId"
                              (nelisp-m365-tools--prop
                               "string" "Message id from m365_search_mail.")))
                  '("messageId"))
         :handler #'nelisp-m365-tools-get-mail)

   (list :name "m365_list_mail_folders"
         :title "List Outlook mail folders"
         :description "List mail folders with unread and total item counts, to find a folder id for m365_search_mail."
         :read-only t
         :untrusted t
         :schema (nelisp-m365-tools--schema
                  (list (cons "maxResults" (nelisp-m365-tools--max-results 30 100)))
                  nil)
         :handler #'nelisp-m365-tools-list-mail-folders)

   (list :name "m365_list_calendar"
         :title "List calendar events"
         :description "Return calendar events between two ISO 8601 timestamps, in chronological order."
         :read-only t
         :untrusted t
         :schema (nelisp-m365-tools--schema
                  (list (cons "start"
                              (nelisp-m365-tools--prop
                               "string" "Window start, ISO 8601 with offset, e.g. 2026-08-24T00:00:00+09:00."))
                        (cons "end"
                              (nelisp-m365-tools--prop
                               "string" "Window end, ISO 8601 with offset."))
                        (cons "timeZone"
                              (nelisp-m365-tools--prop
                               "string" "Windows time zone name for the returned times."
                               (cons "default" "Tokyo Standard Time")))
                        (cons "maxResults" (nelisp-m365-tools--max-results 25 100)))
                  '("start" "end"))
         :handler #'nelisp-m365-tools-list-calendar)

   (list :name "m365_search_onedrive"
         :title "Search OneDrive"
         :description "Search personal OneDrive for files and folders by name and content."
         :read-only t
         :untrusted t
         :schema (nelisp-m365-tools--schema
                  (list (cons "query"
                              (nelisp-m365-tools--prop "string" "Search text."))
                        (cons "maxResults" (nelisp-m365-tools--max-results 20 50)))
                  '("query"))
         :handler #'nelisp-m365-tools-search-onedrive)

   (list :name "m365_list_onedrive"
         :title "List a OneDrive folder"
         :description "List the children of a OneDrive folder. Give itemId or path; with neither, lists the drive root."
         :read-only t
         :untrusted t
         :schema (nelisp-m365-tools--schema
                  (list (cons "itemId"
                              (nelisp-m365-tools--prop "string" "Folder item id."))
                        (cons "path"
                              (nelisp-m365-tools--prop
                               "string" "Folder path relative to the drive root, e.g. Documents/2026."))
                        (cons "maxResults" (nelisp-m365-tools--max-results 50 200)))
                  nil)
         :handler #'nelisp-m365-tools-list-onedrive)

   (list :name "m365_get_onedrive_text"
         :title "Read a OneDrive text file"
         :description "Return the contents of a small text file in OneDrive. Binary and Office formats are refused; use m365_excel_read_range for workbooks."
         :read-only t
         :untrusted t
         :schema (nelisp-m365-tools--schema
                  (list (cons "itemId"
                              (nelisp-m365-tools--prop
                               "string" "Item id from m365_search_onedrive or m365_list_onedrive.")))
                  '("itemId"))
         :handler #'nelisp-m365-tools-get-onedrive-text)

   (list :name "m365_excel_list_worksheets"
         :title "List Excel worksheets"
         :description "List the worksheets in a workbook stored in OneDrive."
         :read-only t
         :untrusted t
         :schema (nelisp-m365-tools--schema
                  (list (cons "itemId"
                              (nelisp-m365-tools--prop
                               "string" "Workbook item id from m365_search_onedrive.")))
                  '("itemId"))
         :handler #'nelisp-m365-tools-excel-worksheets)

   (list :name "m365_excel_read_range"
         :title "Read an Excel range"
         :description "Read cell values from a worksheet in a OneDrive workbook. Without a range, returns the used range."
         :read-only t
         :untrusted t
         :schema (nelisp-m365-tools--schema
                  (list (cons "itemId"
                              (nelisp-m365-tools--prop "string" "Workbook item id."))
                        (cons "worksheet"
                              (nelisp-m365-tools--prop "string" "Worksheet name or id."))
                        (cons "range"
                              (nelisp-m365-tools--prop
                               "string" "A1-style address such as A1:D50. Omit for the used range.")))
                  '("itemId" "worksheet"))
         :handler #'nelisp-m365-tools-excel-read-range)

   (list :name "m365_list_contacts"
         :title "List Outlook contacts"
         :description "List or search the personal Outlook contact list."
         :read-only t
         :untrusted t
         :schema (nelisp-m365-tools--schema
                  (list (cons "query"
                              (nelisp-m365-tools--prop
                               "string" "Optional search text; omit to list recent contacts."))
                        (cons "maxResults" (nelisp-m365-tools--max-results 25 100)))
                  nil)
         :handler #'nelisp-m365-tools-list-contacts)

   (list :name "m365_todo_lists"
         :title "List Microsoft To Do lists"
         :description "List the task lists in Microsoft To Do, to find a listId for m365_todo_tasks."
         :read-only t
         :untrusted t
         :schema (nelisp-m365-tools--schema nil nil)
         :handler #'nelisp-m365-tools-todo-lists)

   (list :name "m365_todo_tasks"
         :title "List Microsoft To Do tasks"
         :description "List the tasks in one Microsoft To Do list. Completed tasks are excluded unless asked for."
         :read-only t
         :untrusted t
         :schema (nelisp-m365-tools--schema
                  (list (cons "listId"
                              (nelisp-m365-tools--prop
                               "string" "List id from m365_todo_lists."))
                        (cons "includeCompleted"
                              (nelisp-m365-tools--prop
                               "boolean" "Include completed tasks."
                               (cons "default" :json-false)))
                        (cons "maxResults" (nelisp-m365-tools--max-results 50 200)))
                  '("listId"))
         :handler #'nelisp-m365-tools-todo-tasks)

   (list :name "m365_onenote_pages"
         :title "List OneNote pages"
         :description "List OneNote pages, most recently changed first, optionally filtered by title."
         :read-only t
         :untrusted t
         :schema (nelisp-m365-tools--schema
                  (list (cons "query"
                              (nelisp-m365-tools--prop
                               "string" "Optional text the page title must contain."))
                        (cons "maxResults" (nelisp-m365-tools--max-results 25 100)))
                  nil)
         :handler #'nelisp-m365-tools-onenote-pages)

   (list :name "m365_get_onenote_page"
         :title "Read a OneNote page"
         :description "Return one OneNote page with its HTML rendered as plain text."
         :read-only t
         :untrusted t
         :schema (nelisp-m365-tools--schema
                  (list (cons "pageId"
                              (nelisp-m365-tools--prop
                               "string" "Page id from m365_onenote_pages.")))
                  '("pageId"))
         :handler #'nelisp-m365-tools-onenote-page)

   (list :name "m365_get_mail_thread"
         :title "Read an Outlook conversation"
         :description "Return every message in one Outlook conversation, oldest first. Use the conversationId from m365_search_mail to follow a thread."
         :read-only t
         :untrusted t
         :schema (nelisp-m365-tools--schema
                  (list (cons "conversationId"
                              (nelisp-m365-tools--prop
                               "string" "conversationId from m365_search_mail."))
                        (cons "maxResults" (nelisp-m365-tools--max-results 20 100)))
                  '("conversationId"))
         :handler #'nelisp-m365-tools-get-mail-thread)

   (list :name "m365_list_mail_attachments"
         :title "List message attachments"
         :description "List the attachments on one message, with name, type and size."
         :read-only t
         :untrusted t
         :schema (nelisp-m365-tools--schema
                  (list (cons "messageId"
                              (nelisp-m365-tools--prop
                               "string" "Message id from m365_search_mail.")))
                  '("messageId"))
         :handler #'nelisp-m365-tools-list-mail-attachments)

   (list :name "m365_save_mail_attachment"
         :title "Save a message attachment"
         :description "Download one attachment to the local download directory and return its path. The file name is sanitised; nothing is written outside that directory."
         :read-only nil
         :untrusted nil
         :schema (nelisp-m365-tools--schema
                  (list (cons "messageId"
                              (nelisp-m365-tools--prop "string" "Message id."))
                        (cons "attachmentId"
                              (nelisp-m365-tools--prop
                               "string" "Attachment id from m365_list_mail_attachments.")))
                  '("messageId" "attachmentId"))
         :handler #'nelisp-m365-tools-save-mail-attachment)

   (list :name "m365_download_onedrive_file"
         :title "Download a OneDrive file"
         :description "Download any OneDrive file -- PDF, workbook, image -- to the local download directory and return its path. Use m365_get_onedrive_text instead when the content is text and you want it inline."
         :read-only nil
         :untrusted nil
         :schema (nelisp-m365-tools--schema
                  (list (cons "itemId"
                              (nelisp-m365-tools--prop
                               "string" "Item id from m365_search_onedrive.")))
                  '("itemId"))
         :handler #'nelisp-m365-tools-download-onedrive-file)

   (list :name "m365_recent_files"
         :title "Recently used OneDrive files"
         :description "List the OneDrive items this account opened or changed most recently."
         :read-only t
         :untrusted t
         :schema (nelisp-m365-tools--schema
                  (list (cons "maxResults" (nelisp-m365-tools--max-results 25 100)))
                  nil)
         :handler #'nelisp-m365-tools-recent-files)

   (list :name "m365_shared_with_me"
         :title "Files shared with me"
         :description "List OneDrive items other people have shared with this account."
         :read-only t
         :untrusted t
         :schema (nelisp-m365-tools--schema
                  (list (cons "maxResults" (nelisp-m365-tools--max-results 25 100)))
                  nil)
         :handler #'nelisp-m365-tools-shared-with-me)

   (list :name "m365_drive_info"
         :title "OneDrive storage"
         :description "Return the drive's identity and its storage quota."
         :read-only t
         :untrusted nil
         :schema (nelisp-m365-tools--schema nil nil)
         :handler #'nelisp-m365-tools-drive-info)

   (list :name "m365_list_calendars"
         :title "List calendars"
         :description "List the calendars on this account, to pick one for a calendar query."
         :read-only t
         :untrusted t
         :schema (nelisp-m365-tools--schema
                  (list (cons "maxResults" (nelisp-m365-tools--max-results 25 100)))
                  nil)
         :handler #'nelisp-m365-tools-list-calendars)

   (list :name "m365_excel_list_tables"
         :title "List Excel tables"
         :description "List the named tables in a workbook, optionally within one worksheet. A named table is usually a cleaner read than a raw range."
         :read-only t
         :untrusted t
         :schema (nelisp-m365-tools--schema
                  (list (cons "itemId"
                              (nelisp-m365-tools--prop "string" "Workbook item id."))
                        (cons "worksheet"
                              (nelisp-m365-tools--prop
                               "string" "Restrict to one worksheet name or id.")))
                  '("itemId"))
         :handler #'nelisp-m365-tools-excel-tables)

   (list :name "m365_excel_read_table"
         :title "Read an Excel table"
         :description "Read every cell of one named table in a workbook, header row included."
         :read-only t
         :untrusted t
         :schema (nelisp-m365-tools--schema
                  (list (cons "itemId"
                              (nelisp-m365-tools--prop "string" "Workbook item id."))
                        (cons "table"
                              (nelisp-m365-tools--prop
                               "string" "Table name or id from m365_excel_list_tables.")))
                  '("itemId" "table"))
         :handler #'nelisp-m365-tools-excel-read-table)

   (list :name "m365_onenote_notebooks"
         :title "List OneNote notebooks"
         :description "List the OneNote notebooks on this account."
         :read-only t
         :untrusted t
         :schema (nelisp-m365-tools--schema nil nil)
         :handler #'nelisp-m365-tools-onenote-notebooks)

   (list :name "m365_onenote_sections"
         :title "List OneNote sections"
         :description "List OneNote sections, optionally within one notebook."
         :read-only t
         :untrusted t
         :schema (nelisp-m365-tools--schema
                  (list (cons "notebookId"
                              (nelisp-m365-tools--prop
                               "string" "Notebook id from m365_onenote_notebooks.")))
                  nil)
         :handler #'nelisp-m365-tools-onenote-sections)))

(provide 'nelisp-m365-tools)

;;; nelisp-m365-tools.el ends here
