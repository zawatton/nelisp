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
         (space (string-search " " trimmed))
         (name (downcase (if space (substring trimmed 0 space) trimmed))))
    ;; A self-closing tag arrives as "br/"; drop the slash so it matches.
    (if (string-suffix-p "/" name) (substring name 0 -1) name)))

(defun nelisp-m365-tools--skip-container (text lower start name)
  "Return the index just past the closing tag for NAME, from START.
TEXT is the source and LOWER its downcased twin, which is index-aligned
because `downcase' preserves length.  Falls back to the end of TEXT when
the container is never closed."
  (let* ((close (concat "</" name))
         (idx (string-search close lower start)))
    (if (not idx)
        (length text)
      (let ((gt (string-search ">" text idx)))
        (if gt (1+ gt) (length text))))))

(defun nelisp-m365-tools--decode-entities (text)
  "Decode the HTML entities that appear in Outlook and OneNote output."
  (let ((out text)
        (pairs '(("&nbsp;" . " ") ("&#160;" . " ")
                 ("&lt;" . "<") ("&gt;" . ">")
                 ("&quot;" . "\"") ("&#34;" . "\"")
                 ("&#39;" . "'") ("&#x27;" . "'") ("&apos;" . "'")
                 ("&hellip;" . "...") ("&mdash;" . "--") ("&ndash;" . "-")
                 ;; Ampersand last: decoding it first would let an
                 ;; encoded "&amp;lt;" turn into a live "<".
                 ("&amp;" . "&"))))
    (dolist (p pairs)
      (setq out (string-replace (car p) (cdr p) out)))
    out))

(defun nelisp-m365-tools--collapse-blank-lines (text)
  "Collapse runs of three or more newlines in TEXT down to two."
  (let ((out text)
        (changed t))
    (while changed
      (let ((next (string-replace "\n\n\n" "\n\n" out)))
        (setq changed (not (equal next out)))
        (setq out next)))
    out))

(defun nelisp-m365-tools-html-to-text (html)
  "Return HTML rendered as plain text.

Walks tag to tag with `string-search' rather than scanning characters,
so cost is proportional to the number of tags, not the document length
-- an interpreted per-character loop is too slow for a 200 KB mail body
on this runtime.  Script and style contents are dropped entirely."
  (if (or (null html) (equal html ""))
      ""
    (let* ((lower (downcase html))
           (len (length html))
           (pos 0)
           (out nil))
      (while (< pos len)
        (let ((lt (string-search "<" html pos)))
          (if (not lt)
              (progn (push (substring html pos) out)
                     (setq pos len))
            (when (> lt pos) (push (substring html pos lt) out))
            (let ((gt (string-search ">" html lt)))
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
                               html lower gt name)))
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
  "id,subject,from,receivedDateTime,bodyPreview,isRead,hasAttachments,webLink"
  "Fields returned for a message in list results.")

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
         :handler #'nelisp-m365-tools-onenote-page)))

(provide 'nelisp-m365-tools)

;;; nelisp-m365-tools.el ends here
