(in-package :sonic.client)

(defvar *salt-len* 24)
(defconstant +charset+ "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")

(define-condition subsonic-error (error)
  ((code :initarg :code :reader error-code)
   (message :initarg :message :reader error-message))
  (:documentation "Custom base condition for when an OpenSubsonic server
returns an error over the REST API.")
  (:report (lambda (condition stream)
             (format stream "Server returned error ~a: ~a"
                     (error-code condition)
                     (error-message condition)))))

(defclass sonic-login ()
  ((uri :initarg :uri :accessor uri)
   (user :initarg :user :accessor user)
   (pass :initarg :pass :accessor pass)
   (client :initarg :client :accessor client)
   (extensions :initarg :extensions
               :accessor extensions))
  (:default-initargs
   :extensions nil
   :client "cl-opensubsonic"))

(defclass sonic-login-get (sonic-login) ())
(defclass sonic-login-post (sonic-login) ())

(defgeneric sonic-request (login path params)
  (:documentation "Make a request to an OpenSubsonic server depending
on what extensions are implemented."))

(defun gen-salt (&optional (len *salt-len*))
  (map 'string
       (lambda (n)
         (declare (ignore n))
         (aref +charset+ (random 61 (make-random-state t))))
       (make-string len)))

(defun sonic-token (pass salt)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence
    :md5 (ironclad:ascii-string-to-byte-array
          (concatenate 'string pass salt)))))

(defun sonic-auth (user pass client &optional params)
  (let* ((salt (gen-salt))
         (token (sonic-token pass salt)))
    `(("u" . ,user)
      ("s" . ,salt)
      ("t" . ,token)
      ("v" . "1.15.1")
      ("c" . ,client)
      ("f" . "json")
      ,@params)))

(defun sonic-path (path)
  (format nil "/rest~@[/~a~]" path))

(defun sonic-endpoint (user pass client path &optional params)
  (quri:make-uri
   :path (sonic-path path)
   :query (sonic-auth user pass client params)))

(defun sonic-query (psyms pvals)
  (remove nil (mapcar (lambda (k v)
                        (cons (remove-kebab k) v))
                      psyms pvals)
          :key #'cdr))

(defun sonic-uri (uri endpoint)
  (quri:merge-uris endpoint (quri:uri uri)))

(defun sonic-render-uri (uri user pass client path &optional params)
  (quri:render-uri
   (sonic-uri
    uri (sonic-endpoint user pass client path params))))

(defun sonic-response (raw)
  (let* ((json (jzon:parse raw))
         (response (gethash "subsonic-response" json))
         (status (gethash "status" response)))
    (if (equalp status "failed")
        (let ((error-info (gethash "error" response)))
          (error 'subsonic-error
                 :code (gethash "code" error-info)
                 :message (gethash "message" error-info)))
        response)))

(defmethod sonic-request ((login sonic-login-get)
                          path params)
  (with-slots (uri user pass client) login
    (sonic-response
     (drakma:http-request
      (sonic-render-uri user user pass client path params)))))

(defmethod sonic-request ((login sonic-login-post)
                          path params)
  (with-slots (uri user pass client) login
    (sonic-response
     (drakma:http-request
      (quri:render-uri
       (quri:merge-uris
        (sonic-path path)
        uri))
      :method :post
      :parameters (sonic-auth user pass client params)))))

(defmacro sonic (name req-params &optional opt-params)
  `(defun ,name (login ,@req-params &optional ,@opt-params)
     (sonic-request login ,(remove-kebab name)
                    ,(when (or req-params opt-params)
                       `(sonic-query
                            '(,@req-params ,@opt-params)
                            (list ,@req-params ,@opt-params))))))

(defun form-post-p (uri user pass &optional client)
  (let ((response (sonic-response
                   (drakma:http-request
                    (sonic-render-uri uri user pass client
                                      "getOpenSubsonicExtensions")))))
    (if (gethash "openSubsonicExtensions" response)
        (loop for x across (gethash "openSubsonicExtensions" response)
              as name = (gethash "name" x)
              when (equalp name "formPost")
                return (gethash "openSubsonicExtensions" response))
        (warn "Server doesn't support OpenSubsonic extensions"))))

(defun sonic-init (uri user pass &optional (client "cl-opensubsonic"))
  (let ((ext (form-post-p uri user pass)))
    (if ext
        (make-instance
         'sonic-login-post
         :uri uri :user user :pass pass
         :client client :extensions ext)
        (make-instance
         'sonic-login-get
         :uri uri :user user :pass pass
         :client client :extensions ext))))

;;;; system endpoints
(sonic system/ping ())
(sonic system/get-license ())
(sonic system/get-open-subsonic-extensions ())
(sonic system/token-info ())

;;;; browsing endpoints
(sonic browsing/get-music-folders ())
(sonic browsing/get-indexes
    () (music-folder-id if-modified-since))
(sonic browsing/get-music-directory (id)) ;; TODO: param not being provided
(sonic browsing/get-genres ())
(sonic browsing/get-artists () (music-folder-id))
(sonic browsing/get-artist (id))
(sonic browsing/get-album (id))
(sonic browsing/get-song (id))
(sonic browsing/get-videos ())
(sonic browsing/get-video-info (id))
(sonic browsing/get-artist-info (id) (count include-not-present))
(sonic browsing/get-artist-info-2 (id) (count include-not-present))
(sonic browsing/get-album-info (id))
(sonic browsing/get-album-info-2 (id))
(sonic browsing/get-similar-songs (id) (count))
(sonic browsing/get-similar-songs-2 (id)  (count))
(sonic browsing/get-top-songs (artist) (count))

;;;; album/song list endpoints
    ;; TODO: `fromYear' and `toYear' required if `type' = `byYear'
    ;; TODO: `genre' required if `type' = `byGenre'
(sonic album-song-lists/get-album-list
    (type) (size offset music-folder-id))
(sonic album-song-lists/get-album-list-2
    (type) (size offset music-folder-id))
(sonic album-song-lists/get-random-songs
    () (size genre from-year to-year music-folder-id))
(sonic album-song-lists/get-songs-by-genre
    (genre) (count offset music-folder-id))
(sonic album-song-lists/get-now-playing ())
(sonic album-song-lists/get-starred () (music-folder-id))
(sonic album-song-lists/get-starred2 () (music-folder-id))

;;;; searching endpoints
(sonic searching/search
    () (artist album title any count offset newer-than))
(sonic searching/search2
    (query) (artist-count artist-offset
             album-count album-offset
             song-count song-offset
             music-folder-id))
(sonic searching/search3
    (query) (artist-count artist-offset
             album-count album-offset
             song-count song-offset
             music-folder-id))

;;;; playlist endpoints
(sonic playlists/get-playlists () (username))
(sonic playlists/get-playlist (id))
(sonic playlists/create-playlist (playlist-id name) (song-id))
(sonic playlists/update-playlist
    (playlist-id) (name comment public song-id-to-add song-index-to-remove))
(sonic playlists/delete-playlist (id))

;;;; media retrieval endpoints
(sonic media-retrieval/stream
    (id) (max-bit-rate format time-offset size
          estimate-content-length converted))
(sonic media-retrieval/download (id))
(sonic media-retrieval/hls (id) (bit-rate audio-track))
(sonic media-retrieval/get-captions (id) (format))
(sonic media-retrieval/get-cover-art (id) (size))
(sonic media-retrieval/get-lyrics () (artist title))
(sonic media-retrieval/get-avatar (username))
(sonic media-retrieval/get-lyrics-by-song-id (get-lyrics-by-song-id))

;;;; media annotation endpoints
(sonic media-annotation/star () (id album-id artist-id))
(sonic media-annotation/unstar () (id album-id artist-id))
(sonic media-annotation/set-rating (id rating))
(sonic media-annotation/scrobble (id) (time submission))

;;;; sharing endpoints
(sonic sharing/get-shares ())
(sonic sharing/create-share (id) (description examples))
(sonic sharing/update-share (id) (description examples))
(sonic sharing/delete-share (id))

;;;; podcast endpoints
(sonic podcast/get-podcasts () (include-episodes id))
(sonic podcast/get-newest-podcasts () (count))
(sonic podcast/refresh-podcasts ())
(sonic podcast/create-podcast-channel (url))
(sonic podcast/delete-podcast-channel (id))
(sonic podcast/delete-podcast-episode (id))
(sonic podcast/download-podcast-episode (id))

;;;; jukebox endpoints
;; TODO: `action' Must be one of: `get', `status' (since 1.7.0), `set' (since 1.7.0),
;; `start', `stop', `skip', `add', `clear', `remove', `shuffle', `setGain'
(sonic jukebox/jukebox-control (action) (index offset id gain))

;;;; internet radio endpoints
(sonic internet-radio/get-internet-radio-stations ())
(sonic internet-radio/create-internet-radio-station
    (stream-url name) (homepage-url))
(sonic internet-radio/update-internet-radio-station
    (id stream-url name) (homepage-url))
(sonic internet-radio/delete-internet-radio-station (id))

;;;; chat endpoints
(sonic chat/get-chat-messages () (since))
(sonic chat/add-chat-message (message))

;;;; user management endpoints
(sonic user-management/get-user (username))
(sonic user-management/get-users ())
(sonic user-management/create-user
    (username password email)
    (ldap-authenticated admin-role settings-role stream-role jukebox-role
     download-role upload-role playlist-role cover-art-role comment-role
     podcast-role share-role video-conversion-role music-folder-id)
    )
(sonic user-management/update-user
    (username)
    (password email ldap-authenticated admin-role settings-role stream-role jukebox-role
     download-role upload-role playlist-role cover-art-role comment-role
     podcast-role share-role video-conversion-role music-folder-id))
(sonic user-management/delete-user (username))
(sonic user-management/change-password (username password))

;;;; bookmark endpoints
(sonic bookmarks/get-bookmarks ())
(sonic bookmarks/create-bookmark (id position) (comment))
(sonic bookmarks/delete-bookmark (id))
(sonic bookmarks/get-play-queue ())
(sonic bookmarks/save-play-queue () (id current position))

;;;; media library scanning endpoints
(sonic media-library-scanning/get-scan-status ())
(sonic media-library-scanning/start-scan ())
