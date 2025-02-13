(in-package :sone)

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

(defstruct sonic-login
  uri user pass (client "sone"))

(defun split-api (name)
  (subseq name (1+ (position #\/ name))))

(defun remove-kebab (sym)
  (let ((split (uiop:split-string
                (string-downcase (split-api (symbol-name sym)))
                :separator '(#\-))))
    (format nil "~a~{~:(~a~)~}"
            (car split) (cdr split))))

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

(defun sonic-uri (uri user pass &optional client path)
  (let* ((salt (gen-salt))
         (token (sonic-token pass salt)))
    (quri:merge-uris
     (quri:make-uri
      :path (format nil "/rest~@[/~a~]" path)
      :query `(("u" . ,user)
               ("t" . ,token)
               ("s" . ,salt)
               ("v" . "1.15.1")
               ("c" . ,client)
               ("f" . "json")))
     (quri:uri uri))))

(defun sonic-response (json)
  (let* ((response (gethash "subsonic-response" json))
         (status (gethash "status" response)))
    (if (equalp status "failed")
        (let ((error-info (gethash "error" response)))
          (error 'subsonic-error
                 :code (gethash "code" error-info)
                 :message (gethash "message" error-info)))
        response)))

(defmacro sonic (name (&rest lambda-list) method
                 &key (parameters nil))
  `(defun ,name
       ,(push 'login lambda-list)
     (sonic-response
      (com.inuoe.jzon:parse
       (drakma:http-request
        (quri:render-uri
         (with-slots (uri user pass client) login
           (sonic-uri uri user pass client
                      ,(remove-kebab name))))
        :method ,method
        :parameters ,parameters)))))

;;;; system endpoints
(sonic system/ping () :get)
(sonic system/get-license () :get)
(sonic system/get-open-subsonic-extensions () :get)
(sonic system/token-info () :get)

;;;; browsing endpoints
(sonic browsing/get-music-folders () :get)
;; browsing/get-indexes
;; browsing/get-music-directory
(sonic browsing/get-genres () :get)
;; browsing/get-artists
;; browsing/get-artist
;; browsing/get-album
;; browsing/get-song
(sonic browsing/get-videos () :get)
;; browsing/get-video-info
;; browsing/get-artist-info
;; browsing/get-artist-info-2
;; browsing/get-album-info
;; browsing/get-album-info-2
;; browsing/get-similar-songs
;; browsing/get-similar-songs-2
;; browsing/get-top-songs

;;;; album/song list endpoints
;; album-song-lists/get-album-list
;; album-song-lists/get-album-list-2
;; album-song-lists/get-random-songs
;; album-song-lists/get-songs-by-genre
(sonic album-song-lists/get-now-playing () :get)
;; album-song-lists/get-starred
;; album-song-lists/get-starred2

;;;; searching endpoints
;; searching/search
;; searching/search2
;; searching/search3

;;;; playlist endpoints
;; playlists/get-playlists
;; playlists/get-playlist
;; playlists/create-playlist
;; playlists/update-playlist
;; playlists/delete-playlist

;;;; media retrieval endpoints
;; media-retrieval/stream
;; media-retrieval/download
;; media-retrieval/hls
;; media-retrieval/get-captions
;; media-retrieval/get-cover-art
;; media-retrieval/get-lyrics
;; media-retrieval/get-avatar
;; media-retrieval/get-lyrics-by-song-id

;;;; media annotation endpoints
;; media-annotation/star
;; media-annotation/unstar
;; media-annotation/set-rating
;; media-annotation/scrobble

;;;; sharing endpoints
;; sharing/get-shares
;; sharing/create-share
;; sharing/update-share
;; sharing/delete-share

;;;; podcast endpoints
;; podcast/get-podcasts
;; podcast/get-newest-podcasts
;; podcast/refresh-podcasts
;; podcast/create-podcast-phannel
;; podcast/delete-podcast-channel
;; podcast/delete-podcast-episode
;; podcast/download-podcast-episode

;;;; jukebox endpoitsn
;; jukebox/jukebox-control

;;;; internet radio endpoints
;; internet-radio/get-internet-radio-stations
;; internet-radio/create-internet-radio-station
;; internet-radio/update-internet-radio-station
;; internet-radio/delete-internet-radio-station

;;;; chat endpoints
;; chat/get-chat-messages
;; chat/add-chat-message

;;;; user management endpoints
;; user-management/get-user
;; user-management/get-users
;; user-management/create-user
;; user-management/update-user
;; user-management/delete-user
;; user-management/change-password

;;;; bookmark endpoints
;; bookmarks/get-bookmarks
;; bookmarks/create-bookmark
;; bookmarks/delete-bookmark
;; bookmarks/get-play-queue
;; bookmarks/save-play-queue

;;;; media library scanning endpoints
(sonic media-library-scanning/get-scan-status () :get)
(sonic media-library-scanning/start-scan () :get)
