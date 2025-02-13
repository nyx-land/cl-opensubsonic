(in-package :sone)

(defvar *salt-len* 24)
(defconstant +charset+ "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")

(defstruct sonic-login
  uri user pass (client "sone"))

(defun split-api (name)
  (subseq name (1+ (position #\/ name))))

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

(defmacro sonic (name (&rest lambda-list) method
                 &key (parameters nil))
  `(defun ,(read-from-string
            (string-upcase (symbol-name name)))
       ,(push 'login lambda-list)
     (gethash "subsonic-response"
              (com.inuoe.jzon:parse
               (drakma:http-request
                (quri:render-uri
                 (with-slots (uri user pass client) login
                   (sonic-uri uri user pass client
                              ,(split-api (symbol-name name)))))
                :method ,method
                :parameters ,parameters)))))

;;;; system methods
(sonic |system/ping| () :get)
(sonic |system/getLicense| () :get)
(sonic |system/getOpenSubsonicExtensions| () :get)
(sonic |system/tokenInfo| () :get)

;;;; browsing methods
(sonic |browsing/getMusicFolders| () :get)
;; browsing/getIndexes
;; browsing/getMusicDirectory
(sonic browsing/getGenres () :get)
;; browsing/getArtists
;; browsing/getArtist
;; browsing/getAlbum
;; browsing/getSong
(sonic browsing/getVideos () :get)
;; browsing/getVideoInfo
;; browsing/getArtistInfo
;; browsing/getArtistInfo2
;; browsing/getAlbumInfo
;; browsing/getAlbumInfo2
;; browsing/getSimilarSongs
;; browsing/getSimilarSongs2
;; browsing/getTopSongs

;;;; album/song lists
;; album-song-lists/getAlbumList
;; album-song-lists/getAlbumList2
;; album-song-lists/getRandomSongs
;; album-song-lists/getSongsByGenre
(sonic album-song-lists/getNowPlaying () :get)
;; album-song-lists/getStarred
;; album-song-lists/getStarred2

;;;; searching
;; searching/search
;; searching/search2
;; searching/search3

;;;; playlists
;; playlists/getPlaylists
;; playlists/getPlaylist
;; playlists/createPlaylist
;; playlists/updatePlaylist
;; playlists/deletePlaylist

;;;; media retrieval
;; media-retrieval/stream
;; media-retrieval/download
;; media-retrieval/hls
;; media-retrieval/getCaptions
;; media-retrieval/getCoverArt
;; media-retrieval/getLyrics
;; media-retrieval/getAvatar
;; media-retrieval/getLyricsBySongId

;;;; media annotation
;; media-annotation/star
;; media-annotation/unstar
;; media-annotation/setRating
;; media-annotation/scrobble

;;;; sharing
;; sharing/getShares
;; sharing/createShare
;; sharing/updateShare
;; sharing/deleteShare

;;;; podcast
;; podcast/getPodcasts
;; podcast/getNewestPodcasts
;; podcast/refreshPodcasts
;; podcast/createPodcastChannel
;; podcast/deletePodcastChannel
;; podcast/deletePodcastEpisode
;; podcast/downloadPodcastEpisode

;;;; jukebox
;; jukebox/jukeboxControl

;;;; internet radio
;; internet-;;radio/;;getInternetRadioStations
;; internet-;;radio/;;createInternetRadioStation
;; internet-;;radio/;;updateInternetRadioStation
;; internet-;;radio/;;deleteInternetRadioStation

;;;; chat
;; chat/getChatMessages
;; chat/addChatMessage

;;;; user management
;; user-management/getUser
;; user-management/getUsers
;; user-management/createUser
;; user-management/updateUser
;; user-management/deleteUser
;; user-management/changePassword

;;;; bookmarks
;; bookmarks/getBookmarks
;; bookmarks/createBookmark
;; bookmarks/deleteBookmark
;; bookmarks/getPlayQueue
;; bookmarks/savePlayQueue

;;;; media library scanning
;; media-library-scanning/getScanStatus
;; media-library-scanning/startScan
