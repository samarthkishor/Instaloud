#!usr/bin/env hy

(import [pyinstapaper.instapaper [Instapaper]]
        [google.cloud [texttospeech]]
        [pydub [AudioSegment]]
        json
        html2text
        os
        re)

(defn get-bookmarks []
  "Login to Instapaper and return a list of bookmarks"
  (with [f (open "credentials.json")]
    (setv credentials (.load json f))
    (setv client-id     (get credentials "client-id")
          client-secret (get credentials "client-secret")
          username      (get credentials "username")
          password      (get credentials "password"))
    (setv insta (Instapaper client-id client-secret))
    (.login insta username password)
    (return (.get_bookmarks insta))))

(defn bookmark->text [bookmark]
  "Take a bookmark (html) and format it to plain text"
  (setv text-format (.HTML2Text html2text))
  (setv (. text-format ignore_links) True
        (. text-format unicode_snob) True
        (. text-format images_to_alt) True
        (. text-format ignore_emphasis) True
        (. text-format re_unescape) True
        (. text-format re_md_chars_matcher_all) False)
  (.handle text-format (-> (.get_text bookmark)
                          (get "data")
                          (.decode "utf-8")
                          (.strip))))

(defn slugify [filename]
  "Modified version of the function from django.utils.text.py.
   Convert spaces to hyphens.
   Remove characters that aren't alphanumerics, underscores, or hyphens.
   Convert to lowercase.
   Strip leading and trailing whitespace."
  (setv value (-> (.sub re r"[^\w\s-]" "" filename) (.strip) (.lower)))
  (.sub re r"[-\s]+" "-" value))

(defn format-title [bookmark]
  "Slugify the bookmark's title"
  (slugify (. bookmark title)))

;; This function doesn't work for this application since it sometimes splits the word
;; at the end of the list but I'm too lazy to remove it so here it is
;; (defn pretty-but-broken-split-text [bookmark]
;;   "Split the bookmark text into a list of strings that are 4000 characters.
;;    The last string in the list may be less than 4000 characters."
;;   (lfor char-tuple
;;         (-> (list (bookmark->text bookmark)) (partition 4000 :fillvalue "") (list))
;;         (.join "" char-tuple)))

(defn split-text [bookmark]
  "Split the bookmark text into a list of strings that are 4000 characters
   without breaking up words. The last string in the list may be less than
   4000 characters."
  (setv word-list (.split (bookmark->text bookmark))
        new-list []
        char-count 0
        text "")
  (for [word word-list]
    (setv char-count (+ char-count (len word)))
    (if (> char-count 4000)
        (do
          (.append new-list text)
          (setv char-count 0
                text (+ word " ")))
        (setv text (+ text word " "))))
  (do (.append new-list text) new-list))

(defn save-file [bookmark]
  "Save the text from a bookmark to a file. Split it into multiple files
   (i.e. `title1.txt`, `title2.txt`, etc.) if the text contains more than
   4000 characters."
  (for [[file-number file-content] (enumerate (split-text bookmark))]
    (with [f (open
               (+ "text/" (format-title bookmark) "-" (str (inc file-number)) ".txt")
               "w")]
      (.write f file-content))))

(defn text->audio [text filename]
  "Convert a string into an mp3 file given the string and the file name"
  ;; set the $GOOGLE_APPLICATION_CREDENTIALS environment variable to authenticate
  (with [f (open "credentials.json" "r")]
    (setv credentials (.load json f))
    (setv (get (. os environ) "GOOGLE_APPLICATION_CREDENTIALS")
          (get credentials "google-auth-path")))
  (setv client (.TextToSpeechClient texttospeech)
        input-text (.SynthesisInput (. texttospeech types) :text text)
        voice (.VoiceSelectionParams (. texttospeech types)
                                     :language_code "en-US"
                                     :ssml_gender (. texttospeech
                                                     enums
                                                     SsmlVoiceGender
                                                     FEMALE))
        audio-config (.AudioConfig (. texttospeech types)
                                   :audio_encoding (. texttospeech
                                                      enums
                                                      AudioEncoding
                                                      MP3))
        response (.synthesize_speech client input-text voice audio-config))
  (with [out (open (+ "audio/" filename ".mp3") "wb")]
    (.write out (. response audio_content))
    (print "audio written to file" (+ filename ".mp3"))))

(defn concat-audio [filename-list]
  "Concatenate the audio files generated from a bookmark into a single mp3 file"
  (reduce + (map (fn [filename] (.from_mp3 AudioSegment (+ "audio/" filename)))
                 filename-list)))

(defn bookmark->audio [bookmark]
  "Convert a bookmark into a mp3 file"
  (setv text-list (split-text bookmark))
  (setv title (format-title bookmark))
  (setv filename-list (list (map (fn [file-number]
                                   (+ title "-" (str file-number) ".mp3"))
                                 (->> (len text-list) (inc) (range 1) (list)))))
  (for [[file-number file-content] (enumerate text-list)]
    (setv filename (+ title "-" (str (inc file-number))))
    (text->audio file-content filename))
  (.export (concat-audio filename-list) (+ "audio/" title ".mp3") :format "mp3")
  (print "audio exported to file" (+ title ".mp3")))

(defmain [&rest args]
  (for [bookmark (get-bookmarks)]
    ;; (save-file bookmark)
    (bookmark->audio bookmark)))
