(import scheme)
(cond-expand
 (chicken-4
  (use inotify utils))
 (chicken-5
  (import (chicken base))
  (import (chicken format))
  (import (chicken io))
  (import (chicken process))
  (import inotify)))

(system "touch ./config")

(init!)
(on-exit clean-up!)

(add-watch! "./config" '(close-write delete-self))

(let loop ()
  (let* ((event (next-event!))
         (flags (event-flags event)))
    (cond
     ((equal? flags '(close-write))
      (printf "File contents: ~s\n"
              (call-with-input-file "./config" (cut read-string #f <>)))
      (loop))
     ((equal? flags '(delete-self))
      (printf "Monitored file got removed, quitting\n")))))
