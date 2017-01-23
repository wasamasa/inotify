(use inotify utils)

(system "touch ./config")

(init!)
(on-exit clean-up!)

(add-watch! "./config" '(close-write delete-self))

(let loop ()
  (let* ((event (next-event!))
         (flags (event-flags event)))
    (cond
     ((equal? flags '(close-write))
      (printf "File contents: ~s\n" (read-all "./config"))
      (loop))
     ((equal? flags '(delete-self))
      (printf "Monitored file got removed, quitting\n")))))
