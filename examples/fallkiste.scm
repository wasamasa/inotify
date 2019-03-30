(import scheme)
(cond-expand
 (chicken-4
  (use posix inotify scsh-process))
 (chicken-5
  (import (chicken base))
  (import (chicken pathname))
  (import (chicken process-context))
  (import inotify)
  (import scsh-process)))

(define local-path (make-pathname (get-environment-variable "HOME") "fallkiste/"))
(define remote-path "lab:/srv/http/fallkiste")

(define watch-flags '(create close-write delete move))

(init!)
(on-exit clean-up!)
(add-watch-recursively! local-path watch-flags)

(let loop ()
  (let* ((event (next-event!))
         (flags (event-flags event)))
    (cond
     ;; file created / moved / deleted
     ((= (length flags) 1)
      ;; file creation triggers '(create) and '(close-write), so let's
      ;; just ignore '(create) to not trigger rsync twice
      (when (not (equal? flags '(create)))
        (run (rsync "-auvzz" "--chmod=a+r" "--delete" "-e" "ssh -p9000"
                    ,local-path ,remote-path))))
     ;; new directory, without a watch yet
     ((equal? flags '(isdir create))
      (add-watch! (event->pathname event) watch-flags))))
  (loop))
