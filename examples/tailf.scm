;; adapted from http://www.upyum.com/en/post/4.xhtml

(import scheme)
(cond-expand
 (chicken-4
  (use inotify utils))
 (chicken-5
  (import (chicken base))
  (import (chicken file posix))
  (import (chicken io))
  (import (chicken process-context))
  (import inotify)))

(init!)
(on-exit clean-up!)

(define path (car (command-line-arguments)))
(add-watch! path '(modify))

(with-input-from-file path
  (lambda ()
    (let loop ()
      (display (read-string #f))
      (flush-output)
      (next-event!)
      (set-file-position! (current-input-port) 0 seek/cur)
      (loop))))
