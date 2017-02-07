;; as seen on http://www.upyum.com/en/post/4.xhtml

;; tailf.scm
(use inotify utils)

(init!)
(on-exit clean-up!)

(define path (car (command-line-arguments)))
(add-watch! path '(modify))

(with-input-from-file path
  (lambda ()
    (let loop ()
      (display (read-all))
      (flush-output)
      (next-event!)
      (set-file-position! (current-input-port) 0 seek/cur)
      (loop))))
