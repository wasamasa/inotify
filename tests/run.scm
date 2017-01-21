(use inotify test srfi-18 posix srfi-1)

(test-group "Basic API failures"
  (test #f (%fd))
  (test #t (init!))
  (test #f (init!))
  (test #t (number? (%fd)))
  (test #t (clean-up!))
  (test #f (clean-up!))
  (test #f (%fd)))

(test-group "Exceptional API failures"
  (%fd -1)
  (test-error (clean-up!))
  (%fd #f)

  (test-error (add-watch! "." '(all-events))) ; uninitialized
  (init!)
  (test-error (add-watch! "bogus" '(all-events)))
  (test-error (add-watch! "." '()))
  (test-error (add-watch! "." '(bogus)))
  (clean-up!)

  (init!) ; this test doesn't make any sense uninitialized
  (let ((wd (add-watch! "." '(all-events)))
        (bogus-wd 42))
    (test #t (not (= wd bogus-wd)))
    (test-error (remove-watch! bogus-wd))
    (test #t (remove-watch! wd)))
  (clean-up!)

  (test-error (next-event!))
  (test-error (next-events!)))

(test-group "/proc"
  (test #t (positive? (max-queued-events)))
  (test #t (positive? (max-user-instances)))
  (test #t (positive? (max-user-watches))))

(create-directory "test-data")
(init!)

(define events '())
(define bg-thread
  (thread-start!
   (make-thread
    (lambda ()
      (let loop ()
        (set! events (cons (next-event!) events))
        (loop))))))

(define (find-event type events)
  (find (lambda (event) (memv type (event-flags event))) events))

(define (find-events type events)
  (filter (lambda (event) (memv type (event-flags event))) events))

(define (dedup pred lis)
  (apply lset-adjoin pred '() lis))

(test-group "Recursive watches"
  (create-directory "test-data/foo")
  (create-directory "test-data/bar")
  (let ((wds (add-watch-recursively! "test-data" '(all-events))))
    (test 3 (length wds))
    (system "ls test-data")
    (thread-sleep! 0.1)
    (system "ls test-data/foo")
    (thread-sleep! 0.1)
    (system "ls test-data/bar")
    (thread-sleep! 0.1)
    (test #t (lset= = wds (dedup = (map event-wd events))))
    (set! events '())
    (for-each remove-watch! wds)
    (system "ls test-data")
    (thread-sleep! 0.1)
    (test #t (lset= eq? '(ignored) (dedup eq? (append-map event-flags events)))))
  (delete-directory "test-data/foo")
  (delete-directory "test-data/bar"))

(test-group "Watches"
  (let ((wd (add-watch! "test-data" '(all-events))))
    (test #t (number? wd))
    (test "test-data" (wd->path wd))
    (test (list wd) (wd-list))
    (test '("test-data") (path-list))))

(test-group "File creation"
  (set! events '())
  (system "touch test-data/foo")
  (thread-sleep! 0.1)
  (system "touch test-data/bar")
  (thread-sleep! 0.1)
  (let ((events (find-events 'create events)))
    (test 2 (length events))
    (test #t (lset= eq? '(create) (append-map event-flags events)))
    (test #t (lset= equal? '("foo" "bar") (map event-name events)))))

(test-group "File modification"
  (set! events '())
  (system "echo 123 > test-data/foo")
  (thread-sleep! 0.1)
  (let ((event (find-event 'modify events)))
    (test '(modify) (event-flags event))
    (test "foo" (event-name event))))

(test-group "File deletion"
  (set! events '())
  (system "rm test-data/bar")
  (thread-sleep! 0.1)
  (let ((event (find-event 'delete events)))
    (test '(delete) (event-flags event))
    (test "bar" (event-name event))))

(test-group "File movement"
  (set! events '())
  (system "mv test-data/foo test-data/bar")
  (thread-sleep! 0.1)
  (let ((from (find-event 'moved-from events))
        (to (find-event 'moved-to events)))
    (test '(moved-from) (event-flags from))
    (test '(moved-to) (event-flags to))
    (test "foo" (event-name from))
    (test "bar" (event-name to))
    (test (event-cookie from) (event-cookie to))))

(test-group "Reconstructing paths"
  (set! events '())
  (let ((wd (add-watch! "test-data/bar" '(all-events))))
    (system "touch test-data/bar")
    (thread-sleep! 0.1)
    (let* ((events (find-events 'attrib events))
           (event1 (car events))
           (event2 (cadr events))
           (file-event (if (event-name event1) event2 event1))
           (dir-event (if (event-name event1) event1 event2)))
      (test 2 (length events))

      (test "test-data" (wd->path (event-wd dir-event)))
      (test "bar" (event-name dir-event))
      (test "test-data/bar" (event->pathname dir-event))

      (test "test-data/bar" (wd->path (event-wd file-event)))
      (test #f (event-name file-event))
      (test "test-data/bar" (event->pathname file-event)))))

(delete-directory "test-data" #t)
(clean-up!)
(test-exit)
