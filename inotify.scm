(module inotify
  (%fd init! clean-up!
   add-watch! remove-watch!
   add-watch-recursively!
   wd->path wd-list path-list
   next-events! %events next-event!
   event? event-wd event-flags event-cookie event-name
   event->pathname
   max-queued-events max-user-instances max-user-watches)

(import chicken scheme foreign)

(use inotify-lolevel extras srfi-18 srfi-69 lolevel posix data-structures files)

;;; errors

(define (make-error message location types)
  (let ((conditions (map make-property-condition types))
        (condition (make-property-condition 'exn
                                            'message message
                                            'location location)))
    (apply make-composite-condition condition conditions)))

(define (usage-error message location)
  (make-error message location '(usage)))

;;; API

(define %fd (make-parameter #f))

(define (init!)
  (if (not (%fd))
      (let ((fd (%init!)))
        (%fd fd)
        #t)
      #f))

(define (clean-up!)
  (if (%fd)
      (let ((fd (%clean-up! (%fd))))
        (%fd #f)
        #t)
      #f))

(define (ensure-initialized! location)
  (when (not (%fd))
    (abort (usage-error "init! hasn't been called yet" location))))

(define %watch->path-table (make-hash-table = number-hash))

(define (add-watch! path flags)
  (ensure-initialized! 'add-watch!)
  (let ((wd (%add-watch! (%fd) path flags)))
    (hash-table-set! %watch->path-table wd path)
    wd))

(define (remove-watch! wd)
  (ensure-initialized! 'remove-watch!)
  (%remove-watch! (%fd) wd)
  (hash-table-delete! %watch->path-table wd)
  #t)

(define (add-watch-recursively! path flags)
  (ensure-initialized! 'add-watch-recursively!)
  (when (not (directory? path))
    (abort (usage-error "path argument must be a directory"
                        'add-watch-recursively!)))
  (let ((wd (add-watch! path flags)))
    (find-files path test: directory?
                action: (lambda (x acc)
                          (let ((wd (add-watch! x flags)))
                            (cons wd acc)))
                seed: (list wd))))

(define (wd->path wd) (hash-table-ref %watch->path-table wd))
;; NOTE: I'm deliberately omitting path->wd as I doubt it's needed
(define (wd-list) (hash-table-keys %watch->path-table))
(define (path-list) (hash-table-values %watch->path-table))

(define (next-events!)
  (ensure-initialized! 'next-events!)
  (%next-events! (%fd)))

(define %events (make-parameter (make-queue)))

(define (next-event!)
  (ensure-initialized! 'next-event!)
  (when (queue-empty? (%events))
    (queue-push-back-list! (%events) (next-events!)))
  (queue-remove! (%events)))

(define (event->pathname event)
  (let ((path (wd->path (event-wd event)))
        (name (event-name event)))
    (if name
        (make-pathname path name)
        path)))

(define (proc-file->number path)
  (string->number (with-input-from-file path read-line)))

(define (max-queued-events)
  (proc-file->number "/proc/sys/fs/inotify/max_queued_events"))

(define (max-user-instances)
  (proc-file->number "/proc/sys/fs/inotify/max_user_instances"))

(define (max-user-watches)
  (proc-file->number "/proc/sys/fs/inotify/max_user_watches"))

)
