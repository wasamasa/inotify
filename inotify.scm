(module inotify
  (%fd init! clean-up!
   add-watch!
   next-event!
   event-wd event-flags event-cookie event-name
   max-queued-events max-user-watches max-user-watches)

(import chicken scheme foreign)

(use extras srfi-18 lolevel)

#>
#include <errno.h>
#include <limits.h>
#include <sys/inotify.h>
<#

;;; typedefs

(define-foreign-type inotify_event* (nonnull-c-pointer (struct "inotify_event")))
(define-foreign-type nullable-inotify_event* (c-pointer (struct "inotify_event")))
(define-foreign-type uint32 unsigned-int32)

;;; foreign functions

(define inotify_init (foreign-lambda int "inotify_init"))
(define inotify_add_watch (foreign-lambda int "inotify_add_watch" int c-string int))
(define close (foreign-lambda int "close" int))

(define errno->string (foreign-lambda* c-string () "C_return(strerror(errno));"))
(define inotify_next_event
  (foreign-lambda* nullable-inotify_event* ((inotify_event* event) (int fd))
    "int EVENT_SIZE = sizeof(struct inotify_event);"
    "int BUF_LEN = EVENT_SIZE + NAME_MAX + 1;"

    "int length = read(fd, event, BUF_LEN);"
    ;; generic error
    "if (length <= 0)"
    "  C_return(NULL);"
    ;; incomplete read, shouldn't happen
    "else if (length < EVENT_SIZE + event->len)"
    "  C_return(NULL);"

    "C_return(event);"))

;;; records

(define-record event wd flags cookie name)
(define-record-printer (event e out)
  (fprintf out "#<event ~a: ~s>" (event-wd e) (event-flags e)))

(define (pointer->event ptr)
  (let ((wd ((foreign-lambda* int ((inotify_event* event))
               "C_return(event->wd);") ptr))
        (mask ((foreign-lambda* uint32 ((inotify_event* event))
                 "C_return(event->mask);") ptr))
        (cookie ((foreign-lambda* uint32 ((inotify_event* event))
                   "C_return(event->cookie);") ptr))
        (name ((foreign-lambda* c-string ((inotify_event* event))
                 "C_return(event->len ? event->name : NULL);") ptr)))
    (make-event wd (int->event-flags mask) cookie name)))

;;; errors

(define (make-error message location types)
  (let ((conditions (map make-property-condition types))
        (condition (make-property-condition 'exn
                                            'message message
                                            'location location)))
    (apply make-composite-condition condition conditions)))

(define (errno-error location)
  (make-error (errno->string) location '(i/o file)))

(define (event-flag-error flag location)
  (make-error (format "Unknown event flag ~a" flag) location '(match)))

(define (usage-error message location)
  (make-error message location '(usage)))

;;; foreign constants

(define IN_ACCESS (foreign-value "IN_ACCESS" int))
(define IN_MODIFY (foreign-value "IN_MODIFY" int))
(define IN_ATTRIB (foreign-value "IN_ATTRIB" int))
(define IN_CLOSE_WRITE (foreign-value "IN_CLOSE_WRITE" int))
(define IN_CLOSE_NOWRITE (foreign-value "IN_CLOSE_NOWRITE" int))
(define IN_OPEN (foreign-value "IN_OPEN" int))
(define IN_MOVED_FROM (foreign-value "IN_MOVED_FROM" int))
(define IN_MOVED_TO (foreign-value "IN_MOVED_TO" int))
(define IN_CREATE (foreign-value "IN_CREATE" int))
(define IN_DELETE (foreign-value "IN_DELETE" int))
(define IN_DELETE_SELF (foreign-value "IN_DELETE_SELF" int))
(define IN_MOVE_SELF (foreign-value "IN_MOVE_SELF" int))

(define IN_UNMOUNT (foreign-value "IN_UNMOUNT" int))
(define IN_Q_OVERFLOW (foreign-value "IN_Q_OVERFLOW" int))
(define IN_IGNORED (foreign-value "IN_IGNORED" int))

(define IN_CLOSE (foreign-value "IN_CLOSE" int))
(define IN_MOVE (foreign-value "IN_MOVE" int))

(define IN_ONLYDIR (foreign-value "IN_ONLYDIR" int))
(define IN_DONT_FOLLOW (foreign-value "IN_DONT_FOLLOW" int))
(define IN_EXCL_UNLINK (foreign-value "IN_EXCL_UNLINK" int))
(define IN_MASK_ADD (foreign-value "IN_MASK_ADD" int))
(define IN_ISDIR (foreign-value "IN_ISDIR" int))
(define IN_ONESHOT (foreign-value "IN_ONESHOT" int))

(define IN_ALL_EVENTS (foreign-value "IN_ALL_EVENTS" int))

(define (event-flag->int flag location)
  (case flag
    ((access) IN_ACCESS)
    ((modify) IN_MODIFY)
    ((attrib) IN_ATTRIB)
    ((close-write) IN_CLOSE_WRITE)
    ((close-nowrite) IN_CLOSE_NOWRITE)
    ((open) IN_OPEN)
    ((moved-from) IN_MOVED_FROM)
    ((moved-to) IN_MOVED_TO)
    ((create) IN_CREATE)
    ((delete) IN_DELETE)
    ((delete-self) IN_DELETE_SELF)
    ((move-self) IN_MOVE_SELF)

    ((unmount) IN_UNMOUNT)
    ((q-overflow) IN_Q_OVERFLOW)
    ((ignored) IN_IGNORED)

    ((close) IN_CLOSE)
    ((move) IN_MOVE)

    ((onlydir) IN_ONLYDIR)
    ((dont-follow) IN_DONT_FOLLOW)
    ((excl-unlink) IN_EXCL_UNLINK)
    ((mask-add) IN_MASK_ADD)
    ((isdir) IN_ISDIR)
    ((oneshot) IN_ONESHOT)

    ((all-events) IN_ALL_EVENTS)

    (else (abort (event-flag-error flag location)))))

(define (event-flags->int flags location)
  (apply bitwise-ior (map (cut event-flag->int <> location) flags)))

(define (int->event-flag value)
  (select value
    ((0) #f)
    ((IN_ACCESS) 'access)
    ((IN_MODIFY) 'modify)
    ((IN_ATTRIB) 'attrib)
    ((IN_CLOSE_WRITE) 'close-write)
    ((IN_CLOSE_NOWRITE) 'close-nowrite)
    ((IN_OPEN) 'open)
    ((IN_MOVED_FROM) 'moved-from)
    ((IN_MOVED_TO) 'moved-to)
    ((IN_CREATE) 'create)
    ((IN_DELETE) 'delete)
    ((IN_DELETE_SELF) 'delete-self)
    ((IN_MOVE_SELF) 'move-self)

    ((IN_UNMOUNT) 'unmount)
    ((IN_Q_OVERFLOW) 'q-overflow)
    ((IN_IGNORED) 'ignored)

    ((IN_CLOSE) 'close)
    ((IN_MOVE) 'move)

    ((IN_ONLYDIR) 'onlydir)
    ((IN_DONT_FOLLOW) 'dont-follow)
    ((IN_EXCL_UNLINK) 'excl-unlink)
    ((IN_MASK_ADD) 'mask-add)
    ((IN_ISDIR) 'isdir)
    ((IN_ONESHOT) 'oneshot)

    ((IN_ALL_EVENTS) 'all-events)))

(define (int->event-flags value)
  (let loop ((flag 1) (events '()))
    (if (<= flag value)
        (let ((match? (not (zero? (bitwise-and value flag)))))
          (if match?
              (loop (* flag 2) (cons (int->event-flag flag) events))
              (loop (* flag 2) events)))
        events)))

;;; API

(define %fd (make-parameter #f))

(define (init!)
  (if (not (%fd))
      (let ((ret (inotify_init)))
        (if (< ret 0)
            (abort (errno-error 'init!))
            (%fd ret))
        #t)
      #f))

(define (clean-up!)
  (if (%fd)
      (let ((ret (inotify_init)))
        (if (< ret 0)
            (abort (errno-error 'clean-up!))
            (%fd #f))
        #t)
      #f))

(define (ensure-initialized! location)
  (when (not (%fd))
    (abort (usage-error "init! hasn't been called yet" location))))

(define (add-watch! path flags)
  (ensure-initialized! 'add-watch!)
  (let* ((mask (event-flags->int flags 'add-watch!))
         (ret (inotify_add_watch (%fd) path mask)))
    (if (< ret 0)
        (abort (errno-error 'add-watch!))
        ret)))

(define event-buffer-size (foreign-value "sizeof(struct inotify_event) + NAME_MAX + 1" int))
(define event-buffer-pointer (make-locative (make-blob event-buffer-size)))

(define (next-event!)
  (ensure-initialized! 'next-event!)
  (thread-wait-for-i/o! (%fd))
  (let ((ret (inotify_next_event event-buffer-pointer (%fd))))
    (if (not ret)
        (abort (errno-error 'next-event!))
        (pointer->event ret))))

(define (proc-file->number path)
  (string->number (with-input-from-file path read-line)))

(define (max-queued-events)
  (proc-file->number "/proc/sys/fs/inotify/max_queued_events"))

(define (max-user-instances)
  (proc-file->number "/proc/sys/fs/inotify/max_user_instances"))

(define (max-user-watches)
  (proc-file->number "/proc/sys/fs/inotify/max_user_watches"))

)
