(module inotify
  (%fd init! clean-up!
   add-watch! remove-watch!
   next-events! %events next-event!
   event-wd event-flags event-cookie event-name
   max-queued-events max-user-watches max-user-watches)

(import chicken scheme foreign)

(use extras srfi-18 lolevel data-structures)

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

(define strerror (foreign-lambda c-string "strerror" int))

(define inotify_init
  (foreign-lambda* int ()
    "int ret = inotify_init();"
    "C_return(ret < 0 ? -errno : ret);"))

(define inotify_add_watch
  (foreign-lambda* int ((int fd) (c-string path) (int mask))
    "int ret = inotify_add_watch(fd, path, mask);"
    "C_return(ret < 0 ? -errno : ret);"))

(define inotify_rm_watch
  (foreign-lambda* int ((int fd) (int wd))
    "int ret = inotify_rm_watch(fd, wd);"
    "C_return(ret < 0 ? -errno : ret);"))

(define close
  (foreign-lambda* int ((int fd))
    "int ret = close(fd);"
    "C_return(ret < 0 ? -errno : ret);"))

(define inotify_next_events
  (foreign-lambda* int ((blob buf) (int buf_len) (pointer-vector events) (int fd))
    ;; adapted from inotify(7)
    "struct inotify_event *event;"
    "ssize_t len;"
    "char *ptr;"
    "int i;"

    "len = read(fd, (char *) buf, buf_len);"
    "if (len <= 0)"
    "  C_return(-errno);"

    "for (ptr = (char *) buf, i = 0; ptr < (char *) buf + len;"
    "     ptr += sizeof(struct inotify_event) + event->len, ++i) {"
    "  event = (struct inotify_event *) ptr;"
    "  events[i] = event;"
    "}"

    "C_return(i);"))

;;; records

(define-record event wd flags cookie name)
(define-record-printer (event e out)
  (if (event-name e)
      (fprintf out "#<event ~a: ~s ~s>" (event-wd e) (event-flags e) (event-name e))
      (fprintf out "#<event ~a: ~s>" (event-wd e) (event-flags e))))

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

(define (errno-error errno location)
  (make-error (strerror errno) location '(i/o file)))

(define (event-flag-error flag location)
  (make-error (format "Unknown event flag ~a" flag) location '(match)))

(define (usage-error message location)
  (make-error message location '(usage)))

;;; foreign constants

(define IN_ACCESS (foreign-value "IN_ACCESS" uint32))
(define IN_MODIFY (foreign-value "IN_MODIFY" uint32))
(define IN_ATTRIB (foreign-value "IN_ATTRIB" uint32))
(define IN_CLOSE_WRITE (foreign-value "IN_CLOSE_WRITE" uint32))
(define IN_CLOSE_NOWRITE (foreign-value "IN_CLOSE_NOWRITE" uint32))
(define IN_OPEN (foreign-value "IN_OPEN" uint32))
(define IN_MOVED_FROM (foreign-value "IN_MOVED_FROM" uint32))
(define IN_MOVED_TO (foreign-value "IN_MOVED_TO" uint32))
(define IN_CREATE (foreign-value "IN_CREATE" uint32))
(define IN_DELETE (foreign-value "IN_DELETE" uint32))
(define IN_DELETE_SELF (foreign-value "IN_DELETE_SELF" uint32))
(define IN_MOVE_SELF (foreign-value "IN_MOVE_SELF" uint32))

(define IN_UNMOUNT (foreign-value "IN_UNMOUNT" uint32))
(define IN_Q_OVERFLOW (foreign-value "IN_Q_OVERFLOW" uint32))
(define IN_IGNORED (foreign-value "IN_IGNORED" uint32))

(define IN_CLOSE (foreign-value "IN_CLOSE" uint32))
(define IN_MOVE (foreign-value "IN_MOVE" uint32))

(define IN_ONLYDIR (foreign-value "IN_ONLYDIR" uint32))
(define IN_DONT_FOLLOW (foreign-value "IN_DONT_FOLLOW" uint32))
(define IN_EXCL_UNLINK (foreign-value "IN_EXCL_UNLINK" uint32))
(define IN_MASK_ADD (foreign-value "IN_MASK_ADD" uint32))
(define IN_ISDIR (foreign-value "IN_ISDIR" uint32))
(define IN_ONESHOT (foreign-value "IN_ONESHOT" uint32))

(define IN_ALL_EVENTS (foreign-value "IN_ALL_EVENTS" uint32))

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
            (abort (errno-error (- ret) 'init!))
            (%fd ret))
        #t)
      #f))

(define (clean-up!)
  (if (%fd)
      (let ((ret (inotify_init)))
        (if (< ret 0)
            (abort (errno-error (- ret) 'clean-up!))
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
        (abort (errno-error (- ret) 'add-watch!))
        ret)))

(define (remove-watch! wd)
  (ensure-initialized! 'remove-watch!)
  (let ((ret (inotify_rm_watch (%fd) wd)))
    (if (< ret 0)
        (abort (errno-error (- ret) 'remove-watch!))
        #t)))

(define %events-buffer-size 4096)
(define %min-event-size (foreign-value "sizeof(struct inotify_event)" int))
(define %max-event-count (/ %events-buffer-size %min-event-size))
(define %events-buffer (make-blob %events-buffer-size))
(define %events-pointers (make-pointer-vector %max-event-count))

(define (next-events!)
  (ensure-initialized! 'next-events!)
  (thread-wait-for-i/o! (%fd))
  (let* ((ret (inotify_next_events %events-buffer %events-buffer-size
                                   %events-pointers (%fd))))
    (if (< ret 0)
        (abort (errno-error (- ret) 'next-events!))
        (reverse
         (let loop ((i 0) (acc '()))
           (if (< i ret)
               (let* ((pointer (pointer-vector-ref %events-pointers i))
                      (event (pointer->event pointer)))
                 (loop (add1 i) (cons event acc)))
               acc))))))

(define %events (make-parameter (make-queue)))

(define (next-event!)
  (ensure-initialized! 'next-event!)
  (when (queue-empty? (%events))
    (queue-push-back-list! (%events) (next-events!)))
  (queue-remove! (%events)))

(define (proc-file->number path)
  (string->number (with-input-from-file path read-line)))

(define (max-queued-events)
  (proc-file->number "/proc/sys/fs/inotify/max_queued_events"))

(define (max-user-instances)
  (proc-file->number "/proc/sys/fs/inotify/max_user_instances"))

(define (max-user-watches)
  (proc-file->number "/proc/sys/fs/inotify/max_user_watches"))

)
