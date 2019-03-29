(module inotify
  (%fd init! clean-up!
   add-watch! remove-watch!
   add-watch-recursively!
   wd->path wd-list path-list
   next-events! %events next-event!
   event? event-wd event-flags event-cookie event-name
   event->pathname
   max-queued-events max-user-instances max-user-watches)

  (import scheme)
  (cond-expand
   (chicken-4
    (import chicken foreign)
    (use inotify-lolevel extras srfi-18 srfi-69
         lolevel posix data-structures files))
   (chicken-5
    (import (chicken base))
    (import (chicken condition))
    (import (chicken file))
    (import (chicken file posix))
    (import (chicken foreign))
    (import (chicken io))
    (import (chicken pathname))
    (import (srfi 18))
    (import (srfi 69))
    (import queues)
    (import inotify-lolevel)))

  (include "inotify-impl.scm"))
