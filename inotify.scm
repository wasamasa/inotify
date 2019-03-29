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

(include "inotify-impl.scm")

)
