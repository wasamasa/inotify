(module inotify-lolevel
  (event? event-wd event-flags event-cookie event-name
   %init! %clean-up!
   %add-watch! %remove-watch!
   %next-events!)

(import chicken scheme foreign)

(use srfi-18 lolevel)

(include "inotify-lolevel-impl.scm")

)
