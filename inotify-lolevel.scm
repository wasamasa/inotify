(module inotify-lolevel
  (event? event-wd event-flags event-cookie event-name
   %init! %clean-up!
   %add-watch! %remove-watch!
   %next-events!)

  (import scheme)
  (cond-expand
   (chicken-4
    (import chicken foreign)
    (use srfi-18 lolevel))
   (chicken-5
    (import (chicken base))
    (import (chicken bitwise))
    (import (chicken blob))
    (import (chicken condition))
    (import (chicken foreign))
    (import (chicken format))
    (import (chicken memory))
    (import (srfi 18))
    (import miscmacros)))

  (include "inotify-lolevel-impl.scm"))
