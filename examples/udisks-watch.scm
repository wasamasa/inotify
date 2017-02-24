(use irregex inotify scsh-process srfi-18)

(init!)
(on-exit clean-up!)
(add-watch! "/dev" '(create))

(define re '(or (: "sd" alpha (? num))
                (: "mmcblk" num "p" num)))

(define timeout 3)

(let loop ()
  (let* ((event (next-event!))
         (device (event-name event))
         (path (format "/dev/~a" device)))
    (when (irregex-match re device)
      (printf "Attempting to mount ~a at ~a\n" device path)
      (thread-sleep! timeout)
      (run (udisksctl mount "--block-device" ,path "--no-user-interaction"))))
  (loop))
