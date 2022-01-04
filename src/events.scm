(declare (unit events)
         (uses cpu input))

(use (prefix allegro al:))

;; The clock rate (the frequency at which operations are executed) also
;; determines the event handler timeout, or the amount of time spend waiting for
;; events on the event queue. For example, a clock rate of 500 hertz corresponds
;; to a timeout of 2 ms (1 / 500 = 0.002)
(define *clock-rate* 500)

;; Delay and sound timers both operate at 60 hertz
(define *timer-rate* 60)

;; Display refresh rate
(define *refresh-rate* 60)

;;; Event handling resources

(define *display*)
(define *event-queue*)
(define *timer*)

(define *refresh-timer*)

;; Initialize and register resources to enable event handling
(define (init-events)
  (al:init)
  (al:init-this '(keyboard primitives))

  ;; Display
  (al:new-display-flags-set!
   (al:combine-flags al:display-flag->int '(windowed resizable)))
  (set! *display*
        (al:make-display (* *cols-scale* *cols*) (* *rows-scale* *rows*)))

  ;; Timers
  (set! *timer* (al:make-timer (/ 1 *timer-rate*)))
  (set! *refresh-timer* (al:make-timer (/ 1 *refresh-rate*)))

  ;; Event queue
  (set! *event-queue* (al:make-event-queue))
  (al:event-queue-register-source! *event-queue*
                                   (al:keyboard-event-source))
  (al:event-queue-register-source! *event-queue*
                                   (al:display-event-source *display*))
  (al:event-queue-register-source! *event-queue*
                                   (al:timer-source *timer*))
  (al:event-queue-register-source! *event-queue*
                                   (al:timer-source *refresh-timer*))

  (al:timer-start! *timer*)
  (al:timer-start! *refresh-timer*))

;; Returns #t if display refreshes are timer based
(define (timer-based-refresh?)
  (al:timer-started? *refresh-timer*))

;; Handles a timer event
(define (handle-timer event)
  (let ((source (al:timer-event-source event)))
    (cond ((equal? source *timer*)
           (begin
             (if (> *DT* 0) (set! *DT* (- *DT* 1)))
             (if (> *ST* 0) (set! *ST* (- *ST* 1)))))
          ((equal? source *refresh-timer*)
           (refresh-display)))))

;; Blocks and waits for a valid keyboard input keydown event, then returns the
;; hexadecimal numeric value corresponding to the keycode
(define (await-valid-keydown-event)
  (let* ((event (al:make-event))
         (_ (al:event-queue-wait! *event-queue* event))
         (value (case (al:event-type event)
                  ((key-down) (handle-key-down event))
                  ((key-up) (begin (handle-key-up event) #f))
                  ((timer) (begin (handle-timer event) #f))
                  (else #f))))
    ;; Return the hexadecimal key value in the case of a valid keydown event,
    ;; or continue waiting for a valid event
    (or value (await-valid-keydown-event))))

;; Waits for the event queue to be non-empty for some amount of time, then
;; processes all events until the queue is empty
(define (process-events)
  (if (al:event-queue-timed-wait! *event-queue* #f (/ 1 *clock-rate*))
      (let loop ((event (al:make-event)))
        (if (al:event-queue-next! *event-queue* event)
            (let ((handler (case (al:event-type event)
                             ((key-down) handle-key-down)
                             ((key-up) handle-key-up)
                             ((timer) handle-timer)
                             (else #f))))
              (if handler (handler event))
              (loop (al:make-event)))))))
