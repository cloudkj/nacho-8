(declare (unit input))

(use (prefix allegro al:) srfi-18)

;;; Input

(define *down* 1)
(define *up* 0)

;; State of 16-key hexadecimal keyboard, where each key maps to a value of 1 or
;; 0, corresponding to the down and up positions, respectively
(define *keyboard* (make-u8vector 16 0))

;; Association list of valid keycodes to corresponding hexadecimal values
(define *keycode-values*
  (let ((keys '(zero one two three four five six seven eight nine a b c d e f)))
    (let loop ((i 0) (k keys) (vals (list)))
      (if (null? k)
          vals
          (let ((keycode (al:key->int (car k))))
            (loop (+ i 1) (cdr k) (cons (cons keycode i) vals)))))))

;;; Input functions

;; Returns #t if the hexadecimal key is in the down position, #f otherwise
(define (key-down? key)
  (= (u8vector-ref *keyboard* key) *down*))

;; Returns #t if the hexadecimal key is in the up position, #f otherwise
(define (key-up? key)
  (= (u8vector-ref *keyboard* key) *up*))

;; Returns the hexadecimal value and updates the keyboard state for a valid key
;; down event, or #f otherwise
(define (handle-key-down event)
  (let* ((keycode (al:keyboard-event-keycode event))
         (mapping (assoc keycode *keycode-values*)))
    (if mapping
        (let ((value (cdr mapping)))
          (u8vector-set! *keyboard* value *down*)
          value)
        #f)))

;; Returns the hexadecimal value and updates the keyboard state for a valid key
;; up event, or #f otherwise
(define (handle-key-up event)
  (let* ((keycode (al:keyboard-event-keycode event))
         (mapping (assoc keycode *keycode-values*)))
    (if mapping
        (let ((value (cdr mapping)))
          (u8vector-set! *keyboard* value *up*)
          value)
        #f)))
