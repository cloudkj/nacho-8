(declare (unit display))

(use ezxdisp)

;;; Display

(define *rows* 32)
(define *cols* 64)

;; The underlying pixels are a 64x32 bitmap represented by a 64-length vector of
;; 32-bit values, where each pair of 32-bit values represents a row of pixels.
(define *cols-per-value* 32)
(define *values-per-row* 2)
(define *pixels* (make-u32vector (* (/ *cols* *cols-per-value*) *rows*) 0))

;;; ezxdisp helpers

(define *ezx* (ezx-init *cols* *rows* ""))
(define *color-black* (make-ezx-color 0 0 0))
(define *color-white* (make-ezx-color 1 1 1))

(define (draw-pixel x y color)
  (ezx-point-2d *ezx* x y color))

(define (init-display)
  (ezx-set-background *ezx* *color-white*))

(define (refresh-display)
  (ezx-redraw *ezx*))

;;; Debugging helpers

(define (print-pixels)
  (let loop ((i 0))
    (if (< i (u32vector-length *pixels*))
        (begin
          (print (number->binary-string (u32vector-ref *pixels* i))
                 (number->binary-string (u32vector-ref *pixels* (+ i 1))))
          (loop (+ i 2))))))

;;; Display functions

;; Returns #t if the pixel was erased (i.e. 1 -> 0)
(define (update-pixel x y val)
  (let* ((index (if (< x *cols-per-value*)
                    (* *values-per-row* y)
                    (+ (* *values-per-row* y) 1)))
         (bits (u32vector-ref *pixels* index))
         (shift (- *cols-per-value* (modulo x *cols-per-value*) 1))
         (curr (if (= (bitwise-and (arithmetic-shift #x1 shift) bits) 0) 0 1))
         ;; New pixel value
         (new (bitwise-xor curr val))
         ;; Clear current bit
         (bits (bitwise-and bits (bitwise-not (arithmetic-shift #x1 shift)))))
    ;; Update pixel bitmap by setting new value in place of current bit
    (u32vector-set! *pixels* index
                    (bitwise-ior (arithmetic-shift new shift) bits))
    ;; Draw pixel
    (draw-pixel x y (if (= new 0) *color-white* *color-black*))
    ;; Current pixel was erased iff both are 1
    (and (= curr 1) (= val 1))))

;; Returns #t if any pixels were erased (i.e. 1 -> 0)
(define (update-pixels x y byte)
  (let loop ((i 0)
             (erased #f))
    (if (>= i 8)
        erased
        (let* ((mask (arithmetic-shift 1 (- 7 i)))
               (val (if (= (bitwise-and mask byte) 0) 0 1))
               (x (modulo (+ x i) *cols*))
               (result (update-pixel x y val)))
          (loop (+ i 1) (or erased result))))))
