(declare (unit display)
         (uses util))

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

(define *ezx*)

(define *rows-scale* 1)
(define *cols-scale* 1)

(define *color-black* (make-ezx-color 0 0 0))
(define *color-white* (make-ezx-color 1 1 1))

(define (draw-pixel x y color)
  (let ((x0 (* *cols-scale* x))
        (y0 (* *rows-scale* y)))
    (ezx-fillrect-2d *ezx*
                     x0 y0
                     (+ x0 *cols-scale*) (+ y0 *rows-scale*)
                     color)))

;; Returns #t if the display was initialized successfully
(define (init-display width height)
  ;; Ensure that the desired width and height are of valid dimensions
  (cond ((and width (> (modulo width *cols*) 0) (> width 0))
         #f)
        ((and height (> (modulo height *rows*) 0) (> height 0))
         #f)
        (else
         (if width (set! *cols-scale* (/ width *cols*)))
         (if height (set! *rows-scale* (/ height *rows*)))
         (set! *ezx*
               (ezx-init (* *cols-scale* *cols*) (* *rows-scale* *rows*) ""))
         (ezx-set-background *ezx* *color-white*)
         #t)))

(define (refresh-display)
  (ezx-redraw *ezx*))

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
