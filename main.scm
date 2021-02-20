(use format srfi-4)

;;; Macros

;; Macros for defining instructions with various variables derived from
;; the two instruction bytes available in scope:
;;
;; `nnn` - 12-bit value, the lowest 12 bits of the instruction
;; `n`   - 4-bit value, the lowest 4 bits of the instruction
;; `x`   - 4-bit value, the lower 4 bits of the high byte of the instruction
;; `y`   - 4-bit value, the upper 4 bits of the low byte of the instruction
;;
;; Source: http://devernay.free.fr/hacks/chip8/C8TECH10.HTM

(define-syntax define-op-with-x
  (ir-macro-transformer
   (lambda (exp inject compare)
     (let* ((signature (cadr exp))
            (msb (cadr signature))
            (body (cddr exp)))
       `(define ,signature
          (let ((,(inject 'x) (bitwise-and ,msb #xF)))
            ,@body))))))

(define-syntax define-op-with-xy
  (ir-macro-transformer
   (lambda (exp inject compare)
     (let* ((signature (cadr exp))
            (msb (cadr signature))
            (lsb (caddr signature))
            (body (cddr exp)))
       `(define ,signature
          (let ((,(inject 'x) (bitwise-and ,msb #xF))
                (,(inject 'y) (bitwise-and (arithmetic-shift ,lsb -4) #xF)))
            ,@body))))))

;; Memory

(define *ram* (make-u8vector (* 4 1024)))

;; Registers

(define *stack* (make-u16vector 16))

(define *PC* 0)
(define *SP* 0)

(define *V* (make-u8vector 16 0))

(define *I* 0)

(define *DT* 0)
(define *ST* 0)

;; Debugging

(define (print-instruction msb lsb error)
  (format #t "~3d: " *PC*)
  (if error (format #t "ERROR: ~A - " error))
  (format #t "0x~2,'0x~2,'0x~%" msb lsb))

(define (print-registers)
  (let loop ((i 0) (names '()) (values '()))
    (if (>= i (u8vector-length *V*))
        (begin
          (print (apply string-append (reverse names)))
          (print (apply string-append (reverse values))))
        (loop (+ i 1)
              (cons (format #f "  V~x " i) names)
              (cons (format #f "0x~2,'0x " (u8vector-ref *V* i)) values)))))

;; Instructions

(define-op-with-x (add-vx-byte msb lsb)
  (u8vector-set! *V* x (+ (u8vector-ref *V* x) lsb)))

(define-op-with-x (ld-vx-byte msb lsb)
  (u8vector-set! *V* x lsb))

(define-op-with-x (ld-dt-vx msb lsb)
  (set! *DT* (u8vector-ref *V* x)))

(define-op-with-xy (ld-vx-vy msb lsb)
  (u8vector-set! *V* y (u8vector-ref *V* x)))

(define-op-with-x (se-vx-byte msb lsb)
  (if (= (u8vector-ref *V* x) lsb)
      (set! *PC* (+ *PC* 2))))

(define-op-with-xy (se-vx-vy msb lsb)
  (if (= (u8vector-ref *V* x) (u8vector-ref *V* y))
      (set! *PC* (+ *PC* 2))))

(define-op-with-x (sne-vx-byte msb lsb)
  (if (not (= (u8vector-ref *V* x) lsb))
      (set! *PC* (+ *PC* 2))))

(define (ops msb lsb)
  (cond ((and (>= msb #x30) (<= msb #x3F))
         se-vx-byte)
        ((and (>= msb #x40) (<= msb #x4F))
         sne-vx-byte)
        ((and (>= msb #x50) (<= msb #x5F))
         se-vx-vy)
        ((and (>= msb #x60) (<= msb #x6F))
         ld-vx-byte)
        ((and (>= msb #x70) (<= msb #x7F))
         add-vx-byte)
        ((and (= #x80 (bitwise-and msb #xF0)) (= #x0 (bitwise-and lsb #xF)))
         ld-vx-vy)
        ((and (= #xF0 (bitwise-and msb #xF0)) (= lsb #x15))
         ld-dt-vx)
        (else #f)))

(define (execute)
  (let* ((msb (u8vector-ref *ram* *PC*))
         (lsb (u8vector-ref *ram* (+ *PC* 1)))
         (op (ops msb lsb)))
    (if op
        (begin
          (print-instruction msb lsb #f)
;          (print-registers)
          (op msb lsb)
          (set! *PC* (+ *PC* 2)))
        (begin
          (print-instruction msb lsb "Instruction not found")
          (print-registers)
          #f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load test ROM
(let ((port (open-input-file "c8_test.c8")))
  (let loop ((i 0))
    (let ((c (read-char port)))
      (unless (eof-object? c)
        (u8vector-set! *ram* i (char->integer c))
        (loop (+ i 1))))))

;; Main loop
(let loop ()
  (if (execute)
      (loop)))
