(declare (unit cpu)
         (uses display events))

(use srfi-4)

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

(define-syntax define-op-with-nnn
  (ir-macro-transformer
   (lambda (exp inject compare)
     (let* ((signature (cadr exp))
            (msb (cadr signature))
            (lsb (caddr signature))
            (body (cddr exp)))
       `(define ,signature
          (let ((,(inject 'nnn) (bitwise-ior
                                 (arithmetic-shift (bitwise-and ,msb #xF) 8)
                                 ,lsb)))
            ,@body))))))

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

(define-syntax define-op-with-xyn
  (ir-macro-transformer
   (lambda (exp inject compare)
     (let* ((signature (cadr exp))
            (msb (cadr signature))
            (lsb (caddr signature))
            (body (cddr exp)))
       `(define ,signature
          (let ((,(inject 'x) (bitwise-and ,msb #xF))
                (,(inject 'y) (bitwise-and (arithmetic-shift ,lsb -4) #xF))
                (,(inject 'n) (bitwise-and ,lsb #xF)))
            ,@body))))))

;;; Memory

(define *ram* (make-u8vector (* 4 1024) 0))

;;; Registers

(define *stack* (make-u16vector 16 0))

(define *PC* #x200)
(define *SP* -1)

(define *V* (make-u8vector 16 0))

(define *I* 0)
(define *DT* 0)
(define *ST* 0)

;;; Instructions

(define-op-with-x (add-i-vx msb lsb)
  (set! *I* (+ *I* (u8vector-ref *V* x))))

(define-op-with-x (add-vx-byte msb lsb)
  (u8vector-set! *V* x (+ (u8vector-ref *V* x) lsb)))

(define-op-with-xy (add-vx-vy msb lsb)
  (let ((sum (+ (u8vector-ref *V* x) (u8vector-ref *V* y))))
    (u8vector-set! *V* #xF (if (> sum #xFF) 1 0))
    (u8vector-set! *V* x (bitwise-and sum #xFF))))

(define-op-with-xy (and-vx-vy msb lsb)
  (u8vector-set! *V* x (bitwise-and (u8vector-ref *V* x)
                                    (u8vector-ref *V* y))))

(define-op-with-nnn (call-addr msb lsb)
  (set! *SP* (+ *SP* 1))
  ;; Note: PC + 2 should be pushed onto stack to allow subroutine to return to
  ;; instruction _after_ the invocation
  (u16vector-set! *stack* *SP* (+ *PC* 2))
  (set! *PC* nnn))

(define-op-with-xyn (drw-vx-vy-nibble msb lsb)
  (let ((Vx (u8vector-ref *V* x))
        (Vy (u8vector-ref *V* y)))
    (let loop ((i 0)
               (collision #f))
      (if (>= i n)
          (u8vector-set! *V* #xF (if collision 1 0))
          (let* ((byte (u8vector-ref *ram* (+ *I* i)))
                 (row (modulo (+ Vy i) *rows*))
                 (result (update-pixels Vx row byte)))
            (loop (+ i 1) (or collision result)))))))

(define-op-with-nnn (jp-addr msb lsb)
  (set! *PC* nnn))

(define-op-with-nnn (jp-v0-addr msb lsb)
  (set! *PC* (+ nnn (u8vector-ref *V* 0))))

(define-op-with-x (ld-b-vx msb lsb)
  (let ((Vx (u8vector-ref *V* x)))
    (u8vector-set! *ram* *I* (quotient Vx 100))
    (u8vector-set! *ram* (+ *I* 1) (quotient (modulo Vx 100) 10))
    (u8vector-set! *ram* (+ *I* 2) (modulo Vx 10))))

(define-op-with-x (ld-dt-vx msb lsb)
  (set! *DT* (u8vector-ref *V* x)))

(define-op-with-nnn (ld-i-addr msb lsb)
  (set! *I* nnn))

(define-op-with-x (ld-i-vx msb lsb)
  (let loop ((i 0))
    (if (<= i x)
        (begin
          (u8vector-set! *ram* (+ *I* i) (u8vector-ref *V* i))
          (loop (+ i 1))))))

(define-op-with-x (ld-st-vx msb lsb)
  (set! *ST* (u8vector-ref *V* x)))

(define-op-with-x (ld-vx-byte msb lsb)
  (u8vector-set! *V* x lsb))

(define-op-with-x (ld-vx-dt msb lsb)
  (u8vector-set! *V* x *DT*))

(define-op-with-x (ld-vx-k msb lsb)
  (let ((value (await-valid-keydown-event)))
    (u8vector-set! *V* x value)))

(define-op-with-x (ld-vx-i msb lsb)
  (let loop ((i 0))
    (if (<= i x)
        (begin
          (u8vector-set! *V* i (u8vector-ref *ram* (+ *I* i)))
          (loop (+ i 1))))))

(define-op-with-xy (ld-vx-vy msb lsb)
  (u8vector-set! *V* y (u8vector-ref *V* x)))

(define-op-with-xy (or-vx-vy msb lsb)
  (u8vector-set! *V* x (bitwise-ior (u8vector-ref *V* x)
                                    (u8vector-ref *V* y))))

(define (ret msb lsb)
  (set! *PC* (u16vector-ref *stack* *SP*))
  (set! *SP* (- *SP* 1)))

(define-op-with-x (rnd-vx-byte msb lsb)
  (u8vector-set! *V* x (bitwise-and (random #x100) lsb)))

(define-op-with-x (se-vx-byte msb lsb)
  (if (= (u8vector-ref *V* x) lsb)
      (set! *PC* (+ *PC* 2))))

(define-op-with-xy (se-vx-vy msb lsb)
  (if (= (u8vector-ref *V* x) (u8vector-ref *V* y))
      (set! *PC* (+ *PC* 2))))

(define-op-with-x (shl-vx msb lsb)
  (let ((Vx (u8vector-ref *V* x)))
    (u8vector-set! *V* #xF (if (> (bitwise-and Vx #x80) 0) 1 0))
    (u8vector-set! *V* x (arithmetic-shift Vx 1))))

(define-op-with-x (shr-vx msb lsb)
  (let ((Vx (u8vector-ref *V* x)))
    (u8vector-set! *V* #xF (bitwise-and Vx 1))
    (u8vector-set! *V* x (arithmetic-shift Vx -1))))

(define-op-with-x (sknp-vx msb lsb)
  (if (key-up? (u8vector-ref *V* x))
      (set! *PC* (+ *PC* 2))))

(define-op-with-x (skp-vx msb lsb)
  (if (key-down? (u8vector-ref *V* x))
      (set! *PC* (+ *PC* 2))))

(define-op-with-x (sne-vx-byte msb lsb)
  (if (not (= (u8vector-ref *V* x) lsb))
      (set! *PC* (+ *PC* 2))))

(define-op-with-xy (sne-vx-vy msb lsb)
  (if (not (= (u8vector-ref *V* x) (u8vector-ref *V* y)))
      (set! *PC* (+ *PC* 2))))

(define-op-with-xy (sub-vx-vy msb lsb)
  (let ((diff (- (u8vector-ref *V* x) (u8vector-ref *V* y))))
    (u8vector-set! *V* #xF (if (> diff 0) 1 0))
    (u8vector-set! *V* x (if (< diff 0) (+ #x100 diff) diff))))

(define-op-with-xy (subn-vx-vy msb lsb)
  (let ((diff (- (u8vector-ref *V* y) (u8vector-ref *V* x))))
    (u8vector-set! *V* #xF (if (> diff 0) 1 0))
    (u8vector-set! *V* x (if (< diff 0) (+ #x100 diff) diff))))

(define-op-with-xy (xor-vx-vy msb lsb)
  (u8vector-set! *V* x (bitwise-xor (u8vector-ref *V* x)
                                    (u8vector-ref *V* y))))

(define (ops msb lsb)
  (cond
   ((and (= msb #x00) (= lsb #xEE))
    ret)
   ((and (>= msb #x10) (<= msb #x1F))
    jp-addr)
   ((and (>= msb #x20) (<= msb #x2F))
    call-addr)
   ((and (>= msb #x30) (<= msb #x3F))
    se-vx-byte)
   ((and (>= msb #x40) (<= msb #x4F))
    sne-vx-byte)
   ((and (= #x50 (bitwise-and msb #xF0)) (= #x0 (bitwise-and lsb #xF)))
    se-vx-vy)
   ((and (>= msb #x60) (<= msb #x6F))
    ld-vx-byte)
   ((and (>= msb #x70) (<= msb #x7F))
    add-vx-byte)
   ((and (= #x80 (bitwise-and msb #xF0)) (= #x0 (bitwise-and lsb #xF)))
    ld-vx-vy)
   ((and (= #x80 (bitwise-and msb #xF0)) (= #x1 (bitwise-and lsb #xF)))
    or-vx-vy)
   ((and (= #x80 (bitwise-and msb #xF0)) (= #x2 (bitwise-and lsb #xF)))
    and-vx-vy)
   ((and (= #x80 (bitwise-and msb #xF0)) (= #x3 (bitwise-and lsb #xF)))
    xor-vx-vy)
   ((and (= #x80 (bitwise-and msb #xF0)) (= #x4 (bitwise-and lsb #xF)))
    add-vx-vy)
   ((and (= #x80 (bitwise-and msb #xF0)) (= #x5 (bitwise-and lsb #xF)))
    sub-vx-vy)
   ((and (= #x80 (bitwise-and msb #xF0)) (= #x6 (bitwise-and lsb #xF)))
    shr-vx)
   ((and (= #x80 (bitwise-and msb #xF0)) (= #x7 (bitwise-and lsb #xF)))
    subn-vx-vy)
   ((and (= #x80 (bitwise-and msb #xF0)) (= #xE (bitwise-and lsb #xF)))
    shl-vx)
   ((and (= #x90 (bitwise-and msb #xF0)) (= #x0 (bitwise-and lsb #xF)))
    sne-vx-vy)
   ((and (>= msb #xA0) (<= msb #xAF))
    ld-i-addr)
   ((and (>= msb #xB0) (<= msb #xBF))
    jp-v0-addr)
   ((and (>= msb #xC0) (<= msb #xCF))
    rnd-vx-byte)
   ((and (>= msb #xD0) (<= msb #xDF))
    drw-vx-vy-nibble)
   ((and (= #xE0 (bitwise-and msb #xF0)) (= lsb #x9E))
    skp-vx)
   ((and (= #xE0 (bitwise-and msb #xF0)) (= lsb #xA1))
    sknp-vx)
   ((and (= #xF0 (bitwise-and msb #xF0)) (= lsb #x07))
    ld-vx-dt)
   ((and (= #xF0 (bitwise-and msb #xF0)) (= lsb #x0A))
    ld-vx-k)
   ((and (= #xF0 (bitwise-and msb #xF0)) (= lsb #x15))
    ld-dt-vx)
   ((and (= #xF0 (bitwise-and msb #xF0)) (= lsb #x18))
    ld-st-vx)
   ((and (= #xF0 (bitwise-and msb #xF0)) (= lsb #x1E))
    add-i-vx)
   ((and (= #xF0 (bitwise-and msb #xF0)) (= lsb #x33))
    ld-b-vx)
   ((and (= #xF0 (bitwise-and msb #xF0)) (= lsb #x55))
    ld-i-vx)
   ((and (= #xF0 (bitwise-and msb #xF0)) (= lsb #x65))
    ld-vx-i)
   (else #f)))

(define *jump-ops*
  (list ret jp-addr call-addr jp-v0-addr))
