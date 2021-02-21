(use format srfi-4)

;;; Helpers

(define (bytes->hex-string msb lsb)
  (format #f "0x~2,'0x~2,'0x" msb lsb))

(define (number->hex-string num)
  (format #f "0x~2,'0x" num))

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

;; Memory

(define *ram* (make-u8vector (* 4 1024) 0))

;; Registers

(define *stack* (make-u16vector 16))

(define *PC* #x200)
(define *SP* 0)

(define *V* (make-u8vector 16 0))

(define *I* 0)

(define *DT* 0)
(define *ST* 0)

;; Debugging

(define (print-instruction msb lsb error)
  (format #t "0x~3x: " *PC*)
  (if error (format #t "ERROR: ~A - " error))
  (print (bytes->hex-string msb lsb)))

(define (print-registers)
  (let loop ((i 0)
             (names (list "  DT " "  I  "))
             (values (map (lambda (r) (string-append (number->hex-string r) " "))
                          (list *DT* *I*))))
    (if (>= i (u8vector-length *V*))
        (begin
          (print (apply string-append (reverse names)))
          (print (apply string-append (reverse values))))
        (loop (+ i 1)
              (cons (format #f "  V~x " i) names)
              (cons (string-append (number->hex-string (u8vector-ref *V* i))
                                   " ")
                    values)))))

;; Instructions

(define-op-with-x (add-vx-byte msb lsb)
  (u8vector-set! *V* x (+ (u8vector-ref *V* x) lsb)))

(define-op-with-xy (add-vx-vy msb lsb)
  (let ((sum (+ (u8vector-ref *V* x) (u8vector-ref *V* y))))
    (u8vector-set! *V* #xF (if (> sum #xFF) 1 0))
    (u8vector-set! *V* x (bitwise-and sum #xFF))))

(define-op-with-xy (and-vx-vy msb lsb)
  (u8vector-set! *V* x (bitwise-and (u8vector-ref *V* x)
                                    (u8vector-ref *V* y))))

(define-op-with-nnn (jp-v0-addr msb lsb)
  (set! *PC* (+ nnn (u8vector-ref *V* 0))))

(define-op-with-x (ld-vx-byte msb lsb)
  (u8vector-set! *V* x lsb))

(define-op-with-x (ld-dt-vx msb lsb)
  (set! *DT* (u8vector-ref *V* x)))

(define-op-with-x (ld-vx-dt msb lsb)
  (u8vector-set! *V* x *DT*))

(define-op-with-xy (ld-vx-vy msb lsb)
  (u8vector-set! *V* y (u8vector-ref *V* x)))

(define-op-with-xy (or-vx-vy msb lsb)
  (u8vector-set! *V* x (bitwise-ior (u8vector-ref *V* x)
                                    (u8vector-ref *V* y))))

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

(define (jump-ops msb lsb)
  (let ((op (cond
             ((and (>= msb #xB0) (<= msb #xBF))
              jp-v0-addr)
             (else #f))))
    (if op (lambda () (op msb lsb)) #f)))

(define (ops msb lsb)
  (let ((op (cond
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
             ((and (>= msb #xC0) (<= msb #xCF))
              rnd-vx-byte)
             ((and (= #xF0 (bitwise-and msb #xF0)) (= lsb #x07))
              ld-vx-dt)
             ((and (= #xF0 (bitwise-and msb #xF0)) (= lsb #x15))
              ld-dt-vx)
             (else #f))))
    (if op
        (lambda ()
          (begin
            (op msb lsb)
            (set! *PC* (+ *PC* 2))))
        #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load test ROM
(let ((port (open-input-file "c8_test.c8")))
  (let loop ((i 0))
    (let ((c (read-char port)))
      (unless (eof-object? c)
        (u8vector-set! *ram* (+ *PC* i) (char->integer c))
        (loop (+ i 1))))))

;; Execution
;; TODO: implement clock rate
(define (execute)
  (let* ((msb (u8vector-ref *ram* *PC*))
         (lsb (u8vector-ref *ram* (+ *PC* 1)))
         (op (or (jump-ops msb lsb) (ops msb lsb))))
    (if op
        (begin
          (print-instruction msb lsb #f)
;;          (print-registers) (print)
          (if (> *DT* 0) (set! *DT* (- *DT* 1)))
          (if (> *ST* 0) (set! *ST* (- *ST* 1)))
          (op))
        (begin
          (print-instruction msb lsb "Instruction not found")
          (print-registers)
          #f))))

;; Main loop
(let loop ()
  (if (execute)
      (loop)))
