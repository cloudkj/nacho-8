(use format srfi-4)

;; Macros/functions to write:
;; * Deferencing V registers
;;   (u8vector-ref *V* x)
;;      => (V x)
;;      => or simply, e.g. V0, V1, etc ??

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
  (print *V*))

;; Instructions

(define (ld-vx-byte msb lsb)
  (let ((x (bitwise-and msb #xF)))
    (u8vector-set! *V* x lsb)))

(define (ld-dt-vx msb lsb)
  (let ((x (bitwise-and msb #xF)))
    (set! *DT* (u8vector-ref *V* x))))

(define (se-vx-byte msb lsb)
  (let ((x (bitwise-and msb #xF)))
    (if (= (u8vector-ref *V* x) lsb)
        (set! *PC* (+ *PC* 2)))))

(define (execute)
  (let* ((msb (u8vector-ref *ram* *PC*))
         (lsb (u8vector-ref *ram* (+ *PC* 1)))
         (op (cond ((and (>= msb #x30) (<= msb #x3F))
                    se-vx-byte)
                   ((and (>= msb #x60) (<= msb #x6F))
                    ld-vx-byte)
                   ((and (= #xF0 (bitwise-and msb #xF0)) (= lsb #x15))
                    ld-dt-vx)
                   (else #f))))
    (if op
        (begin
          (print-instruction msb lsb #f)
          (print-registers)
          (op msb lsb)
          (set! *PC* (+ *PC* 2)))
        (begin
          (print-instruction msb lsb "Instruction not found")
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
