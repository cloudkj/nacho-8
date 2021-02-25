(declare (uses cpu debug display util))

(use srfi-4)

;; Initialization
(init-display)

;; Load test ROM
;; TODO: load rom from command line arg
(let ((port (open-input-file "test_opcode.ch8")))
;;(let ((port (open-input-file "c8_test.c8")))
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
          (refresh-display)
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
