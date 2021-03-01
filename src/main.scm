(declare (uses cpu debug display util))

(use srfi-4)

(define (load-rom filename)
  (let ((port (open-input-file filename)))
    (let loop ((i 0))
      (let ((c (read-char port)))
        (unless (eof-object? c)
          (u8vector-set! *ram* (+ *PC* i) (char->integer c))
          (loop (+ i 1)))))))

(define (execute)
  ;; TODO: implement clock rate
  (let* ((msb (u8vector-ref *ram* *PC*))
         (lsb (u8vector-ref *ram* (+ *PC* 1)))
         (op (or (jump-ops msb lsb) (ops msb lsb))))
    (if op
        (begin
          (refresh-display)
          (if (> *DT* 0) (set! *DT* (- *DT* 1)))
          (if (> *ST* 0) (set! *ST* (- *ST* 1)))
          (op)
          (execute))
        (begin
          (print-instruction msb lsb "Instruction not found")
          (print-registers)))))

;;; Run

(let* ((args (command-line-arguments))
       (filename (if (= (length args) 0) #f (car args))))
  (if filename
      (begin
        (load-rom filename)
        (init-display)
        (execute))))
