(declare (uses cpu display util))

(use format srfi-4)

;;; Debugging

(define (print-instruction msb lsb error)
  (format #t "0x~3x: " *PC*)
  (if error (format #t "ERROR: ~A - " error))
  (print (bytes->hex-string msb lsb)))

(define (print-registers)
  (let loop ((i 0)
             (names (list "  DT " "  I  "))
             (values (map (lambda (r) (string-append (u8->hex-string r) " "))
                          (list *DT* *I*))))
    (if (>= i (u8vector-length *V*))
        (begin
          (print (apply string-append (reverse names)))
          (print (apply string-append (reverse values))))
        (loop (+ i 1)
              (cons (format #f "  V~x " i) names)
              (cons (string-append (u8->hex-string (u8vector-ref *V* i))
                                   " ")
                    values)))))

(define (print-stack)
  (print "SP: " *SP* " stack size: " (+ *SP* 1))
  (print (apply string-append
                (map (lambda (r) (string-append (u16->hex-string r) " "))
                     (u16vector->list *stack*)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
