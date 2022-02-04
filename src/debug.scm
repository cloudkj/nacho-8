(declare (unit debug)
         (uses cpu display input util))

(use format srfi-4)

;;; Debugging functions

(define (print-instruction msb lsb error)
  (format #t "0x~3,48x: " *PC*)
  (if error (format #t "ERROR: ~A - " error))
  (print (bytes->hex-string msb lsb)))

(define (print-keyboard)
  (let loop ((i 0)
             (keys (list))
             (values (list)))
    (if (>= i (u8vector-length *keyboard*))
        (begin
          (print (apply string-append (reverse keys)))
          (print (apply string-append (reverse values))))
        (loop (+ i 1)
              (cons (format #f " ~x " i) keys)
              (cons (format #f " ~d " (u8vector-ref *keyboard* i)) values)))))

(define (print-pixels)
  (let loop ((i 0))
    (if (< i (u32vector-length *pixels*))
        (begin
          (print (number->binary-string (u32vector-ref *pixels* i))
                 (number->binary-string (u32vector-ref *pixels* (+ i 1))))
          (loop (+ i 2))))))

(define (print-ram)
  (let loop ((i 0))
    (if (< i (u8vector-length *ram*))
        (begin
          (format #t "RAM 0x~3,48x: " i)
          (print (u8->hex-string (u8vector-ref *ram* i)))
          (loop (+ i 1))))))

(define (print-registers)
  (let loop ((i 0)
             (names (list "  DT " "  ST  " "  I  "))
             (values (map (lambda (r) (string-append (u8->hex-string r) " "))
                          (list *DT* *ST* *I*))))
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
