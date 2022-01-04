(declare (uses cpu debug display events util))

(use getopt-long srfi-4)

;;; Options handling

(define *program-name* "nacho-8")
(define *program-description* "An emulator for the CHIP-8 system")

(define *options-grammar*
  '((width  "Width of the display, in pixels"
            (required #f) (value (required num)))
    (height "Height of the display, in pixels"
            (required #f) (value (required num)))
    (help   "Print this help message"
            (required #f) (single-char #\h) (value #f))))

(define (print-program-usage)
  (format #t "Usage: ~A [OPTIONS] FILENAME\n\n~A.\n\n"
          *program-name*
          *program-description*)
  (print "Optionals:\n")
  (print (usage *options-grammar*)))

(define (get-numeric-option-value key options)
  (let ((val (assoc key options)))
    (if val
        (string->number (cdr val))
        #f)))

;;;

(define (load-rom filename)
  (let ((port (open-input-file filename)))
    (let loop ((i 0))
      (let ((c (read-char port)))
        (unless (eof-object? c)
          (u8vector-set! *ram* (+ *PC* i) (char->integer c))
          (loop (+ i 1)))))))

(define (execute)
  (let* ((msb (u8vector-ref *ram* *PC*))
         (lsb (u8vector-ref *ram* (+ *PC* 1)))
         (op (ops msb lsb)))
    (if op
        (begin
          ;; Execute instruction and increment program counter if applicable
          (op msb lsb)
          (unless (member op *jump-ops*) (set! *PC* (+ *PC* 2)))
          ;; Directly refresh display after each DRW operation if refresh timer
          ;; is disabled
          (if (and (not (timer-based-refresh?)) (= op drw-vx-vy-nibble))
              (refresh-display))
          (process-events)
          (execute))
        (begin
          (print-instruction msb lsb "Instruction not found")
          (print-registers)))))

;;; Run

(let* ((options (getopt-long (command-line-arguments) *options-grammar*))
       (args (assoc '@ options))
       (filename (if (< (length args) 2) #f (cadr args))))
  (cond ((assoc 'help options)
         (print-program-usage))
        ((not filename)
         (print "ROM filename required.\n")
         (print-program-usage))
        ((not (init-display (get-numeric-option-value 'width options)
                            (get-numeric-option-value 'height options)))
         (print "Invalid display options.\n")
         (print-program-usage))
        (else
         (begin
           (init-events)
           (load-rom filename)
           (execute)))))
