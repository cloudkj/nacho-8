(declare (unit util))

(use format)

;;; Utility functions

;; String helpers

(define (bytes->hex-string msb lsb)
  (format #f "0x~2,'0x~2,'0x" msb lsb))

(define (number->binary-string num)
  (format #f "~32,'0b" num))

(define (u8->hex-string num)
  (format #f "0x~2,'0x" num))

(define (u16->hex-string num)
  (format #f "0x~4,'0x" num))
