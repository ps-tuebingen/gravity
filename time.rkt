;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname time) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; This file is part of the orbital simulation challenge
; of the lecture "Informatik 1" at Tübingen University
; in winter term 2015/16.
;
; By Jonathan Brachthäuser and Tillmann Rendel.
;
;
; This file contains definitions for computing with time.

; Time is a Number
; interp. elapsed time since start of something, in seconds

(define SECOND 1)
(define MINUTE (* 60 SECOND))
(define HOUR (* 60 MINUTE))
(define DAY (* 24 HOUR))

; Time -> Number
; How many full seconds are contained in the time, not counting full minutes
(check-expect (time-seconds SECOND) 1)
(check-expect (time-seconds MINUTE) 0)
(check-expect (time-seconds (+ MINUTE SECOND)) 1)
(check-expect (time-seconds (* 1.5 MINUTE)) 30)
(define (time-seconds time)
  (remainder (inexact->exact (round time)) MINUTE))

; Time -> Number
; How many full minutes are contained in the time, not counting full hours
(check-expect (time-minutes MINUTE) 1)
(check-expect (time-minutes (+ MINUTE SECOND)) 1)
(check-expect (time-minutes (* 1.5 HOUR)) 30)
(define (time-minutes time)
  (quotient (remainder (inexact->exact (round time)) HOUR) MINUTE))

; Time -> Number
; How many full hours are contained in the time, not counting full days
(check-expect (time-hours HOUR) 1)
(check-expect (time-hours (+ HOUR MINUTE)) 1)
(check-expect (time-hours (* 1.5 DAY)) 12)
(define (time-hours time)
  (quotient (remainder (inexact->exact (round time)) DAY) HOUR))

; Time -> Number
; How many full days are contained in the time
(check-expect (time-days DAY) 1)
(check-expect (time-days (+ DAY HOUR)) 1)
(check-expect (time-days (* 3.5 DAY)) 3)
(define (time-days time)
  (quotient (inexact->exact (round time)) DAY))

; Number -> String
; Print the integral part of a number, filled with a leading zero to two digits
(check-expect (number->string/two-digits 0) "00")
(check-expect (number->string/two-digits 7) "07")
(check-expect (number->string/two-digits 42) "42")
(check-expect (number->string/two-digits 123) "123")
(check-expect (number->string/two-digits 3.3) "03")
(check-expect (number->string/two-digits #i3.3) "03")
(define (number->string/two-digits number)
  (if (<= 0 number 9)
    (string-append "0" (number->string (inexact->exact (round number))))
    (number->string (round number))))

; Time -> String
; Print time as human-readable string.
(check-expect (time->string 0) "00:00:00")
(check-expect (time->string SECOND) "00:00:01")
(check-expect (time->string MINUTE) "00:01:00")
(check-expect (time->string HOUR) "01:00:00")
(check-expect (time->string DAY) "1d 00:00:00")
(check-expect (time->string (+ (* 3 DAY) (* 5 HOUR) (* 12 MINUTE) (* 7 SECOND))) "3d 05:12:07")
(define (time->string time)
  (string-append (if (< time DAY) "" (string-append (number->string (time-days time)) "d "))
                 (number->string/two-digits (time-hours time))
                 ":"
                 (number->string/two-digits (time-minutes time))
                 ":"
                 (number->string/two-digits (time-seconds time))))

; The following two lines make all definitions in this file
; available to other files that contain:
;
;   (require "time.rkt")
;
; Attention: These two lines have to come at the end of this file
; to avoid confusing the (define-struct ...) from the teaching
; languages with the (define-struct ...) from racket/base.
(require racket/base)
(provide (all-defined-out))
