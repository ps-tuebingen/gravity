;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname vector) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; This file is part of the orbital simulation challenge
; of the lecture "Informatik 1" at Tübingen University
; in winter term 2015/16.
;
; By Jonathan Brachthäuser and Tillmann Rendel.
;
;
; This file contains vector arithmetics for posn values.

; Vector is a posn.
; interp. a vector in 2D space.
;
; example vectors
(define v1 (make-posn 12.3 4.5))
(define v2 (make-posn -3 5))

; The zero vector
(define ZERO-VECTOR (make-posn 0 0))

; Number Number -> Vector
; Construct a vector with given x and y components
(check-expect (make-posn 0 0) ZERO-VECTOR)
(check-expect (make-posn 12.3 4.5) v1)
(check-expect (make-posn -3 5) v2)

; Vector -> Boolean
; Detect zero vector.
(check-expect (zero-vector? ZERO-VECTOR) #true)
(check-expect (zero-vector? v1) #false)
(check-expect (zero-vector? v2) #false)
(define (zero-vector? vector)
  (and (zero? (posn-x vector))
       (zero? (posn-y vector))))

; Vector Vector -> Vector
; Add two vectors.
(check-expect (v+ v1 v2) (make-posn 9.3 9.5))
(check-expect (v+ v2 v1) (make-posn 9.3 9.5))
(define (v+ fst snd)
  (make-posn (+ (posn-x fst) (posn-x snd))
             (+ (posn-y fst) (posn-y snd))))

; Vector Vector -> Vector
; Subtract a vector from a vector
(check-expect (v- v1 v1) ZERO-VECTOR)
(check-expect (v- v1 ZERO-VECTOR) v1)
(define (v- fst snd)
  (v+ fst (v* -1 snd)))


; Number Vector -> Vector
; Scale a vector by a scalar
(check-expect (v* 0 v1) ZERO-VECTOR)
(check-expect (v* 1 v1) v1)
(check-expect (v* 1 v2) v2)
(check-expect (v* 2 v1) (v+ v1 v1))
(define (v* scalar vector)
  (make-posn (* scalar (posn-x vector))
             (* scalar (posn-y vector))))

; Posn -> String
; Convert vector to string representation
(check-expect (posn->string (make-posn 1 2)) "(make-posn 1 2)")
(define (posn->string vec)
  (string-append
   "(make-posn "
   (number->string (posn-x vec))
   " "
   (number->string (posn-y vec))
   ")"))

; Vector -> Vector
; normalize the vector
(check-expect (v-norm ZERO-VECTOR) ZERO-VECTOR)
(check-within (v* (v-length v1) (v-norm v1)) v1 1e-5)
(check-within (v* (v-length v2) (v-norm v2)) v2 1e-5)
(define (v-norm vec)
  (if (zero-vector? vec)
      ZERO-VECTOR
      (v* (/ 1 (v-length vec)) vec)))

; Vector -> Number
; compute the magnitude of a vector
(check-expect (v-length ZERO-VECTOR) 0)
(check-expect (v-length (make-posn 0 5)) 5)
(check-expect (v-length (make-posn 7 0)) 7)
(check-expect (v-length (make-posn 3 4)) 5)
(check-expect (v-length (make-posn 8 6)) 10)
(define (v-length vec)
  (sqrt (+ (sqr (posn-x vec)) (sqr (posn-y vec)))))

; Vector -> Vector
; Rotate a vector by 90° clockwise
(check-expect (rotate-clockwise ZERO-VECTOR) ZERO-VECTOR)
(check-expect (rotate-clockwise (make-posn 1 0)) (make-posn 0 -1))
(define (rotate-clockwise vec)
  (make-posn (posn-y vec) (* (posn-x vec) -1)))

; Vector -> Vector
; Construct the unit vector, orthogonal to vec
(check-expect (orthogonal-unit-vector ZERO-VECTOR) ZERO-VECTOR)
(check-expect (orthogonal-unit-vector (make-posn 15 0)) (make-posn 0 -1))
(check-expect (orthogonal-unit-vector (make-posn 3 4)) (make-posn 0.8 -0.6))
(define (orthogonal-unit-vector vec)
  (v-norm (rotate-clockwise vec)))

; Radian is a Number. It falls into the interval:
;   - between 0 and 2*pi
; interp. angle, in radians, counterclockwise, 0 is straight up

; Degree is a Number
; interp. angle, in degrees, counterclockwise, 0 is straight up

(define RADIANS-TO-DEGREE (/ 180 pi))

; Radians -> Degree
; Convert angle in radians to angle in degree
(check-within (radians-to-degrees (* 0.0 pi)) 0 1e-5)
(check-within (radians-to-degrees (* 0.5 pi)) 90 1e-5)
(check-within (radians-to-degrees (* 1.0 pi)) 180 1e-5)
(check-within (radians-to-degrees (* 1.5 pi)) 270 1e-5)
(define (radians-to-degrees arc)
  (* arc RADIANS-TO-DEGREE))

(check-within (degrees-to-radians 0)   (* 0.0 pi) 1e-5)
(check-within (degrees-to-radians 90)  (* 0.5 pi) 1e-5)
(check-within (degrees-to-radians 180) (* 1.0 pi) 1e-5)
(check-within (degrees-to-radians 270) (* 1.5 pi) 1e-5)
(define (degrees-to-radians dec)
  (/ dec RADIANS-TO-DEGREE))

; Vector Vector -> Degree
; returns the angle between two vectors.
(check-within (v-angle v1 v1) 0 1e-5)
(check-within (v-angle (make-posn 1 0) (make-posn 1 1)) 45 1e-5)
(define (v-angle fst snd)
  (- (v-direction snd) (v-direction fst)))

; Vector -> Degree
; returns the direction of a vector in degrees.
(check-within (v-direction (make-posn 0 1)) 0 1e-5)
(check-within (v-direction (make-posn -1 1)) 45 1e-5)
(check-within (v-direction (make-posn -1 0)) 90 1e-5)
(check-within (v-direction (make-posn -1 -1)) 135 1e-5)
(check-within (v-direction (make-posn 0 -1)) -180 1e-5)
(check-within (v-direction (make-posn 1 -1)) -135 1e-5)
(check-within (v-direction (make-posn 1 0)) -90 1e-5)
(check-within (v-direction (make-posn 1 1)) -45 1e-5)
(define (v-direction vector)
  (if (zero-vector? vector)
      0
      (- 90 (radians-to-degrees (+ (atan (posn-x vector) (posn-y vector)) (/ pi 2))))))

; Degree -> Vector
; returns a normalized vector pointing at the given direction.
(check-within (v-direction (direction->vector 0)) 0 1e-5)
(check-within (v-direction (direction->vector 45)) 45 1e-5)
(check-within (v-direction (direction->vector 90)) 90 1e-5)
(check-within (v-direction (direction->vector 135)) 135 1e-5)
(check-within (v-direction (direction->vector -90)) -90 1e-5)
(define (direction->vector dec)
  (local [(define rad (+ (degrees-to-radians dec) (/ pi 2)))]
    (v-norm (make-posn (cos rad) (sin rad)))))

(require racket/base)
(provide (all-defined-out))

; The following two lines make all definitions in this file
; available to other files that contain:
;
;   (require "vector.rkt")
;
; Attention: These two lines have to come at the end of this file
; to avoid confusing the (define-struct ...) from the teaching
; languages with the (define-struct ...) from racket/base.
(require racket/base)
(provide (all-defined-out))
