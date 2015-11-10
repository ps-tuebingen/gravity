;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname orbital) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; This file is part of the orbital simulation challenge
; of the lecture "Informatik 1" at Tübingen University
; in winter term 2015/16.
;
; By Jonathan Brachthäuser and Tillmann Rendel.
;
;
; This file contains functions that simulate gravity and the
; movement of individual celestial bodies.

; The definitions in this file use definitions from the following files:
(require "vector.rkt")
(require "bodies.rkt")

; Body Body -> Force
; gravitational force between two bodies
(check-expect (gravity EARTH EARTH) 0)
(define (gravity body1 body2)
  (if (zero? (distance body1 body2))
      0
      (/ (* G (mass body1) (mass body2)) (sqr (distance body1 body2)))))

; Body Body -> DirectedForce
; the gravital force vector between two bodies
(check-expect (gravital-vector EARTH EARTH) ZERO-VECTOR)
(define (gravital-vector body1 body2)
  (v* (gravity body1 body2)
      (v-norm (v- (position body1) (position body2)))))

; Body Body -> Distance
; distance between celestial bodies
(check-within (distance EARTH MOON) MOON-ORBIT 1e-5)
(check-within (distance EARTH SAT) SAT-ORBIT 1e-5)
(check-within (distance EARTH SHIP) SHIP-ORBIT 1e-5)
(define (distance body1 body2)
  (v-length (v- (position body1) (position body2))))

; Body DirectedForce Time -> Body
; update a celestial body by moving it for the given time difference
(define (move body gravity dt)
  (simulate-movement dt
    (simulate-thrust dt
      (simulate-gravity dt gravity
        body))))

; Time Body -> Body
; simulate movement of celestial body
(define (simulate-movement dt body)
  (set-position (v+ (position body)
                    (v* dt (velocity body)))
                body))

; Time Body -> Mass
; compute fuel use by a celestial body during a time period
(check-expect (fuel-use 1 EARTH) 0)
(check-expect (fuel-use 1 MOON) 0)
(check-expect (fuel-use 1 SAT) 0)
(check-expect (fuel-use 1 SHIP) 0)
(check-expect (fuel-use 2 SHIP) 0)
(check-expect (fuel-use 1 (set-thrust (make-posn 1 0) SHIP)) AJ10-FUEL-FLOW)
(check-expect (fuel-use 2 (set-thrust (make-posn 1 0) SHIP)) (* 2 AJ10-FUEL-FLOW))
(check-expect (fuel-use 1 (set-fuel 0 SHIP)) 0)
(check-expect (fuel-use 2 (set-fuel AJ10-FUEL-FLOW (set-thrust (make-posn 1 0) SHIP))) AJ10-FUEL-FLOW)
(define (fuel-use dt body)
  (if (zero-vector? (thrust body)) 0
      (min (fuel body)
           (* dt (fuel-flow body)))))

; Time Body -> Velocity
; compute effective velocity change by thrust
(define (effective-thrust dt body)
  (v* (* (/ (fuel-use dt body)
            (mass body))
         AJ10-EXHAUST-SPEED)
      (v-norm (thrust body))))

; Time Body -> Body
; simulate engine of celestial body
(define (simulate-thrust dt body)
  (set-fuel (- (fuel body)
               (fuel-use dt body))
    (set-velocity (v+ (velocity body)
                      (effective-thrust dt body))
                  body)))

; Time DirectedForce Body -> Body
; simulate gravity on celestial body
(define (simulate-gravity dt force body)
  (set-velocity (v+ (velocity body)
                    (v* (/ dt (mass body)) force))
                body))

; The following two lines make all definitions in this file
; available to other files that contain:
;
;   (require "orbital.rkt")
;
; Attention: These two lines have to come at the end of this file
; to avoid confusing the (define-struct ...) from the teaching
; languages with the (define-struct ...) from racket/base.
(require racket/base)
(provide (all-defined-out))
