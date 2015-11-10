;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname world) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; This file is part of the orbital simulation challenge
; of the lecture "Informatik 1" at Tübingen University
; in winter term 2015/16.
;
; By Jonathan Brachthäuser and Tillmann Rendel.
;
;
; This file contains functions that simulate the interaction of four
; celestial bodies.

; The definitions in this file use definitions from the following files:
(require "vector.rkt")
(require "bodies.rkt")
(require "orbital.rkt")

(define-struct world (ship earth moon sat))

; World is a structure: (make-world Ship Earth Moon Sat)
; interp. simulation data about all celestial bodies in simulated world.

(define INIT-WORLD (make-world SHIP EARTH MOON SAT))

; Body World -> DirectedForce
; returns the accumulated force of all bodies on that body
(define (compute-forces body world)
  (v+ (gravital-vector (world-ship world) body)
    (v+ (gravital-vector (world-earth world) body)
      (v+ (gravital-vector (world-moon world) body)
        (v+ (gravital-vector (world-sat world) body)
          ZERO-VECTOR)))))

; Body World Time -> Body
; simulates the physical behaviour of a body for dt seconds in the world
(define (simulate-body body world dt)
  (move body (compute-forces body world) dt))

; World Time -> Body
(define (simulate-world world dt)
  (make-world (simulate-body (world-ship world) world dt)
              (simulate-body (world-earth world) world dt)
              (simulate-body (world-moon world) world dt)
              (simulate-body (world-sat world) world dt)))

; The following two lines make all definitions in this file
; available to other files that contain:
;
;   (require "world.rkt")
;
; Attention: These two lines have to come at the end of this file
; to avoid confusing the (define-struct ...) from the teaching
; languages with the (define-struct ...) from racket/base.
(require racket/base)
(provide (all-defined-out))
