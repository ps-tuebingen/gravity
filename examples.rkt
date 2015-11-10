;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname examples) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; This file is part of the orbital simulation challenge
; of the lecture "Informatik 1" at Tübingen University
; in winter term 2015/16.
;
; By Jonathan Brachthäuser and Tillmann Rendel.
;
;
; This file contains example mission control functions.

; The definitions in this file use definitions from the following files:
(require "time.rkt")
(require "vector.rkt")
(require "bodies.rkt")
(require "orbital.rkt")
(require "start-mission.rkt")

; Ship Earth Sat Moon Time -> Direction
; Example mission-control, not giving any instructions to the space-ship.
(define (do-nothing ship earth sat moon wall-time)
  ZERO-VECTOR)

; Ship Earth Sat Moon Time -> Direction
; Example mission-control, trying to fly by the moon.
;
; Houston, we need more fuel for that!
(define (fly-by-moon ship earth sat moon wall-time)
  (cond
    [(and (> wall-time (+ (* DAY 0) (* HOUR 1) (* MINUTE 28)))
          (> (fuel ship) 0))
     (velocity ship)]
    [else ZERO-VECTOR]))

; Ship Earth Sat Moon Time -> Direction
; Example mission-control, trying to crash into earth.
(define (crash-into-earth ship earth sat moon wall-time)
  (rotate-clockwise (v- (position earth) (position ship))))

; start the mission "do nothing"
; (change this to start other missions)
(start-mission do-nothing)

; You can also use this variant to equip your spaceship with more fuel
; (start-mission/fuel do-nothing 50000)