;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname mission) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; This file is part of the orbital simulation challenge
; of the lecture "Informatik 1" at Tübingen University
; in winter term 2015/16.
;
; By Jonathan Brachthäuser and Tillmann Rendel.
;
;
; This file contains definitions that model the state of a mission.

; The definitions in this file use definitions from the following files:
(require "vector.rkt")
(require "bodies.rkt")
(require "orbital.rkt")
(require "world.rkt")

; Zoom-Level is a Number.
; interp. factor to zoom the viewport. Between `0.015` and `1.1`.

; Simulation-Speed is a Number.
; interp. factor to speed up the simulation.

; A MissionState is unspecified.
; interp. the state as defined by mission-control, storing additional mission information
(define NO-STATE #f)

(define-struct mission (time zoom simulation-speed world state centered-body))

; Mission is a structure:
; (make-mission Time Zoom-Level Simulation-Speed World MissionState CenteredBody)
; interp. simulation data about the whole mission.

(define INIT-TIME 0)
(define INIT-ZOOM 0.05)
(define INIT-SPEED 1)
(define INIT-CENTERED-BODY (make-ship-centered))

(define INIT-MISSION
  (make-mission INIT-TIME INIT-ZOOM INIT-SPEED INIT-WORLD NO-STATE INIT-CENTERED-BODY))

(define MAX-SPEEDUP-UNDER-ACCEL 128)
(define TICKS-PER-SEC 29)

; Mission -> Simulation-Speed
; computes effective simulation speed.
(define (effective-simulation-speed mission)
  (if (zero? (fuel-use 1 (world-ship (mission-world mission))))
    (mission-simulation-speed mission)
    (min MAX-SPEEDUP-UNDER-ACCEL
         (mission-simulation-speed mission))))

; Mission -> Time
; returns the difference of Time between two simulation steps
(define (delta-t m)
  (/ (effective-simulation-speed m) TICKS-PER-SEC))

; Mission Direction -> Mission
; simulates the world state for one step.
; `thrust` is the Direction the space-ship should move to by firing its bursters.
(check-expect (> (v-length (velocity (world-ship (mission-world (simulate INIT-MISSION)))))
                 (v-length (velocity (world-ship (mission-world (simulate (set-mission-thrust (v* -1 SHIP-VEL) INIT-MISSION))))))) #true)
(check-expect (< (v-length (velocity (world-ship (mission-world (simulate INIT-MISSION)))))
                 (v-length (velocity (world-ship (mission-world (simulate (set-mission-thrust SHIP-VEL INIT-MISSION))))))) #true)
(check-within (v-length (velocity (world-ship (mission-world (simulate (set-simulation-speed 2 INIT-MISSION))))))
              (v-length (velocity (world-ship (mission-world (simulate (simulate INIT-MISSION))))))
              1)
(check-within (v-length (velocity (world-ship (mission-world (simulate (set-simulation-speed 2 (set-mission-thrust SHIP-VEL INIT-MISSION)))))))
              (v-length (velocity (world-ship (mission-world (simulate (simulate (set-mission-thrust SHIP-VEL INIT-MISSION)))))))
              1)
(define (simulate m)
  (make-mission (+ (mission-time m) (delta-t m))
                (mission-zoom m)
                (mission-simulation-speed m)
                (simulate-world (mission-world m) (delta-t m))
                (mission-state m)
                (mission-centered-body m)))

; Zoom-Level Mission -> Mission
; updates the zoom-level of the mission.
(check-expect (mission-zoom (set-zoom 0.5 INIT-MISSION)) 0.5)
(check-expect (set-zoom (mission-zoom INIT-MISSION) INIT-MISSION) INIT-MISSION)
(define (set-zoom new-zoom mission)
  (make-mission (mission-time mission)
                new-zoom
                (mission-simulation-speed mission)
                (mission-world mission)
                (mission-state mission)
                (mission-centered-body mission)))

; Simulation-Speed Mission -> Mission
; updates the simulation-speed of the mission.
(check-expect (mission-simulation-speed (set-simulation-speed 2 INIT-MISSION)) 2)
(check-expect (set-simulation-speed (mission-simulation-speed INIT-MISSION) INIT-MISSION) INIT-MISSION)
(define (set-simulation-speed new-speed mission)
  (make-mission (mission-time mission)
                (mission-zoom mission)
                new-speed
                (mission-world mission)
                (mission-state mission)
                (mission-centered-body mission)))

; Mission -> Mass
; returns the remaining fuel available for the mission
(check-expect (mission-fuel INIT-MISSION) SHIP-FUEL)
(define (mission-fuel mission)
  (fuel (world-ship (mission-world mission))))

; Mass Mission -> Mission
; returns a new mission with the updated mission fuel
(check-expect (mission-fuel (set-mission-fuel 14 INIT-MISSION)) 14)
(define (set-mission-fuel new-fuel mission)
  (make-mission (mission-time mission)
                (mission-zoom mission)
                (mission-simulation-speed mission)
                (make-world (set-fuel new-fuel (world-ship (mission-world mission)))
                            (world-earth (mission-world mission))
                            (world-moon (mission-world mission))
                            (world-sat (mission-world mission)))
                (mission-state mission)
                (mission-centered-body mission)))

; Mission -> Thrust
; returns currently computed thrust direction for this mission
(check-expect (mission-thrust INIT-MISSION) ZERO-VECTOR)
(define (mission-thrust mission)
  (thrust (world-ship (mission-world mission))))

; Thrust Mission -> Mission
; returns a new mission with the updated thrust direction
(check-expect (mission-thrust (set-mission-thrust v1 INIT-MISSION)) v1)
(check-expect (set-mission-thrust (mission-thrust INIT-MISSION) INIT-MISSION) INIT-MISSION)
(define (set-mission-thrust new-thrust mission)
  (make-mission (mission-time mission)
                (mission-zoom mission)
                (mission-simulation-speed mission)
                (make-world (set-thrust new-thrust (world-ship (mission-world mission)))
                            (world-earth (mission-world mission))
                            (world-moon (mission-world mission))
                            (world-sat (mission-world mission)))
                (mission-state mission)
                (mission-centered-body mission)))

; MissionState Mission -> Mission
; returns a new mission with the updated mission state
(check-expect (mission-state (set-mission-state 42 INIT-MISSION)) 42)
(check-expect (set-mission-state (mission-state INIT-MISSION) INIT-MISSION) INIT-MISSION)
(define (set-mission-state new-state mission)
  (make-mission (mission-time mission)
                (mission-zoom mission)
                (mission-simulation-speed mission)
                (mission-world mission)
                new-state
                (mission-centered-body mission)))

; Operation CenteredBody -> CenteredBody
; returns the resulting centered body according to the specified operation
(define (update-centered-body operation centered-body)
  (cond
    [(string=? "next" operation)
     (cond
       [(earth-centered? centered-body) (make-moon-centered)]
       [(moon-centered? centered-body) (make-sat-centered)]
       [(sat-centered? centered-body) (make-ship-centered)]
       [(ship-centered? centered-body) (make-earth-centered)])]
    [(string=? "previous" operation)
     (cond
       [(earth-centered? centered-body) (make-ship-centered)]
       [(moon-centered? centered-body) (make-earth-centered)]
       [(sat-centered? centered-body) (make-moon-centered)]
       [(ship-centered? centered-body) (make-sat-centered)])]))

; Mission -> Mission
; returns a new mission with the updated centered body
(define (change-mission-centered-body operation mission)
  (make-mission (mission-time mission)
                (mission-zoom mission)
                (mission-simulation-speed mission)
                (mission-world mission)
                (mission-state mission)
                (update-centered-body operation (mission-centered-body mission))))

; The following two lines make all definitions in this file
; available to other files that contain:
;
;   (require "mission.rkt")
;
; Attention: These two lines have to come at the end of this file
; to avoid confusing the (define-struct ...) from the teaching
; languages with the (define-struct ...) from racket/base.
(require racket/base)
(provide (all-defined-out))
