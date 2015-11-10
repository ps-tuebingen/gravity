;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname gravity) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; This file is part of the orbital simulation challenge
; of the lecture "Informatik 1" at Tübingen University
; in winter term 2015/16.
;
; By Jonathan Brachthäuser and Tillmann Rendel.
;
;
; This file contains the start-mission operation.

; start-mission uses the 2htdp/universe library:
(require 2htdp/universe)

; And it uses definitions from the following files:
(require "world.rkt")
(require "mission.rkt")
(require "render.rkt")
(require "interaction.rkt")

; And it uses some features from the Racket language:
(require racket/base)

; We cannot implement start-mission as a function
; in the beginner's language, so you might or might
; not understand it.
;
; Don't worry about it. Just look at the examples for
; using start-mission in the file examples.rkt.

(define-syntax-rule (start-mission mission-control)
  (start-mission/fuel mission-control (mission-fuel INIT-MISSION)))

(define-syntax-rule (start-mission/fuel mission-control init-fuel)
  (big-bang (set-mission-fuel init-fuel INIT-MISSION)
            [on-tick (lambda (m)
                        (simulate (set-mission-thrust
                                    (mission-control
                                      (world-ship (mission-world m))
                                      (world-earth (mission-world m))
                                      (world-sat (mission-world m))
                                      (world-moon (mission-world m))
                                      (mission-time m))
                                    m)))
                     (/ 1 TICKS-PER-SEC)]
            [on-draw render]
            [on-key handle-on-key]))

; The following line makes the definition of start-mission in this file
; available to other files that contain:
;
;   (require "start-mission.rkt")
(provide start-mission start-mission/fuel)
