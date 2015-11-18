;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname interaction) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; This file is part of the orbital simulation challenge
; of the lecture "Informatik 1" at Tübingen University
; in winter term 2015/16.
;
; By Jonathan Brachthäuser and Tillmann Rendel.
;
;
; This file contains functions that implement user interaction.

; The definitions in this file use definitions from the following files:
(require "time.rkt")
(require "mission.rkt")

; Zoom-Level -> Zoom-Level
(check-expect (zoom-in 0.1) 0.09)
(define (zoom-in zoom)
  (if (> zoom 0.015)
      (* zoom 0.9)
      zoom))

; Zoom-Level -> Zoom-Level
(check-expect (zoom-out 0.1) 0.11)
(define (zoom-out zoom)
  (if (< zoom 1)
      (* zoom 1.1)
      zoom))

; Minimum simulation speed: 1 second per second.
(define MIN-SPEEDUP SECOND)

; Maximum simulation speed: 1 hour per second.
(define MAX-SPEEDUP HOUR)

; Simulation-Speed -> Simulation-Speed
(check-expect (faster MAX-SPEEDUP) MAX-SPEEDUP)
(check-expect (faster (/ MAX-SPEEDUP 2)) MAX-SPEEDUP)
(define (faster speed)
  (if (< speed MAX-SPEEDUP)
      (* speed 2)
      speed))

; Simulation-Speed -> Simulation-Speed
(check-expect (slower MIN-SPEEDUP) MIN-SPEEDUP)
(check-expect (slower (* 2 MIN-SPEEDUP)) MIN-SPEEDUP)
(define (slower speed)
  (if (> speed MIN-SPEEDUP)
      (/ speed 2)
      speed))

; Mission KeyEvent -> Mission
; handles keypresses for controlling mission parameters
(define (handle-on-key m k)
  (cond
    [(or (string=? k "+") (string=? k "add") (string=? k "wheel-up"))
     (set-zoom (zoom-in (mission-zoom m)) m)]
    [(or (string=? k "-") (string=? k "subtract") (string=? k "wheel-down"))
     (set-zoom (zoom-out (mission-zoom m)) m)]
    [(string=? k ",")
     (set-simulation-speed (slower (mission-simulation-speed m)) m)]
    [(string=? k ".")
     (set-simulation-speed (faster (mission-simulation-speed m)) m)]
    [else m]))

; The following two lines make all definitions in this file
; available to other files that contain:
;
;   (require "interaction.rkt")
;
; Attention: These two lines have to come at the end of this file
; to avoid confusing the (define-struct ...) from the teaching
; languages with the (define-struct ...) from racket/base.
(require racket/base)
(provide (all-defined-out))
