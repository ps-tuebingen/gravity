;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname render) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; This file is part of the orbital simulation challenge
; of the lecture "Informatik 1" at Tübingen University
; in winter term 2015/16.
;
; By Jonathan Brachthäuser and Tillmann Rendel.
;
;
; This file contains functions that render the simulated world as images.

; The definitions in this file use the 2htdp/image library:
(require 2htdp/image)

; And the definitions in this file use definitions from the following files:
(require "time.rkt")
(require "vector.rkt")
(require "bodies.rkt")
(require "orbital.rkt")
(require "world.rkt")
(require "mission.rkt")

(define WIDTH 800)
(define HEIGHT 800)
(define CENTER-X (/ WIDTH 2))
(define CENTER-Y (/ HEIGHT 2))
(define CENTER (make-posn CENTER-X CENTER-Y))

(define SPACE (rectangle WIDTH HEIGHT "solid" "black"))

; image depicting the earth
(define EARTH-IMAGE (bitmap/file "images/earth_small.jpg"))

; image depicting the moon
(define MOON-IMAGE (bitmap/file "images/moon_small.jpg"))

; image depicting the gps-satellite
(define SAT-IMAGE (scale 0.1 (bitmap/file "images/satellite.png")))

; image depicting the space-ship
(define SHIP-IMAGE (bitmap/file "images/rocket.png"))

; Mission -> Number
; computes the pixel-per-meter ration for the current zoom-level.
(define (zoom m)
  (/ WIDTH (* 2.5 MOON-ORBIT) (mission-zoom m)))

; Image Distance Zoom-Level -> Image
; returns a scaled image, respecting the zoom-level and the object's
; real world size.
(define (scale-image img real-world-size zoom)
  (scale (/ (* zoom real-world-size) (image-width img)) img))

; Number Color -> Image
; returns an image of an arrow-tip.
(define (arrow-tip width color)
  (polygon (list (make-posn 0 0)
                 (make-posn width 10)
                 (make-posn 0 60)
                 (make-posn (* -1 width) 10))
           "solid"
           color))

; Color -> Image
; returns an image of an arrow with a transparent handle.
(define (arrow color)
  (above (line 0 -60 "transparent")
         (scale 0.25 (arrow-tip 30 color))))

; Body Zoom-Level -> Image
; returns an image representation of the celestial body in the correct size.
(define (render-celestial body zoom)
  (cond
    [(earth? body) (scale-image EARTH-IMAGE (* 2 EARTH-RADIUS) zoom)]
    [(moon? body) (scale-image MOON-IMAGE (* 2 MOON-RADIUS) zoom)]
    [(sat? body) SAT-IMAGE]
    [(ship? body) (overlay (render-direction-indicator (effective-thrust 1 body)
                                                       (make-color 250 151 45 180))
                           (render-direction-indicator (velocity body)
                                                       (make-color 153 51 102 180))
                           SHIP-IMAGE)]))

; Direction Color Image -> Image
; render arrow to indicate a direction
(define (render-direction-indicator direction color)
  (if (zero-vector? direction)
    empty-image
    (rotate (v-angle direction (make-posn 0 1))
                     (arrow color))))

; Mission Position -> Position
; Translates a location in space to a location on screen
(define (space-to-screen m loc)
  (v+ CENTER
      (v* (zoom m)
          (v- loc
              (position (world-ship (mission-world m)))))))

; Image Location Scene -> Scene
; a variant of place-image that takes a posn instead of two numbers
(define (place-image/posn img loc scene)
  (place-image img (posn-x loc) (posn-y loc) scene))

; Mission Body Scene -> Scene
; places the given body at the right position and in the right size.
(define (place-celestial m body scene)
  (place-image/posn (render-celestial body (zoom m))
                    (space-to-screen m (position body))
                    scene))

; Mission World -> Image
; returns an image representation of the world.
(define (render-world m world)
  (place-celestial m (world-sat world)
    (place-celestial m (world-ship world)
      (place-celestial m (world-earth world)
        (place-celestial m (world-moon world)
          SPACE)))))

; Mission -> Image
; returns an image representation of the mission state.
(define (render m)
  (place-image/align (render-stats m)
                     (- WIDTH 20) (- HEIGHT 20)
                     "right" "bottom"
    (render-world m (mission-world m))))

; Mission -> Image
; returns an image containing textual information about the mission.
(define (render-stats m)
  (above/align "left"
               (text (string-append "Zoom: " (number->string (round (/ 100 (mission-zoom m)))) "%") 10 "white")
               (if (< (effective-simulation-speed m)
                      (mission-simulation-speed m))
                 (text (string-append "Time Warp: " (number->string (mission-simulation-speed m))
                                      " [" (number->string (effective-simulation-speed m)) " while thrusting]") 10 "white")
                 (text (string-append "Time Warp: " (number->string (effective-simulation-speed m))) 10 "white"))
               (text (string-append "Mission Time: T+" (time->string (mission-time m))) 10 "white")
               (text (string-append "Fuel left: " (number->string (round (mission-fuel m))) "kg") 10 "white")))

; The following two lines make all definitions in this file
; available to other files that contain:
;
;   (require "render.rkt")
;
; Attention: These two lines have to come at the end of this file
; to avoid confusing the (define-struct ...) from the teaching
; languages with the (define-struct ...) from racket/base.
(require racket/base)
(provide (all-defined-out))
