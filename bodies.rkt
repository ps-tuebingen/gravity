;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname bodies) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; This file is part of the orbital simulation challenge
; of the lecture "Informatik 1" at Tübingen University
; in winter term 2015/16.
;
; By Jonathan Brachthäuser and Tillmann Rendel.
;
;
; This file contains definitions to model four celestial bodies:
; the earth, the moon, a space ship, and a satellite.

; The definitions in this file use definitions from the following file:
(require "vector.rkt")

; Force is a Number
; interp. physical force in newton (N).

; Mass is a Number
; interp. weight in kilograms (kg).

; Distance is a Number
; interp. the distance between two points in meters (m).

; Speed is a Number
; interp. the speed of an object in meters per second (m/s)

; Position is a structure: (make-posn Distance Distance)
; interp. a position in a 2D-space, relative to the origin.

; Direction is a structure: (make-posn Distance Distance)
; interp. a direction in a 2D-space, magnitude of the vector should be ignored.

; DirectedForce is a structure: (make-posn Force Force)
; interp. directed force with a horizontal and vertical force component.

; Velocity is a structure: (make-posn Speed Speed)
; interp. directed speed of an object.

; Flow is a Number
; interp. mass flow in kg/s.

(define-struct earth (position velocity))
(define-struct moon (position velocity))
(define-struct sat (position velocity))
(define-struct ship (position velocity fuel orientation thrust))

(define-struct earth-centered ())
(define-struct moon-centered ())
(define-struct sat-centered ())
(define-struct ship-centered ())

; Earth is a structure: (make-earth Position Velocity)
; interp. simulation data about the earth.

; Moon is a structure: (make-moon Position Velocity)
; interp. simulation data about the moon.

; Sat is a structure: (make-sat Position Velocity)
; interp. simulation data about the gps satellite.

; Ship is a structure: (make-ship Position Velocity Mass Direction Direction)
; interp. simulation data about the spaceship.

; Body is one of:
;   - Earth
;   - Moon
;   - Sat
;   - Ship
; interp. simulation data about celestial body.

; A CenteredBody is one of:
; - (make-earth-centered)
; - (make-moon-centered)
; - (make-sat-centered)
; - (make-ship-centered)
; interp. a flag, which celestial body is currently centered.

; a Force that represents the gravitational constant
(define G 6.667408e-11)

; EARTH
; -----
; initial Position of the earth in the simulated world
(define EARTH-POS (make-posn 0 0))

; initial Velocity of the earth in the simulated world
(define EARTH-VEL (make-posn 0 0))

; Mass of the earth
(define EARTH-MASS 5.974e24)

; radius of the earth, as a Distance
(define EARTH-RADIUS 6371e3)

; gravity of a 1kg object on the surface of the earth, as a Force
(define EARTH-GRAVITY 9.80665)

; initial simulation state of the earth
(define EARTH (make-earth EARTH-POS EARTH-VEL))


; MOON
; ----
; orbital distance of the moon to the earth, as a Distance
(define MOON-ORBIT 363.3e6)

; initial orbital speed of the moon, as a Speed
(define MOON-INIT-V 1.023e3)

; Mass of the moon
(define MOON-MASS 7.349e22)

; radius of the moon, as a Distance
(define MOON-RADIUS 1.738e6)

; initial Position of the moon in the simulated world
(define MOON-POS (make-posn (* -1 MOON-ORBIT) 0))

; initial Velocity of the moon in the simulated world
(define MOON-VEL (make-posn 0 MOON-INIT-V))

; initial simulation state of the moon
(define MOON (make-moon MOON-POS MOON-VEL))


; GPS-SATELLITE AROUND THE EARTH
; ------------------------------
; orbital Distance of the gps-satellite to the earth
(define SAT-ORBIT (+ 20.2e6 EARTH-RADIUS))

; initial orbital Speed of the satellite
(define SAT-INIT-V (sqrt (/ (* G EARTH-MASS) SAT-ORBIT)))

; Mass of the gps satellite
(define SAT-MASS 1080)

; initial Position of the satellite in the simulated world
(define SAT-POS (v* SAT-ORBIT (v-norm (make-posn 1 1))))

; initial Velocity of the satellite in the simulated in
(define SAT-VEL (v* SAT-INIT-V (orthogonal-unit-vector SAT-POS)))

; initial simulation state of the satellite
(define SAT (make-sat SAT-POS SAT-VEL))


; SPACE SHIP: Apollo 10
; ---------------------
; orbital Distance of the space ship in parking orbit around earth
(define SHIP-ORBIT (+ 185e3 EARTH-RADIUS))

; initial orbital Speed of the ship
(define SHIP-INIT-V (sqrt (/ (* G EARTH-MASS) SHIP-ORBIT)))

; engine propellants as a Mass
(define SHIP-FUEL 18410)

; structural Mass of the space-ship without fuel
(define SHIP-MASS-EMPTY 8590)

; initial Mass of the space-ship including fuel
(define SHIP-MASS (+ SHIP-FUEL SHIP-MASS-EMPTY))

; initial Position of the space-ship in the simulated world
(define SHIP-POS (make-posn SHIP-ORBIT 0))

; initial Velocity of the space-ship in the simulated world
(define SHIP-VEL (make-posn 0 (* -1 SHIP-INIT-V)))

; initial simulation state of the space-ship
(define SHIP (make-ship SHIP-POS SHIP-VEL SHIP-FUEL SHIP-VEL ZERO-VECTOR))


; PROPULSION SYSTEM OF THE CSM (AJ10)
; -----------------------------------
; specific impulse of the engine
(define AJ10-ISP-PER-SEC 314)

; thrust of the AJ10 engine in vacuum, as a Force
(define AJ10-THRUST-VAC 43.7e3)

; Mass flow rate of exhausted fuel in kg/sec
(define AJ10-FUEL-FLOW (/ AJ10-THRUST-VAC EARTH-GRAVITY AJ10-ISP-PER-SEC))

; Exhaust Speed of the engine
(define AJ10-EXHAUST-SPEED (/ AJ10-THRUST-VAC AJ10-FUEL-FLOW))


; =====================================
; Library to work with celestial bodies
; =====================================

; Body -> Mass
; return a celestial body's mass
(check-expect (mass EARTH) EARTH-MASS)
(check-expect (mass MOON) MOON-MASS)
(check-expect (mass SAT) SAT-MASS)
(check-expect (mass SHIP) (+ SHIP-MASS-EMPTY SHIP-FUEL))
(define (mass body)
  (cond
    [(earth? body) EARTH-MASS]
    [(moon? body) MOON-MASS]
    [(sat? body) SAT-MASS]
    [(ship? body) (+ (ship-fuel body) SHIP-MASS-EMPTY)]))

; Body -> Velocity
; return a celestial body's velocity
(check-expect (velocity EARTH) EARTH-VEL)
(check-expect (velocity MOON) MOON-VEL)
(check-expect (velocity SAT) SAT-VEL)
(check-expect (velocity SHIP) SHIP-VEL)
(define (velocity body)
  (cond
    [(earth? body) (earth-velocity body)]
    [(moon? body) (moon-velocity body)]
    [(sat? body) (sat-velocity body)]
    [(ship? body) (ship-velocity body)]))

; Body -> Position
; return a celestial body's position
(check-expect (position EARTH) EARTH-POS)
(check-expect (position MOON) MOON-POS)
(check-expect (position SAT) SAT-POS)
(check-expect (position SHIP) SHIP-POS)
(define (position body)
  (cond
    [(earth? body) (earth-position body)]
    [(moon? body) (moon-position body)]
    [(sat? body) (sat-position body)]
    [(ship? body) (ship-position body)]))

; Body -> DirectedForce
; returns a celestial body's engine's thrust
(check-expect (thrust EARTH) ZERO-VECTOR)
(check-expect (thrust MOON) ZERO-VECTOR)
(check-expect (thrust SAT) ZERO-VECTOR)
(check-expect (thrust SHIP) ZERO-VECTOR)
(check-expect (thrust (set-thrust v1 SHIP)) v1)
(define (thrust body)
  (cond
    [(ship? body) (ship-thrust body)]
    [else ZERO-VECTOR]))

; Position Body -> Body
; returns a new body with the updated position
(check-within (position (set-position ZERO-VECTOR EARTH)) ZERO-VECTOR 1e-5)
(check-within (position (set-position ZERO-VECTOR MOON)) ZERO-VECTOR 1e-5)
(check-within (position (set-position ZERO-VECTOR SAT)) ZERO-VECTOR 1e-5)
(check-within (position (set-position ZERO-VECTOR SHIP)) ZERO-VECTOR 1e-5)
(check-within (set-position (position EARTH) EARTH) EARTH 1e-5)
(check-within (set-position (position MOON) MOON) MOON 1e-5)
(check-within (set-position (position SAT) SAT) SAT 1e-5)
(check-within (set-position (position SHIP) SHIP) SHIP 1e-5)
(define (set-position new-position body)
  (cond
    [(earth? body) (make-earth new-position (velocity body))]
    [(moon? body) (make-moon new-position (velocity body))]
    [(sat? body) (make-sat new-position (velocity body))]
    [(ship? body) (make-ship new-position
                             (velocity body)
                             (fuel body)
                             (ship-orientation body)
                             (thrust body))]))

; Velocity Body -> Body
; returns a new body with the updated velocity
(check-within (velocity (set-velocity ZERO-VECTOR EARTH)) ZERO-VECTOR 1e-5)
(check-within (velocity (set-velocity ZERO-VECTOR MOON)) ZERO-VECTOR 1e-5)
(check-within (velocity (set-velocity ZERO-VECTOR SAT)) ZERO-VECTOR 1e-5)
(check-within (velocity (set-velocity ZERO-VECTOR SHIP)) ZERO-VECTOR 1e-5)
(check-within (set-velocity (velocity EARTH) EARTH) EARTH 1e-5)
(check-within (set-velocity (velocity MOON) MOON) MOON 1e-5)
(check-within (set-velocity (velocity SAT) SAT) SAT 1e-5)
(check-within (set-velocity (velocity SHIP) SHIP) SHIP 1e-5)
(define (set-velocity new-velocity body)
  (cond
    [(earth? body) (make-earth (position body) new-velocity)]
    [(moon? body) (make-moon (position body) new-velocity)]
    [(sat? body) (make-sat (position body) new-velocity)]
    [(ship? body) (make-ship (position body)
                             new-velocity
                             (fuel body)
                             (ship-orientation body)
                             (thrust body))]))

; Body -> Mass
; returns the fuel of an celestial body
(check-expect (fuel SHIP) SHIP-FUEL)
(check-expect (fuel EARTH) 0)
(check-expect (fuel MOON) 0)
(check-expect (fuel SAT) 0)
(define (fuel body)
  (if (ship? body)
    (ship-fuel body)
    0))

; Body -> Flow
; returns the maximum fuel use per second of an celestial body
(check-expect (fuel-flow SHIP) AJ10-FUEL-FLOW)
(check-expect (fuel-flow EARTH) 0)
(check-expect (fuel-flow MOON) 0)
(check-expect (fuel-flow SAT) 0)
(define (fuel-flow body)
  (if (ship? body)
    AJ10-FUEL-FLOW
    0))

; Body -> Speed
; returns the exhaust speed of a celestial body's rocket engine
(check-expect (exhaust-speed SHIP) AJ10-EXHAUST-SPEED)
(check-expect (exhaust-speed EARTH) 0)
(check-expect (exhaust-speed MOON) 0)
(check-expect (exhaust-speed SAT) 0)
(define (exhaust-speed body)
  (if (ship? body)
    AJ10-EXHAUST-SPEED
    0))

; Mass Body -> Body
; returns a new ship with the updated fuel, or other celestial bodies unchanged
(check-within (fuel (set-fuel 0 SHIP)) 0 1e-5)
(check-within (set-fuel (fuel SHIP) SHIP) SHIP 1e-5)
(check-within (set-fuel 42 EARTH) EARTH 1e-5)
(check-within (set-fuel 42 MOON) MOON 1e-5)
(check-within (set-fuel 42 SAT) SAT 1e-5)
(define (set-fuel new-fuel body)
  (if (ship? body)
    (make-ship (position body)
               (velocity body)
               new-fuel
               (ship-orientation body)
               (thrust body))
    body))

; Direction Ship -> Ship
; returns a new space-ship with the updated orientation
(check-within (ship-orientation (set-orientation ZERO-VECTOR SHIP)) ZERO-VECTOR 1e-5)
(check-within (set-orientation (ship-orientation SHIP) SHIP) SHIP 1e-5)
(define (set-orientation new-orientation ship)
  (make-ship (position ship)
             (velocity ship)
             (ship-fuel ship)
             new-orientation
             (thrust ship)))

; Direction Ship -> Ship
; returns a new space-ship with the updated thrust vector
(check-within (thrust (set-thrust ZERO-VECTOR SHIP)) ZERO-VECTOR 1e-5)
(check-within (set-thrust (thrust SHIP) SHIP) SHIP 1e-5)
(define (set-thrust thrust ship)
  (make-ship (position ship)
             (velocity ship)
             (ship-fuel ship)
             (ship-orientation ship)
             thrust))

; The following two lines make all definitions in this file
; available to other files that contain:
;
;   (require "bodies.rkt")
;
; Attention: These two lines have to come at the end of this file
; to avoid confusing the (define-struct ...) from the teaching
; languages with the (define-struct ...) from racket/base.
(require racket/base)
(provide (all-defined-out))
