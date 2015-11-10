# Gravity
An orbit simulation game, written in the teaching language BSL. Gravity has been 
developed for teaching purposes at the university of Tübingen.

The idea: Students can plan space-missions by programming a mission-control function.

## Source Overview
The game is based on a numeric simulation of celestial bodies. Its implementation is
divided into several modules:

### Examples
[examples.rkt](https://github.com/ps-tuebingen/gravity/blob/master/examples.rkt) 
: Gives examples on how to use the programming interface to develop own mission-control. A mission-control is a function that takes the 
current state of world (sensor-data) and computes a direction. If the direction (a vector) is not the zero-vector 
`(make-posn 0 0)`, then the rocket will fire its bursters (if there is fuel :) ) and try to move into that direction. 

> Don't forget, it is a orbit simulator. Just firing into a particular direciton might not actually move you there!

The very easiest mission-control is the one, not doing anything:

```racket
; Ship Earth Sat Moon Time -> Direction
; Example mission-control, not giving any instructions to the space-ship.
(define (do-nothing ship earth sat moon wall-time)
  ZERO-VECTOR)
```

A mission can then be started with:

```racket
; start the mission "do nothing"
(start-mission do-nothing)
```

If you need more fuel for your space-mission, you can also use the alternative

```racket
(start-mission/fuel do-nothing 50000)
```
which equips the space-ship with 50.000kg fuel in this case.

### Bodies
The datatypes `Ship`, `Eearth`, `Sat` and `Moon` are defined in [bodies.rkt](https://github.com/ps-tuebingen/gravity/blob/master/bodies.rkt). 
There you can also find functions to access properties like `velocity` or `fuel` of a celestial body. In the very file one can 
also find all the constants that make up the game world. For instance, the moon orbit is defined as:

```racket
; orbital distance of the moon to the earth, as a Distance
(define MOON-ORBIT 363.3e6)
```
which approximates the real world.

The satellite is an example for a real world GPS satellite. The initial orbit of the space-ship is the parking orbit 
of Apollo 10. Also fuel and thrust is approximately the one of the Apollo 10 
[Command Service Module (CSM)](https://en.wikipedia.org/wiki/Apollo_Command/Service_Module). Since Apollo 10 also had a
[yet another stage](https://en.wikipedia.org/wiki/S-IVB) for transfer from earth to moon, this is not 100% accurate :)

### Vectors and Time
The modules [vector.rkt](https://github.com/ps-tuebingen/gravity/blob/master/vector.rkt) and [time.rkt](https://github.com/ps-tuebingen/gravity/blob/master/time.rkt)
implement functions for computing with vectors and time, respectively. For instance vector.rkt provides a function for mulitplication with a scalar:

```racket
; Number Vector -> Vector
; Scale a vector by a scalar
(check-expect (v* 0 v1) ZERO-VECTOR)
(check-expect (v* 1 v1) v1)
(check-expect (v* 1 v2) v2)
(check-expect (v* 2 v1) (v+ v1 v1))
(define (v* scalar vector) ...)
```

### Other Modules
There are also a few other modules which implement the game-mechanics, orbital physics and rendering of the game:

- [orbital.rkt](https://github.com/ps-tuebingen/gravity/blob/master/orbital.rkt) : Implements the "physics-engine" for orbital simulations
- [world.rkt](https://github.com/ps-tuebingen/gravity/blob/master/world.rkt) : Contains functions that simulate the interaction of four celestial bodies.
- [mission.rkt](https://github.com/ps-tuebingen/gravity/blob/master/mission.rkt) : Contains definitions that model the state of a mission.
- [interact.rkt](https://github.com/ps-tuebingen/gravity/blob/master/interaction.rkt) : Contains functions that implement user-interactions like zooming-in.
- [render.rkt](https://github.com/ps-tuebingen/gravity/blob/master/render.rkt) : Contains functions that compute a graphical representation of the world.
- [start-mission.rkt](https://github.com/ps-tuebingen/gravity/blob/master/start-mission.rkt) : The only "non-BSL" file: Implements the programming interface to start a mission.

