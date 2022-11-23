;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname m4_l2_v6p3_starter-1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; Design the program multi-moons, that
; animates two moons moving across
; the (Martian) sky (each with their own
; x-position & velocity)

(define-struct moon [x v])

; A Moon is a (make-moon Real Real)
; Interpretation: a moon with an x-position
; and velocity

(define MOON-1 (make-moon 10 1))
(define MOON-2 (make-moon 350 -2))

(define (moon-temp m)
  (... (moon-x m) ...
       (moon-v m) ...))

(define-struct tm [m1 m2])

; A TwoMoons is a (make-tm Moon Moon)
; Interpretation: a system of moons!

(define TM-1 (make-tm MOON-1 MOON-2))

(define (tm-temp tm)
  (... (moon-temp (tm-m1 tm)) ...
       (moon-temp (tm-m2 tm)) ...))

(define-struct system [m others])

; A System is one of:
; - #false
; - (make-system Moon System)
; Interpretation: a system of moons!

(define SYSTEM-0 #false)
(define SYSTEM-1 (make-system MOON-1 SYSTEM-0))
(define SYSTEM-2 (make-system MOON-2 SYSTEM-1))

(define (system-temp s)
  (...
   (cond
     [(boolean? s) ...]
     [(system? s) ...
      (moon-temp (system-m s)) ...
      (system-temp (system-others s)) ...])))

; multi-moons : System -> System
; animates a system of moons!

(define (multi-moons initial-s)
  (big-bang initial-s
    [to-draw draw-system]
    [on-tick move-system]
    [on-key add-moon]))

; draw-tm : TwoMoons -> Image
; visualizes two moons on the sky

(define SIZE 400)
(define HALF (/ SIZE 2))

(define MOON (circle 25 "solid" "gray"))
(define SKY (square SIZE "solid" "pink"))

(check-expect
 (draw-tm TM-1)
 (place-image
  MOON
  10 HALF
  (place-image
   MOON
   350 HALF
   SKY)))

(define (draw-tm tm)
  (draw-moon (tm-m1 tm)
             (draw-moon (tm-m2 tm) SKY)))

; draw-system : System -> Image
; visualizes a system of moons

(check-expect
 (draw-system SYSTEM-0)
 SKY)

(check-expect
 (draw-system SYSTEM-2)
 (place-image
  MOON
  350 HALF
  (place-image
   MOON
   10 HALF
   SKY)))

(define (draw-system s)
  (cond
    [(boolean? s) SKY]
    [(system? s)
     (draw-moon (system-m s)
                (draw-system (system-others s)))]))

; draw-moon : Moon Image -> Image
; draws a moon on a background

(check-expect
 (draw-moon MOON-1 SKY)
 (place-image
  MOON
  10 HALF
  SKY))

(define (draw-moon m bg)
  (place-image
   MOON
   (moon-x m) HALF
   bg))

; move-tm : TwoMoons -> TwoMoons
; move two moons

(check-expect
 (move-tm TM-1)
 (make-tm
  (make-moon 11 1)
  (make-moon 348 -2)))

(define (move-tm tm)
  (make-tm (move-moon (tm-m1 tm))
           (move-moon (tm-m2 tm))))

; move-system : System -> System
; move a system of moons

(check-expect
 (move-system SYSTEM-0)
 SYSTEM-0)

(check-expect
 (move-system SYSTEM-2)
 (make-system
  (make-moon 348 -2)
  (make-system
   (make-moon 11 1)
   #false)))

(define (move-system s)
  (cond
    [(boolean? s) s]
    [(system? s)
     (make-system
      (move-moon (system-m s))
      (move-system (system-others s)))]))

; move-moon : Moon -> Moon
; moves the moon!

(check-expect (move-moon MOON-1)
              (make-moon 11 1))

(check-expect (move-moon MOON-2)
              (make-moon 348 -2))

(define (move-moon m)
  (make-moon (+ (moon-x m) (moon-v m))
             (moon-v m)))

; add-moon : System KeyEvent -> System
; adds a moon to the system

(define NEW-X 10)
(define NEW-V 3)

(check-expect (add-moon SYSTEM-0 " ")
              (make-system (make-moon NEW-X NEW-V) SYSTEM-0))

(check-expect
 (add-moon SYSTEM-2 "a")
 (make-system (make-moon NEW-X NEW-V) SYSTEM-2))

(define (add-moon s ke)
    (make-system (make-moon NEW-X NEW-V)s))












