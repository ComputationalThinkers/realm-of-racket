#lang racket

(require 2htdp/image 2htdp/universe)

;; constants
(define DINO-X 5)
(define DINO-Y 20)
(define TICK-RATE 1/10)
(define SCREEN-WIDTH 500)
(define SCREEN-HEIGHT 400)
(define MAX-JUMP (/ SCREEN-HEIGHT 2))
(define DINO-UP-DOWN-RATE 10)
(define CACTUS-MOVE-RATE 5)
(define MT-SCENE (empty-scene SCREEN-WIDTH SCREEN-HEIGHT))
(define DINO-IMG  (bitmap "resources/trex.png"))
(define SEG-SIZE 10)
(define CACTUS-IMG  (bitmap "resources/cactus.png"))
(define ENDGAME-TEXT-SIZE 10)

;; data structure of the dino game
;; cactuses is a list of posn where the cactus is currently
(struct screen (dino cactuses) #:transparent)
;; dino data
;; we need to know the y of the dino and if it is jumping anot
(struct dino (posn jumping) #:transparent)
;; position
(struct posn (x y) #:transparent)

;; big bang
(define (start-dino-game)
  (define new-screen (screen (dino (posn DINO-X DINO-Y) #f)
                             (list (fresh-cactus)
                                   (fresh-cactus))))
  (big-bang new-screen
            (on-tick next-screen TICK-RATE)
            (on-key jump-dino)
            (to-draw render-screen)
            (stop-when dead? render-end)))

(define (fresh-cactus)
  (posn (random SCREEN-WIDTH) 0)
  )

;; clock ticks
(define (next-screen current-screen)
  ;; (print current-screen)
  (printf "[next-screen] current-screen = ~a \n" current-screen)
  (define current-dino (screen-dino current-screen))
  (define dino-position (dino-posn current-dino))
  (define dino-is-jumping (dino-jumping current-dino))
  
  (define cactuses (screen-cactuses current-screen))
  
  (cond [(dino-jumping current-dino)
         (define dino-posn-y (posn-y current-dino))
         (cond [(> dino-posn-y MAX-JUMP)
                (screen (dino-coming-down current-dino) (update-cactuses cactuses))
                ]
               [else
                (screen (dino-going-up current-dino) (update-cactuses cactuses))
                ])]
        [else
         (define new-dino (dino-coming-down current-dino))
         (printf "[next-screen] new-dino = ~a \n" new-dino)
         (define new-cactuses (update-cactuses cactuses))
         (printf "[next-screen] new-cactuses = ~a \n" new-cactuses)
         (screen new-dino new-cactuses)]
        )
  )

(define (dino-coming-down current-dino)
  (printf "[dino-coming-down] dino = ~a \n" current-dino)
  (define dino-position (dino-posn current-dino))
  (cond [(< (posn-y dino-position) (+ DINO-Y MAX-JUMP))
         (dino (posn DINO-X DINO-Y) (dino-jumping current-dino))]
        [else
         (define new-position (posn DINO-X (- (posn-y dino-position) DINO-UP-DOWN-RATE)))
         (printf "[dino-coming-down] new-position = ~a \n" new-position)
         (define is-dino-jumping (dino-jumping current-dino))
         (printf "[dino-coming-down] is-dino-jumping = ~a \n" is-dino-jumping)
         (define new-dino (dino new-position is-dino-jumping))
         (printf "[dino-coming-down] new-dino = ~a \n" new-dino) 
         new-dino])
  )

(define (update-cactuses cactuses)
  (cond [(empty? cactuses) empty]
        [else cons (update-cactus (first cactuses)) (update-cactuses (rest cactuses))])
  )

(define (update-cactus cactus)
  posn (posn-x cactus) (+ (posn-y cactus) CACTUS-MOVE-RATE)
  )

(define (dino-going-up dino)
  (define dino-position (dino-posn dino))
  dino (posn 5 (+ (posn-y dino-position) DINO-UP-DOWN-RATE)) (dino-jumping dino)
  )

;; key-events
(define (jump-dino screen key)
  (cond [(valid-key? key) (make-dino-jump screen)]
        [else screen])
  )

(define (valid-key? key)
  (key=? key "up")
  )

(define (make-dino-jump screen)
  (define dino (screen-dino screen))
  (define cactuses (screen-cactuses screen))
  screen (dino (DINO-UP-DOWN-RATE (dino-posn dino)) #t) cactuses
  )

;; rendering
(define (render-screen screen)
  (cond [(screen? screen)
         (printf "[render-scene] dino = ~a \n" (screen-dino screen))
         (dino+scene (screen-dino screen)
                     (cactus-list+scene (screen-cactuses screen) MT-SCENE))]
        [else MT-SCENE])
  )

(define (dino+scene dino scene)
  (printf "[dino+scene] dino = ~a \n" dino)
  ;; bug
  (define dino-position (dino-posn dino))
  (img+scene dino-position DINO-IMG scene)
  )

(define (img+scene posn img scene)
  (place-image img
               (* (posn-x posn) SEG-SIZE)
               (* (posn-y posn) SEG-SIZE)
               scene)
  )

(define (cactus-list+scene cactuses scene)
  (define (get-posns-from-cactus cactuses)
    (cond [(empty? cactuses) empty]
          [else cons (first cactuses) (get-posns-from-cactus (rest cactuses))])
    )
  (img-list+scene (get-posns-from-cactus cactuses) CACTUS-IMG scene)
  )

(define (img-list+scene posns img scene)
  (cond [(empty? posns) scene]
        [else (img+scene (first posns) img (img-list+scene (rest posns) img scene))])
  )

;; end game
(define (dead? screen)
  #f
  )

(define (render-end screen)
  (overlay (text "Game Over" ENDGAME-TEXT-SIZE "black")
           (render-screen screen))
  )

;; start the game
(start-dino-game)

  