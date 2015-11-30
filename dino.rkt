#lang racket

(require 2htdp/image 2htdp/universe)

;; constants
(define DINO-X 5)
(define DINO-Y 20)
(define TICK-RATE 1/20)
(define SCREEN-WIDTH 500)
(define SCREEN-HEIGHT 400)
(define MAX-JUMP (/ SCREEN-HEIGHT 2))
(define DINO-UP-DOWN-RATE 4)
(define CACTUS-MOVE-RATE 1)
(define MT-SCENE (empty-scene SCREEN-WIDTH SCREEN-HEIGHT))
(define DINO-IMG  (bitmap "resources/trex.png"))
(define SEG-SIZE 10)
(define CACTUS-IMG  (bitmap "resources/cactus.png"))
(define ENDGAME-TEXT-SIZE 20)

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
                             (list (fresh-cactus) (fresh-cactus)
                                   )))
  (big-bang new-screen
            (on-tick next-screen TICK-RATE)
            (on-key jump-dino)
            (to-draw render-screen)
            (stop-when dead? render-end)))

(define (fresh-cactus)
  (define new-cactus (posn (random SCREEN-HEIGHT) DINO-Y))
  new-cactus
  )

;; clock ticks
(define (next-screen current-screen)
  ;; (printf "[next-screen] current-screen = ~a \n" current-screen)
  (define current-dino (screen-dino current-screen))
  (define dino-position (dino-posn current-dino))
  (define dino-is-jumping (dino-jumping current-dino))
  
  (define cactuses (screen-cactuses current-screen))
  
  (cond [(dino-jumping current-dino)
         ;; dino is jumping
         (define dino-posn-y (posn-y (dino-posn current-dino)))
         (screen (dino-going-up current-dino) (update-cactuses cactuses))]
        [else
         ;; dino coming down
         (screen (dino-coming-down current-dino) (update-cactuses cactuses))])
  )

(define (dino-coming-down current-dino)
  (define dino-position (dino-posn current-dino))
  (define new-y (+ (posn-y dino-position) DINO-UP-DOWN-RATE))
  (dino (posn DINO-X (if (> new-y DINO-Y) DINO-Y new-y)) (dino-jumping current-dino))
  )

(define (update-cactuses cactuses)
  (cond [(empty? cactuses) empty]
        [else (cons
               (update-cactus (first cactuses))
               (update-cactuses (rest cactuses))
               )])
  )

(define (update-cactus cactus)
  (cond [(< (posn-x cactus) 0) (fresh-cactus)]
        [else (posn (- (posn-x cactus) CACTUS-MOVE-RATE) (posn-y cactus))])
  )

(define (dino-going-up current-dino)
  (define dino-position (dino-posn current-dino))
  (define new-y (- (posn-y dino-position) DINO-UP-DOWN-RATE))
  (define is-dino-jumping (if (positive? new-y) (dino-jumping current-dino) #f))
  (dino (posn DINO-X new-y) is-dino-jumping)
  )

;; key-events
(define (jump-dino screen key)
  (cond [(valid-key? key) (make-dino-jump screen)]
        [else screen])
  )

(define (valid-key? key)
  (key=? key "up")
  )

(define (make-dino-jump previous-screen)
  (define jumping-dino (dino (dino-posn (screen-dino previous-screen)) #t))
  (screen jumping-dino (screen-cactuses previous-screen))
)

;; rendering
(define (render-screen current-screen)
  (cond [(screen? current-screen)
         ;; (printf "[render-scene] dino = ~a \n" (screen-dino current-screen))
         (dino+scene (screen-dino current-screen)
                     (cactus-list+scene (screen-cactuses current-screen) MT-SCENE))]
        [else MT-SCENE])
  )

(define (dino+scene dino scene)
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
          [else (cons (first cactuses) (get-posns-from-cactus (rest cactuses)))])
    )
  (img-list+scene (get-posns-from-cactus cactuses) CACTUS-IMG scene)
  )

(define (img-list+scene posns img scene)
  (cond [(empty? posns) scene]
        [else (img+scene (first posns) img (img-list+scene (rest posns) img scene))])
  )

;; end game
(define (dead? screen)
  (define current-dino-position (dino-posn (screen-dino screen)))
  (define (hit? dino-posn cactuses)
    (cond [(empty? cactuses) #f]
          [else (or (equal? dino-posn (first cactuses)) (hit? dino-posn (rest cactuses)))])
    )
  (hit? current-dino-position (screen-cactuses screen))
  )

(define (render-end screen)
  (overlay (text "Game Over" ENDGAME-TEXT-SIZE "black")
           (render-screen screen))
  )

;; start the game
(start-dino-game)

  