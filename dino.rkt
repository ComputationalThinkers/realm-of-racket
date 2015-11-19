#lang racket

(require 2htdp/image 2htdp/universe)

;; constants
(define SCREEN-WIDTH 500)
(define SCREEN-HEIGHT 200)

;; data structure of the dino game
;; cactuses is a list of posn where the cactus is currently
(struct screen (dino cactuses))
;; dino data
;; we need to know the y of the dino and if it is jumping anot
(struct dino (posn jumping))
;; position
(struct posn (x y))

;; big bang
(define (start-dino-game)
  (big-bang (screen (dino (posn (5 0)) #f)
                    (list (fresh-cactus)
                          (fresh-cactus)))
            (on-tick next-screen TICK-RATE)
            (on-key jump-dino)
            (to-draw render-screen)
            (stop-when dead? render-end)))

(define (fresh-cactus)
  (posn 0 (random WIDTH-SIZE))
  )

;; clock ticks
(define (next-screen current-screen)
  (define dino (screen-dino current-screen))
  (define dino-position (dino-posn dino))
  (define dino-is-jumping (dino-jumping dino))
  
  (define cactuses (screen-cactuses current-screen))
  
  (cond [(dino-jumping dino)
         (cond [(> dino-position-y MAX-JUMP) (screen (dino-coming-down dino) (update-cactuses cactuses))]
               [else (screen (dino-going-up dino) (update-cactuses cactuses))])]
        [else (screen dino (update-cactuses cactuses))]
        )
  )

(define (update-cactuses cactuses)
  (cond [(empty? cactuses) empty]
        [else (cons (update-cactus (first cactuses)) (update-cactuses (rest cactuses)))])
  )

(define (update-cactus cactus)
  (posn cactus-x (+ cactus-y CACTUS-MOVE-RATE))
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
  (screen (dino (jump-position (dino-posn dino)) #t) cactuces)
  )

;; rendering
(define (render-screen screen)
  (dino+scene (screen-dino screen)
              (cactus-list+scene (screen-cactuses screen) MT-SCENE))
  )

(define (dino+scene dino scene)
  (img+scene dino DINO-IMG scene)
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
          [else (cons (cactus-loc (first cactuses)) (get-posns-from-cactus (rest cactuses)))])
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

  