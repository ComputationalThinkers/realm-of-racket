#lang racket

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
(define render-screen screen)

  