#lang htdp/isl+
(require 2htdp/image)
(require 2htdp/universe)
;; N-Puzzle Problem Setup

;; [General Definitions]:
 
(define TILE-LEN 200)
(define TILE-COLOR 'blue)
(define BORDER-COLOR 'black)
(define TEXT-COLOR 'black)
(define TEXT-SIZE 72)
(define SCENE-LENGTH (* 3 TILE-LEN))

;; ------ THE QUEUE -------

;; Definition: A Queue, a-queue, is a list of x
;; Sample a-queue Instances
(define E-QUEUE '())
(define QUEUE1 '(1 2 4))
(define QUEUE2 '(p l a n t))

;; qempty?: Queue -> Boolean
;; Purpose: To determine if the queue is empty
(define (qempty? a-queue)
  (empty? a-queue))

;; qfirst: Queue -> Queue
;; Purpose: To identify the first element of a queue
(define (qfirst a-queue)
  (if (empty? a-queue)
      (error "Cannot get first of empty queue.")
      (first a-queue)))

;; enqueue: Listof X Queue -> Queue
;; Purpose: To append the queue to the front of a given list
(define (enqueue a-lox a-queue)
  (append a-lox
          a-queue))

;; dequeue: Queue -> Queue
;; Purpose: To remove the first element from a queue
(define (dequeue a-queue)
  (if (empty? a-queue)
      (error "Cannot use dequeue on empty queue.")
      (rest a-queue)))

;; [Logic To Refer To]:

;; A tile value whatever value rests on a tile
;; Ex. a blank tile -> 0
;; Ex. a non-blank tile -> 1-8

;; [The Good Stuff (actually coding the logic)]:

;; world holds tile values, these are the things that will shift around
(define-struct world (t0 t1 t2 t3 t4 t5 t6 t7 t8))
;; sample worlds
(define SAMPLE (make-world 1 2 3 4 0 5 6 7 8))
(define WIN (make-world 1 2 3 4 5 6 7 8 0))
(define ALMOST-WIN (make-world 1 2 3 4 5 6 7 0 8))
(define JUMBLED-W1 (make-world 1 8 3 7 5 4 2 6 0))
(define JUMBLED-W2 (make-world 4 3 2 7 6 1 8 5 0))

;; Define a shuffle function
;; Shuffle: List -> List
;; Purpose: Shuffle the order of a list
(define (shuffle-list l)
  (cond
    [(empty? l) '()]
    [else (local
            [(define random-tile (list-ref l (random (length l))))
             (define new-tile-list (remove random-tile l))]
            (cons random-tile (shuffle-list new-tile-list)))]))

;Sample Lists
(define L0 '(1 2 3 4 5 6 7 8))
(define L1 '())
(define L2 '(2389 4302 329 48 1))

;Sample instances
(define SHUF0 (shuffle-list L0))
(define SHUF1 (shuffle-list L1))
(define SHUF2 (shuffle-list L2))

;Tests for shuffle list
(check-random (shuffle-list L0) )

(define world-shuffle
  (local
    [(define (shuffle-func w)
       (shuffle-list (list (world-t0 w) (world-t1 w) (world-t2 w) (world-t3 w) (world-t4 w) (world-t5 w) (world-t6 w) (world-t7 w) (world-t8 w))))
     (define (solvable? lon)
       (foldl
        (lambda (n m) (if (< n m) 0 n))))] (shuffle-func SAMPLE)))

(define init-world
(make-world (list-ref world-shuffle 0) (list-ref world-shuffle 1) (list-ref world-shuffle 2) (list-ref world-shuffle 3) (list-ref world-shuffle 4) (list-ref world-shuffle 5) (list-ref world-shuffle 6) (list-ref world-shuffle 7) (list-ref world-shuffle 8)))

;; return tile image for a specific number
(define (make-tile-img tile-num)
  (if (= tile-num 0)
      (overlay (square TILE-LEN 'outline TEXT-COLOR)
               (square TILE-LEN 'solid TILE-COLOR))
      (overlay (text (number->string tile-num) TEXT-SIZE TEXT-COLOR)
               (square TILE-LEN 'outline TEXT-COLOR)
               (square TILE-LEN 'solid TILE-COLOR))))

(define T0 (make-tile-img 0))
(define T1 (make-tile-img 1))
(define T2 (make-tile-img 2))
(define T3 (make-tile-img 3))
(define T4 (make-tile-img 4))
(define T5 (make-tile-img 5))
(define T6 (make-tile-img 6))
(define T7 (make-tile-img 7))
(define T8 (make-tile-img 8))

;; As the slides said we can think of each group of tiles as rows above each other
;; like so:

(above (beside (make-tile-img (world-t0 SAMPLE))
               (make-tile-img (world-t1 SAMPLE))
               (make-tile-img (world-t2 SAMPLE)))
       (beside (make-tile-img (world-t3 SAMPLE))
               (make-tile-img (world-t4 SAMPLE))
               (make-tile-img (world-t5 SAMPLE)))
       (beside (make-tile-img (world-t6 SAMPLE))
               (make-tile-img (world-t7 SAMPLE))
               (make-tile-img (world-t8 SAMPLE))))

;; ------ The Reoccurring Functions ------

(define neighbors ' ((1 3) ;; -> Bpos next to blank if its in the top left (BPOS = 0)
                     (4 0 2)
                     (1 5)
                     (0 4 6)
                     (7 1 3 5)
                     (2 4 8)
                     (3 7)
                     (8 4 6)
                     (5 7)))

;; ------ Big Bang Functions ------
(define (draw-world a-world)
  (local
    [(define (tile-img a-tval)
       ;; Purpose: make an image for each tile value in world 
       (cond
         [(= a-tval 0) T0]
         [(= a-tval 1) T1]
         [(= a-tval 2) T2]
         [(= a-tval 3) T3]
         [(= a-tval 4) T4]
         [(= a-tval 5) T5]
         [(= a-tval 6) T6]
         [(= a-tval 7) T7]
         [(= a-tval 8) T8]))]
    (above(beside (tile-img (world-t0 a-world))
                  (tile-img (world-t1 a-world))
                  (tile-img (world-t2 a-world)))
          (beside (tile-img (world-t3 a-world))
                  (tile-img (world-t4 a-world))
                  (tile-img (world-t5 a-world)))
          (beside (tile-img (world-t6 a-world))
                  (tile-img (world-t7 a-world))
                  (tile-img (world-t8 a-world))))))

;; game-won?: World -> Boolean
;; Purpose: To determine if a given world is equal to the Winning world
(define (game-won? a-world)
  (equal? a-world WIN))

;Sample ...
(define W0 (make-world 1 2 3 4 5 6 7 8 0))
(define W1 (make-world 0 2 4 6 8 1 3 5 7))
(define W2 (make-world 0 8 7 6 5 4 3 2 1))

;Sample ...
(define W0-VAL (equal? W0 WIN))
(define W1-VAL (equal? W1 WIN))
(define W2-VAL (equal? W2 WIN))

;Tests for sample...
;(check-expect (...)...)

;Tests for game-won?
(check-expect (game-won? W0) #t)
(check-expect (game-won? W1) #f)
(check-expect (game-won? W2) #f)

;; draw-last-world: World -> World
;; Purpose: To draw the world that indicates the game is over
(define (draw-last-world a-world)
  (overlay/xy (overlay
               (text "YOU WIN!" 40 TEXT-COLOR)
               (square SCENE-LENGTH 'solid TILE-COLOR))
              0
              0
              (draw-world a-world)))

;; Before we push for key functionality, we must make some easy definitions to reuse for valid keys

;; Key Definitions
(define RIGHT "right")
(define LEFT "left")
(define UP "up")
(define DOWN "down")
(define SPACE " ")
(define HELP "h")
   

;; ---------------(PROCESS KEY)------------------------------ 

;; world -> natnum
;; Purpose: Compute the manhattan distance for the world
(define (manhattan-distance a-world)
  (local
    [;; bpos → natnum Purpose: Sum the tile distances
     (define (sum-distances a-bpos)
       (local [
               ;; world bpos → bval Purpose: Return given bpos’ bval
               (define (tile-value a-bpos)
                 (cond
                   [(= a-bpos 0) (world-t0 a-world)]
                   [(= a-bpos 1) (world-t1 a-world)]
                   [(= a-bpos 2) (world-t2 a-world)]
                   [(= a-bpos 3) (world-t3 a-world)]
                   [(= a-bpos 4) (world-t4 a-world)]
                   [(= a-bpos 5) (world-t5 a-world)]
                   [(= a-bpos 6) (world-t6 a-world)]
                   [(= a-bpos 7) (world-t7 a-world)]
                   [(= a-bpos 8) (world-t8 a-world)]))
               ;; tval → bpos Purpose: Return WIN bpos for given tval
               (define (final-bpos a-tval)
                 (if (= a-tval 0)
                     8
                     (sub1 a-tval)))
               ;; bpos → natnum Purpose: Return bpos’ row
               (define (get-row a-bpos)
                 (quotient a-bpos 3))
               ;; bpos → natnum Purpose: Return bpos’ column
               (define (get-col a-bpos)
                 (remainder a-bpos 3))
               ;; bpos bpos → natnum Purpose: Return distance
               (define (distance bpos1 bpos2)
                 (if (= (tile-value bpos1) 0) 0
                     (+ (abs
                         (- (get-row bpos1) (get-row bpos2)))
                        (abs (- (get-col bpos1) (get-col bpos2))))))
               (define win-bpos (final-bpos (tile-value a-bpos)))]
         (if (= a-bpos 0)
             (distance a-bpos win-bpos)
             (+ (distance a-bpos win-bpos) (sum-distances (sub1 a-bpos))))))]
    (sum-distances 8)))

(define (misplaced-tiles a-world)
  (local
    [(define (win-pos? t-val a-bpos)
       (if (or (= t-val 0)
               (= t-val a-bpos))
           0
           1))]
    (+ (win-pos? (world-t0 a-world) (world-t0 WIN))
       (win-pos? (world-t1 a-world) (world-t1 WIN))
       (win-pos? (world-t2 a-world) (world-t2 WIN))
       (win-pos? (world-t3 a-world) (world-t3 WIN))
       (win-pos? (world-t4 a-world) (world-t4 WIN))
       (win-pos? (world-t5 a-world) (world-t5 WIN))
       (win-pos? (world-t6 a-world) (world-t6 WIN))
       (win-pos? (world-t7 a-world) (world-t7 WIN)))))

(define (find-solution-a* func a-llow visited)
  (local
    [(define best-path
       (foldl
        (λ (p accum)
          (if (< (func (first p))
                 (func (first accum)))
              p
              accum))
        (first a-llow)
        (rest a-llow)))
     (define first-world (first best-path))]
    (if (equal? first-world WIN)
        (reverse best-path)
        (local
          [(define successors
             (filter
              (λ (w) (not (member? w visited)))
              (map (λ (neigh)
                     (swap-empty first-world neigh))
                   (list-ref neighbors
                             (get-blank-pos first-world)))))
           (define new-paths
             (map (λ (w) (cons w best-path))
                  successors))
           (define new-llow
             (append (remove best-path a-llow)
                     new-paths))]
          (find-solution-a*
           func new-llow
           (cons first-world visited))))))

(define (make-move a-world)
  (if (equal? a-world WIN)
      a-world
      (second
       (find-solution-a* misplaced-tiles
                         (list (list a-world))
                         '()))))

(define (make-move-manhattan a-world)
  (if (equal? a-world WIN)
      a-world
      (second
       (find-solution-a* manhattan-distance
                         (list (list a-world))
                         '()))))

(define (process-key a-world key)
  (local
    [(define (is-valid-key key)
       (or (key=? key LEFT)
           (key=? key RIGHT)
           (key=? key UP)
           (key=? key DOWN)
           (key=? key HELP)))

     (define (get-blank-pos a-world)
       (cond
         [(= (world-t0 a-world) 0) 0]
         [(= (world-t1 a-world) 0) 1]
         [(= (world-t2 a-world) 0) 2]
         [(= (world-t3 a-world) 0) 3]
         [(= (world-t4 a-world) 0) 4]
         [(= (world-t5 a-world) 0) 5]
         [(= (world-t6 a-world) 0) 6]
         [(= (world-t7 a-world) 0) 7]
         [(= (world-t8 a-world) 0) 8]))

     ;; a-vk a-world
     ;; returns the board position value of the target b-pos
     (define (get-target-bpos a-vk a-world)
       (local [(define BLNK (get-blank-pos a-world))]
         (cond
           [(key=? a-vk LEFT)
            (if (member BLNK '(0 3 6)) BLNK (- BLNK 1))]
           [(key=? a-vk RIGHT)
            (if (member BLNK '(2 5 8)) BLNK (+ BLNK 1))]
           [(key=? a-vk UP)
            (if (member BLNK '(0 1 2)) BLNK (- BLNK 3))]
           [(key=? a-vk DOWN)
            (if (member BLNK '(6 7 8)) BLNK (+ BLNK 3))])))

     ;; ---- swap-empty IS HERE, INSIDE ----

     ;; a-world a-bpos -> a-world
     ;; Using one of its helper function "new-tile-value"
     ;; returns a whole new world (haha) with the target b-pos' and blan b-pos' positions swapped
     
     (define (swap-empty a-world target)
       (local
         [(define BLANK (get-blank-pos a-world))

          ;; board position -> tile value
          ;; returns the tile value of a given board position
          (define (get-tile-value a-bpos)
            (cond
              [(= a-bpos 0) (world-t0 a-world)]
              [(= a-bpos 1) (world-t1 a-world)]
              [(= a-bpos 2) (world-t2 a-world)]
              [(= a-bpos 3) (world-t3 a-world)]
              [(= a-bpos 4) (world-t4 a-world)]
              [(= a-bpos 5) (world-t5 a-world)]
              [(= a-bpos 6) (world-t6 a-world)]
              [(= a-bpos 7) (world-t7 a-world)]
              [(= a-bpos 8) (world-t8 a-world)]))

          ;; a-bpos -> a tile value
          ;; returns the transformed tile value after a move is made
          ;; if the bpos is neither the target nor the blank it stays the same
          (define (new-tile-value a-bpos)
            (cond
              [(= a-bpos target) 0]
              [(= a-bpos BLANK)
               (get-tile-value target)]
              [else
               (get-tile-value a-bpos)]))]
         
         (make-world
          (new-tile-value 0)
          (new-tile-value 1)
          (new-tile-value 2)
          (new-tile-value 3)
          (new-tile-value 4)
          (new-tile-value 5)
          (new-tile-value 6)
          (new-tile-value 7)
          (new-tile-value 8))))]
    
    ;; -------- FINAL EXPRESSION --------
    (cond
      [(not (is-valid-key key)) a-world]
      [(key=? key HELP)
       (make-move a-world)]
      [else
       (local [(define target (get-target-bpos key a-world))]
         (if (= target (get-blank-pos a-world))
             a-world
             (swap-empty a-world target)))])))

;;----------------------------------------------------

;; Duplicated Expressions For Testing:


;; world → natnum Purpose: Compute the Manhattan
(define (get-blank-pos a-world)
  (cond
    [(= (world-t0 a-world) 0) 0]
    [(= (world-t1 a-world) 0) 1]
    [(= (world-t2 a-world) 0) 2]
    [(= (world-t3 a-world) 0) 3]
    [(= (world-t4 a-world) 0) 4]
    [(= (world-t5 a-world) 0) 5]
    [(= (world-t6 a-world) 0) 6]
    [(= (world-t7 a-world) 0) 7]
    [(= (world-t8 a-world) 0) 8]))


(define (swap-empty a-world target)
  (local
    [(define BLANK (get-blank-pos a-world))
     (define (get-tile-value a-bpos)
       (cond
         [(= a-bpos 0) (world-t0 a-world)]
         [(= a-bpos 1) (world-t1 a-world)]
         [(= a-bpos 2) (world-t2 a-world)]
         [(= a-bpos 3) (world-t3 a-world)]
         [(= a-bpos 4) (world-t4 a-world)]
         [(= a-bpos 5) (world-t5 a-world)]
         [(= a-bpos 6) (world-t6 a-world)]
         [(= a-bpos 7) (world-t7 a-world)]
         [(= a-bpos 8) (world-t8 a-world)]))
          
     (define (new-tile-value a-bpos)
       (cond
         [(= a-bpos target) 0]
         [(= a-bpos BLANK)
          (get-tile-value target)]
         [else
          (get-tile-value a-bpos)]))]
         
    (make-world
     (new-tile-value 0)
     (new-tile-value 1)
     (new-tile-value 2)
     (new-tile-value 3)
     (new-tile-value 4)
     (new-tile-value 5)
     (new-tile-value 6)
     (new-tile-value 7)
     (new-tile-value 8))))

;;----------------------------------------------------


#;(check-expect (make-move (make-world 1 2 3
                                       4 0 6
                                       7 5 8))
                (make-world 1 2 3
                            4 5 6
                            7 0 8))

(big-bang init-world
  (to-draw draw-world)
  (on-key process-key)
  (stop-when game-won? draw-last-world))

"Misplaced Tiles Tests"
(define mt-test1 (time (make-move JUMBLED-W1)))
(define mt-test2 (time (make-move-manhattan JUMBLED-W2)))
#;(define mt-test2 (time (make-move SAMPLE)))
;init-world
;(define mt-test3 (time (make-move init-world)))

"Manhattan Distance Tests"
(define md-test1 (time (make-move-manhattan JUMBLED-W1)))
(define md-test2 (time (make-move-manhattan JUMBLED-W2)))
#;(define md-test2 (time (make-move-manhattan SAMPLE)))
#;(define md-test3 (time (make-move-manhattan init-world))) 

;; A world like (make-world 4 3 1 2 6 0 8 7 5), where all of the tiles are misplaced, reveals flaws in both the misplaced-tiles and manhattan heuristics. 1. All the tiles are misplaced so the search will be longer. 2. Making the next move might not guarantee that you're getting closer to the winning solution. 3. You might end up increasing the value to find the right solution, making the search longer.

"JUMBLED-W1 Tests"
"Run 1: MT - 156 MD - 171"
"Run 2: MT - 0 MD - 15"
"Run 3: MT - 15 MD - 0"
"Run 4: MT - 46 MD - 78"
"Run 5: MT - 0 MD - 15"
"Average Times: MT(Outlier 156)- 15.25 MD(Outlier 171)- 27"
"Relative Difference (f-s-a*): MT - 44% faster MD - 77% slower"

"JUMBLED-W2 Tests"
"Run 1: MT -  MD - 171"
"Run 2: MT - 0 MD - 15"
"Run 3: MT - 15 MD - 0"
"Run 4: MT - 46 MD - 78"
"Run 5: MT - 0 MD - 15"
"Average Times: MT(Outlier )-  MD(Outlier )- "
"Relative Difference (f-s-a*): MT - 44% faster MD - 77% slower"