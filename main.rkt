#lang racket/gui
(require embedded-gui)

(define depth-level 2)
(define chinese-checkers-piece-snip-class
  (make-object
   (class snip-class%
     (super-new)
     (send this set-classname "chinese-checkers-piece-snip"))))

(send (get-the-snip-class-list) add chinese-checkers-piece-snip-class)

(define chinese-checkers-piece%
  (class snip%
    (init-field id name glyph font size moves [location #f])
    (super-new)
    (send this set-snipclass chinese-checkers-piece-snip-class)
    (define/public (set-location l) (set! location l))
    (define/public (get-location) location)
    (define/public (color)
      (if (equal? name "W") 'white 'black))
    (define/public (set-id i) (set! id i))
    (define/public (get-id) id)
    (define/public (valid-moves)
      (let ((admin (send this get-admin)))
        (if (and admin location)        ; can be #f is the snip is not owned
            (let ((board (send admin get-editor)))
              (moves board location))
            ;; Return an empty list if this piece is not on a board
            '())))

    (define/override (get-extent dc x y width height descent space lspace rspace)
      (when width (set-box! width size))
      (when height (set-box! height size))
      (when descent (set-box! descent 0.0))
      (when space (set-box! space 0.0))
      (when lspace (set-box! lspace 0.0))
      (when rspace (set-box! rspace 0.0)))

    (define/override (draw dc x y . other)
      (send dc set-font font)
      (send dc set-text-foreground "black")
      (define-values (glyph-width glyph-height baseline extra-space)
        (send dc get-text-extent glyph font #t))
      (let ((ox (/ (- size glyph-width) 2))
            (oy (/ (- size glyph-height 2))))
        (send dc draw-text glyph (+ x ox) (+ y oy))))
    ))
(define (valid-rank? rank) (and (>= rank 0) (< rank 10)))
(define (valid-file? file) (and (>= file 0) (< file 10)))


(define (valid-jump-by-hash-offset white-pieces black-pieces location nrank nfile moves first-call visited color double-jumps)
   (if (and (valid-rank? nrank) (valid-file? nfile))
        (let ((jump-candidate (rank-file->location nrank nfile)))
                  (let ((piece (hash-ref hash-positions2 jump-candidate)))
             (cond
               [(and (not piece) (not (member jump-candidate visited))) 
               (cons (cons jump-candidate double-jumps) (valid-moves-by-hashes-offset white-pieces black-pieces jump-candidate #f moves (cons jump-candidate visited) color (+ double-jumps 10)))]
               [else moves])
   ))moves))

(define (valid-moves-by-hashes-offset white-pieces black-pieces location first-call moves visited color double-jumps)
  (define-values (rank file) (location->rank-file location))
  (for/fold ([moves moves])
            ([offset (in-list (get-piece-offsets color))])
    
    (match-define (list roffset foffset) offset)
    (define-values (nrank nfile) (values (+ rank roffset) (+ file foffset)))
    (if (and (valid-rank? nrank) (valid-file? nfile))
        (let ((candidate (rank-file->location nrank nfile)))
          (let ((piece (hash-ref hash-positions2 candidate)))
            (cond [(and (not piece) (not first-call)) moves]
                [(and (not piece) first-call) (cons (cons candidate double-jumps) moves)]
                [else (valid-jump-by-hash-offset white-pieces black-pieces location (+ nrank roffset) (+ nfile foffset) moves first-call visited color double-jumps)])))
    moves)))

(define (valid-jump-by-offset nrank nfile moves board location first-call visited)
   (if (and (valid-rank? nrank) (valid-file? nfile))
        (let ((jump-candidate (rank-file->location nrank nfile))) 
          (let ((piece (piece-at-location board jump-candidate)))
             (if (and (not piece) (not (member jump-candidate visited)))
                 (cons jump-candidate (valid-moves-by-board-offset board jump-candidate #f moves (cons jump-candidate visited)))
                moves)
  ))moves))

(define (valid-moves-by-board-offset board location first-call moves visited)
  (define-values (rank file) (location->rank-file location))
  (for/fold ([moves moves])
            ([offset (in-list piece-offsets)])

    (match-define (list roffset foffset) offset)
    (define-values (nrank nfile) (values (+ rank roffset) (+ file foffset)))
    (if (and (valid-rank? nrank) (valid-file? nfile))
        (let ((candidate (rank-file->location nrank nfile)))
          (let ((piece (piece-at-location board candidate)))
            (cond [(and (not piece) (not first-call)) moves]
                [(and (not piece) first-call) (cons candidate moves)]
                [else (valid-jump-by-offset (+ nrank roffset) (+ nfile foffset) moves board location first-call visited)])))
    moves)))

(define piece-offsets '((-1 -1) (-1 0) (0 1) (0 -1) (1 0) (1 1)))

(define (get-piece-offsets color)
  (if (eq? color 'black) '((-1 0) (0 1))
      '((0 -1) (1 0))))

;(0 -1) (1 0) N
;(-1 0) (0 1) W

(define ((piece-moves color) board location)
  (valid-moves-by-board-offset
   board location #t '() '())
  )

(define chinese-checkers-piece-data
  (hash
   "W" (cons #\u26AA (piece-moves 'white))
   "B" (cons #\u26AB (piece-moves 'black))))


(define (get-id-counter)
  (let ((current-id piece-id-counter))
    (set! piece-id-counter (+ piece-id-counter 1))
    current-id))

(define piece-id-counter 0)
(define (make-chinese-checkers-piece color-name [location #f])
  (match-define (cons glyph moves) (hash-ref chinese-checkers-piece-data color-name))
  (define font (send the-font-list find-or-create-font 20 'default 'normal 'normal))
  (new chinese-checkers-piece% [id (get-id-counter)] [name color-name] [glyph (string glyph)] [font font]
                    [size 35] [location location] [moves moves]))

(define chinese-checkers-board%
  (class pasteboard%
    (super-new)

    (define drag-dx 0)
    (define drag-dy 0)
    (define highlight-location #f)
    (define valid-move-locations '())
    (define turn 'black)
    (define message #f)
    (define message-timer
      (new timer%
           [notify-callback (lambda ()
                              (set! message #f)
                              (send (send this get-canvas) refresh))]))

    
    (define (set-message m)
      (set! message m)
      (send message-timer start 2000)
      (send (send this get-canvas) refresh))


     (define/override (on-paint before? dc . other)
      (if before?
          (begin
            (draw-chinese-checkers-board dc)
            (for ((location (in-list valid-move-locations)))
              (highlight-square dc location #f "green"))
            (when highlight-location
              (highlight-square dc highlight-location #f "red")))
          ;; message is drawn after the snips
          (when message
            (display-message dc message))))
    
    
    (define/augment (after-insert chinese-checkers-piece . rest)
      (position-piece this chinese-checkers-piece))
    
    (define/augment (on-display-size)
      (send this begin-edit-sequence)
      (let loop ([snip (send this find-first-snip)])
        (when snip
          ;; Reposition the piece, since the location is stored as text
          ;; (e.g. d3) its new coordinates will be recomputed to the correct
          ;; place
          (position-piece this snip)
          (loop (send snip next))))
      (send this end-edit-sequence))

    (define/augment (on-move-to snip x y dragging?)
      (when dragging?
        (let ((location (xy->location this (+ x drag-dx) (+ y drag-dy))))
          (unless (equal? highlight-location location)
            (set! highlight-location location)
            (send (send this get-canvas) refresh)))))

    (define/augment (can-interactive-move? event)
      (define piece (send this find-next-selected-snip #f))
      ;; The user tried to move a piece of the opposite color, remind them
      ;; again
      (unless (eq? turn (send piece color))
        (set-message (format "It's ~a turn to move"
                      (if (eq? turn 'white) "AI" "Player's"))))
      (eq? turn (send piece color)))


    (define/augment (on-interactive-move event)
      (define piece (send this find-next-selected-snip #f))
      (define-values (x y) (values (box 0) (box 0)))
      (send this get-snip-location piece x y #f)
      (set! drag-dx (- (send event get-x) (unbox x)))
      (set! drag-dy (- (send event get-y) (unbox y))))

    (define/augment (after-interactive-move event)
      (define piece (send this find-next-selected-snip #f))
      (define location (xy->location this (send event get-x) (send event get-y)))
      (define valid-moves (send piece valid-moves))
      (when (member location valid-moves)
        ;; This is a valid move, remove any target piece and update the piece
        ;; location
        (let ((target-piece (piece-at-location this location)))
          (when (and target-piece (not (eq? piece target-piece)))
            (send target-piece set-location #f)
            (send this remove target-piece)))
        (set! turn (if (eq? turn 'white) 'black 'white))
        (send piece set-location location)
        (set! hash-positions2 (hash-set hash-positions2 (hash-ref hash-black-pieces (send piece get-id)) #f))
        (set! hash-positions2 (hash-set hash-positions2 location #t))
        (set! hash-black-pieces (hash-set hash-black-pieces (send piece get-id) (list location 0)))
        
        ; check if the player won
        (cond [(check-win? 'black board '("j6" "i7" "h8" "g9" "j7" "i8" "h9") '("j9" "j8" "i9")) (set-message "Player Wins")]
              [(check-win? 'white board '("c0" "b1" "a2" "d0" "c1" "b2" "a3") '("a0" "a1" "b0")) (set-message "AI Wins")])
        (set! initial-flag #f)
        (when (eq? turn 'white)
          (when (< initial-move-counter 5)
            (let ((positions (second (get-initial-move))))
              (send (piece-at-location board (first positions)) set-location (second positions))
              (position-piece board (piece-at-location board (second positions))))
            (set! initial-flag #t))
          (when (not initial-flag)
            (let ((positions (alpha-beta-search hash-black-pieces hash-white-pieces depth-level)))
              (printf "positions: ~a\n" positions)
              (set! hash-white-pieces (hash-set hash-white-pieces (send (piece-at-location board (second positions)) get-id) (third positions)))
              (set! hash-positions2 (hash-set hash-positions2 (second positions) #f))
              (set! hash-positions2 (hash-set hash-positions2 (car(third positions)) #t))
              (send (piece-at-location board (second positions)) set-location (car(third positions)))
              (position-piece board (piece-at-location board (car(third positions))))))
          (set! turn 'black)
         )
        )
      
      ;; If the move is not valid it will be moved back.
      (position-piece this piece)
      (set! highlight-location #f)
      ;; Note: piece is still selected, but the valid moves are relative to
      ;; the new position
      (set! valid-move-locations (send piece valid-moves))
      (send (send this get-canvas) refresh))

    (define/augment (after-select snip on?)
      (if on?
          (begin
            (unless (eq? turn (send snip color))
              (set-message (format "It's ~a turn to move"
                            (if (eq? turn 'white) "IA" "Player's"))))
            (set! valid-move-locations (send snip valid-moves)))
          (begin
            (set! valid-move-locations '())))
      (send (send this get-canvas) refresh))

    ))



(define black-pieces-points
    (hash
    "a0" 100
    "b0" 95 "a1" 95 
    "c0" 90 "b1" 90 "a2" 90
    "d0" 85 "c1" 85 "b2" 85 "a3" 85
    "e0" 80 "d1" 80 "c2" 80 "b3" 80 "a4" 80
    "f0" 75 "e1" 75 "d2" 75 "c3" 75 "b4" 75 "a5" 75 
    "g0" 70 "f1" 70 "e2" 70 "d3" 70 "c4" 70 "b5" 70 "a6" 70
    "h0" 65 "g1" 65 "f2" 65 "e3" 65 "d4" 65 "c5" 65 "b6" 65 "a7" 65
    "i0" 60 "h1" 60 "g2" 60 "f3" 60 "e4" 60 "d5" 60 "c6" 60 "b7" 60 "a8" 60
    "j0" 55 "i1" 55 "h2" 55 "g3" 55 "f4" 55 "e5" 55 "d6" 55 "c7" 55 "b8" 55 "a9" 55
    "j1" 50 "i2" 50 "h3" 50 "g4" 50 "f5" 50 "e6" 50 "d7" 50 "c8" 50 "b9" 50 
    "j2" 45 "i3" 45 "h4" 45 "g5" 45 "f6" 45 "e7" 45 "d8" 45 "c9" 45
    "j3" 40 "i4" 40 "h5" 40 "g6" 40 "f7" 40 "e8" 40 "d9" 40
    "j4" 35 "i5" 35 "h6" 35 "g7" 35 "f8" 35 "e9" 35
    "j5" 30 "i6" 30 "h7" 30 "g8" 30 "f9" 30 
    "j6" 25 "i7" 25 "h8" 25 "g9" 25
    "j7" 20 "i8" 20 "h9" 20
    "j8" 15 "i9" 15
    "j9" 10 
    ))

(define white-pieces-points
    (hash
   "a0" 10
    "b0" 15 "a1" 15 
    "c0" 20 "b1" 20 "a2" 20
    "d0" 25 "c1" 25 "b2" 25 "a3" 25
    "e0" 30 "d1" 30 "c2" 30 "b3" 30 "a4" 30
    "f0" 35 "e1" 35 "d2" 35 "c3" 35 "b4" 35 "a5" 35 
    "g0" 40 "f1" 40 "e2" 40 "d3" 40 "c4" 40 "b5" 40 "a6" 40
    "h0" 45 "g1" 45 "f2" 45 "e3" 45 "d4" 45 "c5" 45 "b6" 45 "a7" 45
    "i0" 50 "h1" 50 "g2" 50 "f3" 50 "e4" 50 "d5" 50 "c6" 50 "b7" 50 "a8" 50
    "j0" 55 "i1" 55 "h2" 55 "g3" 55 "f4" 55 "e5" 55 "d6" 55 "c7" 55 "b8" 55 "a9" 55
    "j1" 60 "i2" 60 "h3" 60 "g4" 60 "f5" 60 "e6" 60 "d7" 60 "c8" 60 "b9" 60 
    "j2" 65 "i3" 65 "h4" 65 "g5" 65 "f6" 65 "e7" 65 "d8" 65 "c9" 65
    "j3" 70 "i4" 70 "h5" 70 "g6" 70 "f7" 70 "e8" 70 "d9" 70
    "j4" 75 "i5" 75 "h6" 75 "g7" 75 "f8" 75 "e9" 75
    "j5" 80 "i6" 80 "h7" 80 "g8" 80 "f9" 80 
    "j6" 85 "i7" 85 "h8" 85 "g9" 85
    "j7" 90 "i8" 90 "h9" 90
    "j8" 95 "i9" 95
    "j9" 100 
    ))

; (substring "a1" 0 1) = a
; (substring "a1" 1 2) = 1
(define white-distance-points
    (hash
     "j" 10
     "i" 15
     "h" 20
     "g" 25
     "f" 30
     "e" 35
     "d" 40
     "c" 45
     "b" 50
     "a" 55
      "0"  5
      "1"  30 
      "2"  30
      "3"  30
      "4"  25
      "5"  20
      "6"  15
      "7"  10
      "8"  10
      "9"  5   ))

(define black-distance-points
    (hash
     "a" 10
     "b" 15
     "c" 20
     "d" 25
     "e" 30
     "f" 35
     "g" 40
     "h" 45
     "i" 50
     "j" 55
     "0" 5
     "1" 10
      "2"  10
      "3"  15
      "4"  20
      "5"  25
      "6"  30
      "7"  30
      "8"  30
      "9"  5   ))

(define initial-flag #t)

(define (get-initial-move)
  (if (empty? initial-AI-moves) #f
  (let ((current-move (car initial-AI-moves)))
  (set! initial-AI-moves (cdr initial-AI-moves))
  (set! initial-move-counter (+ initial-move-counter 1))
    current-move)))


(define (sum-black-points blacks)
  (let ((sum 0)) 
  (for ([(piece-id pos-and-jumps) blacks])
   ;; (displayln (hash-ref black-pieces-points (car pos-and-jumps)))
     (set! sum (+ sum (hash-ref black-pieces-points (car pos-and-jumps))
                      (hash-ref black-distance-points (substring (car pos-and-jumps) 0 1))
                      (hash-ref black-distance-points (substring (car pos-and-jumps) 1 2)))))
     ;(displayln sum))
    sum))

(define (sum-white-points whites)
  (let ((sum 0)) 
  (for ([(piece-id pos-and-jumps) whites])
    ;(displayln (hash-ref white-pieces-points (car pos-and-jumps)))
     (set! sum (+ sum (hash-ref white-pieces-points (car pos-and-jumps))
                      (hash-ref white-distance-points (substring (car pos-and-jumps) 0 1))
                      (hash-ref white-distance-points (substring (car pos-and-jumps) 1 2)))))
    sum))


(define (eval white-pieces black-pieces)
  (+ (sum-black-points black-pieces) (sum-white-points white-pieces)))

(define (alpha-beta-search black-pieces white-pieces total-depth)
  (max-value white-pieces black-pieces -10000 10000 0 total-depth))


(define (max-value white-pieces black-pieces a b current-depth total-depth)
  (cond
    [(eq? current-depth total-depth) (list (eval white-pieces black-pieces) #f #f #f)];En esta parte se llama al eval
    [else
    (define b-value -10000)
    (define b-original-pos #f)
    (define b-new-pos #f)
    (for ([(piece-id current-piece) white-pieces])
            (let ((possible-piece-moves (remove-duplicates (valid-moves-by-hashes-offset white-pieces black-pieces (car current-piece) #t '() '() 'white 0))))
              (for ([current-move possible-piece-moves])
                (let ((result (min-value black-pieces (hash-set white-pieces piece-id current-move) a b current-depth total-depth)))
                (when (> result b-value)
                  (set! b-value result)
                  (when (eq? current-depth 0)
                  (set! b-original-pos (car current-piece))
                  (set! b-new-pos current-move))
                  (when (>= b-value b)
                    (list b-value b-original-pos b-new-pos))
                      (set! a (max a b-value)))))))
    (list b-value b-original-pos b-new-pos)]))


(define (min-value black-pieces white-pieces a b current-depth total-depth)
    (define b-value 10000)
    (for ([(piece-id current-piece) black-pieces])
            (let ((possible-piece-moves (remove-duplicates (valid-moves-by-hashes-offset white-pieces black-pieces (car current-piece) #t '() '() 'black 0))))
              (for ([current-move possible-piece-moves])
                (let ((result (max-value white-pieces (hash-set black-pieces piece-id current-move) a b (+ current-depth 1) total-depth)))
                (when (< (first result) b-value)
                  (set! b-value (first result))
                  (when (<= b-value a)
                    b-value)
                      (set! b (min b b-value)))))))
    b-value)


(define initial-AI-moves '( '("h9" "h7") '("h7" "g7") '("j9" "f7") '("i8" "g6") '("g9" "g8")))
(define initial-move-counter 0)

(define hash-positions2
  (hash "a0" #t "a1" #t "a2" #t "a3" #t "a4" #f "a5" #f "a6" #f "a7" #f "a8" #f "a9" #f
        "b0" #t "b1" #t "b2" #t "b3" #f "b4" #f "b5" #f "b6" #f "b7" #f "b8" #f "b9" #f
        "c0" #t "c1" #t "c2" #f "c3" #f "c4" #f "c5" #f "c6" #f "c7" #f "c8" #f "c9" #f
        "d0" #t "d1" #f "d2" #f "d3" #f "d4" #f "d5" #f "d6" #f "d7" #f "d8" #f "d9" #f
        "e0" #f "e1" #f "e2" #f "e3" #f "e4" #f "e5" #f "e6" #f "e7" #f "e8" #f "e9" #f
        "f0" #f "f1" #f "f2" #f "f3" #f "f4" #f "f5" #f "f6" #f "f7" #t "f8" #f "f9" #f
        "g0" #f "g1" #f "g2" #f "g3" #f "g4" #f "g5" #f "g6" #t "g7" #t "g8" #t "g9" #f
        "h0" #f "h1" #f "h2" #f "h3" #f "h4" #f "h5" #f "h6" #f "h7" #f "h8" #t "h9" #f
        "i0" #f "i1" #f "i2" #f "i3" #f "i4" #f "i5" #f "i6" #f "i7" #t "i8" #f "i9" #t
        "j0" #f "j1" #f "j2" #f "j3" #f "j4" #f "j5" #f "j6" #t "j7" #t "j8" #t "j9" #f))


(define hash-black-pieces
    (hash
     0 '("a0" 0)
     1 '("a1" 0)
     2 '("a2" 0)
     3 '("a3" 0)
     4 '("b0" 0)
     5 '("b1" 0)
     6 '("b2" 0)
     7 '("c0" 0)
     8 '("c1" 0)
     9 '("d0" 0)))

(define hash-white-pieces
    (hash
     10 '("f7" 0)
     11 '("j8" 0)
     12 '("j7" 0)
     13 '("j6" 0)
     14 '("i9" 0)
     15 '("g6" 0)
     16 '("i7" 0)
     17 '("g7" 0)
     18 '("h8" 0)
     19 '("g8" 0)))


(define (position-piece board piece)
  (define-values (canvas-width canvas-height)
    (let ((c (send board get-canvas)))
      (send c get-size)))
  (define-values (square-width square-height)
    (values (/ canvas-width 10) (/ canvas-height 10)))
  
  (define-values (rank file)
    (location->rank-file (send piece get-location)))
  (define-values (square-x square-y)
    (values (* file square-width) (* rank square-height)))
  (define piece-width (snip-width piece))
  (define piece-height (snip-height piece))
  (send board move-to piece
        (+ square-x (/ (- square-width piece-width) 2))
        (+ square-y (/ (- square-height piece-height) 2))))

(define (location->rank-file location )
  (unless (and (string? location) (= (string-length location) 2))
    (raise-argument-error 'location "valid chinese-checkers position a0 .. j9" location))
  (define file
    (index-of '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j) (string-ref location 0)))
  (define rank
    (index-of '(#\9 #\8 #\7 #\6 #\5 #\4 #\3 #\2 #\1 #\0) (string-ref location 1)))
  (unless (and rank file)
    (raise-argument-error 'location "valid chinese-checkers position a0 .. j9" location))
  (values rank file))


(define (rank-file->location rank file)
  (unless (<= 0 rank 10)
    (raise-argument-error 'rank "integer between 0 and 9" rank))
  (unless (<= 0 file 10)
    (raise-argument-error 'rank "integer between 0 and 9" file))
  (string
   (list-ref '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j) file)
   (list-ref '(#\9 #\8 #\7 #\6 #\5 #\4 #\3 #\2 #\1 #\0) rank)))

(define (black-positions hash-1 hash-2)
  (if (hash-has-key? hash-1 1) hash-1 hash-2))
(define (white-positions hash-1 hash-2)
  (if (hash-has-key? hash-1 1) hash-2 hash-1))


(define (xy->location board x y)
  (define-values (canvas-width canvas-height)
    (let ((c (send board get-canvas)))
      (send c get-size)))
  (define-values (square-width square-height)
    (values (/ canvas-width 10) (/ canvas-height 10)))
  (define-values (rank file)
    (values (exact-truncate (/ y square-height)) (exact-truncate (/ x square-width))))
  (rank-file->location rank file))

(define (piece-at-location board location)
  (let loop ((snip (send board find-first-snip)))
    (if snip
        (if (equal? location (send snip get-location))
            snip
            (loop (send snip next)))
        #f)))

(define (display-message dc message)
  (define font (send the-font-list find-or-create-font 24 'default 'normal 'normal))
  (define-values [w h _1 _2] (send dc get-text-extent message font #t))
  (define-values (dc-width dc-height) (send dc get-size))
  (define-values (x y) (values (/ (- dc-width w) 2) (/ (- dc-height h) 2)))

  (define brush (send the-brush-list find-or-create-brush "bisque" 'solid))
  (define pen (send the-pen-list find-or-create-pen "black" 1 'transparent))
  (send dc set-brush brush)
  (send dc set-pen pen)
  (send dc draw-rectangle 0 y dc-width h)
  (send dc set-font font)
  (send dc set-text-foreground "firebrick")
  (send dc draw-text message x y))


(define (draw-chinese-checkers-board dc)
  (define brush (send the-brush-list find-or-create-brush "Tan" 'solid))
  (define pen (send the-pen-list find-or-create-pen "black" 1 'transparent))
  (define font (send the-font-list find-or-create-font 10 'default 'normal 'normal))
  (define-values (dc-width dc-height) (send dc get-size))
  (define cell-width (/ dc-width 10))
  (define cell-height (/ dc-height 10))
  (define margin 3)
    
  (send dc clear)
  (send dc set-brush brush)
  (send dc set-pen pen)
  (send dc set-font font)
  
  (for* ([row (in-range 10)] [col (in-range 10)]
         #:when (or (and (odd? row) (even? col))
                    (and (even? row) (odd? col))))
    (define-values [x y] (values (* col cell-width) (* row cell-height)))
    (send dc draw-rectangle x y cell-width cell-height))

  (define brush-2 (send the-brush-list find-or-create-brush "Saddle Brown" 'solid))
  (send dc set-brush brush-2)
  
  (for* ([row (in-range 10)] [col (in-range 10)]
         #:when (or (and (even? row) (even? col))
                    (and (odd? row) (odd? col))))
    (define-values [x y] (values (* col cell-width) (* row cell-height)))
    (send dc draw-rectangle x y cell-width cell-height))

  (for ([(rank index) (in-indexed '("9" "8" "7" "6" "5" "4" "3" "2" "1" "0"))])
    (define-values [_0 h _1 _2] (send dc get-text-extent rank font #t))
    (define y (+ (* index cell-height) (- (/ cell-height 2) (/ h 2))))
    (send dc draw-text rank margin y))

  (for ([(file index) (in-indexed '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j"))])
    (define-values [w h _1 _2] (send dc get-text-extent file font #t))
    (define x (+ (* index cell-width) (- (/ cell-width 2) (/ w 2))))
    (send dc draw-text file x (- dc-height h margin))))


(define (highlight-square dc location color-name border-color-name)
  (define-values (rank file) (location->rank-file location))
  (define brush
    (if color-name
        (let* ((base (send the-color-database find-color color-name))
               (color (make-object color% (send base red) (send base green) (send base blue) 0.3)))
          (send the-brush-list find-or-create-brush color 'solid))
        (send the-brush-list find-or-create-brush "black" 'transparent)))
  (define pen
    (if border-color-name
        (send the-pen-list find-or-create-pen border-color-name 2 'solid)
        (send the-pen-list find-or-create-pen "black" 1 'transparent)))
  (send dc set-pen pen)
  (send dc set-brush brush)
  (define-values (dc-width dc-height) (send dc get-size))
  (define-values (cell-width cell-height) (values (/ dc-width 10) (/ dc-height 10)))
  (send dc draw-rectangle (* file cell-width) (* rank cell-height) cell-width cell-height))


;; A test program for our chinese-checkers-piece% objects:

;; The pasteboard% that will hold and manage the chinese-checkers pieces
(define board (new chinese-checkers-board%))
;; Toplevel window for our application
(define toplevel (new frame% [label "Chinese Checkers Board"] [width 1024] [height 720]))
;; The canvas which will display the pasteboard contents
(define canvas (new editor-canvas%
                    [parent toplevel]
                    [style '(no-hscroll no-vscroll)]
                    [horizontal-inset 0]
                    [vertical-inset 0]
                    [editor board]))
(send toplevel show #t)

(define letters '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j"))
(define numbers '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))


(define initial
  (string-append
   "Ba0Ba1Ba2Ba3Bb0Bb1Bb2Bc0Bc1Bd0"
   "Wj9Wj8Wj7Wj6Wi9Wi8Wi7Wh9Wh8Wg9"))

(define (color-at-location location board)
    (let ((piece (piece-at-location board location)))
             (if piece
                 (send piece color)
                 #f)))

(define (colors-at-locations locations packed board)
 (if (empty? locations)
     packed
     (colors-at-locations (cdr locations) (cons (color-at-location (car locations) board) packed) board)))

(define (check-win? color board diagonals trappeds)
  (if (equal? color 'black) (and (equal? (colors-at-locations diagonals '() board) first-diagonals-black-win)
       (member (colors-at-locations trappeds '() board) black-win-cases))
      (and (equal? (colors-at-locations diagonals '() board) first-diagonals-white-win)
       (member (colors-at-locations trappeds '() board) white-win-cases)))
  )

; c0 b1 a2 d0 c1 b2 a3
(define first-diagonals-white-win (list 'white 'white 'white 'white 'white 'white 'white))
; j6 i7 h8 g9 j7 i8 h9
(define first-diagonals-black-win (list 'black 'black 'black 'black 'black 'black 'black))



; a0 a1 b0
(define one-trapped1-white-win (list 'black 'white 'white))
(define one-trapped2-white-win (list 'white 'black 'white))
(define one-trapped3-white-win (list 'white 'white 'black))
(define two-trapped1-white-win (list 'white 'black 'black))
(define two-trapped2-white-win (list 'black 'white 'black))
(define two-trapped3-white-win (list 'black 'white 'black))
(define all-trapped-white-win (list 'white 'white 'white))
(define all-trapped-black-win (list 'black 'black 'black))

; j9 j8 i9
(define one-trapped1-black-win (list 'white 'black 'black))
(define one-trapped2-black-win (list 'black 'white 'black))
(define one-trapped3-black-win (list 'black 'black 'white))
(define two-trapped1-black-win (list 'white 'black 'white))
(define two-trapped2-black-win (list 'white 'white 'black))
(define two-trapped3-black-win (list 'black 'white 'white))


(define white-win-cases (list one-trapped1-white-win one-trapped2-white-win one-trapped3-white-win
                                                    two-trapped1-white-win two-trapped2-white-win two-trapped3-white-win all-trapped-white-win all-trapped-black-win))

(define black-win-cases (list one-trapped1-black-win one-trapped2-black-win one-trapped3-black-win
                                                    two-trapped1-black-win two-trapped2-black-win two-trapped3-black-win all-trapped-black-win all-trapped-white-win))


(define (setup-board board position)
  (send board clear)
  (define piece-count (/ (string-length position) 3))
  (for ([index (in-range piece-count)])
    (define pos (* index 3))
    (define name (substring position pos (add1 pos)))
    (define location (substring position (add1 pos) (+ (add1 pos) 2)))
    (send board insert (make-chinese-checkers-piece name location))))

(setup-board board initial)