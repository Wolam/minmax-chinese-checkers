#lang racket/gui
(require embedded-gui)

(define depth-level 3)
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



(define (valid-jump-by-hash-offset white-pieces black-pieces location nrank nfile moves first-call visited color)
   (if (and (valid-rank? nrank) (valid-file? nfile))
        (let ((jump-candidate (rank-file->location nrank nfile)))
                  (let ((piece (or (eq? (hash-ref hash-positions2 jump-candidate) 1) #f)))
             (if (and (not piece) (not (member jump-candidate visited)))
                 (cons jump-candidate (valid-moves-by-hashes-offset white-pieces black-pieces jump-candidate #f moves (cons jump-candidate visited) color))
                moves)
  ))moves))


(define (valid-moves-by-hashes-offset white-pieces black-pieces location first-call moves visited color)
  (define-values (rank file) (location->rank-file location))
  (for/fold ([moves moves])
            ([offset (in-list (get-piece-offsets color))])

    (match-define (list roffset foffset) offset)
    (define-values (nrank nfile) (values (+ rank roffset) (+ file foffset)))
    (if (and (valid-rank? nrank) (valid-file? nfile))
        (let ((candidate (rank-file->location nrank nfile)))
          (let ((piece (or (eq? (hash-ref hash-positions2 candidate) 1) #f)))
            (cond [(and (not piece) (not first-call)) moves]
                [(and (not piece) first-call) (cons candidate moves)]
                [else (valid-jump-by-hash-offset white-pieces black-pieces location (+ nrank roffset) (+ nfile foffset) moves first-call visited color)])))
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
        (set! hash-positions2 (hash-set hash-positions2 (hash-ref hash-black-pieces (send piece get-id)) 0))
        (set! hash-positions2 (hash-set hash-positions2 location 1))
        (set! hash-black-pieces (hash-set hash-black-pieces (send piece get-id) location))
        
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
              (set! hash-white-pieces (hash-set hash-white-pieces (send (piece-at-location board (second positions)) get-id) (third positions)))
              (set! hash-positions2 (hash-set hash-positions2 (second positions) 0))
              (set! hash-positions2 (hash-set hash-positions2 (third positions) 1))
              (send (piece-at-location board (second positions)) set-location (third positions))
              (position-piece board (piece-at-location board (third positions)))))
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

(define initial-flag #t)

(define (get-initial-move)
  (if (empty? initial-AI-moves) #f
  (let ((current-move (car initial-AI-moves)))
  (set! initial-AI-moves (cdr initial-AI-moves))
  (set! initial-move-counter (+ initial-move-counter 1))
    current-move)))

(define (eval white-pieces black-pieces)
  (random -500 500))

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
            (let ((possible-piece-moves (remove-duplicates (valid-moves-by-hashes-offset white-pieces black-pieces current-piece #t '() '() 'white))))
              (for ([current-move possible-piece-moves])
                (let ((result (min-value black-pieces (hash-set white-pieces piece-id current-move) a b current-depth total-depth)))
                (when (> result b-value)
                  (set! b-value result)
                  (when (eq? current-depth 0)
                  (set! b-original-pos current-piece)
                  (set! b-new-pos current-move))
                  (when (>= b-value b)
                    (list b-value b-original-pos b-new-pos))
                      (set! a (max a b-value)))))))
    (list b-value b-original-pos b-new-pos)]))

(define (min-value black-pieces white-pieces a b current-depth total-depth)
    (define b-value 10000)
    (for ([(piece-id current-piece) black-pieces])
            (let ((possible-piece-moves (remove-duplicates (valid-moves-by-hashes-offset white-pieces black-pieces current-piece #t '() '() 'black))))
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
  (hash "a0" 1 "a1" 1 "a2" 1 "a3" 1 "a4" 0 "a5" 0 "a6" 0 "a7" 0 "a8" 0 "a9" 0
        "b0" 1 "b1" 1 "b2" 1 "b3" 0 "b4" 0 "b5" 0 "b6" 0 "b7" 0 "b8" 0 "b9" 0
        "c0" 1 "c1" 1 "c2" 0 "c3" 0 "c4" 0 "c5" 0 "c6" 0 "c7" 0 "c8" 0 "c9" 0
        "d0" 1 "d1" 0 "d2" 0 "d3" 0 "d4" 0 "d5" 0 "d6" 0 "d7" 0 "d8" 0 "d9" 0
        "e0" 0 "e1" 0 "e2" 0 "e3" 0 "e4" 0 "e5" 0 "e6" 0 "e7" 0 "e8" 0 "e9" 0
        "f0" 0 "f1" 0 "f2" 0 "f3" 0 "f4" 0 "f5" 0 "f6" 0 "f7" 1 "f8" 0 "f9" 0
        "g0" 0 "g1" 0 "g2" 0 "g3" 0 "g4" 0 "g5" 0 "g6" 1 "g7" 1 "g8" 1 "g9" 0
        "h0" 0 "h1" 0 "h2" 0 "h3" 0 "h4" 0 "h5" 0 "h6" 0 "h7" 0 "h8" 1 "h9" 0
        "i0" 0 "i1" 0 "i2" 0 "i3" 0 "i4" 0 "i5" 0 "i6" 0 "i7" 1 "i8" 0 "i9" 1
        "j0" 0 "j1" 0 "j2" 0 "j3" 0 "j4" 0 "j5" 0 "j6" 1 "j7" 1 "j8" 1 "j9" 0))

(define hash-black-pieces
    (hash
     0 "a0"
     1 "a1"
     2 "a2"
     3 "a3"
     4 "b0"
     5 "b1"
     6 "b2"
     7 "c0"
     8 "c1"
     9 "d0"))

(define hash-white-pieces
    (hash
     10 "f7"
     11 "j8"
     12 "j7"
     13 "j6"
     14 "i9"
     15 "g6"
     16 "i7"
     17 "g7"
     18 "h8"
     19 "g8"))


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

(define (location->rank-file location)
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

#|
(define hash-positions
  (hash
   (for ([letter letters])
     (for ([number numbers])
     (string-append letter number) #f))))
|#


  
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