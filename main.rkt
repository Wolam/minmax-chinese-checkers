#lang racket/gui
(require embedded-gui)

(define chinese-checkers-piece-snip-class
  (make-object
   (class snip-class%
     (super-new)
     (send this set-classname "chinese-checkers-piece-snip"))))

(send (get-the-snip-class-list) add chinese-checkers-piece-snip-class)

(define chinese-checkers-piece%
  (class snip%
    (init-field name glyph font size moves [location #f])
    (super-new)
    (send this set-snipclass chinese-checkers-piece-snip-class)
    (define/public (set-location l) (set! location l))
    (define/public (get-location) location)
    (define/public (color)
      (if (equal? name "W") 'white 'black))
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


(define (valid-jump-by-offset foffset roffset rank file moves board location first-call visited)
   (define-values (nrank nfile) (values (+ rank roffset) (+ file foffset)))
   (if (and (valid-rank? nrank) (valid-file? nfile))
        (let ((jump-candidate (rank-file->location nrank nfile)))
          (let ((piece (piece-at-location board jump-candidate)))
             (if (and (not piece) (not (member jump-candidate visited)))
                 (cons jump-candidate (valid-moves-by-offset board jump-candidate #f moves (cons jump-candidate visited)))
                moves)
  ))moves))

(define (valid-moves-by-offset board location first-call moves visited)
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
                [else (valid-jump-by-offset foffset roffset nrank nfile moves board location first-call visited)])))
    moves)))

(define piece-offsets '((-1 -1) (-1 0) (0 -1) (0 1) (1 0) (1 1)))

(define ((piece-moves color) board location)
  (valid-moves-by-offset
   board location #t '() '()))

(define chinese-checkers-piece-data
  (hash
   "W" (cons #\u26AA (piece-moves 'white))
   "B" (cons #\u26AB (piece-moves 'black))))

(define (make-chinese-checkers-piece id [location #f])
  (match-define (cons glyph moves) (hash-ref chinese-checkers-piece-data id))
  (define font (send the-font-list find-or-create-font 20 'default 'normal 'normal))
  (new chinese-checkers-piece% [name id] [glyph (string glyph)] [font font]
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
              (highlight-square dc location #f "seagreen"))
            (when highlight-location
              (highlight-square dc highlight-location #f "indianred")))
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
                      (if (eq? turn 'white) "IA" "Player's"))))
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
        ; check if the player won
        (cond [(check-win? 'black board '("j6" "i7" "h8" "g9" "j7" "i8" "h9") '("j9" "j8" "i9")) (set-message "Player Wins")]
              [(check-win? 'white board '("c0" "b1" "a2" "d0" "c1" "b2" "a3") '("a0" "a1" "b0")) (set-message "AI Wins")]))
      
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
  (define brush (send the-brush-list find-or-create-brush "gray" 'solid))
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
  ;agarrar diagonales
  ;comparar diagonales
  ;agarrar encerrados
  ;comparar encerrados
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