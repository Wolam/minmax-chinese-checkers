#lang racket/gui
(require embedded-gui)

(define depth-level 2)

; A snip class is needed for every "kind" of snip that is managed in the pasteboard
(define chinese-checkers-piece-snip-class
  (make-object
   (class snip-class%
     (super-new)
     (send this set-classname "chinese-checkers-piece-snip"))))

; The chinese-checkers-piece-snip instance needs to be registered
(send (get-the-snip-class-list) add chinese-checkers-piece-snip-class)

; A snip% class to represent our chinese checkers pieces.
; There is a single class for all pieces
; The fields of the constructor are: 
; id, name, glyph (unicode character for the chinese checkers piece), font, size, moves, location, color 
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
            ; Return an empty list if this piece is not on a board
            '())))

    ; Return the size of this snip on the board
    (define/override (get-extent dc x y width height descent space lspace rspace)
      (when width (set-box! width size))
      (when height (set-box! height size))
      (when descent (set-box! descent 0.0))
      (when space (set-box! space 0.0))
      (when lspace (set-box! lspace 0.0))
      (when rspace (set-box! rspace 0.0)))

    ; Draw the chinese checkers piece on the board at X, Y location
    (define/override (draw dc x y . other)
      (send dc set-font font)
      (send dc set-text-foreground "black")
      ; Find the dimensions of the glyph so that it is drawn in the middle of
      ; the chinese checkers piece
      (define-values (glyph-width glyph-height baseline extra-space)
        (send dc get-text-extent glyph font #t))
      (let ((ox (/ (- size glyph-width) 2))
            (oy (/ (- size glyph-height 2))))
        (send dc draw-text glyph (+ x ox) (+ y oy))))
    ))

; Valid row and column ranges
(define (valid-rank? rank) (and (>= rank 0) (< rank 10)))
(define (valid-file? file) (and (>= file 0) (< file 10)))

; Auxiliary function that returns the valid double jumps
(define (valid-jump-by-hash-offset hash-tile-status nrank nfile moves visited color double-jumps)
   (if (and (valid-rank? nrank) (valid-file? nfile))
        (let ((jump-candidate (rank-file->location nrank nfile)))
                  (let ((piece (hash-ref hash-tile-status jump-candidate)))
             (cond
               [(and (not piece) (not (member jump-candidate visited))) 
               (cons (cons jump-candidate double-jumps) (valid-moves-by-hashes-offset hash-tile-status jump-candidate #f moves (cons jump-candidate visited) color (+ double-jumps 7)))]
               [else moves])
   ))moves))

; Determine the list of valid moves by applying an offset to the current location
; This function returns the list of valid moves and works with a hash that contains locations and booleans
(define (valid-moves-by-hashes-offset hash-tile-status location first-call moves visited color double-jumps)
  (define-values (rank file) (location->rank-file location))
  (for/fold ([moves moves])
            ([offset (in-list (get-piece-offsets color))])
    
    (match-define (list roffset foffset) offset)
    (define-values (nrank nfile) (values (+ rank roffset) (+ file foffset)))
    (if (and (valid-rank? nrank) (valid-file? nfile))
        (let ((candidate (rank-file->location nrank nfile)))
          (let ((piece (hash-ref hash-tile-status candidate)))
            (cond [(and (not piece) (not first-call)) moves]
                [(and (not piece) first-call) (cons (cons candidate double-jumps) moves)]
                [else (valid-jump-by-hash-offset hash-tile-status (+ nrank roffset) (+ nfile foffset) moves visited color double-jumps)])))
    moves)))

; Auxiliary function that returns the valid double jumps
(define (valid-jump-by-offset nrank nfile moves board location first-call visited)
   (if (and (valid-rank? nrank) (valid-file? nfile))
        (let ((jump-candidate (rank-file->location nrank nfile))) 
          (let ((piece (piece-at-location board jump-candidate)))
             (if (and (not piece) (not (member jump-candidate visited)))
                 (cons jump-candidate (valid-moves-by-board-offset board jump-candidate #f moves (cons jump-candidate visited)))
                moves)
  ))moves))

; Determine the list of valid moves by applying an offset to the current location
; This function returns the list of valid moves and works with the board positions
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

; Offsets for player's pieces
(define piece-offsets '((-1 -1) (-1 0) (0 1) (0 -1) (1 0) (1 1)))

; Offsets for AI's pieces
(define (get-piece-offsets color)
  (if (eq? color 'black) '((-1 0) (0 1) (-1 -1) (1 1))
      '((-1 -1) (0 -1) (1 0) (1 1))))

; Returns the valid list of moves for a chinese checkers piece
(define ((piece-moves color) board location)
  (valid-moves-by-board-offset
   board location #t '() '())
  )

; Data of the two types of pieces
; White (AI)
; Black (Player)
(define chinese-checkers-piece-data
  (hash
   "W" (cons #\u26AA (piece-moves 'white))
   "B" (cons #\u26AB (piece-moves 'black))))

; sequence to create the id of the pieces
(define (get-id-counter)
  (let ((current-id piece-id-counter))
    (set! piece-id-counter (+ piece-id-counter 1))
    current-id))

(define piece-id-counter 0)

; Create a new chinese checkers piece snip based on color and location. This function
; determines the rest of the parameters required by the chinese-checkers-piece% class
(define (make-chinese-checkers-piece color-name [location #f])
  (match-define (cons glyph moves) (hash-ref chinese-checkers-piece-data color-name))
  (define font (send the-font-list find-or-create-font 20 'default 'normal 'normal))
  (new chinese-checkers-piece% [id (get-id-counter)] [name color-name] [glyph (string glyph)] [font font]
                    [size 35] [location location] [moves moves]))

; A pasteboard to represent a chinese checkers game board. It allows placing pieces at
; the correct locations and moving them according to chinese checkers game rules
(define chinese-checkers-board%
  (class pasteboard%
    (super-new)

    (define drag-dx 0)
    (define drag-dy 0)
    (define highlight-location #f) ; A location name that should be highlighted on the board
    (define valid-move-locations '()) ; Valid moves that will be highlighted in special way
    (define turn 'black)
    (define message #f) ; Message to be displayed to the user

    ; A message timer is used to clear the message after a timeout period
    (define message-timer
      (new timer%
           [notify-callback (lambda ()
                              (set! message #f)
                              (send (send this get-canvas) refresh))]))

    ; Set a message to be displayed to the user. The message will be
    ; displayed for a period of time, then it will disappear automatically
    (define (set-message m)
      (set! message m)
      (send message-timer start 2000)
      ; refresh the canvas
      (send (send this get-canvas) refresh))

     ; This method is responsible for drawing any non-interactive
     ; parts of the chinese checkers board game: the board itself, any highlighted
     ; squares plus a message displayed to the user (if any).  This method is
     ; invoked twice: first, before the chinese checkers pieces are drawn and once after.
     (define/override (on-paint before? dc . other)
      (if before?
          (begin
            (draw-chinese-checkers-board dc)
            (for ((location (in-list valid-move-locations)))
              (highlight-square dc location #f "green"))
            (when highlight-location
              (highlight-square dc highlight-location #f "red")))
          ; message is drawn after the snips
          (when message
            (display-message dc message))))
    
    ; This method is invoked after a snip is inserted into the pasteboard.
    ; We use this opportunity to position the snip at the right coordinates,
    ; corresponding to its location.
    (define/augment (after-insert chinese-checkers-piece . rest)
      (position-piece this chinese-checkers-piece))
    
    (define/augment (on-display-size)
      (send this begin-edit-sequence)
      (let loop ([snip (send this find-first-snip)])
        (when snip
          ; Reposition the piece, since the location is stored as text
          ; (e.g. d3) its new coordinates will be recomputed to the correct
          ; place
          (position-piece this snip)
          (loop (send snip next))))
      (send this end-edit-sequence))

    ; This method is invoked repeatedly when a piece is moved (whether by
    ; dragging it or by a call to move-to)
    (define/augment (on-move-to snip x y dragging?)
      (when dragging?
        (let ((location (xy->location this (+ x drag-dx) (+ y drag-dy))))
          (unless (equal? highlight-location location)
            (set! highlight-location location)
            (send (send this get-canvas) refresh)))))

    ; This method is invoked when the user attempts to drag a chinese checkers piece on
    ; the board. If it returns #f, the drag is not permitted 
    (define/augment (can-interactive-move? event)
      (define piece (send this find-next-selected-snip #f))
      ; The user tried to move a piece of the opposite color, remind them again
      (unless (eq? turn (send piece color))
        (set-message (format "It's ~a turn to move"
                      (if (eq? turn 'white) "AI" "Player's"))))
      (eq? turn (send piece color)))

    ; This method is invoked once only when the user begins to drag a chinese checkers
    ; piece and only if can-interactive-move? allowed the drag to happen
    (define/augment (on-interactive-move event)
      (define piece (send this find-next-selected-snip #f))
      (define-values (x y) (values (box 0) (box 0)))
      (send this get-snip-location piece x y #f)
      (set! drag-dx (- (send event get-x) (unbox x)))
      (set! drag-dy (- (send event get-y) (unbox y))))

    ; This method is invoked when the user finished dragging a chinese checkers piece on
    ; the board. We find the location where the piece was dropped, check if
    ; it is a valid move and update the board accordingly
    (define/augment (after-interactive-move event)
      (define piece (send this find-next-selected-snip #f))
      (define location (xy->location this (send event get-x) (send event get-y)))
      (define valid-moves (send piece valid-moves))
      (when (member location valid-moves)
        ; This is a valid move, remove any target piece and update the piece location
        (let ((target-piece (piece-at-location this location)))
          (when (and target-piece (not (eq? piece target-piece)))
            (send target-piece set-location #f)
            (send this remove target-piece)))
        (set! turn (if (eq? turn 'white) 'black 'white))
        (send piece set-location location)
        (let ((piece-id (send piece get-id)))
        (set! hash-positions2 (hash-set hash-positions2 (car (hash-ref hash-black-pieces piece-id)) #f))
        (set! hash-positions2 (hash-set hash-positions2 location #t))
        (set! prev-black-state hash-black-pieces)
        (set! hash-black-pieces (hash-set hash-black-pieces piece-id (list location 0 (- (third (hash-ref hash-black-pieces piece-id)) 7)) )))
        
        ; check if the player won
        (cond [(check-win? 'black board '("j6" "i7" "h8" "g9" "j7" "i8" "h9") '("j9" "j8" "i9")) (set-message "Player Wins")]
              [(check-win? 'white board '("c0" "b1" "a2" "d0" "c1" "b2" "a3") '("a0" "a1" "b0")) (set-message "AI Wins")]
              [else   (set! initial-flag #f)
        ; Moving the first 5 AI moves
        (when (eq? turn 'white)
          (when (< initial-move-counter 5)
            (let ((positions (second (get-initial-move))))
              (send (piece-at-location board (first positions)) set-location (second positions))
              (position-piece board (piece-at-location board (second positions))))
            (set! initial-flag #t))
          ; Doing Min-Max moves
          (when (not initial-flag)
            (let ((positions (alpha-beta-search depth-level)))
              (let ((piece-id (send (piece-at-location board (second positions)) get-id)))
              (printf "positions: ~a\n" positions)
              (set! prev-white-state hash-white-pieces)  
              (set! hash-white-pieces (hash-set hash-white-pieces piece-id (list (car (third positions)) 0 (- (third (hash-ref hash-white-pieces piece-id)) 7))))
              (set! hash-positions2 (hash-set hash-positions2 (second positions) #f))
              (set! hash-positions2 (hash-set hash-positions2 (car (third positions)) #t))
              (send (piece-at-location board (second positions)) set-location (car (third positions)))
              (position-piece board (piece-at-location board (car (third positions)))))))
          (set! turn 'black)
         )])
        )
      
      ; If the move is not valid it will be moved back.
      (position-piece this piece)
      (set! highlight-location #f)
      ; Note: piece is still selected, but the valid moves are relative to
      ; the new position
      (set! valid-move-locations (send piece valid-moves))
      (send (send this get-canvas) refresh))

    ; This method is invoked each time a snip is selected or un-selected in
    ; the pasteboard and it allows us to perform several operations when this happens
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



; hash for player board points heuristic
(define black-pieces-points
    (hash
    "a0" 1
    "b0" 2 "a1" 2 
    "c0" 3 "b1" 3 "a2" 3
    "d0" 4 "c1" 4 "b2" 4 "a3" 4
    "e0" -10 "d1" 5 "c2" 5 "b3" 5 "a4" -10
    "f0" -10 "e1" 6 "d2" 6 "c3" 6 "b4" 6 "a5" -10 
    "g0" -10 "f1" 1 "e2" 7 "d3" 7 "c4" 7 "b5" 1 "a6" -10
    "h0" -10 "g1" 1 "f2" 8 "e3" 8 "d4" 8 "c5" 8 "b6" 1 "a7" -10
    "i0" -10 "h1" 1 "g2" 1 "f3" 9 "e4" 9 "d5" 9 "c6" 1 "b7" 1 "a8" -10
    "j0" -10 "i1" 1 "h2" 1 "g3" 10 "f4" 10 "e5" 10 "d6" 10 "c7" 1 "b8" 1 "a9" -10
    "j1" -10 "i2" 1 "h3" 1 "g4" 11 "f5" 11 "e6" 11 "d7" 1 "c8" 1 "b9" -10 
    "j2" -10 "i3" 1 "h4" 12 "g5" 12 "f6" 12 "e7" 12 "d8" 1 "c9" -10
    "j3" -10 "i4" 1 "h5" 13 "g6" 13 "f7" 13 "e8" 1 "d9" -10
    "j4" -10 "i5" 14 "h6" 14 "g7" 14 "f8" 14 "e9" -10
    "j5" -10 "i6" 15 "h7" 15 "g8" 15 "f9" -10 
    "j6" 25 "i7" 25 "h8" 25 "g9" 25
    "j7" 30 "i8" 30 "h9" 30
    "j8" 35 "i9" 35
    "j9" 40 
    ))

; hash for AI board points heuristic
(define white-pieces-points
    (hash
   "a0" 40
    "b0" 35 "a1" 35 
    "c0" 30 "b1" 30 "a2" 30
    "d0" 25 "c1" 25 "b2" 25 "a3" 25
    "e0" -10 "d1" 15 "c2" 15 "b3" 15 "a4" -10
    "f0" -10 "e1" 14 "d2" 14 "c3" 14 "b4" 14 "a5" -10
    "g0" -10 "f1" 1 "e2" 13 "d3" 13 "c4" 13 "b5" 1 "a6" -10
    "h0" -10 "g1" 1 "f2" 12 "e3" 12 "d4" 12 "c5" 12 "b6" 1 "a7" -10
    "i0" -10 "h1" 1 "g2" 1 "f3" 11 "e4" 11 "d5" 11 "c6" 1 "b7" 1 "a8" -10
    "j0" -10 "i1" 1 "h2" 1 "g3" 10 "f4" 10 "e5" 10 "d6" 10 "c7" 1 "b8" 1 "a9" -10
    "j1" -10 "i2" 1 "h3" 1 "g4" 9 "f5" 9 "e6" 9 "d7" 1 "c8" 1 "b9" -10
    "j2" -10 "i3" 1 "h4" 8 "g5" 8 "f6" 8 "e7" 8 "d8" 1 "c9" -10
    "j3" -10 "i4" 1 "h5" 7 "g6" 7 "f7" 7 "e8" 1 "d9" -10 
    "j4" -10 "i5" 6 "h6" 6 "g7" 6 "f8" 6 "e9" -10
    "j5" -10 "i6" 5 "h7" 5 "g8" 5 "f9" -10 
    "j6" 4 "i7" 4 "h8" 4 "g9" 4
    "j7" 3 "i8" 3 "h9" 3
    "j8" 2 "i9" 2
    "j9" 1 
    ))

(define initial-flag #t)

; Get the inital moves of the AI
(define (get-initial-move)
  (if (empty? initial-AI-moves) #f
  (let ((current-move (car initial-AI-moves)))
  (set! initial-AI-moves (cdr initial-AI-moves))
  (set! initial-move-counter (+ initial-move-counter 1))
    current-move)))

; Sum player points heuristics
(define (sum-black-points blacks)
  (let ((sum 0)) 
  (for ([(piece-id pos-and-jumps) blacks])
    (define-values (location jump-points first-piece hash-data) (values (first pos-and-jumps) (second pos-and-jumps) (third pos-and-jumps) (hash-ref hash-black-pieces piece-id)))
    (define location-points (hash-ref black-pieces-points location))
     (set! sum (+ sum location-points ; Location points in board heuristic
                      jump-points ; Double jump points heuristic 
                      (third hash-data) 
                      (if (and (> jump-points 14) (not first-piece)) jump-points 0)
                      (if (> location-points 12)
                        (if (eq? location (hash-ref prev-black-state piece-id)) -500 0) 0) ; Previous move heuristic
                      (if (> location-points 11)
                          (if (>= (hash-ref black-pieces-points (car hash-data)) location-points) -500 0) 0)))) 
    sum))

; Sum AI points heuristics
(define (sum-white-points whites)
  (let ((sum 0)) 
  (for ([(piece-id pos-and-jumps) whites])
    (define-values (location jump-points first-piece hash-data) (values (first pos-and-jumps) (second pos-and-jumps) (third pos-and-jumps) (hash-ref hash-white-pieces piece-id)))
    (define location-points (hash-ref white-pieces-points location))
     (set! sum (+ sum location-points ; Location points in board heuristic
                      jump-points ; Double jump points heuristic 
                      (third hash-data)
                      (if (and (> jump-points 14) (not first-piece)) jump-points 0)
                      (if (> location-points 12)
                        (if (eq? location (hash-ref prev-white-state piece-id)) -500 0) 0) ; Previous move heuristic
                      (if (> location-points 11)
                          (if (>= (hash-ref white-pieces-points (car hash-data)) location-points) -500 0) 0)
                      
                      )))
    sum))

; Eval function
; Substracts the AI's points from the player's
(define (eval white-pieces black-pieces)
  (- (sum-white-points white-pieces) (sum-black-points black-pieces)))

; alpha-beta-search
; Calls the max and min functions
(define (alpha-beta-search total-depth)
  (max-value hash-white-pieces hash-black-pieces -10000 10000 0 total-depth hash-positions2 (hash) (hash)))


(define (max-value white-pieces black-pieces a b current-depth total-depth hash-tile-status hash-white-moves hash-black-moves)
  (define first-movement #f)
  (when (eq? current-depth 0) (set! first-movement #t))
  (cond
    [(eq? current-depth total-depth) (list (eval hash-white-moves hash-black-moves) #f #f #f)]
    [else
    (define b-value -10000)
    (define b-original-pos #f)
    (define b-new-pos #f)
    (define flag #f)
    ; Iterate each AI piece 
    (for ([(piece-id current-piece) white-pieces] #:break flag)
            ; Get all possible moves of each AI piece 
            (let ((possible-piece-moves (remove-duplicates (valid-moves-by-hashes-offset hash-tile-status (car current-piece) #t '() '() 'white 0))))
              ; Iterate each possible moves
              (for ([current-move possible-piece-moves] #:break flag)
                ; Get result of min function
                (let ((result (min-value black-pieces (hash-set white-pieces piece-id (list (car current-move) (cdr current-move) (third (hash-ref white-pieces piece-id))))
                               a b current-depth total-depth (hash-set (hash-set hash-tile-status (car current-piece) #f) (car current-move) #t)
                               hash-black-moves (hash-set hash-white-moves piece-id (list (car current-move) (cdr current-move) first-movement)))))
                ; Check if the min value is bigger than b-value
                (when (> result b-value)
                  (set! b-value result)
                  (when (eq? current-depth 0)
                  (set! b-original-pos (car current-piece))
                  (set! b-new-pos current-move))
                  ; Doing the pruning
                  (when (>= b-value b)
                    (set! flag #t))
                      (set! a (max a b-value)))))))
    (if (eq? b-value -10000) (list (eval hash-white-moves hash-black-moves) #f #f #f) (list b-value b-original-pos b-new-pos)
        )]))

(define (min-value black-pieces white-pieces a b current-depth total-depth hash-tile-status hash-black-moves hash-white-moves)
  (define first-movement #f)
  (when (eq? current-depth 0) (set! first-movement #t))
    (define b-value 10000)
  (define flag #f)
    ; Iterate each Player piece 
    (for ([(piece-id current-piece) black-pieces] #:break flag)
            ; Get all possible moves of each Player piece 
            (let ((possible-piece-moves (remove-duplicates (valid-moves-by-hashes-offset hash-tile-status (car current-piece) #t '() '() 'black 0))))
              ; Iterate each possible moves
              (for ([current-move possible-piece-moves] #:break flag)
                ; Get result of max function
                (let ((result (max-value white-pieces (hash-set black-pieces piece-id (list (car current-move) (cdr current-move) (third (hash-ref black-pieces piece-id))))
                               a b (+ current-depth 1) total-depth (hash-set (hash-set hash-tile-status (car current-piece) #f) (car current-move) #t)
                               hash-white-moves (hash-set hash-black-moves piece-id (list (car current-move) (cdr current-move) first-movement)))))
                ; Check if the max value is smaller than b-value
                (when (< (first result) b-value)
                  (set! b-value (first result))
                  ; Doing the pruning
                  (when (<= b-value a)
                    (set! flag #t))
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
; piece-id double-jumps piece-location subtract-each-piece-move
     0 '(0 . "a0" . 150)
     1 '(0 . "a1" . 150)
     2 '(0 . "a2" . 150)
     3 '(0 . "a3" . 150)
     4 '(0 . "b0" . 150)
     5 '(0 . "b1" . 150)
     6 '(0 . "b2" . 150)
     7 '(0 . "c0" . 150)
     8 '(0 . "c1" . 150)
     9 '(0 . "d0" . 150)))

(define hash-white-pieces
    (hash
; piece-id double-jumps piece-location subtract-each-piece-move
     10 '(0 . "f7" . 150)
     11 '(0 . "j8" . 150)
     12 '(0 . "j7" . 150)
     13 '(0 . "j6" . 150)
     14 '(0 . "i9" . 150)
     15 '(0 . "g6" . 150)
     16 '(0 . "i7" . 150)
     17 '(0 . "g7" . 150)
     18 '(0 . "h8" . 150)
     19 '(0 . "g8" . 150)))

(define prev-white-state (hash))
(define prev-black-state (hash))

; Position a piece onto the board according to its location
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

; Convert a location, which is a string, such as "a3" into a rank and file
; position, which are square coordinates on the board.
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

; Convert a rank and file, which are coordinates on the board into a chinese checkers
; location, which is a string, such as "a3"
(define (rank-file->location rank file)
  (unless (<= 0 rank 10)
    (raise-argument-error 'rank "integer between 0 and 9" rank))
  (unless (<= 0 file 10)
    (raise-argument-error 'rank "integer between 0 and 9" file))
  (string
   (list-ref '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j) file)
   (list-ref '(#\9 #\8 #\7 #\6 #\5 #\4 #\3 #\2 #\1 #\0) rank)))


; Determine the location of the coordinate point at X, Y on the board 
(define (xy->location board x y)
  (define-values (canvas-width canvas-height)
    (let ((c (send board get-canvas)))
      (send c get-size)))
  (define-values (square-width square-height)
    (values (/ canvas-width 10) (/ canvas-height 10)))
  (define-values (rank file)
    (values (exact-truncate (/ y square-height)) (exact-truncate (/ x square-width))))
  (rank-file->location rank file))

; Return the chinese checkers piece at location, a string such as "a3", on the board, or
; return #f if that location is empty.
(define (piece-at-location board location)
  (let loop ((snip (send board find-first-snip)))
    (if snip
        (if (equal? location (send snip get-location))
            snip
            (loop (send snip next)))
        #f)))

; Display message onto the device context DC. This is used by the
; pasteboard on-paint method to display messages
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

; Draw the chinese checkers board. In addition,
; this function also draws the letters and numbers corresponding to
; the rank and file locations on the board. This is used by the pasteboard
; on-paint method to display the board 
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

  ; Board creation
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

; Draw a square at location such as "a3" using
; color-name for the background and border-color-name for the border 
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


; The pasteboard% that will hold and manage the chinese-checkers pieces
(define board (new chinese-checkers-board%))
; Toplevel window for our application
(define toplevel (new frame% [label "Chinese Checkers Board"] [width 1024] [height 720]))
; The canvas which will display the pasteboard contents
(define canvas (new editor-canvas%
                    [parent toplevel]
                    [style '(no-hscroll no-vscroll)]
                    [horizontal-inset 0]
                    [vertical-inset 0]
                    [editor board]))

; This actually displays the board window
(send toplevel show #t)

(define letters '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j"))
(define numbers '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))

; Initial pieces positions
(define initial
  (string-append
   "Ba0Ba1Ba2Ba3Bb0Bb1Bb2Bc0Bc1Bd0"
   "Wj9Wj8Wj7Wj6Wi9Wi8Wi7Wh9Wh8Wg9"))

; Get the color of a piece by its position
; Returns false if there is no piece in the position
(define (color-at-location location board)
    (let ((piece (piece-at-location board location)))
             (if piece
                 (send piece color)
                 #f)))

; Returns a list with the color of the pieces
(define (colors-at-locations locations packed board)
 (if (empty? locations)
     packed
     (colors-at-locations (cdr locations) (cons (color-at-location (car locations) board) packed) board)))

; Check if any player won
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