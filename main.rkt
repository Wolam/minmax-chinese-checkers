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
      (if (equal? (string-upcase name) name) 'white 'black))
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


(define (valid-jump-by-offset foffset roffset rank file moves)
   (define-values (nrank nfile) (values (+ rank roffset) (+ file foffset)))
   (if (and (valid-rank? nrank) (valid-file? nfile))
        (let ((jump-candidate (rank-file->location nrank nfile)))
          (let ((piece (piece-at-location board jump-candidate)))
             (if (not piece)
                (cons jump-candidate moves)
                moves)
  ))moves))

(define (valid-moves-by-offset color board location offsets)
  (define-values (rank file) (location->rank-file location))
  (for/fold ([moves '()])
            ([offset (in-list offsets)])

    (match-define (list roffset foffset) offset)
    (define-values (nrank nfile) (values (+ rank roffset) (+ file foffset)))
    (if (and (valid-rank? nrank) (valid-file? nfile))
        (let ((candidate (rank-file->location nrank nfile)))
          (let ((piece (piece-at-location board candidate)))
            (if (not piece)
                (cons candidate moves)
                (valid-jump-by-offset foffset roffset nrank nfile moves))))
    moves)))


(define ((king-moves color) board location)
  (valid-moves-by-offset
   color board location
   '((-1 -1) (-1 0) (-1 1) (0 -1) (0 1) (1 -1) (1 0) (1 1))))

(define chinese-checkers-piece-data
  (hash
   "W" (cons #\u26AA (king-moves 'black))
   "B" (cons #\u26AB (king-moves 'white))))

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
    (define opponent-move-locations '())

    (define/override (on-paint before? dc . other)
      (when before?
        (draw-chinese-checkers-board dc)
        (for ((location (in-list valid-move-locations)))
          (highlight-square dc location #f "seagreen"))
        (for ((location (in-list opponent-move-locations)))
          (highlight-square dc location "firebrick" #f))
        (when highlight-location
          (highlight-square dc highlight-location #f "indianred"))))
    
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
        (send piece set-location location))
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
            (set! valid-move-locations (send snip valid-moves))
            (set! opponent-move-locations
                  (collect-opponent-moves this (send snip color))))
          (begin
            (set! opponent-move-locations '())
            (set! valid-move-locations '())))
      (send (send this get-canvas) refresh))

    ))

(define (collect-opponent-moves board color)
  (define moves '())
  (let loop ((snip (send board find-first-snip)))
    (when snip
      (unless (eq? (send snip color) color)
        (set! moves (append moves (send snip valid-moves))))
      (loop (send snip next))))
  (remove-duplicates moves))

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
    (raise-argument-error 'location "valid chinese-checkers position a1 .. h8" location))
  (define file
    (index-of '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j) (string-ref location 0)))
  (define rank
    (index-of '(#\9 #\8 #\7 #\6 #\5 #\4 #\3 #\2 #\1 #\0) (string-ref location 1)))
  (unless (and rank file)
    (raise-argument-error 'location "valid chinese-checkers position a1 .. h8" location))
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
    (send dc draw-rectangle x y cell-width cell-height)))


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

(define (setup-board board position)
  (send board clear)
  (define piece-count (/ (string-length position) 3))
  (for ([index (in-range piece-count)])
    (define pos (* index 3))
    (define name (substring position pos (add1 pos)))
    (define location (substring position (add1 pos) (+ (add1 pos) 2)))
    (send board insert (make-chinese-checkers-piece name location))))

(setup-board board initial)