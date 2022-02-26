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
    (init-field glyph font size [location #f])
    (super-new)
    (send this set-snipclass chinese-checkers-piece-snip-class)

    (define/public (set-location l) (set! location l))
    (define/public (get-location) location)

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

(define chess-piece-data
  (hash
   "B" #\u26AB "W" #\u26AA))

(define (make-chess-piece id [location #f])
  (define glyph (hash-ref chess-piece-data id))
  (define font (send the-font-list find-or-create-font 20 'default 'normal 'normal))
  (new chinese-checkers-piece% [glyph (string glyph)] [font font] [size 35] [location location]))

(define chess-board%
  (class pasteboard%
    (super-new)

    (define/override (on-paint before? dc . other)
      (when before?
        (draw-chess-board dc)))

    (define/augment (after-insert chess-piece . rest)
      (position-piece this chess-piece))
    
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
    (raise-argument-error 'location "valid chess position a1 .. h8" location))
  (define file
    (index-of '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j) (string-ref location 0)))
  (define rank
    (index-of '(#\9 #\8 #\7 #\6 #\5 #\4 #\3 #\2 #\1 #\0) (string-ref location 1)))
  (unless (and rank file)
    (raise-argument-error 'location "valid chess position a1 .. h8" location))
  (values rank file))

(define (draw-chess-board dc)
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

#|
  (for ([(rank index) (in-indexed '("9" "8" "7" "6" "5" "4" "3" "2" "1" "0"))])
    (define-values [_0 h _1 _2] (send dc get-text-extent rank font #t))
    (define y (+ (* index cell-height) (- (/ cell-height 2) (/ h 2))))
    (send dc draw-text rank margin y))
  
  (for ([(file index) (in-indexed '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j"))])
    (define-values [w h _1 _2] (send dc get-text-extent file font #t))
    (define x (+ (* index cell-width) (- (/ cell-width 2) (/ w 2))))
    (send dc draw-text file x (- dc-height h margin))))
|#

;; A test program for our chinese-checkers-piece% objects:

;; The pasteboard% that will hold and manage the chess pieces
(define board (new chess-board%))
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
    (send board insert (make-chess-piece name location))))

(setup-board board initial)