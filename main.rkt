;; TODO capture interplay between theta and phi - not quite there yet
;; - can change them separately but not together
;; - also need to slow down rotation
;; - problem with bounds, how to go past pi in theta? really need to
;;   start subtracting from pi and change phi instead by pi radians
;;   - might be causing the jumpiness
;; TODO reset geometry when slider changes, otherwise can trap on wrong side
;; TODO offsets for labels aren't going to work when rotated
;; TODO could probably DRY out draw-x,y,z but not sure how exactly. macro?
;; could use parameter to return pen to default
#lang racket/gui

(require racket/gui/base)
(provide main)

(define theta0 0)
(define (theta x y z)
  (if (= x y z 0)
      0
      (acos (/ z (radius x y z)))))

(define phi0 0)
(define (phi x y)
  (if (= x y 0)
      0
      (atan y x)))

(define *bg-color* (make-color 255 255 255))
(define *xangle* (cos (/ (* 5 pi) 4.)))
(define *yangle* (sin (/ (* 5 pi) 4.)))
(define *axis-scale* 0.75)
(define *atom-scale* 0.25)
(define *refresh-rate* 0.2)

(struct vib (freq contribs)
  #:transparent
  #:mutable)

(define infile
  (command-line
   #:args
   ((str "spectro.out")) str))

(define (process-freqs freqs)
  (map string->number freqs))

(define (empty-string? str)
  (not (non-empty-string? str)))

(define (dot? str)
  (string-contains? str "."))

(define ptable
  (hash
   "1.0078250" "H"
   "12.0000000" "C"
   "14.0030740" "N"
   "15.9949146" "O"
   ))

(define pcolor
  (hash
   "H" (make-color 221 228 240)
   "C" (make-color 39 45 54)
   "N" (make-color 10 100 245)
   "O" (make-color 255 0 0)))

(define (atom-color atom)
  (hash-ref pcolor atom))

(define (read-output in)
  (let ((in-lxm? #f)
        (head? #f)
        (geom? #f)
        (freqs null)
        (geom null)
        (skip 0)
        (ht (make-hash)))
    (for ((l (in-lines in)))
      (cond
        ((string-contains? l "LXM MATRIX") (set! in-lxm? #t) (set! head? #t))
        ((string-contains? l "LX MATRIX") (set! in-lxm? #f))
        ((string-contains? l "MOLECULAR CARTESIAN GEOMETRY") (set! geom? #t) (set! skip 2))
        ((> skip 0) (set! skip (- skip 1)))
        ((and geom? (empty-string? l)) (set! geom? #f))
        (geom? (set! geom (cons (cdr (string-split l)) geom)))
        ((and in-lxm? (empty-string? l)) (set! head? #t))
        ((and in-lxm? (not (string-contains? l "---")) (dot? l))
         (cond
           (head? (set! head? #f)
                  (set! freqs (append freqs (process-freqs (string-split l)))))
           (else
            (let* ((slc (string-split l))
                   (fst (car slc)))
              (hash-set! ht fst (append (hash-ref ht fst null) (cdr slc)))))))))
    (values freqs ht (reverse geom))))

(define-values (freqs contribs geom) (call-with-input-file infile read-output))

(set! contribs
      (let ((keys (map number->string (sort (map string->number (hash-keys contribs)) <))))
        (for/list ((k keys))
          (map string->number (hash-ref contribs k)))))

(define (make-vibs freqs contribs)
  (cond
    ((null? freqs) null)
    (else (cons (vib (car freqs) (map car contribs))
                (make-vibs (cdr freqs) (map cdr contribs))))))

(define vibs (make-vibs freqs contribs))

(define (print-vibs)
  (for ((line vibs))
    (displayln line)))

(define (vib-choices)
  (for/list ((v vibs))
    (vib-freq v)))

(define atoms
  (map (lambda (a)
         (hash-ref ptable a))
       (flatten (map (lambda (l)
                       (take-right l 1)) geom))))

(define coords
  (map (lambda (l) (map string->number (take l 3))) geom))

;; dont actually want start -> end, just want start and if drag?
;; then send the start and current dragging position to the angle calculator
;; still record the start like this (actually should probably be a method?) - no a field
;; and then have separate if drag clause
;; ie, if start (record x y), if drag (set angle)

(define (center canvas)
  (let ((width (send canvas get-width))
        (height (send canvas get-height)))
    (values (/ width 2.) (/ height 2.))))

(define (extent canvas)
  (values
   (send canvas get-width)
   (send canvas get-height)))

(define dash-pen (new pen% (style 'long-dash)))
(define def-pen (new pen%))
(define def-brush (new brush%))

(define (x-help-lines dc w h)
  (let ((wend (- w (* *axis-scale* w)))
        (hend h))
    (send dc set-pen dash-pen)
    (send dc draw-line
          w h
          wend hend))
  (let ((wend w)
        (hend (+ h (* *axis-scale* h))))
    (send dc draw-line
          w h
          wend hend))
  (send dc set-pen def-pen))

(define (draw-x canvas dc)
  ;; (x-help-lines dc w h)
  (let-values (((wbeg hbeg) (cart->2d canvas 0 0 0 *axis-scale*))
               ((wend hend) (cart->2d canvas 1 0 0 *axis-scale*)))
    (send dc draw-line
          wbeg hbeg
          wend hend)
    (define-values (woff hoff d a) (send dc get-text-extent "x"))
    (send dc draw-text "x" (- wend woff) hend)))

(define (draw-y canvas dc)
  (let-values (((wbeg hbeg) (cart->2d canvas 0 0 0 *axis-scale*))
               ((wend hend) (cart->2d canvas 0 1 0 *axis-scale*)))
    (send dc draw-line
          wbeg hbeg
          wend hend)
    (define-values (woff hoff d a) (send dc get-text-extent "y"))
    (send dc draw-text "y" (+ wend woff) (- hend (/ hoff 2)))))

(define (draw-z canvas dc)
  (let-values (((wbeg hbeg) (cart->2d canvas 0 0 0 *axis-scale*))
               ((wend hend) (cart->2d canvas 0 0 1 *axis-scale*)))
    (send dc draw-line
          wbeg hbeg
          wend hend)
    (define-values (woff hoff d a) (send dc get-text-extent "z"))
    (send dc draw-text "z" (- wend (/ woff 2)) (- hend hoff))))

(define radius
  (lambda vec
    (sqrt (apply + (map (lambda (l)
                          (* l l)) vec)))))

(define (cart->sphere x y z)
  (let ((r (radius x y z))
        (p (phi x y))
        (t (theta x y z)))
    (set! p (+ p (* (sin t) phi0)))
    (set! t (+ t (* (cos p) theta0)))
    (values
     (* r (sin t) (cos p))
     (* r (sin t) (sin p))
     (* r (cos t)))))

(define (cart->2d canvas x y z scale)
  (let-values (((cw ch) (center canvas))
               ((mw mh) (extent canvas))
               ((x y z) (cart->sphere x y z)))
    (values
     (+ cw (* (+ y (* x *xangle*)) (- mw cw) scale))
     (- ch (* (+ z (* x *yangle*)) (- mh ch) scale)))))

(define (draw-atom canvas dc atom x y z)
  (let-values (((w h) (cart->2d canvas x y z *atom-scale*)))
    (send dc set-brush (atom-color atom) 'solid)
    (send dc draw-ellipse w h 20 20))
  (send dc set-brush def-brush))

(define (draw-axes canvas dc)
  (draw-x canvas dc)
  (draw-y canvas dc)
  (draw-z canvas dc))

(define (draw-geom canvas dc atoms coords)
  (for ((atom atoms) (coord coords))
    (apply draw-atom canvas dc atom coord)))


(define slide-min 0)
(define slide-max 100)
(define slide-init 50)

(define (magnitude)
  (/ (send slider get-value) slide-max))


(define (draw-canvas canvas dc)
  (draw-axes canvas dc)
  (draw-geom canvas dc atoms coords))

(define my-frame%
  (class frame%
    (define/override (on-subwindow-char rec event)
      (cond
        ((equal? (send event get-key-code) #\q)
         (exit))
        ((equal? (send event get-key-code) 'escape)
         (send list-box clear-select))
        ((equal? (send event get-key-code) #\j)
         (send list-box incr-select))
        ((equal? (send event get-key-code) #\k)
         (send list-box decr-select))
        ((equal? (send event get-key-code) #\l)
         (send slider set-value
               (let ((val (+ (send slider get-value) 5))
                     (maxp (send slider get-max)))
                 (if (> val maxp)
                     maxp
                     val))))
        ((equal? (send event get-key-code) #\h)
         (send slider set-value
               (let ((val (- (send slider get-value) 5))
                     (minp (send slider get-min)))
                 (if (< val minp)
                     minp
                     val))))))
    (field (startx 0)
           (starty 0))
    (define/override (on-subwindow-event rec event)
      (if (equal? rec canvas)
          (cond 
            ((send event button-down? 'left)
             (set!-values (startx starty)
                          (values (send event get-x) (send event get-y))))
            ((send event dragging?)
             (let ((x (send event get-x))
                   (y (send event get-y)))
               ;; (set! phi0 (+ phi0 (/ (- x startx) (send rec get-height))))
               ;; (set! theta0 (+ theta0 (/ (- y starty) (send rec get-width))))
               (send msg set-label
                     (format "(~a, ~a) -> (~a, ~a)" startx starty x y)))))
          (super on-subwindow-event rec event)))
    (super-new)))

(define frame (new my-frame%
                   (label "spectroscope")
                   (width 500)
                   (height 500)))

(define msg (new message%
                 (parent frame)
                 (label "Test")))

(define panel (new horizontal-panel%
                   (style '(border))
                   (parent frame)
                   (alignment '(center center))))


(define left-panel (new vertical-panel%
                        (parent panel)))


(define canvas (new canvas%
                    (parent left-panel)
                    (min-width 500)
                    (min-height 500)
                    (paint-callback
                     (lambda (canvas dc)
                       (draw-canvas canvas dc)))))

(send canvas focus)
(define dc (send canvas get-dc))
(send canvas set-canvas-background *bg-color*)

(define my-slider%
  (class slider%
    (init-field (min-value 0)
                (max-value 0))
    (define/public (magnitude)
      (/ (send this get-value) max-value))
    (define/public (get-max)
      max-value)
    (define/public (get-min)
      min-value)
    (super-new (min-value min-value)
               (max-value max-value))))

(define my-list-box%
  (class list-box%
    (define/public (clear-select)
      (let ((test (send this get-selections2)))
        (unless (null? test) (send this select test #f))))
    (define/public (get-selections2)
      (let* ((sel (send this get-selections)))
        (if (not (null? sel)) (car sel) null)))
    (define/public (sel-max)
      (- (send this number-of-visible-items) 1))
    (define/public (incr-select)
      (let ((sel (send this get-selections2)))
        (cond
          ((null? sel) (send this select 0))
          ((< sel (send this sel-max))
           (send this select (+ 1 sel))))))
    (define/public (decr-select)
      (let ((sel (send this get-selections2)))
        (cond
          ((null? sel) (send this select (send this sel-max)))
          ((> sel 0) (send this select (- sel 1))))))
    (super-new)))

(define right-panel (new vertical-panel%
                         (parent panel)))

(define list-box (new my-list-box%
                      (parent right-panel)
                      (min-width 100)
                      (label #f)
                      (choices (map number->string (vib-choices)))
                      (columns '("Frequencies"))
                      (style '(single column-headers))))

(define slider (new my-slider%
                    (label "Magnitude")
                    (parent left-panel)
                    (min-value 0)
                    (max-value 100)
                    (init-value slide-init)))

(define (resplit lst)
  (cond
    ((null? lst) null)
    (else (cons (take lst 3) (resplit (drop lst 3))))))

(define vibrate
  (let ((n 0)
        (steps (list + - - +)))
    (lambda (contribs (reset? #f))
      (when reset?
        (set! n 0)
        (set! coords ref-coords))
      (let ((op (list-ref steps (remainder n 4)))
            (mag (magnitude)))
        (set! coords (resplit (map op
                                   (flatten coords)
                                   (map (lambda (c)
                                          (* mag c)) contribs)))))
      (set! n (add1 n)))))

(define ref-coords coords)

(define (prev)
  (let ((hold null))
    (lambda (next)
      (begin0
          (equal? hold next)
        (set! hold next)))))

(define prev-sel (prev))

(define prev-slide (prev))

(define (loop)
  (let* ((sel (send list-box get-selections2))
         (sli (send slider get-value))
         (diff? (and (prev-sel sel) (prev-slide sli))))
    (when diff? ;; if new selection or slider value, reset coords
      (set! coords ref-coords))
    (unless (null? sel) ;; only vibrate when there is a selection
      (if diff? 
          (vibrate (vib-contribs (list-ref vibs sel)) #f)
          (vibrate (vib-contribs (list-ref vibs sel)) #t))))
  (send canvas refresh)
  (send canvas on-paint)
  (sleep/yield *refresh-rate*)
  (loop))

(define (main)
  (send frame show #t)
  (loop))

;(main)
