#lang racket

(require racket/gui)
(require rsound)

(define notes '(A A# B C C# D D# E F F# G G#))

; mapping layout
(define keys
    '((1 q a z w s x)
      (3 e d c r f v)
      (5 t g b y h n)
      (7 u j m i k )))

(define a4 440)

; scale: {0,1,2,3,4,5,6} -> {0,1,2,3,4,5,6,7,8,9,10,11} has to be injective
; root: a note
(define (note->frequency root scale note)
    (let* ([octave (car note)]
           [scale-index (cadr note)]
           [steps (scale scale-index)] ; steps in the scale
           [skip (index-in-list eq? root notes)]) ; skip to map everything to the root
    (* a4 (expt 2 (/ (+ skip steps (* 12 octave)) 12)))))

; scale: {0,1,2,3,4,5,6} -> {0,1,2,3,4,5,6,7,8,9,10,11} has to be injective
; root: a note
(define (note->symbol root scale note)
    (let* ([octave (car note)]
           [scale-index (cadr note)]
           [steps (scale scale-index)] ; steps in the scale
           [skip (index-in-list eq? root notes)]) ; skip to map everything to the root
    (list-ref notes (modulo (+ skip steps) 12))))

; eq? : e -> e -> bool, e what we're searching for
; returns index
(define (index-in-list eq? e lst [acc 0])
    (cond
        [(empty? lst)       empty]
        [(eq? e (car lst))  acc]
        [else               (index-in-list eq? e (cdr lst) (+ acc 1))]))

; equality of keyboard keys
(define (key-eq? a b)
    (cond [(and (number? a) (number? b)) (= a b)]
          [(and (symbol? a) (symbol? b)) (eq? a b)]
          [(and (symbol? a) (number? b))
           (eq? (char->integer (string-ref (symbol->string a) 0))
                (+ b (char->integer #\0)))]
          [else #f]))

; a note is '(octave-index note-in-scale)
; empty if note is not found
(define (key->note k keys [octave-index 0])
    (if (empty? keys) empty
     (let* ([octave (car keys)]
            [scale-index (index-in-list key-eq? k octave)])
           (if (empty? scale-index)
               (key->note k (cdr keys) (+ octave-index 1))
               (list octave-index scale-index)))))

; mode: {1, 2, 3, 4, 5, 6}
; returns: {0..6} -> {0..11}
(define (diatonic mode)
    (let* ([a (modulo (- 9 mode) 7)]
           [b (modulo (- 6 mode) 7)]
           [first-semi (+ (min a b) 1)]
           [second-semi (+ (max a b) 1)]
           [semi-diff (- second-semi first-semi)])
    (lambda (note)
        (cond [(< note first-semi)  (* 2 note)]
              [(< note second-semi) (- (* 2 note) 1)]
              [else                 (- (* 2 note) 2)]))))

(define (display-func f lst)
    (if (empty? lst) '()
        (begin (display (car lst)) (display ": ") (display (f (car lst)))
               (newline)
               (display-func f (cdr lst)))))

(define frame (new frame% [label "Example"]
                          [width 300]
                          [height 300]))

(define sound-mode 0) ; 0=major, 5=minor
(define sound-root 'C); C scale

(define kbd-canvas%
    (class canvas%
        (define/override (on-char event)
          (call/cc (lambda (return)
            (let* ([raw-key (send event get-key-code)]
                   [_ (when (not (char? raw-key)) (return '()))]
                   [key (string->symbol (string raw-key))]
                   [note (key->note key keys)]
                   [_ (when (empty? note) (return '()))]
                   [symbol (note->symbol sound-root (diatonic sound-mode) note)]
                   [frequency (note->frequency sound-root (diatonic sound-mode) note)]
                   )
                (when (eq? key 'release) (return '()))
                (printf "~a\t ~aHz~n" symbol frequency)
                (play-sound-hz frequency)))))
        (super-new)))

(define canvas (new kbd-canvas% [parent frame]))

(define stream (make-pstream))

(define (play-sound-hz freq)
    (pstream-play stream (signal->rsound 7500
        (network ()
            [sin <= square-wave freq]
            [out = (* 0.1 sin)]))))

(let* ([args (current-command-line-arguments)]
       [root (string->symbol (vector-ref args 0))]
       [mode (string->number (vector-ref args 1))])
    (set! sound-root root)
    (set! sound-mode mode)
    (send frame show #t))
