(module syntax-reprinter mzscheme
  (require (lib "list.ss"))
  (provide syntax-reprint)
  
  
  ;; We need to maintain our current position.
  (define-struct pos (line column))
  
  (define (pos-newline a-pos)
    (make-pos (add1 (pos-line a-pos)) 0))
  
  (define (pos-forward-column a-pos)
    (make-pos (pos-line a-pos) (add1 (pos-column a-pos))))
  
  (define (pos-stx-printed a-pos stx)
    ;; TODO: handle strings with internal newlines
    (make-pos (pos-line a-pos)
              (+ (syntax-span stx) (pos-column a-pos))))
  
  
  ;; syntax-reprint: stx output-port -> void
  (define (syntax-reprint stx outp)
    
    ;; entry-point: -> pos
    ;; Starts off the syntax-reprint.
    (define (entry-point)
      (reprint stx (make-pos (syntax-line stx) (syntax-column stx))))
    

    ;; reprint: syntax pos -> pos
    ;; prints out datum, returns last position.
    (define (reprint stx last-pos)
      (cond
        [(< (pos-line last-pos) (syntax-line stx))
         (newline outp)
         (reprint stx (pos-newline last-pos))]
        [(< (pos-column last-pos) (syntax-column stx))
         (display " " outp)
         (reprint stx (pos-forward-column last-pos))]
        [else
         (main-case-analysis stx last-pos)]))
    
    
    ;; main-case-analysis: syntax number number -> number
    ;; Does the main case analysis on the datum.
    ;; Returns the last syntax object printed.
    (define (main-case-analysis stx last-pos)
      (syntax-case stx ()
        [(_0 . _1)
         (handle-pair/empty stx last-pos)]
        [()
         (handle-pair/empty stx last-pos)]
        [#(_ ...)
         (handle-vector stx last-pos)]
        [else
         (handle-datum stx last-pos)]))
    
    
    ;; handle-pair/empty: syntax pos -> pos
    (define (handle-pair/empty stx last-pos)
      (display (open stx) outp)
      (let ([new-last-pos
             (reprint-sequence-internals (syntax-e stx) (pos-forward-column last-pos))])
        (display (close stx) outp)
        ;; unfortunately, syntax objects do not capture enough
        ;; for us to know if there's some newline between the
        ;; open and close parens.  We just assume that we've just gone forward.
        (pos-forward-column new-last-pos)))
    
    
    (define (handle-vector stx last-pos)
      (display "#(" outp)
      (let* ([vec (syntax-e stx)]
            [len (vector-length vec)])
        (let loop ([i 0]
                   [last-pos (pos-forward-column
                              (pos-forward-column last-pos))])
          (cond [(< i len)
                 (loop (add1 i)
                       (reprint (vector-ref vec i) last-pos))]
                [else
                 (display ")" outp)
                 (pos-forward-column last-pos)]))))
    
    
    ;; handle-datum: syntax pos -> pos
    (define (handle-datum stx last-pos)
      (print (syntax-object->datum stx) outp)
      (pos-stx-printed last-pos stx))
    
    
    ;; reprint-sequence-internals: syntax (union syntax-pair empty syntax-object) -> syntax
    ;; Handles the printing of the internal elements.
    ;; Returns the syntax of the last printed element.
    (define (reprint-sequence-internals stx-pair last-pos)
      (let loop ([stx-pair stx-pair]
                 [last-pos last-pos])
        (cond
          [(empty? stx-pair) last-pos]
          
          [(pair? stx-pair)
           (let ([new-last-pos
                  (reprint (first stx-pair) last-pos)])
             (loop (rest stx-pair) new-last-pos))]
          
          [else
           (display " . " outp)
           (reprint stx-pair
                    (pos-forward-column
                     (pos-forward-column
                      (pos-forward-column last-pos))))])))
    
    
    
    ;; open: syntax -> character
    ;; Depending on the shape of the stx, returns the appropriate opening paren.
    (define (open stx)
      (case (syntax-property stx 'paren-shape)
        [(#\[) "["]
        [(#\{) "{"]
        [else "("]))
    
    
    ;; close: syntax -> character
    ;; Depending on the shape of the stx, returns the appropriate closing paren.
    (define (close stx)
      (case (syntax-property stx 'paren-shape)
        [(#\[) "]"]
        [(#\{) "}"]
        [else ")"]))
    
    (entry-point)))