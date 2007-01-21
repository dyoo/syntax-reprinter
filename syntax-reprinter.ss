(module syntax-reprinter mzscheme
  (require (lib "list.ss"))
  (provide syntax-reprint)
  
  
  ;; syntax-reprint: stx output-port -> void
  (define (syntax-reprint stx outp)
    
    ;; main-case-analysis: syntax number -> syntax
    ;; Does the main case analysis on the datum.
    ;; Returns the last syntax object printed.
    (define (main-case-analysis stx last-line)
      
      (when (not (= last-line (syntax-line stx)))
        (newline outp))
      
      (syntax-case stx ()
        
        [(_0 . _1)
         (begin
           (display (open stx) outp)
           (let ([last-printed-stx
                  (reprint-sequence-internals stx (syntax-e stx))])
             (display (close stx) outp)
             last-printed-stx))]
        
        [()
         (begin
           (display (open stx) outp)
           ;; unfortunately, syntax objects do not capture enough
           ;; for us to know if there's some newline between the
           ;; open and close parens.
           (display (close stx) outp)
           stx)]
        
        ;; TODO: handle vectors
        
        [else
         ;; symbols or other datums
         ;; TODO: handle strings with internal newlines
         (print (syntax-object->datum stx) outp)
         stx]))
    
    ;; reprint-sequence-internals: syntax (union syntax-pair empty syntax-object) -> syntax
    ;; Handles the printing of the internal elements.
    (define (reprint-sequence-internals container-stx stx-pair)
      (let loop ([stx-pair stx-pair]
                 [previous-stx container-stx]
                 [space-in-front #f])
        (cond
          [(empty? stx-pair) previous-stx]
          
          [(pair? stx-pair)
           (when space-in-front
             (display " " outp))
           (let ([last-stx-printed
                  (main-case-analysis (first stx-pair)
                                      (syntax-line previous-stx))])
             (loop (rest stx-pair) last-stx-printed #t))]
          
          [else
           (display " . " outp)
           (main-case-analysis stx-pair (syntax-line previous-stx))])))
    
    (main-case-analysis stx (syntax-line stx)))
  
  
  (define (open stx)
    (case (syntax-property stx 'paren-shape)
      [(#\[) "["]
      [(#\{) "{"]
      [else "("]))
  
  (define (close stx)
    (case (syntax-property stx 'paren-shape)
      [(#\[) "]"]
      [(#\{) "}"]
      [else ")"])))