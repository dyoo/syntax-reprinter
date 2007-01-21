(module syntax-reprinter mzscheme
  (require (lib "list.ss"))
  (provide syntax-reprint)
  
  
  ;; syntax-reprint: stx output-port -> void
  (define (syntax-reprint stx outp)
    
    ;; main-case-analysis: syntax number -> syntax
    ;; Does the main case analysis on the datum.
    ;; Returns the last syntax object printed.
    (define (main-case-analysis stx last-line)
      (printf "main-case-analysis: ~a~n" stx)
      (when (not (= last-line (syntax-line stx)))
        (newline outp))
      
      (syntax-case stx ()
        
        [(e-first . e-rest)
         (begin
           (display (open stx) outp)
           (let ([last-printed-stx
                  (reprint-sequence-internals stx (syntax e-first) (syntax e-rest))])
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
    
    ;; reprint-sequence-internals: syntax syntax syntax -> syntax
    ;; Handles the printing of the internal elements.
    (define (reprint-sequence-internals container-stx e-first e-rest)
      (let loop ([e-rest e-rest]
                 [previous-stx
                  (main-case-analysis e-first (syntax-line container-stx))])
        (cond
          [(empty? (syntax-e e-rest))
           previous-stx]

          ;; NOTE: if e-rest is a syntax-pair, then the case analysis is slightly
          ;; subtle.
          [(pair? (syntax-e e-rest))
           (display " " outp)
           (let ([last-stx-printed
                  (main-case-analysis (first (syntax-e e-rest))
                                      (syntax-line previous-stx))])
             (cond
               [(empty? (rest (syntax-e e-rest)))
                last-stx-printed]
               [else
                (loop (rest (syntax-e e-rest))
                      last-stx-printed)]))]
          
          [else
           (display " . " outp)
           (main-case-analysis e-rest (syntax-line previous-stx))])))
    
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