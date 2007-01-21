(module syntax-reprinter mzscheme
  
  (provide syntax-reprint)
  
  ;; pretty-scheme-stx->string: stx output-port -> void
  (define (syntax-reprint stx outp)
    (let loop ([stx stx]
               [last-line (syntax-line stx)])
      (when (not (= last-line (syntax-line stx)))
        (newline outp))
      (syntax-case stx ()
        [(e1 e2 ...)
         (begin
           (display (open stx) outp)
           (loop (syntax e1) (syntax-line stx))
           (for-each (lambda (sub-stx)
                       (display " " outp)
                       (loop sub-stx (syntax-line stx)))
                     (syntax-e (syntax (e2 ...))))
           (display (close stx) outp))]
        [else
         (print (syntax-object->datum stx) outp)])))
  
  
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