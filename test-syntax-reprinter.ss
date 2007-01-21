(module test-syntax-reprinter mzscheme
  (require "syntax-reprinter.ss"
           (planet "test.ss" ("schematics" "schemeunit.plt" 2 4))
           (planet "text-ui.ss" ("schematics" "schemeunit.plt" 2 4)))
  
  (define (reprint stx)
    (define outp (open-output-string))
    (syntax-reprint stx outp)
    (get-output-string outp))
  
  (define reprinter-tests
    (test-suite
     "Reprinter tests"
     (test-equal? "simple test"
                  (reprint (syntax (hello)))
                  "(hello)")
     
     (test-equal? "boxy empty"
                  (reprint (syntax []))
                  "[]")))
  
  (test/text-ui reprinter-tests))