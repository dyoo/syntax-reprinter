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
                  (reprint (syntax (hello world)))
                  "(hello world)")
     
     (test-equal? "dotted pair"
                  (reprint (syntax (hello . nurse)))
                  "(hello . nurse)")
     
     (test-equal? "boxy empty"
                  (reprint (syntax []))
                  "[]")
     
     (test-equal? "list containing lists"
                  (reprint (syntax (hello (world) testing)))
                  "(hello (world) testing)")
     
     ;; TODO: fix the test to get the columns right.
     (test-equal? "syntax spanning lines"
                  (reprint (syntax (hiya
                                    world)))
                  "(hiya \nworld)")))
  
  (test/text-ui reprinter-tests))