(define-structure srfi-13-test (export srfi-13-tests)
  (open scheme test-suites
	srfi-13)
  (files srfi-13-check))

(define-structure srfi-14-test (export srfi-14-tests srfi-14-slow-tests)
  (open scheme test-suites
	unicode
	srfi-14)
  (files srfi-14-check))

(define-structure srfi-19-test (export srfi-19-tests)
  (open scheme 
	srfi-9 ; DEFINE-RECORD-PROCEDURES
        srfi-19
	test-suites
        formats)
  (files srfi-19-check))

(define-structure portable-srfi-test (export portable-srfi-tests)
  (open scheme test-suites
	srfi-13-test srfi-14-test)
  (begin
    (define-test-suite portable-srfi-tests (srfi-13-tests srfi-14-tests))))

(define-structure srfi-test (export portable-srfi-tests posix-srfi-tests srfi-tests)
  (open scheme test-suites
	portable-srfi-test
	srfi-19-test)
  (begin
    (define-test-suite posix-srfi-tests (srfi-19-tests))
    (define-test-suite srfi-tests (portable-srfi-tests posix-srfi-tests))))
