; Copyright (c) 1993, 1994 Richard Kelsey and Jonathan Rees.  See file COPYING.


; Handy things for debugging the run-time system, byte code compiler,
; and linker.


; Alternative command processor.  Handy for debugging the bigger one.

(define (make-mini-command scheme)
  (define-structure mini-command (export command-processor)
    (open scheme
	  signals conditions handle
	  display-conditions)
    (files (debug mini-command)))
  mini-command)

; Miniature EVAL, for debugging runtime system sans package system.

(define-structures ((mini-eval evaluation-interface)
		    (mini-environments
		     (export interaction-environment
			     scheme-report-environment
			     set-interaction-environment!
			     set-scheme-report-environment!)))
  (open scheme-level-2
	signals)		;error
  (files (debug mini-eval)))

(define (make-scheme environments evaluation) ;cf. initial-packages.scm
  (define-structure scheme scheme-interface
    (open scheme-level-2
	  environments
	  evaluation))
  scheme)

; Stand-alone system that doesn't contain a byte-code compiler.
; This is useful for various testing purposes.

(define mini-scheme (make-scheme mini-environments mini-eval))

(define mini-command (make-mini-command mini-scheme))

(define-structure little-system (export start)
  (open scheme-level-1
	mini-command
	scheme-level-2-internal)
  (begin (define start
	   (usual-resumer
	    (lambda (args) (command-processor #f args))))))

(define (link-little-system)
  (link-simple-system '(debug little)
		      'start
		      little-system))



; --------------------
; Hack: smallest possible reified system.

(define-structures ((mini-for-reification for-reification-interface)
		    (mini-packages (export make-simple-package)))
  (open scheme-level-2
	;; tables
	features		;contents
	locations
	signals)		;error
  (files (debug mini-package)))

(define-structure mini-system (export start)
  (open mini-scheme
	mini-command
	mini-for-reification
	mini-packages
	mini-environments		;set-interaction-environment!
	scheme-level-2-internal		;usual-resumer
	conditions handle		;error? with-handler
	signals)			;error
  (files (debug mini-start)))

(define (link-mini-system)
  (link-reified-system (list (cons 'scheme mini-scheme)
			     (cons 'write-images write-images)
			     (cons 'primitives primitives) ;just for fun
			     (cons 'scheme-level-2-internal
				   scheme-level-2-internal)
			     (cons 'command mini-command))
		       '(debug mini)
		       'start
		       mini-system mini-for-reification))



; --------------------
; S-expression interpreter

(define-structure run evaluation-interface
  (open scheme-level-2 syntactic packages scan meta-types
	environments
	signals
	locations
	features   ;force-output
	tables
	fluids)
  (files (debug run)))


; Hack: an interpreter-based system.

(define (link-medium-system)		;cf. initial.scm

  (def medium-scheme (make-scheme environments run))

  (let ()

    (def command (make-mini-command medium-scheme))

    (let ()

      (def medium-system
	;; Cf. initial-packages.scm
	(make-initial-system medium-scheme command))

      (let ((structs (list (cons 'scheme medium-scheme)
			   (cons 'primitives primitives) ;just for fun
			   (cons 'scheme-level-2-internal
				 scheme-level-2-internal)
			   (cons 'command command))))

	(link-reified-system structs
			     '(debug medium)
			     `(start ',(map car structs))
			     medium-system for-reification)))))
