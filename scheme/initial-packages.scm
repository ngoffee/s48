; Copyright (c) 1993-1999 by Richard Kelsey and Jonathan Rees. See file COPYING.


; Packages involved in building the initial system.


; Access to values from packages and structures 

(define-structure environments environments-interface
  (open scheme-level-2
	packages bindings meta-types
	fluids
	locations	; contents location-assigned?
	signals)	; error
  (files (rts env)))

; EVAL and LOAD

(define-structure evaluation evaluation-interface
  (open scheme-level-2
	packages        	;package-uid package->environment link!
	environments		;package-for-load
	compiler-envs		;bind-source-filename
	reading-forms		;read-forms $note-file-package
	syntactic		;scan-forms expand-forms
	compiler		;compile-forms
	closures		;make-closure
	vm-exposure		;invoke-closure
	features		;current-noise-port force-output
	signals fluids)
  (files (rts eval)))

; Scheme = scheme-level-2 plus EVAL and friends

(define-module (make-scheme environments evaluation)
  (define-structure scheme scheme-interface
    (open scheme-level-2
	  environments
	  evaluation))
  scheme)


; Command processor.

(define-module (make-mini-command scheme) ;copied from debug-packages.scm
  (define-structure mini-command (export command-processor)
    (open scheme
	  signals conditions handle
	  display-conditions
	  i/o)                 ;current-error-port
    (files (debug mini-command)))
  mini-command)


; For building systems.

(define-module (make-initial-system scheme command)

  (define-structure initial-system (export start)
    (open scheme
	  command
	  interfaces		;make-simple-interface
	  packages		;make-simple-package
	  environments		;with-interaction-environment, etc.
	  usual-resumer
	  conditions handle	;error? with-handler
	  signals)		;error
    (files (env start)))

  initial-system)


; Utility to load packages following dependency links (OPEN and ACCESS)
;Cf. (link-initial-system) and Makefile

(define-structure ensures-loaded (export ensure-loaded)
  (open scheme-level-2
	features		;current-noise-port
	packages		;package-uid package-clients
	packages-internal	;package-loaded? set-package-loaded?!
	scan-package		;collect-packages check-structure
	compile-packages	;compile-package
	closures		;make-closure
	vm-exposure		;invoke-closure
	environments		;with-interaction-environment
	weak			;walk-population
	)
  (files (env load-package)))

; Things needed by the expression generated by REIFY-STRUCTURES.

(define-structure for-reification for-reification-interface
  (open scheme-level-1
	packages packages-internal
	signals
	meta-types			;sexp->type structure-type
	interfaces			;make-simple-interface
	bindings
	nodes				;get-operator operator? operator-type
	primops				;get-primop primop? primop-type
	usual-macros			;usual-transform
	inline				;inline-transform
	transforms			;make-transform transform? transform-type
	tables)
  (files (bcomp for-reify)))
