; Copyright (c) 1993 by Richard Kelsey and Jonathan Rees.  See file COPYING.


; Static linker.  Doesn't work very well this way (debug info is
; screwed up), so it's probably better to continue using linker.image
; instead.

(define-package ((linker linker-signature))
  (open scheme-level-2
	table compiler write-images debuginfo
	packages
	packages-internal       ;set-package-integrate?!
	scan
	syntactic		;name->symbol		
	reification
	closures		;make closure to pass to write-image
	filenames
	debug-data		;with-fresh-compiler-state
	locations
	fluids
	signals			;error
	util)			;sublist
  (files (link link)))

(define-package ((reification (export reify-structures)))
  (open scheme-level-2
	packages
	packages-internal ;?
	usual-macros		;find-free-names-in-syntax-rules
	syntactic		;name-hash, etc.
	meta-types			;usual-variable-type
	locations
	table record
	signals			;error
	features		;force-output
	util			;filter
	inline)			;name->extrinsic
  (files (link reify)))

(define-package ((expander expander-signature))
  (open scheme-level-2
	syntactic packages scan meta-types reconstruction
	bummed-define-record-types
	util signals table fluids)
  (files (opt expand)))

(define-package ((analysis (export analyze-forms)))
  (open scheme
	syntactic packages scan inline meta-types expander
	packages-internal	;set-package-integrate?!
	reconstruction
	signals
	locations
	features		;force-output
	table
	fluids
	util)
  (files (opt analyze)))


; Database of procedure names 'n' stuff.
; (copy in more-packages.scm ...)

(define-package ((debuginfo debuginfo-signature))
  (open scheme-level-2
	table
	debug-data
	packages
	packages-internal
	syntactic)
  (files (env debuginfo)))


; Mumble.

(define-package ((flatloading (export flatload)))
  (open scheme packages packages-internal filenames)
  (files (alt flatload)))

(define-package ((loadc (export load-configuration
				(structure-ref syntax))))
  (open scheme
	syntactic		;$source-file-name
	fluids)
  (files (link loadc)))


; Everything.

(define-package ((link-config (export )))  ;dummy structure...
  (open linker
	defpackage
	types
	analysis
	loadc
	flatloading
	signatures))

