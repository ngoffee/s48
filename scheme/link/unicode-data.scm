; Copyright (c) 1993-2004 by Richard Kelsey and Jonathan Rees. See file COPYING.

; Parse UnicodeData.txt and generate character classification and
; conversion tables from it.

(define (string-split string at)
  (let ((count (string-length string)))
    (let loop ((index 0)
	       (rev-result '()))
      (cond
       ((>= index count)
	(reverse (cons "" rev-result)))
       ((string-index string at index)
	=> (lambda (found)
	     (loop (+ 1 found)
		   (cons (substring string index found)
			 rev-result))))
       (else
	(reverse (cons (substring string index count)
		       rev-result)))))))
	  

(define (split-unicode-data-record line)
  (string-split line #\;))

(define (maybe-code-point text default)
  (if (zero? (string-length text))
      default
      (string->number text 16)))

(define-record-type code-point-info :code-point-info
  (make-code-point-info code-point
			name
			general-category
			combining-class
			bidirectional-category-id
			decomposition-id
			decimal-digit-value
			digit-value
			numeric-value
			mirrored?
			unicode-1.0-name
			iso-10646-comment
			uppercase-code-point
			lowercase-code-point
			titlecase-code-point)
  code-point-info?
  ;; number
  (code-point code-point-info-code-point)
  ;; string
  (name code-point-info-name)
  ;; :GENERAL-CATEGORY
  (general-category code-point-info-general-category)
  ;; number
  (combining-class code-point-info-combining-class)
  ;; symbol
  (bidirectional-category-id code-point-info-bidirectional-category-id)
  ;; #f or string
  (decomposition-id code-point-info-decomposition-id)
  ;; number
  (decimal-digit-value code-point-info-decimal-digit-value)
  ;; number
  (digit-value code-point-info-digit-value)
  ;; number
  (numeric-value code-point-info-numeric-value)
  ;; boolean
  (mirrored? code-point-info-mirrored?)
  ;; string
  (unicode-1.0-name code-point-info-unicode-1.0-name)
  ;; string
  (iso-10646-comment code-point-info-iso-10646-comment)
  ;; number
  (uppercase-code-point code-point-info-uppercase-code-point)
  ;; number
  (lowercase-code-point code-point-info-lowercase-code-point)
  ;; number
  (titlecase-code-point code-point-info-titlecase-code-point))

(define-record-discloser :code-point-info
  (lambda (r)
    (list 'code-point-info
	  (code-point-info-code-point r)
	  (code-point-info-name r)
	  (code-point-info-general-category r)
	  (code-point-info-combining-class r)
	  (code-point-info-bidirectional-category-id r)
	  (code-point-info-decomposition-id r)
	  (code-point-info-decimal-digit-value r)
	  (code-point-info-digit-value r)
	  (code-point-info-numeric-value r)
	  (code-point-info-mirrored? r)
	  (code-point-info-unicode-1.0-name r)
	  (code-point-info-iso-10646-comment r)
	  (code-point-info-uppercase-code-point r)
	  (code-point-info-lowercase-code-point r)
	  (code-point-info-titlecase-code-point r))))

(define (unicode-data-record->info line)
  (destructure (((code-point-hex
		  name
		  general-category-id
		  combining-class-id
		  bidirectional-category-text
		  decomposition-id
		  decimal-digit-value-text
		  digit-value-text
		  numeric-value-text
		  mirrored-y/n
		  unicode-1.0-name
		  iso-10646-comment
		  uppercase-code-point-hex
		  lowercase-code-point-hex
		  titlecase-code-point-hex)
		 (split-unicode-data-record line)))
    (let ((code-point (maybe-code-point code-point-hex #f)))
      (let ((uppercase-code-point (maybe-code-point uppercase-code-point-hex code-point))
	    (lowercase-code-point (maybe-code-point lowercase-code-point-hex code-point))
	    (titlecase-code-point (maybe-code-point titlecase-code-point-hex code-point)))
	(make-code-point-info code-point
			      name
			      (id->general-category general-category-id)
			      (string->number combining-class-id)
			      (string->symbol bidirectional-category-text)
			      (if (zero? (string-length decomposition-id))
				  #f
				  decomposition-id)
			      (string->number decimal-digit-value-text)
			      (string->number digit-value-text)
			      (string->number numeric-value-text)
			      (string=? mirrored-y/n "Y")
			      unicode-1.0-name
			      iso-10646-comment
			      uppercase-code-point
			      lowercase-code-point
			      titlecase-code-point)))))

(define (read-line port)
  (let loop ((l '()))
    (let ((c (read-char port)))
      (if (eof-object? c)
          c
          (if (char=? c #\newline)
              (list->string (reverse l))
              (loop (cons c l)))))))

(define (parse-unicode-data filename)
  (call-with-input-file filename
    (lambda (port)
      (let loop ((rev-infos '()))
	(let ((thing (read-line port)))
	  (if (eof-object? thing)
	      (reverse rev-infos)
	      (loop (cons (unicode-data-record->info thing) rev-infos))))))))

; Mapping the relevant info (general category + case mappings) into a
; compact array

(define (mapping-offsets infos accessor)
  (let loop ((infos infos)
	     (offsets '()))
    (if (null? infos)
	(list->vector offsets)
	(let* ((info (car infos))
	       (code-point (code-point-info-code-point info))
	       (other (accessor info))
	       (offset (- other code-point)))
	  (if (member offset offsets)
	      (loop (cdr infos) offsets)
	      (loop (cdr infos) (cons offset offsets)))))))

(define (vector-index vector value)
  (let ((count (vector-length vector)))
    (let loop ((i 0))
      (cond
       ((>= i count) #f)
       ((equal? value (vector-ref vector i)) i)
       (else (loop (+ 1 i)))))))

(define (code-point-info->case+general-category-encoding
	 info
	 uppercase-offsets lowercase-offsets titlecase-offsets
	 uppercase-index-width lowercase-index-width titlecase-index-width)
  (let ((code-point (code-point-info-code-point info)))
    (let ((uppercase-index (vector-index uppercase-offsets
					 (- (code-point-info-uppercase-code-point info)
					    code-point)))
	  (lowercase-index (vector-index lowercase-offsets
					 (- (code-point-info-lowercase-code-point info)
					    code-point)))
	  (titlecase-index (vector-index titlecase-offsets
					 (- (code-point-info-titlecase-code-point info)
					    code-point))))

      (bitwise-ior
       (arithmetic-shift
	(bitwise-ior
	 (arithmetic-shift (bitwise-ior
			    (arithmetic-shift uppercase-index lowercase-index-width)
			    lowercase-index)
			   titlecase-index-width)
	 titlecase-index)
	*general-category-bits*)
       (general-category-index (code-point-info-general-category info))))))

(define (lookup-by-offset-index code-point offset-index offsets)
  (+ code-point (vector-ref offsets offset-index)))

(define (code-point-encoding-uppercase-code-point code-point encoding
						  uppercase-offsets
						  uppercase-index-width lowercase-index-width titlecase-index-width)
  (lookup-by-offset-index
   code-point
   (arithmetic-shift encoding
		     (- (+ lowercase-index-width titlecase-index-width *general-category-bits*)))
   uppercase-offsets))

(define (code-point-encoding-lowercase-code-point code-point encoding
						  lowercase-offsets
						  uppercase-index-width lowercase-index-width titlecase-index-width)
  (lookup-by-offset-index
   code-point
   (bitwise-and (- (arithmetic-shift 1 lowercase-index-width) 1)
		(arithmetic-shift encoding
				  (- (+ titlecase-index-width *general-category-bits*))))
   lowercase-offsets))

(define (code-point-encoding-titlecase-code-point code-point encoding
						  titlecase-offsets
						  uppercase-index-width lowercase-index-width titlecase-index-width)
  (lookup-by-offset-index
   code-point
   (bitwise-and (- (arithmetic-shift 1 titlecase-index-width) 1)
		(arithmetic-shift encoding (- *general-category-bits*)))
   titlecase-offsets))

(define *code-point-encoding-general-category-mask*
  (- (arithmetic-shift 1 *general-category-bits*) 1))

(define (code-point-encoding-general-category encoding)
  (vector-ref general-categories
	      (bitwise-and encoding *code-point-encoding-general-category-mask*)))
						  

(define (max-code-point infos)
  (let loop ((max 0) (infos infos))
    (cond
     ((null? infos) max)
     ((> (code-point-info-code-point (car infos))
	 max)
      (loop (code-point-info-code-point (car infos)) (cdr infos)))
     (else (loop max (cdr infos))))))

; returns a THUNK that will return for each code-point in sequence
; (PROC <code-point>) or DEFAULT if there's no info.

; assumes INFOS are sorted

(define (make-info-source infos default proc)
  (let ((last-code-point 0))
    (lambda ()
      (if (null? infos)
	  #f
	  (let* ((info (car infos))
		 (code-point (code-point-info-code-point info)))
	    (if (< (+ 1 last-code-point) code-point)
		(begin
		  (set! last-code-point (+ 1 last-code-point))
		  default)
		(begin
		  (set! last-code-point code-point)
		  (set! infos (cdr infos))
		  ;; scalar values only
		  (if (eq? (code-point-info-general-category info)
			   (general-category surrogate))
		      default
		      (proc info)))))))))



(define (make-scalar-value-case+general-category-encoding-tables infos block-bits)

  (let ((uppercase-offsets (mapping-offsets infos code-point-info-uppercase-code-point))
	(lowercase-offsets (mapping-offsets infos code-point-info-lowercase-code-point))
	(titlecase-offsets (mapping-offsets infos code-point-info-titlecase-code-point)))
    (let ((uppercase-index-width (bits-necessary (vector-length uppercase-offsets)))
	  (lowercase-index-width (bits-necessary (vector-length lowercase-offsets)))
	  (titlecase-index-width (bits-necessary (vector-length titlecase-offsets)))

	  (block-size (expt 2 block-bits)))

      (call-with-values
	  (lambda ()
	    (compute-compact-table
	     (make-info-source infos
			       0
			       (lambda (info)
				 (code-point-info->case+general-category-encoding
				  info
				  uppercase-offsets lowercase-offsets titlecase-offsets
				  uppercase-index-width lowercase-index-width titlecase-index-width)))
	     block-size))
	(lambda (indices encodings)
	  (values indices encodings
		  uppercase-offsets lowercase-offsets titlecase-offsets))))))

; saves a couple of kilobyes, but probably not worthwhile

(define (write-vector-code/rll name vector port)
  (write `(define ,name (make-vector ,(vector-length vector)))
	 port)
  (newline port)
  (let loop ((values (vector->list vector))
	     (index 0))
    (cond
     ((null? values))
     ((or (null? (cdr values))
	  (not (equal? (car values) (cadr values))))
      (write `(vector-set! ,name ,index ,(car values))
	     port)
      (newline port)
      (loop (cdr values) (+ 1 index)))
     (else
      (let ((value (car values)))
	(let inner-loop ((values values)
			 (last-index index))
	  (cond
	   ((or (null? values)
	       (not (equal? (car values) value)))
	    (write
	     `(do ((i ,index (+ 1 i)))
		  ((>= i ,last-index))
		(vector-set! ,name i ,value))
	     port)
	    (newline port)
	    (loop values last-index))
	   (else
	    (inner-loop (cdr values) (+ 1 last-index))))))))))

(define (create-unicode-tables unicode-data-filename output-file)
  (let ((block-bits 8) ; better than 9, at least
	(infos (parse-unicode-data unicode-data-filename)))
    (call-with-values
	(lambda () (make-scalar-value-case+general-category-encoding-tables infos block-bits))
      (lambda (indices
	       encodings
	       uppercase-offsets lowercase-offsets titlecase-offsets)

	  (call-with-output-file output-file
	    (lambda (port)
	      (display "; Automatically generated by CREATE-UNICODE-TABLES; do not edit."
		       port)
	      (newline port)
	      (newline port)

	      (write `(define *encoding-table-block-bits* ,block-bits)
		     port)
	      (newline port)
	      (newline port)

	      (write `(define *uppercase-index-width*
			,(bits-necessary (vector-length uppercase-offsets)))
		     port)
	      (newline port)
	      (write `(define *lowercase-index-width*
			,(bits-necessary (vector-length lowercase-offsets)))
		     port)
	      (newline port)
	      (write `(define *titlecase-index-width*
			,(bits-necessary (vector-length titlecase-offsets)))
		     port)
	      (newline port)
	      (newline port)

	      (write `(define *scalar-value-info-indices* ',indices)
		     port)
	      (newline port)
	      (write `(define *scalar-value-info-encodings* ',encodings)
		     port)
	      (newline port)
	      (newline port)

	      (write `(define *uppercase-offsets* ',uppercase-offsets)
		     port)
	      (newline port)
	      (write `(define *lowercase-offsets* ',lowercase-offsets)
		     port)
	      (newline port)
	      (write `(define *titlecase-offsets* ',titlecase-offsets)
		     port)
	      (newline port)))))))

; for debugging

(define (test-code-point-case+general-category-encoding-tables
	 infos block-bits
	 indices encodings
	 uppercase-offsets lowercase-offsets titlecase-offsets)

  (let ((lower-mask (- (arithmetic-shift 1 block-bits) 1))
	(uppercase-index-width (bits-necessary (vector-length uppercase-offsets)))
	(lowercase-index-width (bits-necessary (vector-length lowercase-offsets)))
	(titlecase-index-width (bits-necessary (vector-length titlecase-offsets))))

    (for-each
     (lambda (info)
       (let* ((code-point (code-point-info-code-point info))
	      (base-index (vector-ref indices
				      (arithmetic-shift code-point (- block-bits))))
	      (encoding
	       (vector-ref encodings
			   (+ base-index (bitwise-and code-point lower-mask)))))

	 (if (not (eq? (code-point-info-general-category info)
		       (general-category surrogate)))
	     (begin

	       (if (not (eq? (code-point-info-general-category info)
			     (code-point-encoding-general-category encoding)))
		   (error "general category mismatch" (code-point-encoding-general-category encoding)))

	       (let ((uppercase-code-point
		      (code-point-encoding-uppercase-code-point
		       code-point encoding
		       uppercase-offsets
		       uppercase-index-width lowercase-index-width titlecase-index-width))
		     (lowercase-code-point
		      (code-point-encoding-lowercase-code-point
		       code-point encoding
		       lowercase-offsets
		       uppercase-index-width lowercase-index-width titlecase-index-width))
		     (titlecase-code-point
		      (code-point-encoding-titlecase-code-point
		       code-point encoding
		       titlecase-offsets
		       uppercase-index-width lowercase-index-width titlecase-index-width)))

		 (if (not (= (code-point-info-uppercase-code-point info)
			     uppercase-code-point))
		     (error "uppercase mismatch" info uppercase-code-point))

		 (if (not (= (code-point-info-lowercase-code-point info)
			     lowercase-code-point))
		     (error "lowercase mismatch" info lowercase-code-point))
	   
		 (if (not (= (code-point-info-titlecase-code-point info)
			     titlecase-code-point))
		     (error "titlecase mismatch" info titlecase-code-point)))))))
     infos)))

(define (find-code-point-info code-point infos)
  (find (lambda (info)
	  (= code-point (code-point-info-code-point info)))
	infos))