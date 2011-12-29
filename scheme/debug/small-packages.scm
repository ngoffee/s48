; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees

; (link-simple-system '(debug small) 'start tiny-system)

(define-structure small-system (export start)
  (define-all-operators)
  (usual-transforms and cond do let let* or)
  (files (rts defenum) (rts arch) (debug small)))
