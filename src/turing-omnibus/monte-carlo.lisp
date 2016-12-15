(in-package :sand.turing-omnibus.monto-carlo)

;;;; From The New Turing Omnibus, Chapter 4

(defun frequency (alpha time)
  (* (/ alpha)
     (exp (- (/ time alpha)))))

(defun cumulative-frequency (alpha time)
  (- 1 (exp (- (/ time alpha)))))

(defun inverse-cumulative-frequency (alpha x)
  (* alpha (log (- 1 x))))

;; Something is fucky with the above inverse function given by the book...  `α
;; ln(1 - x)` will always give a negative result (for x in [0, 1)), so how can
;; we use it as a number of seconds to wait?

; (defun simulate (1000)
;   (let ((max-time )))
;   )

; (gnuplot-function (curry #'inverse-cumulative-frequency 1.0)
;                   :start 0.0
;                   :end 1.00
;                   :step 0.1)

