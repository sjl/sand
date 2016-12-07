(in-package :sand.zero-suppressed-decision-diagrams)

(adt:defdata zdd
  empty
  unit
  (node t zdd zdd))


(defun zdd-with (element)
  (node element empty unit))

(defun patch-unit (z)
  (adt:match zdd z
    (empty unit)
    (unit unit)
    ((node element low high)
     (node element (patch-unit low) high))))

(defun zdd-union (z1 z2 &key (test #'<))
  (recursively ((z1 z1) (z2 z2))
    (adt:match zdd z1
      (empty z2)
      (unit (patch-unit z2))
      ((node e1 l1 h1) (adt:match zdd z2
                         (empty z1)
                         (unit (patch-unit z1))
                         ((node e2 l2 h2)
                          (cond
                            ((funcall test e1 e2) (node e1 (recur l1 z2) h1))
                            ((funcall test e2 e1) (recur z2 z1))
                            (t (node e1 (recur l1 l2) (recur h1 h2))))))))))

(defun zdd-adjoin (z element &key (test #'<))
  (recursively ((z z))
    (adt:match zdd z
      (empty empty)
      (unit (node element empty unit))
      ((node e low high)
       (cond ((funcall test element e)
              (node element empty z))
             ((funcall test e element)
              (node e (recur low) (recur high)))
             (t
              (node element empty (zdd-union low high :test test))))))))

(defun zdd-disjoin (z element &key (test #'<))
  (recursively ((z z))
    (adt:match zdd z
      (empty empty)
      (unit unit)
      ((node e low high)
       (cond ((funcall test element e)
              z)
             ((funcall test e element)
              (node e (recur low) (recur high)))
             (t
              (zdd-union low high :test test)))))))


(defun enumerate-zdd (zdd)
  (adt:match zdd zdd
    (empty nil)
    (unit (list nil))
    ((node element low high)
     (append (mapcar (lambda (s) (cons element s))
                     (enumerate-zdd high))
             (enumerate-zdd low)))))


; (zdd-union (zdd-union (zdd-with 2) (zdd-with 1))
;            (zdd-adjoin
;              (zdd-union (zdd-union (zdd-with 2) (zdd-with 3))
;                         (zdd-union (zdd-with 1) (zdd-with 3)))
;              2))

; (zdd-adjoin
;   (zdd-union (zdd-union (zdd-with 2) (zdd-with 3))
;              (zdd-union (zdd-with 1) (zdd-with 3)))
;   2)

; (zdd-disjoin * 2)


; (enumerate-zdd *)
