(defpackage :sand.names
  (:use
    :cl
    :losh
    :iterate
    :sand.quickutils
    :sand.utils)
  (:export))

(in-package :sand.names)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun normalize-namespec (namespec)
    (ecase (length namespec)
      (2 (destructuring-bind (pre suf) namespec
           (list (symbol-name pre)
                 nil
                 (symbol-name suf))))
      (3 (destructuring-bind (pre in suf) namespec
           (list (symbol-name pre)
                 (symbol-name in)
                 (symbol-name suf))))))

  (defun parse-namespecs (namespecs)
    (let ((namespecs (mapcar #'normalize-namespec namespecs)))
      (values (map 'vector #'first namespecs)
              (remove nil (map 'vector #'second namespecs))
              (map 'vector #'third namespecs)))))


(defun generate-name (prefixes infixes suffixes)
  (concatenate 'string
               (random-elt prefixes)
               (if (randomp)
                 (random-elt infixes)
                 "")
               (random-elt suffixes)))


(defun build-name-generator% (prefixes infixes suffixes)
  (lambda ()
    (generate-name prefixes infixes suffixes)))

(defun build-name-generator (namespecs)
  (multiple-value-call #'build-name-generator% (parse-namespecs namespecs)))


(defmacro define-name-generator (symbol &body namespecs)
  (multiple-value-bind (prefixes infixes suffixes)
      (parse-namespecs namespecs)
    `(defun ,symbol ()
       (generate-name ,prefixes ,infixes ,suffixes))))


(define-name-generator icelandic-name
  (si grun)
  (kri strún)
  (snó rri)
  (au ður)
  (na nna)
  (gu nnar)
  (fa nney)
  (si gu rður)
  (ha fdís)
  (pa lmí)
  (ha rpa)
  (sæ var)
  (ei nar)
  (ra gnar)
  (ra gnhei ður)
  (ma gnus)
  (hau kur)
  (bja rtur)
  (ey þór)
  (si gu rbjörg)
  (da rri)
  (þo rva ldur))


(define-name-generator spanish-name
  (Sa ntia go)
  (Se ba stián)
  (Ma tías)
  (Ma teo)
  (Ni co lás)
  (Ale ja ndro)
  (Die go)
  (Sa muel)
  (Benja mín)
  (So fia)
  (I sa bella)
  (Ca mi la)
  (Va lenti na)
  (Va le ria)
  (Ma ria na)
  (Lu cia nia)
  (Da niela)
  (Ga brie la))

