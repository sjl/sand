(in-package #:sand.ffi)

(define-alien-routine ("strlen" c-str-len) size-t
  (s c-string :in))

(define-alien-routine ("hypot" hypotenuse) double
  (x double :in)
  (y double :in))

(define-alien-routine ("strchr" string-char) c-string
  (s c-string)
  (c int))

(declaim (inline is-upper))
(define-alien-routine ("isupper" is-upper) int
  (ch int))

(defun uppercasep (character)
  (not (zerop (is-upper (char-code character)))))

(c-str-len "Hello!")
(load-shared-object "~/src/linenoise/linenoise.dylib")

(define-alien-routine linenoise c-string
  (prompt c-string))

(define-alien-routine
  ("linenoiseHistorySetMaxLen" linenoise-history-set-max-len)
  int
  (max-length int))

(define-alien-routine ("linenoiseHistoryAdd" linenoise-history-add) int
  (string c-string))

(define-alien-routine ("linenoiseClearScreen" linenoise-clear-screen) void)



(linenoise-history-set-max-len 10)
(linenoise-history-add "Alice")
(linenoise-history-add "Bob")
(iterate (for i :from 0 :to 20)
         (linenoise-history-add (format nil "history entry ~d" i)))
(linenoise "? ")

