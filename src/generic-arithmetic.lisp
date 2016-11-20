(in-package :sand.generic-arithmetic)

;;;;     ________  __________ __    __________   _       _________  ____  ______
;;;;    / ____/ / / / ____/ //_/   /  _/_  __/  | |     / / ____( )/ __ \/ ____/
;;;;   / /_  / / / / /   / ,<      / /  / /     | | /| / / __/  |// /_/ / __/
;;;;  / __/ / /_/ / /___/ /| |   _/ /  / /  _   | |/ |/ / /___   / _, _/ /___
;;;; /_/    \____/\____/_/ |_|  /___/ /_/  ( )  |__/|__/_____/  /_/ |_/_____/
;;;;    __________  _____   ________   ____|/__  ____    __       ______
;;;;   / ____/ __ \/  _/ | / / ____/  / ____/ / / / /   / /      / ____/__    __
;;;;  / / __/ / / // //  |/ / / __   / /_  / / / / /   / /      / /  __/ /___/ /_
;;;; / /_/ / /_/ // // /|  / /_/ /  / __/ / /_/ / /___/ /___   / /__/_  __/_  __/
;;;; \____/\____/___/_/ |_/\____/  /_/    \____/_____/_____/   \____//_/   /_/
;;;;

;;; This is a quick and dirty experiment at adding generic arithmetic operators
;;; to Common Lisp.  To add support for operating on another type you would add
;;; methods for `unaryOP` and `binaryOP`, e.g.:
;;;
;;; (defmethod unary+ ((v vector)) v)
;;;
;;; (defmethod binary+ ((n number) (v vector))
;;;   (map 'vector (lambda (el) (+ el n)) v))
;;;
;;; (defmethod binary+ ((v vector) (n number))
;;;   (binary+ n v))
;;;
;;; Compiler macros are defined so that if the op is called on things that are
;;; known to be `number`s at compile time it will compile in the vanilla CL op
;;; instead:
;;;
;;;     (defun foo (x)
;;;       (+ 5 x))
;;;     (disassemble 'foo)
;;;     ; disassembly for FOO
;;;     ; Size: 38 bytes. Origin: #x100ACC85F8
;;;     ; 5F8:       498B4C2460       MOV RCX, [R12+96]      ; thread.binding-stack-pointer
;;;                                                          ; no-arg-parsing entry point
;;;     ; 5FD:       48894DF8         MOV [RBP-8], RCX
;;;     ; 601:       BA0A000000       MOV EDX, 10
;;;     ; 606:       488BFE           MOV RDI, RSI
;;;     ; 609:       488B0590FFFFFF   MOV RAX, [RIP-112]     ; #<SB-KERNEL:FDEFN +>
;;;     ; 610:       B904000000       MOV ECX, 4
;;;     ; 615:       FF7508           PUSH QWORD PTR [RBP+8]
;;;     ; 618:       FF6009           JMP QWORD PTR [RAX+9]
;;;     ; 61B:       0F0B10           BREAK 16               ; Invalid argument count trap
;;;
;;;
;;;     (defun foo-number (x)
;;;       (declare (type number x))
;;;       (+ 5 x))
;;;     (disassemble 'foo-number)
;;;     ; disassembly for FOO-NUMBER
;;;     ; Size: 39 bytes. Origin: #x100ACC854B
;;;     ; 4B:       498B4C2460       MOV RCX, [R12+96]       ; thread.binding-stack-pointer
;;;                                                          ; no-arg-parsing entry point
;;;     ; 50:       48894DF8         MOV [RBP-8], RCX
;;;     ; 54:       BF0A000000       MOV EDI, 10
;;;     ; 59:       488BD3           MOV RDX, RBX
;;;     ; 5C:       41BBD0010020     MOV R11D, 536871376     ; GENERIC-+
;;;     ; 62:       41FFD3           CALL R11
;;;     ; 65:       488B5DF0         MOV RBX, [RBP-16]
;;;     ; 69:       488BE5           MOV RSP, RBP
;;;     ; 6C:       F8               CLC
;;;     ; 6D:       5D               POP RBP
;;;     ; 6E:       C3               RET
;;;     ; 6F:       0F0B10           BREAK 16                ; Invalid argument count trap
;;;
;;;
;;;     (defun foo-byte (x)
;;;       (declare (type (unsigned-byte 8) x))
;;;       (+ 5 x))
;;;     (disassemble 'foo-byte)
;;;     ; disassembly for FOO-BYTE
;;;     ; Size: 22 bytes. Origin: #x100290CB4B
;;;     ; 4B:       498B4C2460       MOV RCX, [R12+96]       ; thread.binding-stack-pointer
;;;                                                          ; no-arg-parsing entry point
;;;     ; 50:       48894DF8         MOV [RBP-8], RCX
;;;     ; 54:       488D530A         LEA RDX, [RBX+10]
;;;     ; 58:       488BE5           MOV RSP, RBP
;;;     ; 5B:       F8               CLC
;;;     ; 5C:       5D               POP RBP
;;;     ; 5D:       C3               RET
;;;     ; 5E:       0F0B10           BREAK 16                ; Invalid argument count trap

(defmacro define-generic-operation (name op allow-nullary)
  (let ((unary (symb 'unary name))
        (binary (symb 'binary name)))
    `(progn
      (defgeneric ,unary (x))

      (defmethod ,unary ((x number))
        (,op x))

      (defgeneric ,binary (x y))

      (defmethod ,binary ((x number) (y number))
        (,op x y))

      ,(if allow-nullary
         `(defun ,name (&rest arguments)
           (cond
             ((null arguments) (,op))
             ((null (cdr arguments)) (,unary (car arguments)))
             (t (reduce #',binary arguments))))
         `(defun ,name (argument &rest more)
           (if (null more)
             (,unary argument)
             (reduce #',binary more :initial-value argument))))

      (define-compiler-macro ,name (&whole form &rest arguments &environment env)
        (if (every (rcurry #'subtypep 'number)
                   (mapcar (rcurry #'form-type env) arguments))
          `(,',op ,@arguments)
          form))

      ',name)))


(define-generic-operation + cl:+ t)
(define-generic-operation - cl:- nil)
(define-generic-operation * cl:* t)
(define-generic-operation / cl:/ nil)

;;;; Example: Vectors
;;; Addition
(defmethod unary+ ((v vector)) v)

(defmethod binary+ ((v1 vector) (v2 vector))
  (map 'vector #'+ v1 v2))

(defmethod binary+ ((v vector) (n number))
  (map 'vector (lambda (el) (+ el n)) v))

(defmethod binary+ ((n number) (v vector))
  (binary+ v n))


;;; Multiplication
(defmethod unary* ((v vector)) v)

(defmethod binary* ((v1 vector) (v2 vector))
  (map 'vector #'* v1 v2))

(defmethod binary* ((v vector) (n number))
  (map 'vector (lambda (el) (* el n)) v))

(defmethod binary* ((n number) (v vector))
  (binary* n v))

;;; Subtraction
(defmethod unary- ((v vector))
  (map 'vector #'- v))

(defmethod binary- ((v1 vector) (v2 vector))
  (map 'vector #'- v1 v2))

(defmethod binary- ((v vector) (n number))
  (map 'vector (lambda (el) (- el n)) v))

(defmethod binary- ((n number) (v vector))
  (error "Cannot subtract number by a vector: (- ~S ~S)" n v))

;;; Division
(defmethod unary/ ((v vector))
  (map 'vector #'/ v))

(defmethod binary/ ((v1 vector) (v2 vector))
  (map 'vector #'/ v1 v2))

(defmethod binary/ ((v vector) (n number))
  (map 'vector (lambda (el) (/ el n)) v))

(defmethod binary/ ((n number) (v vector))
  (error "Cannot divide number by a vector: (/ ~S ~S)" n v))
