;;; -*- Gerbil -*-
;;; (C) me at drewc.ca
(import :drewc/smug :drewc/js-syntax/lexical
        :drewc/js-syntax/custom :drewc/js-syntax/expressions
        :std/misc/list)
(export #t)
;; [[file:~/src/js-syntax/statements-and-declarations.org][]]
(defstruct statement-or-declaration ())
(defstruct (statement statement-or-declaration) ())
(defstruct (declaration statement-or-declaration) ())

;;(defstruct (expression-statement statement) (expression) transparent: #t)
;;(def ExpressionStatement (.let* (e Expression) (return (expression-statement e))))

;; ends here
;; [[file:~/src/js-syntax/statements-and-declarations.org][]]
(def (js-syntax-error . args)
  (.let* (t (item))
    (apply error "Invalid Syntax at " (token-start t) (token-value t) args)))

;; ends here
;; [[file:~/src/js-syntax/statements-and-declarations.org::#syntax-42][]]
(defstruct (empty-statement statement) ()) 
(def EmptyStatement
  (.begin (token-production-value? #\;) (return (empty-statement))))
;; ends here
;; [[file:~/src/js-syntax/statements-and-declarations.org::#SingleNameBinding][]]
(defstruct (single-name-binding statement) (identifier initializer)
  transparent: #t)

(def SingleNameBinding
  (.let* (id BindingIdentifier) (return (single-name-binding id #f))))
;; ends here
;; [[file:~/src/js-syntax/statements-and-declarations.org::#BindingElement][]]
(defstruct (binding-element statement) (binding) transparent: #t)
(def binding-elements [SingleNameBinding]) 
(def (add-binding-element! p) (push! p binding-elements)) 
(def (parse-binding-elements)
  (.let* (b (.begin #t (apply .or binding-elements)))
   (return (binding-element b))))
(defsyntax (BindingElement stx)
  (syntax-case stx ()
    ((macro arg) #'((parse-binding-elements) arg))
    (sym #'(parse-binding-elements))))
;; ends here
;; [[file:~/src/js-syntax/statements-and-declarations.org::#syntax-37][]]
(def statements []) 
(def (add-statement! p) (push! p statements)) 
(def (parse-statements) (.begin #t (apply .or CustomStatement statements)))
(defsyntax (Statement stx)
  (syntax-case stx ()
    ((macro arg) #'((parse-statements) arg))
    (sym #'(parse-statements))))
;; ends here
;; [[file:~/src/js-syntax/statements-and-declarations.org::#syntax-37][]]
(def declarations [FAIL]) 
(def (add-declaration! p) (push! p declarations)) 
(def (parse-declarations) (.begin #t (apply .or CustomDeclaration declarations)))
(defsyntax (Declaration stx)
  (syntax-case stx ()
    ((macro arg) #'((parse-declarations) arg))
    (sym #'(parse-declarations))))


;; ends here
;; [[file:~/src/js-syntax/statements-and-declarations.org::*StatementList][]]
(def StatementList (many1 StatementListItem))
;; ends here
;; [[file:~/src/js-syntax/statements-and-declarations.org::*StatementList][]]
(def StatementListItem (.begin #!void (.or Declaration Statement (js-syntax-error))))
;; ends here
