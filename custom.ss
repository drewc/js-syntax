(import :std/misc/list :drewc/smug)
(export #t)

(def custom-tokens [FAIL])
(def (add-custom-token! p) (push! p custom-tokens))
(def (parse-custom-tokens) (apply .or custom-tokens))
(defsyntax (CustomToken stx)
  (syntax-case stx ()
    ((macro arg) #'((parse-custom-tokens) arg))
    (sym #'(parse-custom-tokens))))


(def custom-statements [FAIL])
(def (add-custom-statement! p) (push! p custom-statements))
(def (parse-custom-statements) (apply .or custom-statements))
(defsyntax (CustomStatement stx)
  (syntax-case stx ()
    ((macro arg) #'((parse-custom-statements) arg))
    (sym #'(parse-custom-statements))))


(def custom-declarations [FAIL])
(def (add-custom-declaration! p) (push! p custom-declarations))
(def (parse-custom-declarations) (.begin (apply .or custom-declarations)))
(defsyntax (CustomDeclaration stx)
  (syntax-case stx ()
    ((macro arg) #'((parse-custom-declarations) arg))
    (sym #'(parse-custom-declarations))))
