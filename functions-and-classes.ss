(import :drewc/js-syntax/lexical :drewc/js-syntax/expressions :drewc/js-syntax/statements-and-declarations :drewc/smug)
(export #t)

(def FormalParameter BindingElement)
(defstruct (formal-parameters statement) (elements) transparent: #t)
(def FormalParameters
  (.let* (ps (sepby FormalParameter (tpv? #\,)))
    (return (formal-parameters ps))))

;;;; (defstruct function (identifier parameters body) transparent: #t)
;;;; (def (parse-function)
;;;;   (.let* ((_ (tpv? "function")) 
;;;;           (id (.or
;;;; 
;;;;                 BindingIdentifier
;;;; 
;;;;                 #f))
;;;;           (params
;;;;            (bracket (.or (tpv? #\() (js-syntax-error
;;;;                                     "Invalid function name definition"))
;;;; 
;;;;                     FormalParameters
;;;; 
;;;;                     (.or (tpv? #\)) (js-syntax-error
;;;;                                      "Invalid Function Parameter"))))
;;;;           (body
;;;;            (bracket (.or (tpv? #\{) (js-syntax-error
;;;;                                      "Invalid Function Body start"))
;;;;                     FunctionBody
;;;; 
;;;;                     (.or (tpv? #\}) (js-syntax-error
;;;;                                      "Invalid Function Body end")))))
;;;;     (return (function id params body))))
;;;; (defstruct (function-declaration function) (function) transparent: #t)
;;;; (def FunctionDeclaration
;;;;   (.let* (f (parse-function)) (return (function-declaration f))))
