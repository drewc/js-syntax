(import :drewc/smug :drewc/js-syntax/lexical :drewc/js-syntax/statements-and-declarations)

(export #t)

(def ScriptBody (.begin StatementList))
(def Script (.or ScriptBody #f))

(defstruct script (tree) transparent: #t)

(def (parse-script str)
  (run (.let* (lst Script)
         (return (script (and lst (list->vector lst)))))
       (lexify (tokenize str))))
