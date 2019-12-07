(import :std/misc/list :drewc/smug)
(export #t)

(def HashtagLang (.let* ((_ "#lang ") (v (.read-line))) `(|#lang|, v)))

(def Annotation (.let* (a #\@) (return 'Annotation)))

(def custom-tokens [HashtagLang Annotation])

(def (lex-custom-tokens) (apply .or custom-tokens))

(def CustomToken (.let* (ct (lex-custom-tokens)) (return ct)))
