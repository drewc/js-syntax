(import :drewc/smug)
;; Unicode Format-Control Characters
(def <ZWNJ> (abr #x200C)) (def <ZWJ> (abr #x200D)) (def <ZWNBSP> (abr #xFEFF))
;;  White Space Code Points
(def <TAB> (abr #\Tab)) (def <VT> (abr #x000B)) (def <FF> (abr #x000C))
(def <SP> (abr #\Space)) (def <NBSP> (arb #x00A0))

;; TODO: | Other category “Zs” | Any other Unicode “Space_Separator” code point
;; | <USP> |

(def WhiteSpace (skip (.or <TAB> <VT> <FF> <SP> <NBSP> <ZWNBSP>)))

;; Line Terminator Code Points 
(def <LF> (abr #x000A)) (def <CR> (abr #x000D)) 
(def <LS> (abr #x2028)) (def <PS> (abr #2029))
