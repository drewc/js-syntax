(import :drewc/smug :drewc/js-syntax/source-code :drewc/js-syntax/custom-token :std/srfi/13)
(export #t)
(def (abr code) (.char=? (if (integer? code) (integer->char code) code)))

(def (opt p) (.or p (return #f)))

(def ID_Start (sat char-alphabetic?))
(def ID_Continue (.or (.let* (c ID_Start) c) (sat char-numeric?) #\_))

(def (lex-error . args)
  (.let* ((p (point))
          (v (.or (.read-line) #!eof))) 
   (apply error "LEX Error at " p v args)))

;; Unicode Format-Control Characters
(def <ZWNJ> (abr #x200C)) (def <ZWJ> (abr #x200D)) (def <ZWNBSP> (abr #xFEFF))

;;  White Space Code Points
(def <TAB> (abr #\tab)) (def <VT> (abr #x000B)) (def <FF> (abr #x000C))
(def <SP> (abr #\space)) (def <NBSP> (abr #x00A0))

;; TODO: | Other category “Zs” | Any other Unicode “Space_Separator” code point
;; | <USP> |

(def %ws (.or <TAB> <VT> <FF> <SP> <NBSP> <ZWNBSP>))
(def WhiteSpace (.begin %ws (skip %ws) (return 'WhiteSpace)))

;; Line Terminator Code Points 
(def <LF> (abr #x000A)) (def <CR> (abr #x000D)) 
(def <LS> (abr #x2028)) (def <PS> (abr #x2029))
(def LineTerminator (.begin (.or <LF> <CR> <LS> <PS>) (return 'LineTerminator)))

(def SingleLineCommentChar (.begin (.not LineTerminator) SourceCharacter))
(def SingleLineCommentChars (many1 SingleLineCommentChar))
(def SingleLineComment
  (.let* ((_ "//") (cs (opt SingleLineCommentChars)))
    (return 'single-line-comment)))

(def Comment 
  (.let* (c (.or SingleLineComment)) `(Comment , c)))

(def UnicodeIDStart ID_Start)
(def UnicodeIDContinue ID_Continue)

(def HexDigit (sat (cut string-any <> "0123456789abcdefABDCEF")))
(def HexDigits (.let* (lst (many1 HexDigit)) (list->string lst)))
(def Hex4Digits (.let* (s (.string HexDigit HexDigit HexDigit HexDigit)) s))

(def CodePoint
  (.let* (hd HexDigits)
    (let (n (with-input-from-string (string-append "#x" hd) read))
      (if (<= n #x10FFFF) hd (fail)))))

(def UnicodeEscapeSequence
  (.let* ((values value type)
          (.or (.let* ((_ #\u) (ds Hex4Digits))
                 (values ds 'Hex4Digits))
               (.let* (ds (bracket "u{" CodePoint "}"))
                 (values ds 'CodePoint))))
    ['UnicodeEscapeSequence [type: type] value]))

(def IdentifierStart (.or UnicodeIDStart #\$ #\_ UnicodeEscapeSequence))
(def IdentifierPart
  (.or UnicodeIDContinue #\$ UnicodeEscapeSequence <ZWNJ> <ZWJ>))
(def IdentifierName 
 (.let* ((s IdentifierStart)
         (ps (many IdentifierPart)))
  `(IdentifierName ,(list->string (cons s ps)))))



(def RightBracePunctuator (.list (return 'RightBracePunctuator) #\}))

(def NonZeroDigit (sat (cut string-any <> "123456789")))
(def DecimalDigit (sat (cut string-any <> "0123456789")))
(def DecimalDigits (many1 DecimalDigit))
(def DecimalIntegerLiteral
  (.or (.list #\0)
       (.let* ((d NonZeroDigit)
              (ds (.or DecimalDigits (return []))))
         [d . ds])))

(def ExponentIndicator (.or #\e #\E))
(def SignedInteger
  (.let* ((sign (opt (.or #\+ #\-))) (ds DecimalDigits))
    ((if sign (cut cons sign <>) identity) ds)))
(def ExponentPart (.let* ((e ExponentIndicator) (n SignedInteger)) [e . n]))

(def DecimalLiteral
  (let P ()
    (def dec (.let* ((dot ".") (dec DecimalDigits) (exp (opt ExponentPart)))
               (append [#\.] dec (or exp []))))
    (def float (.let* ((int DecimalIntegerLiteral) (dec dec))
                 (append int dec)))
    (def int (.let* ((int DecimalIntegerLiteral) (exp (opt ExponentPart)))
               (append int (or exp []))))
    (.or float dec int)))

(def NumericLiteral (.let* (n (.or DecimalLiteral))
                     (return `(NumericLiteral , (list->string n)))))

(def Punctuator
  (.let* (p (.or #\{ #\( #\) #\[ #\] "..." #\. #\; #\,
                 "<<=" "<<" "<=" "<"
                 ">>>=" ">>>" ">>=" ">>" ">=" ">"
                 "===" "==" "=>" "="
                 "!==" "!="
                 "++" "+=" "+"
                 "--" "-=" "-"
                 "**=" "*=" "**" "*"
                 "%=" "%"
                 "&&" "&=" "&"
                 "||" "|=" "|"
                 "^=" "^"
                 "!==" "!=" "!"
                 #\~ #\? #\:))
    `(Punctuator ,p)))


(def DoubleStringCharacter 
  (.or (.begin (.not (.or #\" #\\ LineTerminator)) SourceCharacter)
       <LS>
       <PS>))
(def DoubleStringCharacters (many1 DoubleStringCharacter))
(def StringLiteral 
  (.let* (cs (bracket #\" DoubleStringCharacters #\"))
   `(StringLiteral ,(list->string cs))))



(def CommonToken
  (.or IdentifierName 
     ;; Numbers come first because ~.~ is a punctuator
       NumericLiteral
       StringLiteral
       Punctuator))


(def InputElementDiv
  (.or WhiteSpace LineTerminator Comment CommonToken RightBracePunctuator))

(def Token
  (.let* ((b (point))
          (t (.or InputElementDiv CustomToken 
               (.begin (.not (item)) (return #!eof))
               (lex-error "Invalid Token")))
          (e (point)))
    (if (eof-object? t) (fail)
        `(Token ,[begin: b end: e] ,t))))

(def (token? v) (and (pair? v) (eq? 'Token (car v)) (= 3 (length v))))
(def (token-prop t k) (and (token? t) (pgetq k (cadr t))))
(def (token-production t) (and (token? t) (caddr t)))
(def (tokenize str) (run (many1 Token) str))

(def (production? p) (or (symbol? p) (and (pair? p) (symbol? (car p)))))
(def (production-type p) (and (production? p) (if (pair? p) (car p) p)))
