;;; -*- Gerbil -*-
;;; (C) me at drewc.ca
(import :drewc/smug :drewc/js-syntax/lexical) 
(export #t)

;; [[file:~/src/js-syntax/expressions.org][]]
(defstruct expression ())
;; ends here
;; [[file:~/src/js-syntax/expressions.org::#identifiers][]]
(defstruct (identifiers expression) ())
;; ends here
;; [[file:~/src/js-syntax/expressions.org::*Smooth%20Operators][]]
(def (parse-operator side op)
  (.or (.let* ((lhs side) (rhs (.begin (tpv? op) side)))
         (return (values lhs op rhs)))
       (.let* (s side) (return (values s #f #f)))))
;; ends here
;; [[file:~/src/js-syntax/expressions.org::*Smooth%20Operators][]]
(defstruct (operator expression) (lhs op rhs) transparent: #t)
(defrules Operator ()
  ((_ maker side op)
   (.begin
     #t
     (.let* ((values lhs _ rhs) (parse-operator side op))
       (return (if rhs (maker lhs op rhs) lhs))))))
;; ends here
;; [[file:~/src/js-syntax/expressions.org::*No%20%5B%5Bfile:lexical.org::#LineTerminator%5D%5BLineTerminator%5D%5D%20here?%20~no-token-here~!][]]
(def (no-token-here type)
  (def T-ITEM (.begin (.not (token-production-type? type)) ITEM))
  (def (find-token t)
    (.let* (t? T-ITEM)
      (if (eq? t? t) (return #t) (find-token t))))
  (def (.find-token t)
    (.let* (ts (next-source-tokens))
      (sat identity (return (run (find-token t) ts)))))
  (peek (.let* (t (peek T-ITEM)) (.find-token t)))) 
;; ends here
;; [[file:~/src/js-syntax/expressions.org::#BindingIdentifier][]]
(defstruct (identifier expression) (name) transparent: #t) 
(def Identifier
  (.begin (peek (token-production-type? 'IdentifierName))
          (peek (token-production-value? (.not ReservedWord)))
          (.let* (t (item))
            (return (identifier (token-production-value t))))))
;; ends here
;; [[file:~/src/js-syntax/expressions.org::#syntax-identifiers][]]
(defstruct (identifier-reference identifiers) (identifier) transparent: #t) 
(def IdentifierReference
  (.let* (id Identifier) (return (identifier-reference id))))
;; ends here
;; [[file:~/src/js-syntax/expressions.org::#BindingIdentifier][]]
(defstruct (binding-identifier identifiers) (identifier) transparent: #t) 
(def BindingIdentifier
  (.let* (id Identifier) (return (binding-identifier id))))
;; ends here
;; [[file:~/src/js-syntax/expressions.org::#BindingIdentifier][]]
(defstruct (label-identifier identifiers) (identifier) transparent: #t) 
(def LabelIdentifier
  (.let* (id Identifier) (return (label-identifier id))))
;; ends here
;; [[file:~/src/js-syntax/expressions.org::#Literal][]]
(defstruct (literal expression) (value) transparent: #t)
(defstruct (null-literal literal) () transparent: #t)
(defstruct (boolean-literal literal) () transparent: #t)
(defstruct (numeric-literal literal) () transparent: #t)
(defstruct (string-literal literal) () transparent: #t)

(def Literal
  (.or (.let* (l NullLiteral) (return (null-literal
                                        (tpv l))))

       (.let* (l BooleanLiteral) (return (boolean-literal (tpv l))))
       (.let* (t (.begin (peek (tpt? 'StringLiteral)) (item)))
         (return (string-literal (tpv t))))
       (.let* (t (.begin (peek (tpt? 'NumericLiteral)) (item)))
         (return (numeric-literal (tpv t))))))


;; ends here
;; [[file:~/src/js-syntax/expressions.org::*PrimaryExpression][]]
(def PrimaryExpression 
  (.begin #t (.or IdentifierReference Literal)))
;; ends here
;; [[file:~/src/js-syntax/expressions.org::*MemberExpression][]]
(defstruct (member-expression expression) (group expression identifier template)
  transparent: #t)

(def (parse-member-expression (make-it-primary? #f))
 (.let* ((g (if make-it-primary?
              PrimaryExpression
              (parse-member-expression #t)))
               (e (.or (bracket (tpv? #\[) Expression (tpv? #\])) #f))
               (i (if e #f (.or (.begin (tpv? #\.) (parse-member-expression #t)) #f)))
               (t (if (or e i) #f #f #;TemplateLiterate)))
         (if (not (or e i t)) g
             (member-expression g e i t))))
(def MemberExpression (parse-member-expression))

;; ends here
;; [[file:~/src/js-syntax/expressions.org::*CallExpression][]]
(def CallExpression FAIL)
;; ends here
;; [[file:~/src/js-syntax/expressions.org::#NewExpression][]]
(defstruct (new-expression expression) (expression arguments) transparent: #t)
(def NewExpression
  (.begin 
    #t
    (.or (.let* ((exp (.begin (tpv? "new") MemberExpression)))
           (new-expression exp #f))
         MemberExpression)))
;; ends here
;; [[file:~/src/js-syntax/expressions.org::*Arguments][]]
(defstruct (arguments expression) (list) transparent: #t) 
(def Arguments
  (.let* ((_ #t) (lst (bracket (tpv? #\()
                               (.or ArgumentList (return []))
                               (.begin (.or (tpv? #\,) #f) (tpv? #\))))))
          (return (arguments lst))))

;; ends here
;; [[file:~/src/js-syntax/expressions.org::*ArgumentList][]]
(def ArgumentList (.begin #t (sepby1 (.or SpreadElement AssignmentExpression)
                                     (tpv? #\,))))
;; ends here
;; [[file:~/src/js-syntax/expressions.org::*SpreadElement][]]
(defstruct (spread-element expression) (expression) transparent: #t)
(def SpreadElement (.let* ((_ (tpv? "..."))
                           (e AssignmentExpression))
                     (return (spread-element e))))



;; ends here
;; [[file:~/src/js-syntax/expressions.org::#LeftHandSideExpression][]]
(def LeftHandSideExpression (.begin #t (.or NewExpression CallExpression)))
;; ends here
;; [[file:~/src/js-syntax/expressions.org::*UpdateExpression][]]
(defstruct (update-expression expression) (prefix expression postfix) transparent: #t)
(def UpdateExpression
  (let (ops (.or (tpv? "++") (tpv? "--") #f))
    (.begin
      #t (.or (.let* ((pre ops) (exp LeftHandSideExpression)
                      (post (.or (.begin (no-token-here 'LineTerminator) ops)
                                 #f)))
                (if (not (or pre post)) FAIL
                    (return (update-expression pre exp post))))
              LeftHandSideExpression))))
;; ends here
;; [[file:~/src/js-syntax/expressions.org::*UnaryExpression][]]
(defstruct (unary-expression expression) (op expression) transparent: #t)
(def UnaryExpression
  (.begin
    #t
    (.or (.let* ((op (tpv? (.or "delete" "void" "typeof" "+" "-" "~" "!")))
                 (exp UpdateExpression))
           (return (unary-expression op exp)))
         UpdateExpression)))
;; ends here
;; [[file:~/src/js-syntax/expressions.org::*ExponentiationExpression][]]
(defstruct (exponentiation-expression operator) () transparent: #t)
(def ExponentiationExpression
  (Operator exponentiation-expression UnaryExpression "**"))

;; ends here
;; [[file:~/src/js-syntax/expressions.org::*MultiplicativeExpression][]]
(defstruct (multiplicative-expression operator) () transparent: #t)
(def MultiplicativeExpression
  (Operator multiplicative-expression ExponentiationExpression MultiplicativeOperator))

;; ends here
;; [[file:~/src/js-syntax/expressions.org::*MultiplicativeOperator][]]
(def MultiplicativeOperator (.or #\* #\/ #\%))
;; ends here
;; [[file:~/src/js-syntax/expressions.org::*AdditiveExpression][]]
(defstruct (additive-expression operator) () transparent: #t)
(def AdditiveExpression
  (.or (Operator additive-expression MultiplicativeExpression "+")
       (Operator additive-expression MultiplicativeExpression "-")))

;; ends here
;; [[file:~/src/js-syntax/expressions.org::*ShiftExpression][]]
(defstruct (shift-expression operator) () transparent: #t)
(def ShiftExpression
  (.or (Operator shift-expression AdditiveExpression "<<")
       (Operator shift-expression AdditiveExpression ">>")
       (Operator shift-expression AdditiveExpression ">>>")))

;; ends here
;; [[file:~/src/js-syntax/expressions.org::*RelationalExpression][]]
(defstruct (relational-expression operator) () transparent: #t)
(def RelationalExpression
  (.or (Operator relational-expression ShiftExpression "<")
       (Operator relational-expression ShiftExpression ">")
       (Operator relational-expression ShiftExpression "<=")
       (Operator relational-expression ShiftExpression ">=")
       (Operator relational-expression ShiftExpression "instanceof")
       (Operator relational-expression ShiftExpression "in")))

;; ends here
;; [[file:~/src/js-syntax/expressions.org::*EqualityExpression][]]
(defstruct (equality-expression operator) () transparent: #t)
(def EqualityExpression
  (.or (Operator equality-expression RelationalExpression "==")
       (Operator equality-expression RelationalExpression "!=")
       (Operator equality-expression RelationalExpression "===")
       (Operator equality-expression RelationalExpression "!==")))

;; ends here
;; [[file:~/src/js-syntax/expressions.org::*BitwiseANDExpression][]]
(defstruct (bitwise-and-expression operator) () transparent: #t)
(def BitwiseANDExpression
  (Operator bitwise-and-expression EqualityExpression #\&))
;; ends here
;; [[file:~/src/js-syntax/expressions.org::*BitwiseXORExpression][]]
(defstruct (bitwise-xor-expression operator) () transparent: #t)
(def BitwiseXORExpression
  (Operator bitwise-xor-expression BitwiseANDExpression #\^))
;; ends here
;; [[file:~/src/js-syntax/expressions.org::*BitwiseORExpression][]]
(defstruct (bitwise-or-expression operator) () transparent: #t)
(def BitwiseORExpression
  (Operator bitwise-or-expression BitwiseXORExpression #\|))

;; ends here
;; [[file:~/src/js-syntax/expressions.org::*LogicalANDExpression][]]
(defstruct (logical-and-expression operator) () transparent: #t)
(def LogicalANDExpression
  (Operator logical-and-expression BitwiseORExpression "&&"))
;; ends here
;; [[file:~/src/js-syntax/expressions.org::*LogicalORExpression][]]
(defstruct (logical-or-expression operator) () transparent: #t)
(def LogicalORExpression
  (Operator logical-or-expression LogicalANDExpression "||"))
;; ends here
;; [[file:~/src/js-syntax/expressions.org::*ConditionalExpression][]]
(def ConditionalExpression (.begin #t LogicalORExpression))
;; ends here
;; [[file:~/src/js-syntax/expressions.org::*AssignmentExpression][]]
;;; This is a very important parser, as, believe it or not, an
;;; AssigmentExpression is also what ends up leading to being able to parse a
;;; literal.

;;; ie: Assignment -> Conditional -> LogicalOr -> LogicalAnd ->
;;;  Bitwise(Or -> Xor -> And) -> Equality -> Relational -> Shift -> Add -> Mult
;;;  -> Exponential -> Unary -> Update -> LHS -> New -> Member -> Primary ->
;;;  Literal -> Numeric

;;; Holy fscknts.
(defstruct (assignment-expression operator) () transparent: #t)
(def AssignmentExpression
  (.begin
    #t ConditionalExpression))
;; ends here
;; [[file:~/src/js-syntax/expressions.org::*Expression][]]
(defstruct (comma-operator expression) (expressions) transparent: #t)
(def Expression
  (.begin #t (.let* (exps (sepby1 AssignmentExpression (tpv? #\,)))
               (return (if (null? (cdr exps)) (car exps)
                           (comma-operator exps))))))

;; ends here
