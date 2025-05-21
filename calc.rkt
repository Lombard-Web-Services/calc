#lang racket/gui
;; par Thibaut LOMBARD (Lombard Web)
(require racket/math
         (only-in racket/string string-replace))

;; === Fenêtre principale ===
(define frame (new frame%
                   [label "Calculatrice"]
                   [width 320]
                   [height 400]))

;; === Affichage ===
(define display
  (new text-field%
       [parent frame] ;; Le parent est de nouveau la frame directement
       [label ""]
       [style '(single)]
       [min-width 300]
       [min-height 50]))

;; Les lignes suivantes pour set-style sont commentées car text-field% ne les supporte pas.
;; (send display set-style
;;       (make-object style%
;;                    [foreground "lime"]
;;                    [background "black"]
;;                    [font (make-object font% "Courier New" 18 'normal)]))

;; Effacer affichage
(define (clear-display)
  (send display set-value ""))

;; Ajouter texte à l'affichage
(define (append-display txt)
  (send display set-value (string-append (send display get-value) txt)))

;; === Conversion infix -> préfix (très simple et limité) ===
;; Gère uniquement les opérations binaires simples (ex: "1+1")
;; Ne gère PAS la précédence des opérateurs (ex: "1+2*3" sera évalué de gauche à droite)
;; Ne gère PAS les fonctions scientifiques dans ce format (ex: "sin(90)")
(define (infix->prefix expr)
  (define pattern "([0-9.]+)\\s*([-+*/])\\s*([0-9.]+)")
  (define match-result (regexp-match pattern expr))

  (if match-result
      (let* ([full-match (car match-result)]
             [operand1 (cadr match-result)]
             [operator (caddr match-result)]
             [operand2 (cadddr match-result)])
        (string-replace expr full-match (format "(~a ~a ~a)" operator operand1 operand2)))
      expr))

;; === Évaluation de l’expression ===
(define (eval-expression expr)
  ;; Ajout de messages de débogage pour identifier l'origine de l'erreur
  (displayln (string-append "DEBUG: Expression originale : " expr))
  (with-handlers ([exn:fail? (λ (e)
                               (displayln (string-append "DEBUG: Erreur capturée : " (exn-message e)))
                               (string-append "Erreur : " (exn-message e)))])
    (define pre (infix->prefix expr))
    (displayln (string-append "DEBUG: Expression transformée (préfixe) : " pre))
    (define s-expr (read (open-input-string pre)))
    (displayln (string-append "DEBUG: S-expression lue : " (format "~s" s-expr))) ; Utilise ~s pour une représentation lisible
    ;; Utilise make-base-namespace pour que eval reconnaisse les opérateurs standard
    (define result (eval s-expr (make-base-namespace)))
    (number->string result)))

;; === Fonctions spéciales calculatrice sci ===
;; Mappe les labels des boutons aux chaînes de fonctions Racket correspondantes
;; Ces chaînes seront insérées en notation préfixe dans l'affichage.
(define function-map
  (hash
   "√" "sqrt"
   "exp" "exp"
   "sin" "sin"
   "cos" "cos"
   "tan" "tan"
   "ln" "log"    ; Logarithme naturel (base e)
   "log" "log10" ; Logarithme base 10
   "lnv" "log"   ; Supposé être le logarithme naturel (comme 'ln')
   "x²" "sqr"   ; Utilisation de 'sqr' qui est une fonction Racket pour le carré
   "%" "(lambda (x) (/ x 100))" ; Lambda pour le pourcentage
   "1/x" "(lambda (x) (/ 1 x))")) ; Lambda pour l'inverse

;; === Boutons organisés par lignes (pour un look TI-82) ===
(define button-rows
  '(("7" "8" "9" "/")
    ("4" "5" "6" "*")
    ("1" "2" "3" "-")
    ("0" "." "=" "+")
    ;; Fonctions scientifiques groupées comme sur une TI-82
    ("sin" "cos" "tan" "ln")
    ("log" "lnv" "x²" "%")
    ("1/x" "√" "exp" "(")
    (")" "C" "" ""))) ;; Parenthesis, Clear, et des vides pour l'alignement

;; === Grille des boutons ===
(define main-panel (new vertical-panel% [parent frame]))
;; (set-panel-background main-panel "black") ;; Cette ligne a été supprimée

(for-each
  (λ (row)
    (define row-panel (new horizontal-panel% [parent main-panel]))
    (for-each
      (λ (label)
        (when (not (string=? label ""))
          (new button%
               [parent row-panel]
               [label label]
               [callback
                (cond
                  [(equal? label "=")
                   (λ (_ event)
                     (define expr (send display get-value))
                     (define res (eval-expression expr))
                     (clear-display)
                     (append-display res))]
                  [(equal? label "C")
                   (λ (_ event)
                     (clear-display))]
                  ;; Gère les fonctions scientifiques
                  [(hash-has-key? function-map label)
                   (λ (_ event)
                     ;; Insère la fonction en notation préfixe, l'utilisateur doit compléter
                     (append-display (string-append "(" (hash-ref function-map label) " ")))]
                  [else
                   (λ (_ event)
                     (append-display label))])])))
    row))
  button-rows)

;; === Affichage ===
(send frame show #t)
