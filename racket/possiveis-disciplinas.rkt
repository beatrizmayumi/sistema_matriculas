#lang racket
(require csv-reading)
;(provide possiveis-disciplinas)


(define make-csv-reader
  (make-csv-reader-maker
   '((newline-type 'lf)
     (separator-chars            #\;)
     (quote-char                 . #f)
     )))

(define next-row
  (make-csv-reader (open-input-file "Oferta Disciplicas (BCT-BCC).csv")))

(define saida
  (open-output-file "Arquivo-filtrado.csv"
                    #:exists 'replace))


(define (formata-lista lista)
     (let fl ([lista-nova '()] [lista-anterior lista])
         (cond [(null? lista-anterior )  (reverse lista-nova)]
          [else (fl (cons (string-append (car lista-anterior) ";") lista-nova) (cdr lista-anterior))
                ])))

;(formata-lista '("1" "2" "3"))


;(busca-na-lista '("1" "2" "3") 2)

(define (gera-possiveis-disciplinas linhas)
  (let loop ([linha (linhas)])
    (cond [(equal? linha '()) "Iniciar novo Csv-Reader apontando para Arquivo-filtrado.csv"]
          [(equal? (car(cdddr linha)) "Diurno") (displayln (formata-lista linha) saida)(loop (next-row))]
          [else (loop (next-row))])))                                    ;Verifica a proxima linha



(gera-possiveis-disciplinas next-row)
(close-output-port saida)






