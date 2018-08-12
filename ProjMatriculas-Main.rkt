#!/usr/bin/env racket
#lang racket
(require csv-reading);Pacote para manipualação de arquivos CSV...> raco pkg install csv-reading

(define (main args)
  (cond [(< (vector-length args) 2) 
         (display "Usage: <Arquivo contendo Oferta de disciplinas.csv> <Arquivo contendo Disciplinas Cursadas.csv>\n"
                (current-error-port))
         (exit 1)])


;PARAMETROS PARA LEITURA DO ARQUIVO CSV CONTENDO AS DISCIPLINAS OFERTADAS
(define make-csv-reader-ofertadas
  (make-csv-reader-maker
   '((newline-type 'lf)
     (separator-chars            #\;)
     (quote-char                 . #f)
     )))


 ;PARAMETROS PARA LEITURA DO ARQUIVO CSV CONTENDO AS DISCIPLINAS CURSADAS
(define make-csv-reader-cursadas
  (make-csv-reader-maker
   '((newline-type 'lf)
     (separator-chars            #\,)
     (quote-char                 . #f)
     )))


  
;ARQUIVO CSV CONTENDO AS DISCIPLINAS OFERTADAS 
(define next-row
  (make-csv-reader-ofertadas (open-input-file (vector-ref args 0))))

  
;ARQUIVO CSV CONTENDO AS DISCIPLINAS CURSADAS 
(define next-row-cur
  (make-csv-reader-cursadas (open-input-file (vector-ref args 1))))

 
;ARQUIVO CSV QUE IRÁ CONTER O ARQUIVO DE ENTRADA APÓS SER FILTRADO: O FILTRO ROMEVERÁ AS DISCIPLINAS JÁ E AS DISCIPLINAS FORA DO CAMPUS E PERÍODO DESEJADO
(define saida
  (open-output-file "Arquivo-filtrado.csv" ;(vector-ref args 1)
                    #:exists 'replace))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MENU ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;OBTENDO INFORMAÇÕES DO USUÁRIO
(printf "Selecione o CAMPUS: \n [1]-Santo André \n [2]-São Bernardo\n")
  (define campus (read-line))
       (display (cond [(= (string->number campus) 1) "Selecionado: SA \n"]
                      [else "Selecionado: SBC \n"]) )
         (newline)

(printf "Selecione o Período: \n [1]-Diurno \n [2]-Noturno\n")
  (define periodo (read-line))
        (display (cond [(= (string->number periodo) 1) "Selecionado: Diurno \n"]
                       [else "Selecionado: Noturno \n"]) )

(printf "Incluir disciplinas do BC&T?: \n [1]-Sim \n [2]-Não\n")
  (define bct (read-line))
        (display (cond [(= (string->number bct) 1) "Selecionado: Sim \n"]
                       [else "Selecionado: Não \n"]) )
          (newline)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FIM MENU ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
;UMA VEZ COM AS INFORMAÇÕES DO USUÁRIO, já podemos filtrar as Disciplinas possíveis.
(define proc
  (busca-na-lista '() campus periodo "bct"))

(define forma
  (formata-lista '()))

(define camp
   (pega-campus-selecionado campus))
  
(define per
   (pega-periodo-selecionado periodo))


(define (filtra-disciplinas-cursadas linha)
  (""))
  
;Apartir dos parametros informados pelo usuário, filtra apenas as linhas desejadas e inclui no arquivo de saída
(define (gera-possiveis-disciplinas linhas campus periodo)
  (let loop ([linha (linhas)])
    (cond [(equal? linha '()) "nada"]
          [(equal? (busca-na-lista linha campus periodo "bct") #t)  ;AND NÃO ESTÁ NO ARQUIVO DE DISCIPLINAS CURSADAS
           (displayln (string-join (map ~a (formata-lista linha)) " ") saida)(loop (next-row))]  ;ESCREVE NO ARQUIVO FILTRADO
          [else (loop (next-row))])))  

  

  (gera-possiveis-disciplinas next-row (pega-campus-selecionado campus)(pega-periodo-selecionado periodo))
  (close-output-port saida)


  ;ARQUIVO CSV CONTENDO AS DISCIPLINAS APÓS FILTRO INICIAL 
(define next-row-fil
  (make-csv-reader (open-input-file "Arquivo-filtrado.csv")))

  
(void))

;Utilizado para remover paranteses no Inicio e Fim da lista antes de incluir no arquivo CSV
(define (formata-string str)
  (cond [(equal? (string-ref str 0) #\() (string-replace str "(" "")] ;REMOVE PARENTESES NO INICIO
        [(equal? (string-ref str (- (string-length str) 1)) #\)) (string-replace str ")" "") ] ;REMOVE PARENTESES NO FIM
        [else str]))


;Utilizado para formatar a lista para manter o arquivo no formato CSV
(define (formata-lista lista)
     (let fl ([lista-nova '()] [lista-anterior lista])
         (cond [(null? lista-anterior )  (reverse lista-nova)]
          [else (fl (cons (string-append (car lista-anterior) ";") lista-nova) (cdr lista-anterior))
                ])))



;Utilizado para filtrar os parametros informados pelo usuário
(define (busca-na-lista lista campus periodo bct)
     (let fl ([sts_campus #f] [sts_periodo #f] [sts_bct #t] [lista-anterior lista]) ;inicia com todos os status #f
         (cond [(and (equal? sts_campus #t) (equal? sts_periodo #t) (equal? sts_bct #t)) #t]       ;Quando todos os status forem #t, encerra a validação
               [(null? lista-anterior )  #f]                                        ;Se a lista terminar, antes de termos todos os status #t, encerra a validação
               [(and ( equal? (car lista-anterior) campus) (equal? sts_campus #f)) (fl #t sts_periodo sts_bct (cdr lista-anterior))]
               [(and ( equal? (car lista-anterior) periodo) (equal? sts_periodo #f)) (fl sts_campus #t sts_bct (cdr lista-anterior))]
               ;[(and ( = (car lista-anterior) bct) (= sts_bct #f)) (fl sts_campus sts_periodo #t (cdr lista-anterior))] ;Valida cada campo separadamente
               [else (fl sts_campus sts_periodo sts_bct (cdr lista-anterior))
                ])))


(define (pega-campus-selecionado campus)
  (cond [(= (string->number campus) 1) "Santo Andre"]
                       [else "Sao Bernardo do Campo"]))

(define (pega-periodo-selecionado periodo)
  (cond [(= (string->number periodo) 1) "Diurno"]
                       [else "Noturno"]))


;(main (current-command-line-arguments))


(define v (vector "Oferta Disciplicas (BCT-BCC).csv" "Disciplinas Cursadas.csv"))
(main v)
