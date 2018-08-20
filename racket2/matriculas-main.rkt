#!/usr/bin/env racket
#lang racket
(require csv-reading); pacote para manipulacao de arquivos CSV...> raco pkg install csv-reading
(require "menu.rkt")

(define (main args)
  (cond [(< (vector-length args) 4) 
         (display "Informar: <Arquivo contendo Oferta de disciplinas.csv> <Arquivo contendo Disciplinas ja Cursadas.csv>\n"
                  (current-error-port))
         (exit 1)])


  ; PARAMETROS PARA LEITURA DO ARQUIVO CSV CONTENDO AS DISCIPLINAS OFERTADAS
  (define make-csv-reader-ofertadas
    (make-csv-reader-maker
     '((newline-type 'lf)
       (separator-chars            #\;)
       (quote-char                 . #f)
       )))


  ; PARAMETROS PARA LEITURA DO ARQUIVO CSV CONTENDO AS DISCIPLINAS CURSADAS
  (define make-csv-reader-cursadas
    (make-csv-reader-maker
     '((newline-type 'lf)
       (separator-chars            #\,)
       (quote-char                 . #f)
       )))

  ; PARAMETROS PARA LEITURA DO ARQUIVO CSV CONTENDO AS DISCIPLINAS DA GRADE ATUAL DO BCC
  (define make-csv-reader-grade
    (make-csv-reader-maker
     '((newline-type 'lf)
       (separator-chars            #\,)
       (quote-char                 . #f)
       )))

    ; PARAMETROS PARA LEITURA DO ARQUIVO CSV CONTENDO AS DISCIPLINAS DA GRADE ATUAL DO BCT
  (define make-csv-reader-grade2
    (make-csv-reader-maker
     '((newline-type 'lf)
       (separator-chars            #\,)
       (quote-char                 . #f)
       )))

    ; PARAMETROS PARA LEITURA DO ARQUIVO CSV CONTENDO AS DISCIPLINAS FILTRADAS
  (define make-csv-reader-filtro
    (make-csv-reader-maker
     '((newline-type 'lf)
       (separator-chars            #\,)
       (quote-char                 . #f)
       )))

  
  ; ARQUIVO CSV CONTENDO AS DISCIPLINAS OFERTADAS 
  (define next-row
    (make-csv-reader-ofertadas (open-input-file (vector-ref args 0))))

  
  ; ARQUIVO CSV CONTENDO AS DISCIPLINAS CURSADAS 
  (define next-row-cur
    (make-csv-reader-cursadas (open-input-file (vector-ref args 1))))


  ; ARQUIVO CSV CONTENDO AS DISCIPLINAS DA GRADE DO BCC
  (define next-row-grade
    (make-csv-reader-grade (open-input-file (vector-ref args 2))))

    ; ARQUIVO CSV CONTENDO AS DISCIPLINAS DA GRADE DO BCT
  (define next-row-grade2
    (make-csv-reader-grade2 (open-input-file (vector-ref args 3))))

    ; ARQUIVO CSV CONTENDO AS DISCIPLINAS FILTRADAS 
  (define next-row-filtro
    (make-csv-reader-filtro (open-input-file (vector-ref args 1))))

 
  ; ARQUIVO CSV QUE CONTERA O ARQUIVO DE ENTRADA APOS SER FILTRADO: O FILTRO REMOVERA AS DISCIPLINAS JA CURSADAS E AS DISCIPLINAS FORA DO CAMPUS E PERIODO DESEJADO
  (define saida
    (open-output-file "Arquivo-filtrado.csv" ;(vector-ref args 1)
                      #:exists 'replace))

  (define saida2
    (open-output-file "Arquivo-filtradoBCC.csv" ;(vector-ref args 2)
                      #:exists 'replace))

    (define saida3
    (open-output-file "Arquivo-filtradoBCT.csv" ;(vector-ref args 3)
                      #:exists 'replace))
                      
  
  ; UMA VEZ COM AS INFORMACOES DO USUARIO, JA PODEMOS FILTRAR AS DISCIPLINAS POSSIVEIS DE SEERM CURSADAS
  (define proc
    (busca-na-lista '() campus periodo))

  (define forma
    (formata-lista '()))

  (define camp
    (pega-campus-selecionado campus))
  
  (define per
    (pega-periodo-selecionado periodo))

  (define (filtra-disciplinas-cursadas linha)
    (""))

  ; RECEBE UMA A UMA AS DISCIPLINAS OFERTADAS E COMPARA COM AS DISCIPLINAS JA CURSADAS (CONTIDAS NO ARQUIVO CSV)
  (define (cursada? linha)
    (define port-in-cursadas (open-input-file (vector-ref args 1)))
    (define next-row-cursadas
      (make-csv-reader-cursadas port-in-cursadas))
    
    (let loop ([linha-cursada (next-row-cursadas)] )
      (cond [(equal? linha-cursada '()) (close-input-port port-in-cursadas) #f]
            [(equal? (seventh linha-cursada) (second linha))#t ]
            [else (loop (next-row-cursadas))])))

   (define(busca-no-filtro? linha)
    (define port-in-filtro (open-input-file (vector-ref args 1)))
    (define next-row-filtro
      (make-csv-reader-filtro port-in-filtro))
     
    (let loop ([linha-filtro (next-row-filtro)] )
      (cond [(equal? linha-filtro '()) (close-input-port port-in-filtro) #f]
            [(equal? (seventh linha-filtro) (second linha))#t ]
            [else (loop (next-row-filtro))])))
  
  
  ; A partir dos parametros informados pelo usuario, filtra apenas as linhas desejadas e inclui no arquivo de saida
  (define (gera-possiveis-disciplinas linhas campus periodo)
    (let loop ([linha (linhas)])
      (cond [(equal? linha '()) (print "GERADO 'Arquivo-filtrado.csv' CONTENDO APENAS DISCIPLINAS EM POTENCIAL")]
            [(and (equal? (busca-na-lista linha campus periodo) #t) (not (cursada? linha)))  
             (displayln (string-join (map ~a (formata-lista linha)) " ") saida)(loop (next-row))]  ;ESCREVE NO ARQUIVO FILTRADO
            [else (loop (next-row))])))
  
  (newline)
  
  ; RECEBE UMA A UMA AS DISCIPLINAS JA CURSADAS E COMPARA COM AS DA GRADE DE DISCIPLINAS
    (define (cursada-grade? linha)
    (define port-in-grade (open-input-file (vector-ref args 2)))
    (define next-row-grade
      (make-csv-reader-grade port-in-grade))

    (let loop ([linha-cursada-grade (next-row-grade)] )
      (cond [(equal? linha-cursada-grade '()) (close-input-port port-in-grade) #f]
            [(equal? (seventh linha-cursada-grade) (second linha))#t ]
            [else (loop (next-row-grade))])))

    ; RECEBE UMA A UMA AS DISCIPLINAS JA CURSADAS E COMPARA COM AS DA GRADE DE DISCIPLINAS
    (define (cursada-grade2? linha)
    (define port-in-grade2 (open-input-file (vector-ref args 2)))
    (define next-row-grade2
      (make-csv-reader-grade2 port-in-grade2))

    (let loop ([linha-cursada-grade2 (next-row-grade2)] )
      (cond [(equal? linha-cursada-grade2 '()) (close-input-port port-in-grade2) #f]
            [(equal? (seventh linha-cursada-grade2) (second linha))#t ]
            [else (loop (next-row-grade2))])))

  ; A partir dos parametros informados pelo usuario, filtra apenas as linhas desejadas e inclui no arquivo de saida
    (define (gera-disciplinas-possiveis-bcc linhas)
    (let loop ([linha (linhas)])
      (cond [(equal? linha '()) (print "GERADO 'Arquivo-filtradoBCC.csv' CONTENDO APENAS DISCIPLINAS DO BCC EM POTENCIAL")]
            [(busca-no-filtro? linha)
             (displayln (string-join (map ~a (formata-lista linha)) " ") saida2)(loop (next-row-grade))] 
            [else (loop (next-row-grade))])))

    ; A partir dos parametros informados pelo usuario, filtra apenas as linhas desejadas e inclui no arquivo de saida
    (define (gera-disciplinas-possiveis-bct linhas)
    (let loop ([linha (linhas)])
      (cond [(equal? linha '()) (print "GERADO 'Arquivo-filtradoBCT.csv' CONTENDO APENAS DISCIPLINAS DO BCT EM POTENCIAL")]
            [(busca-no-filtro? linha)
             (displayln (string-join (map ~a (formata-lista linha)) " ") saida3)(loop (next-row-grade2))] 
            [else (loop (next-row-grade2))])))

  

  (gera-possiveis-disciplinas next-row (pega-campus-selecionado campus)(pega-periodo-selecionado periodo))
  (gera-disciplinas-possiveis-bcc next-row-grade)
  (gera-disciplinas-possiveis-bct next-row-grade2)
  (close-output-port saida)
  (close-output-port saida2)
  (close-output-port saida3)


  ; ARQUIVO CSV CONTENDO AS DISCIPLINAS APOS FILTRO INICIAL 
  (define next-row-fil
    (make-csv-reader (open-input-file "Arquivo-filtrado.csv")))

  (define next-row-filtrabcc
    (make-csv-reader (open-input-file "Arquivo-filtradoBCC.csv")))
  
  (define next-row-filtrabct
    (make-csv-reader (open-input-file "Arquivo-filtradoBCT.csv")))
  
  (void))

; Utilizado para remover parenteses no Inicio e Fim da lista antes de incluir no arquivo CSV
(define (formata-string str)
  (cond [(equal? (string-ref str 0) #\() (string-replace str "(" "")] ;REMOVE PARENTESES NO INICIO
        [(equal? (string-ref str (- (string-length str) 1)) #\)) (string-replace str ")" "") ] ;REMOVE PARENTESES NO FIM
        [else str]))


; Utilizado para formatar a lista para manter o arquivo no formato CSV
(define (formata-lista lista)
  (let fl ([lista-nova '()] [lista-anterior lista])
    (cond [(null? lista-anterior )  (reverse lista-nova)]
          [else (fl (cons (string-append (car lista-anterior) ";") lista-nova) (cdr lista-anterior))
                ])))


; Utilizado para filtrar os parametros informados pelo usuario, percorre cada campo da LISTA e valida a compatibilidade com os parametros informados pelo usuario
(define (busca-na-lista lista campus periodo)
  (let fl ([sts_campus #f] [sts_periodo #f] [sts_curso #t] [lista-anterior lista])              ; inicia com todos os status #f
    (cond [(and (equal? sts_campus #t) (equal? sts_periodo #t) (equal? sts_curso #t)) #t]       ; quando todos os status forem #t, encerra a validação
          [(null? lista-anterior )  #f]                                                         ; se a lista chegar ao fim antes de termos todos os status #t, encerra a validação
          [(and ( equal? (car lista-anterior) campus) (equal? sts_campus #f)) (fl #t sts_periodo sts_curso (cdr lista-anterior))]
          [(and ( equal? (car lista-anterior) periodo) (equal? sts_periodo #f)) (fl sts_campus #t sts_curso (cdr lista-anterior))]
          [(and ( or (equal? (car lista-anterior) "Bacharelado em Ciencia da Computacao")(equal? (car lista-anterior) "Bacharelado em Ciencia e Tecnologia"))
                (equal? sts_curso #f)) (fl sts_campus sts_periodo #t (cdr lista-anterior))]     ; valida se a disciplina pertence ao BCC ou ao BC&T
          [else (fl sts_campus sts_periodo sts_curso (cdr lista-anterior))
                ])))


(define (pega-campus-selecionado campus)
  (cond [(= (string->number campus) 1) "Santo Andre"]
        [else "Sao Bernardo do Campo"]))

(define (pega-periodo-selecionado periodo)
  (cond [(= (string->number periodo) 1) "Diurno"]
        [else "Noturno"]))


;(main (current-command-line-arguments))

(define v (vector "Oferta Disciplinas (BCT-BCC).csv" "Disciplinas Cursadas.csv" "Base_obrigatorias_BCC.csv" "Base_obrigatorias_BCT.csv"))
(main v)
