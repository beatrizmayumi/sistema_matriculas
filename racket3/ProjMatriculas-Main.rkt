#!/usr/bin/env racket
#lang racket
(require csv-reading);Pacote para manipualação de arquivos CSV...> raco pkg install csv-reading
(require gregor/time);Pacote para manipualação de tempo...> raco pkg install gregor-lib

(define (main args)
  (cond [(< (vector-length args) 3) 
         (display "Informar: <Arquivo contendo Oferta de disciplinas.csv> <Arquivo contendo Disciplinas já Cursadas.csv> <Arquivo contendo Grade de Disciplinas.csv>\n"
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

  ;PARAMETROS PARA LEITURA DO ARQUIVO CSV CONTENDO AS DISCIPLINAS DA GRADE ATUAL DO BCC
  (define make-csv-reader-grade
    (make-csv-reader-maker
     '((newline-type 'lf)
       (separator-chars            #\,)
       (quote-char                 . #f)
       )))


  
  ;ARQUIVO CSV CONTENDO AS DISCIPLINAS OFERTADAS 
  (define next-row
    (make-csv-reader-ofertadas (open-input-file (vector-ref args 0))))

  
  ;ARQUIVO CSV CONTENDO AS DISCIPLINAS CURSADAS 
  ;(define next-row-cur
  ;  (make-csv-reader-cursadas (open-input-file (vector-ref args 1))))


  ;ARQUIVO CSV CONTENDO AS DISCIPLINAS DA GRADE 
  ;(define next-row-grade
  ;  (make-csv-reader-grade (open-input-file (vector-ref args 2))))

 
  ;ARQUIVO CSV QUE IRÁ CONTER O ARQUIVO DE ENTRADA APÓS SER FILTRADO: O FILTRO ROMEVERÁ AS DISCIPLINAS JÁ E AS DISCIPLINAS FORA DO CAMPUS E PERÍODO DESEJADO
  (define saida
    (open-output-file "Arquivo-filtrado.csv" ;(vector-ref args 1)
                      #:exists 'replace))

   ;ARQUIVO CSV QUE IRÁ CONTER A GRADE FINALIZADA DE ACORDO COM A PREFERENCIA DO ALUNO
  (define saida
    (open-output-file "Grade-gerada.csv" ;(vector-ref args 1)
                      #:exists 'replace))


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MENU ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;OBTENDO INFORMAÇÕES DO USUÁRIO
  (printf "Selecione o CAMPUS: \n [1]-Santo André \n [2]-São Bernardo\n")
  (define campus (read-line))
  (display (cond [(= (string->number campus) 1) "Selecionado: SA \n"]
                 [else "Selecionado: SBC \n"]) )
  (newline)
  (newline)
  (printf "Selecione o Período: \n [1]-Diurno \n [2]-Noturno\n")
  (define periodo (read-line))
  (display (cond [(= (string->number periodo) 1) "Selecionado: Diurno \n"]
                 [else "Selecionado: Noturno \n"]) )
  (newline)
  (newline)
  (printf "Selecione a Prioridade: \n [1]-Disciplinas Menos Avançadas \n [2]-Número de Créditos\n")
  (define prioridade (read-line))
  (display (cond [(= (string->number periodo) 1) "Selecionado: Disciplinas Menos Avançadas \n"]
                 [else "Selecionado: Número de Créditos \n"]) )
  (newline)
  (newline)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FIM MENU ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;UMA VEZ COM AS INFORMAÇÕES DO USUÁRIO, já podemos filtrar as Disciplinas possíveis.
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

  ;RECEBE UMA A UMA AS DISCIPLINAS OFERTADAS E COMPARA COM AS DISCIPLINAS JÁ CURSADAS (CONTIDAS NO ARQUIVO CSV)
  (define (cursada? linha)
    (define port-in-cursadas (open-input-file (vector-ref args 1)))
    (define next-row-cursadas
      (make-csv-reader-cursadas port-in-cursadas))

    (let loop ([linha-cursada (next-row-cursadas)] )
      (cond [(equal? linha-cursada '()) (close-input-port port-in-cursadas) #f]
            [(equal? (seventh linha-cursada) (second linha))#t ]
            [else (loop (next-row-cursadas))])))

  
  ;Apartir dos parametros informados pelo usuário, filtra apenas as linhas desejadas e inclui no arquivo de saída
  (define (gera-possiveis-disciplinas linhas campus periodo)
    (let loop ([linha (linhas)])
      (cond [(equal? linha '()) (print "GERADO 'Arquivo-filtrado.csv' CONTENDO APENAS DISCIPLINAS EM POTENCIAL")]
            [(and (equal? (busca-na-lista linha campus periodo) #t) (not (cursada? linha)))  
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



;Utilizado para filtrar os parametros informados pelo usuário, percorre cada campo da LISTA e valida a compatibilidade com os parâmetros informados pelo usuário
(define (busca-na-lista lista campus periodo)
  (let fl ([sts_campus #f] [sts_periodo #f] [sts_curso #t] [lista-anterior lista])              ;inicia com todos os status #f
    (cond [(and (equal? sts_campus #t) (equal? sts_periodo #t) (equal? sts_curso #t)) #t]       ;Quando todos os status forem #t, encerra a validação
          [(null? lista-anterior )  #f]                                                         ;Se a lista chegar ao fim antes de termos todos os status #t, encerra a validação
          [(and ( equal? (car lista-anterior) campus) (equal? sts_campus #f)) (fl #t sts_periodo sts_curso (cdr lista-anterior))]
          [(and ( equal? (car lista-anterior) periodo) (equal? sts_periodo #f)) (fl sts_campus #t sts_curso (cdr lista-anterior))]
          [(and ( or (equal? (car lista-anterior) "Bacharelado em Ciencia da Computacao")(equal? (car lista-anterior) "Bacharelado em Ciencia e Tecnologia"))
                (equal? sts_curso #f)) (fl sts_campus sts_periodo #t (cdr lista-anterior))]     ;Valida se a disciplina pertence ao BCC ou ao BC&T
          [else (fl sts_campus sts_periodo sts_curso (cdr lista-anterior))
                ])))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;Selecao das disciplinas;;;;;;;;;;;;;;;;;;;;;;;;;

  ;PARAMETROS PARA LEITURA DO ARQUIVO CSV CONTENDO AS DISCIPLINAS FILTRADAS
  (define make-csv-reader-filtro
    (make-csv-reader-maker
     '((newline-type 'lf)
       (separator-chars            #\;)
       (quote-char                 . #f)
       )))

  ;ARQUIVO CSV CONTENDO AS DISCIPLINAS DA GRADE 
  (define next-row-grade
    (make-csv-reader-grade (open-input-file (vector-ref args 0)))) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;PONTO DE ATENCAO AQUI

;;;;;;;;;;;;;Priorizacao por menos avancadas;;;;;;;;;;;;

  ;RECEBE UMA A UMA AS DISCIPLINAS DA GRADE E COMPARA COM AS DISCIPLINAS POSSIVEIS (CONTIDAS NO ARQUIVO CSV)
  (define (possivel? linha-grade)
    (define port-in-filtro (open-input-file (vector-ref args 1)))
    (define next-row-filtro
      (make-csv-reader-filtro port-in-filtro))

    (let loop ([linha-filtro (next-row-filtro)] )
      (cond [(equal? linha-filtro '()) (close-input-port port-in-filtro) #f]
            [(equal? (second linha-filtro) (second linha-grade))#t ]
            [else (loop (next-row-filtro))])))

  ;Apartir dos parametros informados pelo usuário, filtra apenas as linhas desejadas e inclui no arquivo de saída para as grades do diurno
  (define (gera-possiveis-disciplinas-grade1 linhas segunda1_1 segunda1_2 segunda1_3 segunda2_1 segunda2_2 segunda2_3 segunda3 segunda4 terca1_1 terca1_2 terca1_3
                                             terca2_1 terca2_2 terca2_3 terca3 terca4 quarta1_1 quarta1_2 quarta1_3 quarta2_1 quarta2_2 quarta2_3 quarta3 quarta4
                                             quinta1_1 quinta1_2 quinta1_3 quinta2_1 quinta2_2 quinta2_3 quinta3 quinta4 sexta1_1 sexta1_2 sexta1_3 sexta2_1 sexta2_2 sexta2_3 sexta3 sexta4 )
;    (define (agenda 
    (let loop ([linha (linhas)] [linha2 '()] [linha3 '()])
      (cond [(and (not linha '()) (not linha2 '()) (not linha3 '()))

             ]
            [(and (equal? linha '()) (not linha2 '()) (not linha3 '()))]
            [(and (equal? linha '()) (equal? linha2 '()) (not linha3 '()))]
            [(and (equal? linha '()) (equal? linha2 '()) (equal? linha3 '()))]
            [else loop (next-row)]
      (if (and (equal? cdar(linha) cdar(linha2) cdar(linha3)) (equal? cddar(linha) cddar(linha2) cddar(linha3)))
      (cond [(and (equal? (cdddddr(linha)) " Segunda-feira")(equal? (cddddddr(linha)) (" 08:00")) (segunda1 #f))(loop (next-row)(linha)(linha2))]
                   [(and (equal? (cdddddr(linha)) " Segunda-feira")(equal? (cddddddr(linha)) (" 10:00"))) (define segunda2 #t)]
                   [(and (equal? (cdddddr(linha)) " Segunda-feira")(equal? (cddddddr(linha)) (" 14:00"))) (define segunda3 #t)]
                   [(and (equal? (cdddddr(linha)) " Segunda-feira")(equal? (cddddddr(linha)) (" 16:00"))) (define segunda4 #t)]
                   [(and (equal? (cdddddr(linha)) " Terca-feira")(equal? (cddddddr(linha)) (" 08:00"))) (define terca1 #t)]
                   [(and (equal? (cdddddr(linha)) " Terca-feira")(equal? (cddddddr(linha)) (" 10:00"))) (define terca2 #t)]
                   [(and (equal? (cdddddr(linha)) " Terca-feira")(equal? (cddddddr(linha)) (" 14:00"))) (define terca3 #t)]
                   [(and (equal? (cdddddr(linha)) " Terca-feira")(equal? (cddddddr(linha)) (" 16:00"))) (define terca4 #t)]
                   [(and (equal? (cdddddr(linha)) " Quarta-feira")(equal? (cddddddr(linha)) (" 08:00"))) (define quarta1 #t)]
                   [(and (equal? (cdddddr(linha)) " Quarta-feira")(equal? (cddddddr(linha)) (" 10:00"))) (define quarta2 #t)]
                   [(and (equal? (cdddddr(linha)) " Quarta-feira")(equal? (cddddddr(linha)) (" 14:00"))) (define quarta3 #t)]
                   [(and (equal? (cdddddr(linha)) " Quarta-feira")(equal? (cddddddr(linha)) (" 16:00"))) (define quarta4 #t)]
                   [(and (equal? (cdddddr(linha)) " Quinta-feira")(equal? (cddddddr(linha)) (" 08:00"))) (define quinta1 #t)]
                   [(and (equal? (cdddddr(linha)) " Quinta-feira")(equal? (cddddddr(linha)) (" 10:00"))) (define quinta2 #t)]
                   [(and (equal? (cdddddr(linha)) " Quinta-feira")(equal? (cddddddr(linha)) (" 14:00"))) (define quinta3 #t)]
                   [(and (equal? (cdddddr(linha)) " Quinta-feira")(equal? (cddddddr(linha)) (" 16:00"))) (define quinta4 #t)]
                   [(and (equal? (cdddddr(linha)) " Sexta-feira")(equal? (cddddddr(linha)) (" 08:00"))) (define sexta1 #t)]
                   [(and (equal? (cdddddr(linha)) " Sexta-feira")(equal? (cddddddr(linha)) (" 10:00"))) (define sexta2 #t)]
                   [(and (equal? (cdddddr(linha)) " Sexta-feira")(equal? (cddddddr(linha)) (" 14:00"))) (define sexta3 #t)]
                   [(and (equal? (cdddddr(linha)) " Sexta-feira")(equal? (cddddddr(linha)) (" 16:00"))) (define sexta4 #t)]
      (cond [(equal? linha '()) (print "GERADO 'Grade-gerada.csv' CONTENDO APENAS DISCIPLINAS SELECIONADAS")]
            [(and (equal? (busca-na-lista linha campus periodo) #t) (not (cursada? linha)))  
             (displayln (string-join (map ~a (formata-lista linha)) " ") saida)
             (cond [(and (equal? (cdddddr(linha)) " Segunda-feira")(equal? (cddddddr(linha)) (" 08:00"))) (define segunda1 #t)]
                   [(and (equal? (cdddddr(linha)) " Segunda-feira")(equal? (cddddddr(linha)) (" 10:00"))) (define segunda2 #t)]
                   [(and (equal? (cdddddr(linha)) " Segunda-feira")(equal? (cddddddr(linha)) (" 14:00"))) (define segunda3 #t)]
                   [(and (equal? (cdddddr(linha)) " Segunda-feira")(equal? (cddddddr(linha)) (" 16:00"))) (define segunda4 #t)]
                   [(and (equal? (cdddddr(linha)) " Terca-feira")(equal? (cddddddr(linha)) (" 08:00"))) (define terca1 #t)]
                   [(and (equal? (cdddddr(linha)) " Terca-feira")(equal? (cddddddr(linha)) (" 10:00"))) (define terca2 #t)]
                   [(and (equal? (cdddddr(linha)) " Terca-feira")(equal? (cddddddr(linha)) (" 14:00"))) (define terca3 #t)]
                   [(and (equal? (cdddddr(linha)) " Terca-feira")(equal? (cddddddr(linha)) (" 16:00"))) (define terca4 #t)]
                   [(and (equal? (cdddddr(linha)) " Quarta-feira")(equal? (cddddddr(linha)) (" 08:00"))) (define quarta1 #t)]
                   [(and (equal? (cdddddr(linha)) " Quarta-feira")(equal? (cddddddr(linha)) (" 10:00"))) (define quarta2 #t)]
                   [(and (equal? (cdddddr(linha)) " Quarta-feira")(equal? (cddddddr(linha)) (" 14:00"))) (define quarta3 #t)]
                   [(and (equal? (cdddddr(linha)) " Quarta-feira")(equal? (cddddddr(linha)) (" 16:00"))) (define quarta4 #t)]
                   [(and (equal? (cdddddr(linha)) " Quinta-feira")(equal? (cddddddr(linha)) (" 08:00"))) (define quinta1 #t)]
                   [(and (equal? (cdddddr(linha)) " Quinta-feira")(equal? (cddddddr(linha)) (" 10:00"))) (define quinta2 #t)]
                   [(and (equal? (cdddddr(linha)) " Quinta-feira")(equal? (cddddddr(linha)) (" 14:00"))) (define quinta3 #t)]
                   [(and (equal? (cdddddr(linha)) " Quinta-feira")(equal? (cddddddr(linha)) (" 16:00"))) (define quinta4 #t)]
                   [(and (equal? (cdddddr(linha)) " Sexta-feira")(equal? (cddddddr(linha)) (" 08:00"))) (define sexta1 #t)]
                   [(and (equal? (cdddddr(linha)) " Sexta-feira")(equal? (cddddddr(linha)) (" 10:00"))) (define sexta2 #t)]
                   [(and (equal? (cdddddr(linha)) " Sexta-feira")(equal? (cddddddr(linha)) (" 14:00"))) (define sexta3 #t)]
                   [(and (equal? (cdddddr(linha)) " Sexta-feira")(equal? (cddddddr(linha)) (" 16:00"))) (define sexta4 #t)])                   
             (loop (next-row))]  ;ESCREVE NO ARQUIVO FILTRADO
            [else (loop (next-row))])))

;Utilizado para filtrar os parametros informados pelo usuário, percorre cada campo da LISTA e valida a compatibilidade com os parâmetros informados pelo usuário
(define (busca-na-lista lista campus periodo)
  (let fl ([segunda1 #f] [segunda2 #f] [segunda3 #f] [segunda4 #f]
           [terca1 #f] [terca2 #f] [terca3 #f] [terca4 #f]
           [quarta1 #f] [quarta2 #f] [quarta3 #f] [quarta4 #f]
           [quinta1 #f] [quinta2 #f] [quinta3 #f] [quinta4 #f]
           [sexta1 #f] [sexta2 #f] [sexta3 #f] [sexta4 #f] [lista-anterior lista])              ;inicia com todos os status #f
    (cond [(and [segunda1 #t] [segunda2 #t] [segunda3 #t] [segunda4 #t]
           [terca1 #t] [terca2 #t] [terca3 #t] [terca4 #t]
           [quarta1 #t] [quarta2 #t] [quarta3 #t] [quarta4 #t]
           [quinta1 #t] [quinta2 #t] [quinta3 #t] [quinta4 #t]
           [sexta1 #t] [sexta2 #t] [sexta3 #t] [sexta4 #t]) #t]       ;Quando todos os status forem #t, encerra a validação
          [(null? lista-anterior )  #f]                                                         ;Se a lista chegar ao fim antes de termos todos os status #t, encerra a validação
          [(and ( equal? (car lista-anterior) campus) (equal? sts_campus #f)) (fl #t sts_periodo sts_curso (cdr lista-anterior))]
          [(and ( equal? (car lista-anterior) periodo) (equal? sts_periodo #f)) (fl sts_campus #t sts_curso (cdr lista-anterior))]
          [(and ( or (equal? (car lista-anterior) "Bacharelado em Ciencia da Computacao")(equal? (car lista-anterior) "Bacharelado em Ciencia e Tecnologia"))
                (equal? sts_curso #f)) (fl sts_campus sts_periodo #t (cdr lista-anterior))]     ;Valida se a disciplina pertence ao BCC ou ao BC&T
          [else (fl sts_campus sts_periodo sts_curso (cdr lista-anterior))
                ])))

(if (= pega-periodo-selecionado 1)
    (gera-possiveis-disciplinas-grade-1 next-row-grade #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f)
    (gera-possiveis-disciplinas-grade-2)
;(gera-possiveis-disciplinas-grade-1 next-row-grade #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f)
;  (gera-possiveis-disciplinas-grade-1 next-row-grade segunda1 segunda2 segunda3 segunda4 terca1 terca2 terca3 terca4 quarta1 quarta2 quarta3 quarta4
;                                      quinta1 quinta2 quinta3 quinta4 sexta1 sexta2 sexta3 sexta4) ;(pega-campus-selecionado campus)(pega-periodo-selecionado periodo))
  (close-output-port saida)

;;;;;;;;;;;;;Fim da priorizacao por menos avancadas;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;Fim da selecao das disciplinas;;;;;;;;;;;;;;;;;;;;;;;;;


(define (pega-campus-selecionado campus)
  (cond [(= (string->number campus) 1) "Santo Andre"]
        [else "Sao Bernardo do Campo"]))

(define (pega-periodo-selecionado periodo)
  (cond [(= (string->number periodo) 1) "Diurno"]
        [else "Noturno"]))

(define (pega-periodo-selecionado prioridade)
  (cond [(= (string->number periodo) 1) "MenosAv"]
        [else "MaisCred"]))



;(main (current-command-line-arguments))


(define v (vector "Oferta Disciplicas (BCT-BCC).csv" "Disciplinas Cursadas.csv" "Base_disciplinas"))
(main v)
