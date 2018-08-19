
#!/usr/bin/env racket
#lang racket
(require csv-reading);Pacote para manipualação de arquivos CSV...> raco pkg install csv-reading

(define (main args)
  (cond [(< (vector-length args) 3) 
         (display "Informar: <Arquivo contendo Oferta de disciplinas.csv> <Arquivo contendo Disciplinas já Cursadas.csv>\n"
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
  (define next-row-cur
    (make-csv-reader-cursadas (open-input-file (vector-ref args 1))))


  ;ARQUIVO CSV CONTENDO AS DISCIPLINAS DA GRADE 
  (define next-row-grade
    (make-csv-reader-grade (open-input-file (vector-ref args 2))))

 
  ;ARQUIVO CSV QUE IRÁ CONTER O ARQUIVO DE ENTRADA APÓS SER FILTRADO: O FILTRO ROMEVERÁ AS DISCIPLINAS JÁ CURSADAS E AS DISCIPLINAS FORA DO CAMPUS E PERÍODO DESEJADO
  (define saida
    (open-output-file "Arquivo-filtrado.csv" ;(vector-ref args 1)
                      #:exists 'replace))

  (define saida2
    (open-output-file "Arquivo-filtradoBCC.csv" ;(vector-ref args 2)
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
  

    (define (cursada-grade? linha)
    (define port-in-grade (open-input-file (vector-ref args 2)))
    (define next-row-grade
      (make-csv-reader-grade port-in-grade))

    (let loop ([linha-cursada-grade (next-row-grade)] )
      (cond [(equal? linha-cursada-grade '()) (close-input-port port-in-grade) #f]
            [(equal? (seventh linha-cursada-grade) (second linha))#t ]
            [else (loop (next-row-grade))])))

    (define (gera-disciplinas-possiveis-bcc linhas)
    (let loop ([linha (linhas)])
      (cond [(equal? linha '()) (print "GERADO 'Arquivo-filtradoBCC.csv' CONTENDO APENAS DISCIPLINAS DO BCC EM POTENCIAL")]
            [ (displayln (string-join (map ~a (formata-lista linha)) " ") saida2)(loop (next-row-grade))] 
            [else (loop (next-row-grade))])))

  

  (gera-possiveis-disciplinas next-row (pega-campus-selecionado campus)(pega-periodo-selecionado periodo))
  (gera-disciplinas-possiveis-bcc next-row-grade)
  (close-output-port saida)
  (close-output-port saida2)


  ;ARQUIVO CSV CONTENDO AS DISCIPLINAS APÓS FILTRO INICIAL 
  (define next-row-fil
    (make-csv-reader (open-input-file "Arquivo-filtrado.csv")))

  (define next-row-filtrabcc
    (make-csv-reader (open-input-file "Arquivo-filtradoBCC.csv")))
  
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


(define (pega-campus-selecionado campus)
  (cond [(= (string->number campus) 1) "Santo Andre"]
        [else "Sao Bernardo do Campo"]))

(define (pega-periodo-selecionado periodo)
  (cond [(= (string->number periodo) 1) "Diurno"]
        [else "Noturno"]))


;(main (current-command-line-arguments))


(define v (vector "Oferta Disciplicas (BCT-BCC).csv" "Disciplinas Cursadas.csv" "Base_disciplinas.csv"))
(main v)
