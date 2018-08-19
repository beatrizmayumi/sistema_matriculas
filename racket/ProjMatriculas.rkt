#lang racket

(require csv-reading);Pacote para manipualação de arquivos CSV...> raco pkg install csv-reading


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;GERAÇÃO DE ARQUIVO FILTRADO COM OS PARAMETROS INFORMADOS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;Apartir dos parametros informados pelo usuário, filtra apenas as linhas desejadas e inclui no arquivo de saída
(define (gera-possiveis-disciplinas linhas campus periodo)
  (let loop ([linha (linhas)])
    (cond [(equal? linha '()) "Iniciar novo Csv-Reader apontando para Arquivo-filtrado.csv"]
          [(equal? (busca-na-lista linha campus periodo "bct") #t) 
           (displayln (formata-lista linha) saida)(loop (next-row))]
          [else (loop (next-row))])))                                    


;(gera-possiveis-disciplinas next-row)
;(close-output-port saida)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MENU ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Primeiro, obtem-se o Campus desejado para a sugestão
(let Menu ()
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
    
  ;Uma vez com o Campus e Período, já podemos filtrar as Disciplinas possíveis.
  
  
  (gera-possiveis-disciplinas next-row (pega-campus-selecionado campus)(pega-periodo-selecionado periodo))
  (close-output-port saida)

  (Menu))


