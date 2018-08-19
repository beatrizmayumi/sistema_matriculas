#lang racket

(provide campus)
(provide periodo)
(provide prioridade)

; Menu - obtém as informacoes do usuario

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