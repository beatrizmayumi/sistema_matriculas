#lang racket

(require "possiveis-disciplinas.rkt") 

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
  ;(possiveis-disciplinas campus periodo bct)
  (Menu))

