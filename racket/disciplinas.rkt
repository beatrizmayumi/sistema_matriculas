#lang racket

(define materias-feitas (lambda x x))
(define materias-ofertadas (lambda x x))
(define materias-requisitos (lambda x x))

(display "Materias feitas pelo aluno: ")
(materias-feitas "PI" "POO" "Logica Basica")

(display "Materias ofertadas nesse quadrimestre: ")
(materias-ofertadas "Matematica Discreta" "Teoria dos Grafos" "Computacao Grafica" "Programacao Matematica")

; requisitos, materias
(define (materias-req a b)
(cons (list b) a))

(define (materias-req2 a b c)
(cons (list b c) a))

;(define (materias-req a b)
;(cons a (list b)))

(materias-req "Matematica Discreta" "Logica Basica")
(materias-req2 "AED I" "PI" "Programacao Estruturada")


; buscaReq::materiafeita
;-> materia a fazer

(define (materia)
(cdr (materias-req "Matematica Discreta" "Logica Basica")))

(materia)

;(define (recomenda)
 ;(cond (= (car (materias-feitas "PI" "POO" "Logica Basica")) (car materias-req "Matematica Discreta" "Logica Basica"))
  ;  (cdr (materias-req "Matematica Discreta" "Logica Basica"))))
;(recomenda)
