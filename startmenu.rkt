#lang racket/gui
(require "player.rkt")
(provide (all-defined-out))

; ------------ Redigera till spelare ------------

(define player-list ; lista med aktiva spelare i matchen
  null)


 ; lägga till/ta bort i playerlist

(define (add-player player)
  (set! player-list (add-to-right-position player player-list)))


; Lägger till spelare i stigande ordning

(define (add-to-right-position player player-list)
  (let ((player-number (send player get-player-number)))
    (cond ((null? player-list) (list player))
          ((> player-number (send (car player-list) get-player-number))
           (cons (car player-list) (add-to-right-position player (cdr player-list))))
          (else (cons player player-list)))))

; Tar bort den sökta spelaren ur player-list

(define (remove-player player)
  (define (remove-loop player player-list)
    (cond ((null? player-list) '())
          ((eq? player (car player-list)) (cdr player-list))
          (else (cons (car player-list) (remove-loop player (cdr player-list))))))
  (set! player-list (remove-loop player player-list)))






               