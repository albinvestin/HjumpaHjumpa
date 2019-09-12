#lang racket/gui
(require "groundlevel.rkt")
(require "player.rkt")
(require "startmenu.rkt")
(provide (all-defined-out))
 
; ---------------Definitioner-------------

; Grundnivån

(define groundlevel
  (new groundlevel%
       [groundlevel-width '(0 . 1300)]
       [groundlevel-height 0]))

;Plattformar

(define platform-lower-left
  (new platform%
       [platform-width '(200 . 300)]
       [platform-height '(80 . 90)]))


(define platform-lower-center
  (new platform%
       [platform-width '(600 . 700)]
       [platform-height '(80 . 90)]))


(define platform-lower-right
  (new platform%
       [platform-width '(1000 . 1100)]
       [platform-height '(80 . 90)]))


(define platform-middle-left
  (new platform%
       [platform-width '(390 . 510)]
       [platform-height '(170 . 180)]))


(define platform-middle-right
  (new platform%
       [platform-width '(790 . 910)]
       [platform-height '(170 . 180)]))






; ------------- Definierar spelare -------------------

(define player1
  (new player%
       [player-number 1]
       [player-position '(150 . 10)]
       [player-last-position '(150 . 10)]
       [player-points 0]
       [player-status 1]
       [player-death-time 0]
       [player-direction-y 'up]
       [player-velocity 0]
       [player-keys '(#\q . #\a)]
       [player-color "Blue"]
       [player-spawn '(150 . 10)]))

(define player2
  (new player%
       [player-number 2]
       [player-position '(350 . 10)] 
       [player-last-position '(250 . 10)]
       [player-points 0]
       [player-status 1]
       [player-death-time 0]
       [player-direction-y 'up]
       [player-velocity 0]
       [player-keys '(#\c . #\v)]
       [player-color "Red"]
       [player-spawn '(350 . 10)]))

(define player3
  (new player%
       [player-number 3]
       [player-position '(550 . 10)]
       [player-last-position '(550 . 10)]
       [player-points 0]
       [player-status 1]
       [player-death-time 0]
       [player-direction-y 'up]
       [player-velocity 0]
       [player-keys '(#\n . #\m)]
       [player-color "Green"]
       [player-spawn '(550 . 10)]))

(define player4
  (new player%
       [player-number 4]
       [player-position '(750 . 10)]
       [player-last-position '(750 . 10)]
       [player-points 0]
       [player-status 1]
       [player-death-time 0]
       [player-direction-y 'up]
       [player-velocity 0]
       [player-keys '(#\t . #\y)]
       [player-color "Pink"]
       [player-spawn '(750 . 10)]))

(define player5
  (new player%
       [player-number 5]
       [player-position '(950 . 10)]
       [player-last-position '(950 . 10)]
       [player-points 0]
       [player-status 1]
       [player-death-time 0]
       [player-direction-y 'up]
       [player-velocity 0]
       [player-keys '(#\a . #\s)]
       [player-color "Yellow"]
       [player-spawn '(950 . 10)]))

(define player6
  (new player%
       [player-number 6]
       [player-position '(1150 . 10)]
       [player-last-position '(1150 . 10)]
       [player-points 0]
       [player-status 1]
       [player-death-time 0]
       [player-direction-y 'up]
       [player-velocity 0]
       [player-keys '(#\z . #\x)]
       [player-color "Gray"]
       [player-spawn '(1150 . 10)]))



(define diameter 20) ; spelarens diameter


; Lista med plattformar

(define platform-list
  (list 
   platform-lower-left 
   platform-lower-center 
   platform-lower-right
   platform-middle-left
   platform-middle-right))



; ----------------- Spelmotorn ----------------

; Funktion som kollar för en spelare om den vänder, slår i marken, slår i en plattform ovanifrån eller underifrån, och samtidigt om den slår i en spelare.
; Funktionen updaterar spelarens position och applicerar gravitation.
; Funktionen kallar på hjälpfunktioner vad som händer.

(define speed-limit 28) ;Högsta hastigheten en spelare kan få under en plattform.

(define (fall player)
  (if (= (send player get-player-status) 1) ; kollar om spelaren är död
      (begin 
        (send player update-position) ;uppdaterar position
        (let ((playerposition (send player get-player-position))
              (playerdirection (send player get-player-direction-y))
              (groundlevelwidth (send groundlevel get-groundlevel-width))
              (groundlevelheight (send groundlevel get-groundlevel-height))
              (platformloopreturn (platform-loop player platform-list))
              (player-velocity (send player get-player-velocity)))
              

          (begin
            (cond 
              ((= player-velocity 0) ;kollar om spelarens hastighet = 0
               (turn player))        ;kallar på hjälpfunktion     
              
              ((and (not (eq? platformloopreturn #f)) (eq? playerdirection 'down)) ; ifall spelaren är påväg ner och träffar en plattform så studsar den på plattformens övre gräns.
               (begin
                 (send player change-player-position-y (cdr (send platformloopreturn get-platform-height))) 
                 (send player jump))) 
              
              ((and (not (eq? platformloopreturn #f)) (eq? playerdirection 'up)) ; om spelaren är under plattformen och studsar i taket bygger den upp hastighet
               (begin
                 (send player change-player-position-y (- (car (send platformloopreturn get-platform-height)) diameter))
                 (turn player)
                 (if (> player-velocity speed-limit)
                     (send player change-player-velocity speed-limit)
                     (send player change-player-velocity (+ 10 player-velocity)))))
              
              ((>= groundlevelheight (cdr playerposition)) ; om spelaren är i höjd med marken kallas en hjälp funktion
               (impact-ground player))
              
              (else (void)))
            
            (send player gravity) ; applicerar gravitation
            
          ;kollar sedan om spelaren slår i en annan spelare
    
            (player-collision player player-list))))
          
          ;if status = 0
          (void)))
    
    


(define (turn player)
  (send player change-player-direction-y)) ; byter håll i y-led på spelaren


; Kollar om spelaren är innanför marknivåns kanter, isånnafall studsar spelaren på marken, annars dör den.

(define (impact-ground player)
  (let ((groundlevelwidth (send groundlevel get-groundlevel-width))
        (playerposition (send player get-player-position)))
  (if (and (< (car groundlevelwidth) (+ (car playerposition) (/ diameter 2))) 
           (< (- (car playerposition) (/ diameter 2)) (cdr groundlevelwidth))) 
      (begin
        (send player change-player-position-y 0) 
        (send player jump))
      (send player kill-player))))





;------------- Plattform --------------

; Studs mot plattform: 
; kollar spelarens riktning (ner eller upp), last-position och nuvarande position. 
; Kollar om spelaren har åkt igenom plattformen övre- resp undre gräns.
; returnerar plattformen eller #f

   
; Kollar om en spelare slår i någon av plattformarna, isåfall returneras plattformen, annars #f

(define (platform-loop player platform-list)
    (cond ((null? platform-list) #f)
          ((player-vs-platform player (car platform-list)) (car platform-list))
          (else (platform-loop player (cdr platform-list)))))


; Kollar om en spelare slår ihop med en plattform, antingen ovanifrån eller underifrån.
; Kollar om spelaren är höjd med och innanför plattformens kanter.
; Returnerar #t eller #f.

(define (player-vs-platform player platform)
  (let ((playerdirection (send player get-player-direction-y))
        (playerposition-x (car (send player get-player-position)))
        (playerposition-y (cdr (send player get-player-position)))
        (playerlastposition-y (cdr (send player get-player-last-position)))
        (platform-left-edge (car (send platform get-platform-width)))
        (platform-right-edge (cdr (send platform get-platform-width)))
        (platformheight-lower (car (send platform get-platform-height)))
        (platformheight-upper (cdr (send platform get-platform-height))))
    
    (if (eq? playerdirection 'down)
        (and (<= platform-left-edge (+ playerposition-x (/ diameter 2))) 
             (<= (- playerposition-x (/ diameter 2)) platform-right-edge) 
             (< platformheight-upper playerlastposition-y)
             (<= playerposition-y platformheight-upper)) 
        
        (and (<= platform-left-edge (+ playerposition-x (/ diameter 2))) 
             (<= (- playerposition-x (/ diameter 2)) platform-right-edge)
             (< (+ playerlastposition-y diameter) platformheight-lower)
             (<= platformheight-lower (+ playerposition-y diameter))))))




; ------------ PvP --------------------

; Interaktion mellan spelare:
; Kollar om spelare1 stöter ihop med spelare2,
; om spelare1 befinner sig mer än en diameter enheter över spelare2, 
; då returneras:
; 'kill (spelare1 dödar spelare2)
; 'impact (spelare 1 slår ihop med spelare 2 från vänster)
; 0 då händer inget

(define (player-vs-player player1 player2)
  (let ((player1direction (send player1 get-player-direction-y))
        (player1position-x (car (send player1 get-player-position)))
        (player1position-y (cdr (send player1 get-player-position)))
        (player1lastposition-y (cdr (send player1 get-player-last-position)))
        (player2direction (send player2 get-player-direction-y))
        (player2position-x (car (send player2 get-player-position)))
        (player2position-y (cdr (send player2 get-player-position)))
        (player2lastposition-y (cdr (send player2 get-player-last-position))))
    
    (cond ((and (>= player1lastposition-y (+ player2lastposition-y diameter)) 
                (<= player1position-y (+ player2position-y diameter))       ; kollar om spelare 1 har åkt igenom spelare 2
                (< (abs (- player1position-x player2position-x)) diameter)) ; kollar om spelarna är inom varandras bredd
           'kill)
          
          ((and (< (abs (- player1position-y player2position-y)) diameter)   ; kollar om de ligger i samma höjdled 
                (< player1position-x player2position-x)                      ; spelare1 till vänster om spelare2
                (<= (abs (- player2position-x player1position-x)) diameter)) ; abs på avståndet skall vara mindre än en diameter
           'impact)
           (else 0))))


; Kollar om en spelare slår ihop med någon av de övriga spelarna.

(define (player-collision player player-list)
  (cond ((null? player-list) (void))
        
        ((eq? player (car player-list)) (player-collision player (cdr player-list)))
        
        ((eq? 'kill (player-vs-player player (car player-list))) ; Spelaren studsar ovanifrån på en annan spelare
         (begin
           (send player change-player-position-y (+ (cdr (send (car player-list) get-player-position)) diameter))
           (send (car player-list) kill-player)
           (send player plus-points)
           (if (eq? (send player get-player-direction-y) 'up) ; Om spelare2 kommer ikapp underifrån överförs dess hastighet till spelare1
               (send player change-player-velocity (+ (send (car player-list) get-player-velocity)
                                                      (send player get-player-velocity))) 
               (send player jump))
           (player-collision player (cdr player-list))))
        
        ((eq? 'impact (player-vs-player player (car player-list))) ; Spelaren knuffar ifrån vänster på den andra spelaren
         (begin
           (send player impact-left)
           (send (car player-list) impact-right)
           (player-collision player (cdr player-list))))
        
        (else (player-collision player (cdr player-list)))))



 
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
