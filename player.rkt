#lang racket
(provide player%)

(define player%
  (class object%
    (init-field
     player-number   ; Nummer som används för sortering av spelare
     player-position ; ett par ( x . y ) koordinater
     player-last-position 
     player-points
     player-status ; Vid liv (1) eller död (0).
     player-death-time ; ett värde på hur länge spelaren har varit död
     player-direction-y ; up eller down
     player-velocity ; hastighet i y-led
     player-keys ; spelartangenter i ett par, (vänster . höger)
     player-color
     player-spawn) ; Startposition för en spelare
    
    ;Grundläggande get funktioner:
    
    (define/public (get-player-number)
      player-number)
    (define/public (get-player-position)
      player-position)
    (define/public (get-player-last-position)
      player-last-position)
    (define/public (get-player-points)
      player-points)
    (define/public (get-player-status)
      player-status)
    (define/public (get-player-death-time)
      player-death-time)
    (define/public (get-player-direction-y)
      player-direction-y)
    (define/public (get-player-velocity)
      player-velocity)
    (define/public (get-player-keys)
      player-keys)
    (define/public (get-player-color)
      player-color)
    (define/public (get-player-spawn)
      player-spawn)
    
    
;------Liv-/dödfunktioner--------
    
    ;Återuppliva på en slumpad position
    
    (define/public (revive-player)
      (begin
        (set! player-status 1)
        (change-player-position-x (car (random-spawn-function)))
        (change-player-position-y (cdr (random-spawn-function)))))
    
    
    ;Slumpar en spawn-position
    
    (define/private (random-spawn-function)
       (let ((spawn-points '((0 . (100 . 150))
                             (1 . (300 . 150))
                             (2 . (500 . 150))
                             (3 . (700 . 150))
                             (4 . (900 . 150))
                             (5 . (1100 . 150)))))
         (cdr (assq (random 6) spawn-points))))
    
    
    ; Dödar en spelare och sätter spelaren ur spel
    
    (define/public (kill-player)
      (begin
        (set! player-status 0)
        (set! player-velocity 0)
        (change-player-position 0 -1337)))
    
    ; Ökar/tar bort spelarens death-time
    
    (define/public (add-player-death-time)
      (set! player-death-time (+ 1 player-death-time)))
    
    (define/public (remove-player-death-time)
      (set! player-death-time 0))

        
; -------------- Adderar/tar bort poäng------------
    
    (define/public (plus-points)
      (set! player-points (+ player-points 1)))
    
    (define/public (minus-points)
      (set! player-points (if (= 0 player-points) ; om poäng=0 ger 0 poäng
                              0
                              (- player-points 1))))
    (define/public (reset-points)
      (set! player-points 0))
    
; ----------- Rörelse-/positionsändring ----------- 
    
    ; Byter riktning i y-led
    
    (define/public (change-player-direction-y)
      (set! player-direction-y (if (eq? player-direction-y 'up) ;Byter håll i y-led
                                 'down
                                 'up)))
        
    
    ; Ändrar hastighet i y-led till ett värde
    
    (define/public (change-player-velocity speed)
      (set! player-velocity speed))
    
    
    ; Ändra position och uppdaterar last-position
   
    (define/public (change-player-position-y new-y)
      (begin
       (set! player-last-position player-position) 
       (set! player-position (cons (car player-position) new-y))))
    
    (define/public (change-player-position-x new-x)
      (begin
        (set! player-last-position player-position)
        (set! player-position (cons new-x (cdr player-position))))) 
    
    
    ; Flyttar en spelare
    
    (define/public (change-player-position new-x new-y) 
      (begin
        (set! player-last-position (cons new-x new-y))
        (set! player-position (cons new-x new-y))))
    
    
    
; ------- Ändra tangenter och styrning------------
    
    (define/public (change-player-key-left new-left)
      (set! player-keys (cons new-left (cdr player-keys))))
    
    (define/public (change-player-key-right new-right)
      (set! player-keys (cons (car player-keys) new-right)))
    
    
    ; Styrning x-led
    
    (define/public (go-right)
      (set! player-position (cons (+ (car player-position) 5) (cdr player-position))))
    
    (define/public (go-left)
      (set! player-position (cons (- (car player-position) 5) (cdr player-position))))
    
    
; -------------- Koordinatuppdatering--------
    
    (define/public (update-position)
      (if (eq? 'down player-direction-y)
          (change-player-position-y (- (cdr player-position) player-velocity))
          (change-player-position-y (+ (cdr player-position) player-velocity))))    
    
      
; ------------- Gravitation---------------
    
    ; Gravitationen kommer påverka varje spelare på samma sätt hela tiden. 
    ; När velocity=0 så kommer spelaren vara i sitt övre vändläge, dvs y-riktningen kommer bli down.
    ; När spelaren träffar t ex marken så kommer den få en hastighet beroende på jumpheight den har och riktningen blir då up.
    ; konstant acceleration --> linjär hastighet --> exponentiell position 
    
    (define/public (gravity)
        (if (eq? player-direction-y 'down)
            (set! player-velocity (+ player-velocity 2))
            (set! player-velocity (- player-velocity 2))))
            
    

; ------------ Hopp funktion------------
    
    ; Spelaren strävar mot en optimal hastighet (hopphöjd)
    ; Hoppar spelaren lägre får den en högre hastighet tills den är optimal
    ; Samma sak om spelaren hoppar högre än höjden
    ; Ändrar även spelarens riktning i y-led
    
    (define optimal-velocity 22)
    (define velocity-change 6) ; hur fort hastigheten ändras mot optimal
    
    (define/public (jump)
      (begin 
        (if (<= optimal-velocity player-velocity) 
            (change-player-velocity (if (> (+ optimal-velocity velocity-change) player-velocity) 
                                        optimal-velocity                     
                                        (- player-velocity velocity-change)))
            (change-player-velocity (if (< (- optimal-velocity velocity-change) player-velocity) 
                                        optimal-velocity
                                        (+ player-velocity velocity-change))))
        (change-player-direction-y)))
    

; ----------- Krock från sida mellan spelare------------
    
    ; styrd spelare kommer från vänster och knuffas åt vänster
    
    (define/public (impact-left)
      (set! player-position (cons (- (car player-position) 5) (cdr player-position)))) ; flyttar spelaren till vänster (ändra värde!)
    
    ; styrd spelare kommer från höger och knuffas åt höger
    
    (define/public (impact-right)
      (set! player-position (cons (+ (car player-position) 5) (cdr player-position)))) ; flyttar spelaren till höger (ändra värde!)
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    (super-new)))
