#lang racket/gui
(require racket/gui/base)
(require "world.rkt")
(require "startmenu.rkt")


;----------- Definitioner-------------

; matchklocka

(define start-time 60000)
(define round-time 60000)

; matchtidsinställare

(define (correct-matchtime text-field)
  (let ((match-time (* (string->number (send match-time-text-field get-value)) 1000)))
    (begin
      (set! round-time match-time)
      (set! start-time match-time))))

; hastighet

(define fps 5)

(define (fps-converter fps)
  (quotient 150 fps))

; hastighetsinställare

(define (correct-gamespeed text-field)
  (let ((gamespeed (string->number (send game-speed-text-field get-value))))
    (if (= gamespeed 0) ; Om 0 skrivs in i gamespeed, settas fps till 1 istället.
        (set! fps 1)
        (set! fps gamespeed))))


; --------------- Startfönster --------------------

(define start-window (new frame%
                          [width 700]
                          [height 500]
                          [label "Welcome to HjumpaHjumpa"]))

(define background (make-object bitmap% "smet.jpg"))
(define victory-image (make-object bitmap% "blom.jpg"))

; paneler

(define master-panel (new horizontal-panel%
                          [parent start-window]
                          [alignment '(center center)]))

(define ultra-left-panel (new vertical-panel%
                              [parent master-panel]
                              [alignment '(left center)]))

(define left-panel (new vertical-panel%
                        [parent master-panel]
                        [alignment '(left center)]))


(define center-panel (new vertical-panel%
                        [parent master-panel]
                        [alignment '(center center)]))

(define left-center-panel (new vertical-panel%
                               [parent center-panel]
                               [alignment '(left center)]))

(define right-center-panel (new vertical-panel%
                               [parent center-panel]
                               [alignment '(right center)]))

(define right-panel (new vertical-panel%
                        [parent master-panel]
                        [alignment '(right center)]))

(define ultra-right-panel (new vertical-panel%
                               [parent master-panel]
                               [alignment '(right center)]))

(define upper-right-panel (new horizontal-panel%
                               [parent ultra-right-panel]
                               [alignment '(left top)]))


; ---------------- Spelfönster---------------

(define world-window (new frame%
                          [width 1500]
                          [height 900]
                          [label "It's time to HjumpaHjumpa"]))



; --------- Startknapp --------------

(new button%
     [parent right-panel]
     [label "Start the game!"]
     [callback (lambda (button event)
                 (if (check-input-controllers text-field-list) ; Kollar om höger- och vänsterkontroll är givna, och startar isåfall spelet
                     (begin
                       (send world-window show #t)
                       (send start-window show #f)
                       (send restart-button show #f)
                       (correct-gamespeed game-speed-text-field)
                       (send game-engine-timer start (fps-converter fps) #f)
                       (send one-sec-timer start 1000 #f)
                       (correct-matchtime match-time-text-field)
                       (send round-timer start round-time #t)
                       (controller-initiation text-field-list reverse-available-players))
                     (new message%
                          [label "Wrong controller input"] ; Om kontrollerna ej är riktiga startar inte spelet 
                          [parent start-window])))])


; Omstartsknapp

(define restart-button
  (new button%
       [parent world-window]
       [label "Restart the game!"]
       [callback (lambda (button event)
                   (begin
                     (send (send game-canvas get-dc) clear)
                     (reset-world player-list)
                     (send game-engine-timer start (fps-converter fps) #f)
                     (send one-sec-timer start 1000 #f)
                     (send round-timer start round-time #t)
                     (send restart-button show #f)))]))



; Spelarknappar i startmeny

(define available-players
  (list player1 player2 player3 player4 player5 player6)) ; Lista med alla spelare

(define reverse-available-players
  (list player6 player5 player4 player3 player2 player1)) ; Lista med alla spelare fast omvänd ordning

(new message% ; hoppar ner några rader och placerar textfälten på rätt plats
     [label "\n"]
     [parent left-center-panel])
(new message%
     [label "\n"]
     [parent left-center-panel])

(define text-field-list
  null)

; Settingsknapp
(new button%
     [parent upper-right-panel]
     [label "Settings"]
     [callback (lambda (button event)
                 (send settings-window show #t))])


; Knappskaparen

(define (create-buttons number-of-buttons available-players)
  (if (> number-of-buttons 0)
      (begin
        
        ;spelarknappar
        
        (new message%
             [label (send (car available-players) get-player-color)] ; spelarens färg står över dens knapp 
             [parent left-panel])
        (new button%
             [parent left-panel]
             [label "You crazy! I'm bailin'!"]
             [callback (lambda (button event)
                         (if (eq? (send button get-label) "You crazy! I'm bailin'!") ; trycks knappen ner en gång läggs en spelare till i spelet
                             (begin
                               (send button set-label "Count me in!")
                               (add-player (car available-players)))
                             (begin                                                  ; trycks knappen ner igen tas samma spelare bort
                               (send button set-label "You crazy! I'm bailin'!")
                               (remove-player (car available-players)))))]
             [min-width 130])
       
        ;Textfält för styrning
        
        (new message%
             [label (send (car available-players) get-player-color)] ; spelarens färg står över dens kontroller
             [parent left-center-panel])
        (set! text-field-list                                        ; lägger till alla textfält i en lista
              (cons 
               (new text-field%                                      ; skapar textfälten där spelarkontrollerna står
                    [label "\n"]
                    [parent left-center-panel]
                    [init-value (let ((key-left (car (send (car available-players) get-player-keys)))
                                      (key-right (cdr (send (car available-players) get-player-keys))))
                                  (list->string (list key-left key-right)))])
               text-field-list))
             
        (create-buttons (- number-of-buttons 1) (cdr available-players)))
      (void)))

(create-buttons 6 available-players) ; skapar 6 knappar och 6 textfält

; Settingsfönster

(define settings-window
  (new frame%
       [width 400]
       [height 320]
       [label "Settings"]))

; Settingspaneler

(define settings-master-panel 
  (new horizontal-panel%
       [parent settings-window]
       [alignment '(center center)]))

(define settings-ultra-left-panel
  (new vertical-panel%
       [parent settings-master-panel]
       [alignment '(left center)]))

(define settings-left-panel
  (new vertical-panel%
       [parent settings-master-panel]
       [alignment '(left center)]))

(define settings-right-panel
  (new vertical-panel%
       [parent settings-master-panel]
       [alignment '(right center)]))

(define settings-ultra-right-panel
  (new vertical-panel%
       [parent settings-master-panel]
       [alignment '(right center)]))
       

; ---------Settingstextfält--------

; Matchtid

(define match-time-text-field
  (new text-field%
       [label "Matchtime (sec)"]
       [parent settings-right-panel]
       [init-value (number->string (/ start-time 1000))]))

; Spelhastighet

(define game-speed-text-field
  (new text-field%
       [label "Gamespeed (1 - 10)"]
       [parent settings-left-panel]
       [init-value (number->string fps)]))




; --------- Font----------

(define normal-font
  (make-object font% 18 'roman))

(define big-font
  (make-object font% 40 'swiss))



;------------------ Timer ------------------

(define (fallall)                        ; funktion som spelmotorn och kontrollerna
  (begin
    (game-engine-on-players player-list)
    (controller)
    (send game-canvas refresh)))

(define (game-engine-on-players player-list)
  (if (null? player-list)
      (void)
      (begin
        (fall (car player-list))
        (game-engine-on-players (cdr player-list)))))

(define game-engine-timer 
  (new timer% 
       [notify-callback fallall]))


; matchtimer
; funktion som avbryter matchen och gör att resultattabellen kommer upp

(define (end-round)
  (let ((dc (send game-canvas get-dc)))
    (begin
      (send game-engine-timer stop)
      (send one-sec-timer stop)
      (send dc clear)
      (send restart-button show #t)
      (printer 1 dc (result-table player-list null) 300))))

; Återställare av spelet
; återställer all data till ursprungsvärden

(define (reset-world player-list)
  (if (null? player-list) 
      (set! round-time start-time)
      (let ((start-x (car (send (car player-list) get-player-spawn)))
            (start-y (cdr (send (car player-list) get-player-spawn))))
      (begin
        (send (car player-list) reset-points)
        (send (car player-list) revive-player)
        (send (car player-list) change-player-position start-x start-y)
        (send (car player-list) change-player-velocity 0)
        (reset-world (cdr player-list))))))


(define round-timer
  (new timer%
       [notify-callback end-round]))

(define (printer number dc result-list height)
  (if (null? result-list) 
      (void)
      (begin
        (send dc draw-bitmap victory-image 0 0)
        (send dc draw-text (number->string number) 660 height) 
        (send dc draw-text (car (car result-list)) 700 height)
        (send dc draw-text (number->string (cdr (car result-list))) 820 height)
        (printer (+ number 1) dc (cdr result-list) (+ height 50)))))


; Resultat loop som returnerar en lista med par i (list (player . score)) 

(define (result-table player-list result-list)
  
  ; En intern loop som tar in en spelare och en resulatlista och lägger in spelaren på rätt plats
  (define (result-loop player result-list)
    (let ((player-score (send player get-player-points))
          (player-color (send player get-player-color)))
      (cond ((null? result-list) (list (cons player-color player-score)))
            ((< (cdr (car result-list)) player-score) (cons (cons player-color player-score) result-list))
            (else (cons (car result-list) (result-loop player (cdr result-list)))))))
  
  ; loopar igenom alla spelare tills alla spelare finns i result-list
  (if (null? player-list) 
      result-list
      (result-table (cdr player-list) (result-loop (car player-list) result-list))))



; --------- timer återuppliv ------------


; Funktion som kollar om spelarna är döda och hur länge de isåfall har varit det.
; Om det har gått 5 tick (sekunder) sen spelaren dog så respawnar spelaren.
; Funktionen loopar igenom player-list och kollar alla spelare samtidigt. 

(define (respawn-players)
  (define (player-loop player-list)
    (cond ((null? player-list) (void))
          ((= 0 (send (car player-list) get-player-status))
           (begin
             (if (>= (send (car player-list) get-player-death-time) 3)
                 (begin
                   (send (car player-list) revive-player)
                   (send (car player-list) remove-player-death-time))
                 (send (car player-list) add-player-death-time))
             (player-loop (cdr player-list))))
          (else (player-loop (cdr player-list)))))
  (player-loop player-list))

(define (one-sec-timer-function)
  (begin
    (respawn-players)
    (in-game-clock)))
    

(define one-sec-timer
  (new timer% 
       [notify-callback one-sec-timer-function]))


; Menyrad för timer och poäng

; timer

(define (in-game-clock)
  (set! round-time (- round-time 1000)))

          
          

; ----------- definierar Canvas -------------


(define game-canvas%
  (class canvas%
    (init-field [keyboard-handler 0])
    (define/override (on-char key-event)
      (keyboard-handler key-event))
    (super-new)))



;---------- Grafik ----------------

(define (render self dc)
  (begin
    (send dc draw-bitmap background 0 0)
    (render-groundlevel dc)
    (render-platforms dc)
    (render-players dc player-list)
    (render-game-status dc)))

; Rita ut statusrad

(define (render-game-status dc)
  (begin
    (send dc set-font big-font)
    (send dc draw-text (number->string (/ round-time 1000)) 710 15)
    (game-status-player-points dc player-list 200)))

(define (game-status-player-points dc player-list width)
  (if (null? player-list)
      (void)
      (let ((player-color (send (car player-list) get-player-color))
            (player-points (send (car player-list) get-player-points))
            (height 100))
        (begin
          (send dc set-font normal-font)
          (send dc draw-text player-color width height)
          (send dc draw-text (number->string player-points) (+ width 100) height)
          (game-status-player-points dc (cdr player-list) (+ width 200))))))



; Rendera Marknivån

(define (render-groundlevel dc)
  (begin
    (send dc set-brush (make-object brush% "black" 'solid))
    (send dc draw-rectangle 100 850 1300 50)))
  

(define (render-platforms dc)
  (define (platform-loop platform-list)
    (if (null? platform-list)
        (void)
        (let ((platform-left-edge (car (send (car platform-list) get-platform-width)))
              (platform-right-edge (cdr (send (car platform-list) get-platform-width)))
              (platform-lower-height (car (send (car platform-list) get-platform-height)))
              (platform-upper-height (cdr (send (car platform-list) get-platform-height))))
          (begin
            (send dc set-brush (make-object brush% "brown" 'solid))
            (send dc draw-rectangle
                  (car (translate platform-left-edge 0))
                  (cdr (translate 0 platform-upper-height))
                  (- platform-right-edge platform-left-edge)
                  (- platform-upper-height platform-lower-height))
            (platform-loop (cdr platform-list))))))
  (platform-loop platform-list))




; Rendera spelare


(define (render-players dc player-list)
  (if (null? player-list)
      (void)
      (let ((new-x (car (translate (car (send (car player-list) get-player-position)) 0)))
            (new-y (cdr (translate 0 (cdr (send (car player-list) get-player-position)))))
            (playercolor (send (car player-list) get-player-color)))
      (begin
        (send dc set-brush (make-object brush% playercolor 'solid))
        (send dc draw-ellipse (- new-x (/ diameter 2)) (- new-y diameter) diameter diameter)
        (render-players dc (cdr player-list))))))
  
          


; ----------- Översättning i koordinater -----------
 ; tar in två värden, returnerar ett par '(x . y)

(define (translate x y)
  (cons (+ 100 x) (- 850 y))) 


(send start-window show #t)


; ---------- Kontroller ------------------


(define (handle-key-event key-event)
  (let ((key (send key-event get-key-code)))

      (if (eq? key 'release)
          (remove-pressed-button (send key-event get-key-release-code))
          (add-pressed-button key))))



(define (controller)
  
  (define (player-loop player-list)
    (if (null? player-list)
        (void)
        (begin
          (pressed-key-loop (car player-list) pressed-buttons)
          (player-loop (cdr player-list)))))
  
  (define (pressed-key-loop player pressed-buttons)
    (let ((left-key (car (send player get-player-keys)))
          (right-key (cdr (send player get-player-keys))))
      
      (cond ((null? pressed-buttons) (void))
            ((eq? left-key (car pressed-buttons)) (send player go-left))
            ((eq? right-key (car pressed-buttons)) (send player go-right))
            (else (pressed-key-loop player (cdr pressed-buttons))))))
  
  (player-loop player-list))


; Ändra kontroller

(define (controller-initiation text-field-list available-players) 
  (if (null? text-field-list) 
      (void)
      (let ((text-value-left (car (string->list (send (car text-field-list) get-value))))
            (text-value-right (cadr (string->list (send (car text-field-list) get-value)))))
        (begin 
          (send (car available-players) change-player-key-left text-value-left)
          (send (car available-players) change-player-key-right text-value-right)
          (controller-initiation (cdr text-field-list) (cdr available-players))))))

; Testar om det är korrekt input på kontroller #t/#f
; kollar först om text-field är tom sedan om det är minst två element i value

(define (check-input-controllers text-field-list)
  (cond ((null? text-field-list) #t)
        ((null? (string->list (send (car text-field-list) get-value))) #f)
        ((null? (cdr (string->list (send (car text-field-list) get-value)))) #f)
        (else (check-input-controllers (cdr text-field-list)))))



; Lägger till och tar bort knappar i vilka som är nertryckta

(define pressed-buttons
  null)

(define (button-exist? key button-list)
  (cond ((null? button-list) #f)
        ((eq? key (car button-list)) #t)
        (else (button-exist? key (cdr button-list)))))

(define (add-pressed-button key)
  (if (button-exist? key pressed-buttons)
      (void)
      (begin
        (set! pressed-buttons (cons key pressed-buttons)))))

(define (remove-pressed-button key)
  (define (remove-key-loop key button-list)
    (cond ((null? button-list) null)
          ((eq? key (car button-list)) (cdr button-list))
          (else (cons (car button-list) (remove-key-loop key (cdr button-list))))))
  (set! pressed-buttons (remove-key-loop key pressed-buttons)))






; ---------- definerat game-canvas --------------

(define game-canvas (new game-canvas%
                         [parent world-window]
                         [paint-callback render]
                         [keyboard-handler handle-key-event]))

