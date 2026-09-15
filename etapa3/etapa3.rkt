#lang racket
(require racket/match)
(require "queue.rkt")

(provide (all-defined-out))

(define ITEMS 5)

;; ATENȚIE: Este necesar să implementați întâi
;;          TDA-ul queue în fișierul queue.rkt.
;; Reveniți la acest fișier după ce ați implementat tipul 
;; queue și ați verificat implementarea folosind checker-ul.


; Structura counter nu se modifică.
; Se modifică însă implementarea câmpului queue:
; - în loc de listă, acesta va fi o structură de tip queue
; - modificarea nu este vizibilă în definiția structurii,
;   ci în implementarea operațiilor tipului counter
(define-struct counter (index tt et queue) #:transparent)


; TODO 6 (20p)
; Actualizați funcțiile de mai jos conform cu 
; noua reprezentare a cozii de persoane.
; Elementele cozii rămân perechi (nume . nr_produse).
; RESTRICȚII (5p per abatere)
;  - Respectați "bariera de abstractizare", adică 
;    operați cu coada folosind exclusiv interfața:
;    - empty-queue
;    - queue-empty?
;    - enqueue
;    - dequeue
;    - top
; Obs: Doar câteva funcții necesită actualizări.
(define (empty-counter index)           ; testată de checker
  (make-counter index 0 0 empty-queue ) )

;----------------------------------------------------------

(define (update f counters index)
  (map (λ(C)
            (if (= index (counter-index C))
                (f C)
                C)
                 ) counters ))
;-----------------------------------------

(define tt+
  (λ(C)
    (λ(minutes)                    
      (struct-copy counter C [tt (+ (match C [(counter index tt et queue) tt]) minutes)])
    )) )

;---------------------------------------
(define et+
  (λ(C)
    (λ(minutes)                     
      (struct-copy counter C [et (+ (match C [(counter index tt et queue) et]) minutes)])
    )))

;---------------------------------------
(define ((add-to-counter name items) C) ; testată de checker
  
        (if (queue-empty? (counter-queue C)) ; daca acum adaug primul client (coada==null ) ->first condition -> et=0+first_client_n-items
                                 ;----------- tt=tt+n-items--;  ;----et=et+first(n-items)-----; ;-------------modify queue----------------------------------;
         (struct-copy counter C [tt (+ (counter-tt C) items)] [et (+ (counter-et C) items)] [queue (enqueue (cons name items) (counter-queue C))])
                                 ;----------- tt=tt+n-items--;  ;-------------modify queue----------------------------------;
         (struct-copy counter C [tt (+ (counter-tt C) items)] [queue (enqueue (cons name items) (counter-queue C))]) )
        )                      ; nu modificați signatura!
    
;---------------------------------------
;(define functie-mai-abstracta-careia-ii-veti-da-un-nume-sugestiv
;  'your-code-here)
;(define min-tt 'your-code-here)
;(define min-et 'your-code-here)

(define ( return-tt C) ; give tt of Counter---HELPER 
  (counter-tt C)
  )

(define ( return-et C) ; give et of Counter---HELPER 
  (counter-et C)
  )

(define (final-minimum f counters acc )
  (cond
    ((null? counters) acc ) ; base case
    ((> (f (car counters)) (cdr acc)) (final-minimum f (cdr counters) acc ));it s not better, i keep mine
    ((and (= (f (car counters)) (cdr acc)) (< (counter-index (car counters)) (car acc))) (final-minimum f (cdr counters) (cons (counter-index (car counters))  (f (car counters))) ) )
    ((and (= (f (car counters)) (cdr acc)) (> (counter-index (car counters)) (car acc) )) (final-minimum f (cdr counters) acc ) ) ;equal but mine is better so i keep mine
    (else (final-minimum f (cdr counters) (cons (counter-index (car counters)) (f (car counters))) )) ) ; smaller so i take their id 
            
  )

(define (min-tt counters)
  (final-minimum return-tt counters (cons (counter-index (car counters)) (counter-tt (car counters)) )) ) ; folosind funcția de mai sus
(define (min-et counters )
  (final-minimum return-et counters (cons (counter-index (car counters)) (counter-et (car counters)) ))) ; folosind funcția de mai sus

;--------------------------------------

;(define (remove-first-from-counter C)   ; testată de checker
;  'your-code-here)

(define (sum q)
  ( if (queue-empty? q)
       0
       (+ (cdr (top q)) (sum (dequeue q)) ))
  )
(define (remove-first-from-counter C);<----
  (let ((q (dequeue (counter-queue C))))
  (if (queue-empty? q)
                            
      (struct-copy counter C [tt 0] [et 0] [queue (dequeue (counter-queue C)) ])
                            
    
      (struct-copy counter C [tt (sum q) ] [et  (cdr (top q))] [queue (dequeue (counter-queue C)) ])
      
   )))
;-----------------------------------


; TODO 7 (10p)
; Implementați o funcție care calculează starea
; unei case după un număr dat de minute.
; Funcția presupune, fără să verifice, că în acest timp
; nu iese nimeni din coadă, deci se modifică
; doar câmpurile tt și et.
; Este responsabilitatea utilizatorului să nu apeleze
; funcția cu minutes > et și coadă nevidă.
; La casele fără clienți, este responsabilitatea
; voastră să nu produceți timpi negativi.
(define ((pass-time-through-counter minutes) C); trece timpul -> minute C
  (if (< minutes (counter-et C)) ; doar scad 
      (struct-copy counter C [tt (- (counter-tt C) minutes) ] [et  (- (counter-et C) minutes) ] )
      (if (>= minutes (counter-tt C)); if it s also greater that tt :  minutes > tt, minutes >= et 
         (struct-copy counter C [tt 0 ] [et  0 ] ) ; 0 0 
         (struct-copy counter C [tt (- (counter-tt C) minutes) ] [et  0 ] ) ; minutes < tt ,   -_ 0 
         )
      
  )
  )
  

; TODO 8 (60p)
; Implementați funcția care simulează fluxul clienților pe la case.
; ATENȚIE: Față de etapa 2, apar modificări în:
; - formatul listei de cereri (requests)
; - formatul rezultatului funcției (explicat mai jos)
; requests conține 4 tipuri de cereri:
;   3 moștenite din etapa 2:
;   - (<name> <n-items>) - așază persoana <name> la coadă la o casă
;   - (delay <index> <minutes>) - întârzie casa <index> cu <minutes> minute
;   - (ensure <average>) - cât timp tt-ul mediu al tuturor caselor depășește 
;                          <average>, adaugă case fără restricții (case slow)
;   plus noutatea:
;   - <x> - actualizează starea caselor conform cu trecerea a <x> minute
;           de la ultima cerere (afectează câmpurile tt, et, queue)
; Obs: Cererile (remove-first) din etapa 2 sunt înlocuite de un mecanism  
; mai sofisticat de a scoate clienții din coadă (pe măsură ce trece timpul).
; Sistemul procesează cererile în ordine, astfel:
; - nicio modificare pentru cererile moștenite din etapa 2
; - când timpul prin sistem avansează cu <x> minute, starea caselor
;   se actualizează pentru a reflecta trecerea timpului;
;   ieșirile clienților din coadă se rețin în ordine cronologică.
; Funcția serve întoarce o pereche cu punct între:
; - lista clienților care au părăsit magazinul, sortată cronologic
;   - elementele listei au forma (index_casă . nume)
;   - când mai mulți clienți ies simultan, sortați după indexul casei
; - lista caselor în starea finală (ca rezultatul din etapele 1 și 2)
; Sugestii:
; - gestionați cronologia folosind în mod repetat funcția min-et 
; - pentru a menține lista clienților plecați, definiți o funcție ajutătoare
; (cu un parametru în plus față de serve), pe care serve doar o apelează.
; RESTRICȚII (5p per abatere)
;  - Folosiți minim un let și un let* (care nu ar putea fi let). (2*5p)
;  - Respectați "bariera de abstractizare" oricând operați cu tipul queue.
;================================================================================
(define (serve requests fast-counters slow-counters)     ;----> APEL FUNCTIE HELPER 
   (serve-help '() requests fast-counters slow-counters ) )

 (define (obtain-removed-person counters idx)  ;--->HELPER 
   (let ( (ctr (car(filter (λ(C) ;se obtine un counter din acest filter 
                         (if (= (counter-index C) idx ) ; daca e counterul de la care scot persoana 
                            #t ; pastrez elementul 
                            #f ; nu pastrez elementul 
                          )) counters )) ) )
     (cons (counter-index ctr) (car (top (counter-queue ctr))))) ; idx . persoana 
   )


  ;----OBTIN LISTA ^^^^
 (define ( eliminate counters idx ) ; scoate primul clinet din counterul_idx -->HELPER 
   (map (λ(C)
          (if (and (= idx (counter-index C)) (not (queue-empty? (counter-queue C)) ))      ; daca e counterul pe care l vreau

              (remove-first-from-counter C)  ; scot primul client 
              C                              ; altfel il las nemodificat 
           )) counters)
   )

 ( define (just-modify x counters); modific counters avand in vedere trecerea timpului    ; MAIN
    (let((fara-zero (filter (λ(C) (not(queue-empty? (counter-queue C))) ) counters ) ) )
        ( if (or (null? fara-zero ) ( < x (cdr (min-et fara-zero)))) ; HERE I ONLY NEED FAST AND SLOW SEPARAT 
           (map (pass-time-through-counter x) counters)
                        ;---------------------------;   ;-----scoate primul client----------------------------
           (just-modify (- x (cdr (min-et fara-zero))) (eliminate (map (λ(C) ((pass-time-through-counter (cdr (min-et fara-zero))) C) ) counters ) (car(min-et fara-zero)))  )
    )))
  ;----OBTIN COUNTERS MODIFICATE ^^^
  ;-------------------------------------etapa 3 ^

 (define (obtain-list cters)
  (map (λ(C) (counter-tt C)) cters ) ; va genera lista (tt1 tt2 tt3...) 
  )

(define (sum-tt cters)
  (foldr + 0  (obtain-list cters) )
  )
;=======================================================================================
;=======================================================================================
(define (serve-help gone requests fast-counters slow-counters); MAIN function dont modify

  ;---------------------------------------recursive function-----------------------------
  (define (add-counters total cters now nr) ; toal=sum, cters=slow cters , now=last_idx , nr=nr dat
   (if (<= (/ total now ) nr)
        cters ; as it is initially 
        (add-counters total (append cters (list (empty-counter (+ 1 now))) ) (+ 1 now) nr) 
         )
  );-------------------------------OBTAIN FINAL LIST---------------------------------------
   ; obtin lista de oameni care au parasit counters pt ca et < x 
  (define (obtain-final-list gone x counters ) ; modific counters dupa trecera a x minute si obtin lista clientilor care au iesit ; MAIN
    (let ((fara-zero (filter (λ(C) (not(queue-empty? (counter-queue C))) ) counters ) ) )
    ( if (null? fara-zero)  ; HERE I NEED APPEND DE FAST AND SLOW -> pt ca scot oameni si din fast si din slow 
          gone                                                                                                                                      ;(eliminate counters (car (min-et counters)))
          (if ( < x (cdr (min-et fara-zero))) ; daca nu mai iese nimeni 
            gone                                  ;----- obtin persoana care a pleacat ---;                                                    ;---- scoate clientii----                          et                     C
            (obtain-final-list (append gone (list(obtain-removed-person counters (car (min-et fara-zero )) ))) (- x (cdr (min-et fara-zero))) (eliminate (map (λ(C) ((pass-time-through-counter (cdr (min-et fara-zero))) C) ) counters ) (car(min-et fara-zero)))  )
          )
    ))
   )
  ;---------------------------------------------------------------------------------------

  
  
  (if (null? requests) ; <-------here starts 
      (cons gone (append fast-counters slow-counters))  ;----BASE CASE---- : LEFT . COUNTERS_stare_finala(1&2)
      (match (car requests)
         [(list 'ensure nr) ; ensure average
          (if (> (/ (sum-tt (append fast-counters slow-counters)) (counter-index (car(reverse(append fast-counters slow-counters)))) ) nr)
              (serve-help gone (cdr requests) fast-counters
                          (add-counters (sum-tt (append fast-counters slow-counters)) slow-counters (counter-index (car(reverse(append fast-counters slow-counters)))) nr) );add counters and SERVE ( modified slow )
              (serve-help gone (cdr requests) fast-counters slow-counters ); just SERVE 
           )
         ]
        
        [(list 'delay index minutes) ; delay 
            (if (< index (counter-index(car slow-counters))); daca idx ul e mai mic decat indexul primului counter din slow-counters
                                                ;--------modify fast-counters----------------------------------------;
                (serve-help gone (cdr requests) (update (λ(C) ((et+ ((tt+ C) minutes) ) minutes) )fast-counters index) slow-counters );-> modifc fast-counters
                (serve-help gone (cdr requests) fast-counters (update (λ(C) ((et+ ((tt+ C) minutes) ) minutes) ) slow-counters index) );-> else modifc slow-counters
                )                                             ;---------------------modify slow-counters----------------------------;
            
           ]
        
        [(list name n-items) ; name n-items
          (cond  ; se poate la fast counters-> si tt ul e mai <= deci adaug la fast 
               ( (and (<= n-items ITEMS) (< (cdr(min-tt fast-counters)) (cdr(min-tt slow-counters))) ) 
                     (serve-help gone (cdr requests) (update (λ(C) ((add-to-counter name n-items)C) ) fast-counters (car (min-tt fast-counters))) slow-counters ) ;min-tt of fast  
                     )
               ( (and (<= n-items ITEMS) (= (cdr(min-tt fast-counters)) (cdr(min-tt slow-counters))) ) 
                     (serve-help gone (cdr requests) (update (λ(C) ((add-to-counter name n-items)C) ) fast-counters (car (min-tt fast-counters))) slow-counters ) ;min-tt of fast  
                     );altfel merg pe slow -> adaug pe slow 
               (else (serve-help gone (cdr requests) fast-counters  (update (λ(C) ((add-to-counter name n-items)C) ) slow-counters (car (min-tt slow-counters))) )); go to min-tt of slow counters 
              )                                     ;-------------------------------------------------------------------------------------------;
         ]
        [x ;(list x)
         (if (> (cdr(min-et (append fast-counters slow-counters ))) x) ; nu iese niciun client -> x e mai mic decat cel mai mic et 
            (serve-help gone (cdr requests) (map (pass-time-through-counter x) fast-counters) (map (pass-time-through-counter x) slow-counters)) ; doar trece timpul si merg mai departe
             ;altfel trebuie sa fac timpul sa treaca + sa scot clienti -> sa i adaug in lista de gone pe cei care au plecat 
            (let* ((union (append fast-counters slow-counters))
                   (updated-gone (obtain-final-list gone x union)) ); cei care au plecat 
              ;(obtain-final-list null x union)
              (serve-help updated-gone (cdr requests) (just-modify x fast-counters) (just-modify x slow-counters))
              )
            )
         ]
     
        )))
