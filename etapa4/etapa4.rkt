#lang racket
(require racket/match)
(require "queue.rkt")

(provide (all-defined-out))

(define ITEMS 5)

(define-struct counter (index tt et open queue) #:transparent) ; added by me 

; TODO (0p)
; Aveți libertatea să vă structurați programul cum doriți
; (dar cu restricțiile de mai jos), astfel încât
; funcția serve să funcționeze conform specificației.
; 
; Restricții (impuse de checker):
; - va exista în continuare funcția (empty-counter index)
; - veți reprezenta cozile folosind noul TDA queue
(define (empty-counter index)
  (make-counter index 0 0 1 empty-queue )) ; the only function in aceasta etapa

;============================================================
;----------------mostenite din etapele anterioare------------
(define (update f counters index) ; aplica functia f counetului cu indexul index
  (map (λ(C)
            (if (= index (counter-index C))
                (f C)
                C)
                 ) counters ))
;-----------------------------------------

(define tt+
  (λ(C)
    (λ(minutes)                    
      (struct-copy counter C [tt (+ (match C [(counter index tt et open queue) tt]) minutes)])
    )) )

;---------------------------------------
(define et+
  (λ(C)
    (λ(minutes)                     
      (struct-copy counter C [et (+ (match C [(counter index tt et open queue) et]) minutes)])
    )))

;---------------------------------------
(define ((add-to-counter name items) C) ; testată de checker
  
        (if (queue-empty? (counter-queue C)) ; daca acum adaug primul client (coada==null ) ->first condition -> et=0+first_client_n-items
                                 ;----------- tt=tt+n-items--;  ;----et=et+first(n-items)-----; ;-------------modify queue----------------------------------;
         (struct-copy counter C [tt (+ (counter-tt C) items)] [et (+ (counter-et C) items)] [queue (enqueue (cons name items) (counter-queue C))])
                                 ;----------- tt=tt+n-items--;  ;-------------modify queue----------------------------------;
         (struct-copy counter C [tt (+ (counter-tt C) items)] [queue (enqueue (cons name items) (counter-queue C))]) )
        )                      ; nu modificați signatura!
    
;----------------------------------------------------------

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

;------------------------------------------------------------

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
;------------------------------------------------------------

(define ((pass-time-through-counter minutes) C) ;todo7 etapa3
  (if (< minutes (counter-et C))
      (struct-copy counter C [tt (- (counter-tt C) minutes) ] [et  (- (counter-et C) minutes) ] )
      (if (>= minutes (counter-tt C))
         (struct-copy counter C [tt 0 ] [et  0 ] )
         (struct-copy counter C [tt (- (counter-tt C) minutes) ] [et  0 ] )
         )
      
  )
  )
  
;----------------mostenite din etapele anterioare------------
;============================================================

; TODO 7 (70p)
; Implementați funcția care simulează fluxul clienților pe la case.
; ATENȚIE: Față de etapa 3, apar modificări în:
; - formatul listei de cereri (requests)
; - formatul rezultatului funcției (explicat mai jos)
; requests conține 6 tipuri de cereri:
;   4 moștenite din etapa 3:
;   - (<name> <n-items>) - așază persoana <name> la coadă la o casă deschisă
;   - (delay <index> <minutes>) - întârzie casa <index> cu <minutes> minute
;   - (ensure <average>) - cât timp tt-ul mediu al caselor deschise depășește 
;                          <average>, adaugă case fără restricții (case slow)
;   - <x> - actualizează starea caselor conform cu trecerea a <x> minute
;           de la ultima cerere (afectează câmpurile tt, et, queue)
;   plus 2 noi:
;   - (close <index>) - închide casa cu indexul <index> (casa există deja)
;   - (open <index>) - deschide casa cu indexul <index> (casa există deja)
; Sistemul procesează cererile în ordine, astfel:
; - așază persoana la casa DESCHISĂ cu tt minim la care are voie;
;   se garantează că persoana poate fi distribuită la o casă
; - nicio modificare pentru situația când o casă suferă o întârziere
; - dacă tt-ul mediu pentru toate casele DESCHISE > <average>,
;   adaugă case slow până când media <= <average>
; - nicio modificare în modelarea trecerii timpului
; - o casă care se închide nu mai primește clienți noi și:
;   - primul client (dacă există) își continuă treaba la această casă
;   - restul clienților se redistribuie la celelalte case,
;     în ordinea în care erau așezați la coadă
; - o casă care se deschide redevine disponibilă pentru clienți
; Funcția serve întoarce o pereche cu punct între:
; - lista clienților care au părăsit magazinul, sortată cronologic
;   - elementele listei au forma (index_casă . nume)
;   - când mai mulți clienți ies simultan, sortați după indexul casei
; - lista cozilor nevide în starea finală, sortată după indexul casei
;   - elementele listei au forma (index_casă . coadă) (coada este de tip queue)
;(define (serve requests fast-counters slow-counters)
;  'your-code-here)
;--------HELPERS ETAPA 4--------------------------------------------------------
(define (obtain-counter counters idx)  ;--->dau index iau counter 
   (car
    (filter (λ(C) ;se obtine un counter din acest filter 
                         (if (= (counter-index C) idx ) ; daca e counterul cu index
                            #t ; pastrez elementul 
                            #f ; nu pastrez elementul 
                          )) counters ))
   )
;--------------------------------------------------------------------------------
(define (filter-counters counters); daca counterul are coada goala , elimina l
  (filter (λ(C) (if (queue-empty? (counter-queue C));daca are coada goala
                    #f; nu l luam
                    #t; il luam
                 )) counters)
  )

(define (obtain-cozi counters);->dau counters iau lista cozilor    ( idx . coada ) 
  (if (null? counters); daca nu mai am elem 
      null
      (cons (cons (counter-index (car counters)) (counter-queue (car counters))) (obtain-cozi (cdr counters)))
      )
  )
;--------------------------------------------------------------------------------
(define (stream->list str)
  ( if (stream-empty? str)
       null
       (cons (stream-first str) (stream->list (stream-rest str)))
   )
  )

(define (get-rest-coada q);lista
  ;(cdr (stream->list (rotate (queue-left q) (queue-right q) empty-stream)))
  ;(struct-copy queue q [left (rotate (queue-left q) (queue-right q) empty-stream )]  [right null] [size-l (+ (queue-size-r q) (queue-size-l q))]  [size-r 0])
  (if (queue-empty? q)
      q
      (dequeue q))
  )
;================================================================================
(define (serve requests fast-counters slow-counters)     ;----> APEL FUNCTIE PRINCIPALA 
   (serve-help '() requests fast-counters slow-counters ) )
;=================================================================================
;----HELPERS-----

 (define (obtain-removed-person counters idx)  ;--->HELPER 
   (let ( (ctr (car(filter (λ(C) ;se obtine un counter din acest filter 
                         (if (= (counter-index C) idx ) ; daca e counterul de la care scot persoana 
                            #t ; pastrez elementul 
                            #f ; nu pastrez elementul 
                          )) counters )) ) )
     (cons (counter-index ctr) (car (top (counter-queue ctr))))) ; idx . persoana 
   )
;----OBTIN LISTA ^^^^----------------------------------------------------------

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
           ;(just-modify (- x (cdr (min-et fara-zero))) (map (λ(C)((pass-time-through-counter (cdr(min-et fara-zero)))C)) (eliminate counters (car (min-et fara-zero))) )  )
           (just-modify (- x (cdr (min-et fara-zero))) (eliminate (map (λ(C) ((pass-time-through-counter (cdr (min-et fara-zero))) C) ) counters ) (car(min-et fara-zero)))  )
    )))
 ;----OBTIN COUNTERS MODIFICATE ^^^----------------------------------------------
 ;-------------------------------------etapa 3 ^

 ;===============================================================================
 ;--SMALL HELPERS----etapa3
 (define (obtain-list cters)
  (map (λ(C)
         (counter-tt C)) (filter (λ(C) (if (= 0 (counter-open C)) ;daca e closed
                                           #f ; nu pastreaza ce e closed
                                           #t ; e open il pastrez 
                                           )) cters)  ) ; va genera lista (tt1 tt2 tt3...) 
  )

(define (sum-tt cters) ; calculeaza suma pt counters deschide 
  (foldr + 0 (obtain-list cters) )
  )
;----SMALL HELPERS----etapa4
(define (sum-help q)
  (let ((lista (append (stream->list (queue-left q)) (reverse (queue-right q))))) ; reconstruiesc coada ca si lista 
    (map (λ(pair) (cdr pair )) (cdr lista)); va genera items1 items2....  pari ( nume . item ) 
    )
  )

(define (sum-items q); trbuie sa obtin sum n-items dintr o coada
   (foldr + 0 (sum-help q) ) ; am obtinut restul cozii sub forma de lista 
  )
;----------------------------
(define (get-open counters acc); gaseste nr de open counters
  (if (null? counters)
      acc
     (if (= 1 (counter-open (car counters))); daca e deschis
        (get-open (cdr counters) (+ 1 acc))
        (get-open (cdr counters) (+ 0 acc))))
  )
;(define (le-ce-impart counters); gaseste nr de open counters
;  (flodr + 0 (get-open counters))
;  )
;=======================================================================================
;=======================================================================================
(define (serve-help gone requests fast-counters slow-counters); MAIN function dont modify

  ;---------------------------------------recursive function-----------------------------
  (define (add-counters total cters now div nr) ;toal=sum, cters=slow cters , now=last_idx , nr=nr dat
   (if (<= (/ total div ) nr)
        cters ; as it is initially 
        (add-counters total (append cters (list (empty-counter (+ 1 now))) ) (+ 1 now) (+ 1 div) nr) 
         )
  );-------------------------------OBTAIN FINAL LIST---------------------------------------

  (define (obtain-final-list gone x counters ) ; modific counters dupa trecera a x minute si obtin lista clientilor care au iesit ; MAIN
    (let ((fara-zero (filter (λ(C) (not(queue-empty? (counter-queue C))) ) counters ) ) )
    ( if (null? fara-zero)  ; HERE I NEED APPEND DE FAST AND SLOW 
          gone                                                                                                                                      ;(eliminate counters (car (min-et counters)))
          (if ( < x (cdr (min-et fara-zero)))
            gone
            (obtain-final-list (append gone (list(obtain-removed-person counters (car (min-et fara-zero )) ))) (- x (cdr (min-et fara-zero))) (eliminate (map (λ(C) ((pass-time-through-counter (cdr (min-et fara-zero))) C) ) counters ) (car(min-et fara-zero)))  )
          )
    ))
   )
  ;---------------------------------------------------------------------------------------
  ;-etapa4--------------------------------------------------------------------------------
  (define (distribute q fast-counters slow-counters); ia fiecare element din queue si l pune unde trebuie, returneaza fast si slow modificati
   
    (if (queue-empty? q) ; (car(top q))-name (cdr(top q))-n_items
        (cons fast-counters slow-counters) ;( fast-counters . slow-counters )<-- ASTA !!!!!
        (let ((open-fast (filter (λ(C) (if (= 0 (counter-open C)) ; ONLY OPEN DIN FAST 
                                           #f
                                           #t
                                           )) fast-counters) )
              (open-slow (filter (λ(C) (if (= 0 (counter-open C)) ; ONLY OPEN DIN SLOW
                                           #f
                                           #t
                                           )) slow-counters)))
          (cond
            ((null? open-fast) ; trimitem pe slow 
              (distribute (dequeue q) fast-counters  (update (λ(C) ((add-to-counter (car(top q)) (cdr(top q)) )C) ) slow-counters (car (min-tt open-slow))) ))
            ((null? open-slow) ; trimitem pe fast
              (distribute (dequeue q) (update (λ(C) ((add-to-counter (car(top q)) (cdr(top q)) )C) ) fast-counters (car (min-tt open-fast))) slow-counters ))
            ( (and (<=(cdr(top q)) ITEMS) (<= (cdr(min-tt open-fast)) (cdr(min-tt open-slow))) ) 
              (distribute (dequeue q) (update (λ(C) ((add-to-counter (car(top q)) (cdr(top q)) )C) ) fast-counters (car (min-tt open-fast))) slow-counters ) ;min-tt of fast  
               )
            (else (distribute (dequeue q) fast-counters  (update (λ(C) ((add-to-counter (car(top q)) (cdr(top q)) )C) ) slow-counters (car (min-tt open-slow))) )); go to min-tt of slow counters 
              ))
     ))
    
  
  
  ;-etapa4--------------------------------------------------------------------------------
  ;------------------MAIN-----------------------------------------------------------------
  
  (if (null? requests) ; <-------HERE STARTS THE PROGRAM  
     
      (cons gone (append (obtain-cozi (filter-counters fast-counters)) (obtain-cozi (filter-counters slow-counters))))  ;----BASE CASE---- : LEFT . cozi-COUNTERS(1&2)
      (match (car requests)
         [(list 'ensure nr) ; ensure average -> va lua doar ce nu e closed 
          (if (> (/ (sum-tt (append fast-counters slow-counters)) (get-open (append fast-counters slow-counters) 0) ) nr)
              (serve-help gone (cdr requests) fast-counters (add-counters (sum-tt (append fast-counters slow-counters))
                                                                          slow-counters
                                                                          (counter-index (car(reverse(append fast-counters slow-counters))))
                                                                          (get-open (append fast-counters slow-counters) 0)
                                                                          nr) );add counters and SERVE
              (serve-help gone (cdr requests) fast-counters slow-counters ); just SERVE 
           )
         ]
        
        [(list 'delay index minutes) ; delay 
            (if (< index (counter-index(car slow-counters))); daca idx ul e mai mic decat indexul primului counter din slow-counters
                                      ;--------modify fast-counters----------------------------------------;
                (serve-help gone (cdr requests) (update (λ(C) ((et+ ((tt+ C) minutes) ) minutes) )fast-counters index) slow-counters );-> modifc fast-counters
                (serve-help gone (cdr requests) fast-counters (update (λ(C) ((et+ ((tt+ C) minutes) ) minutes) ) slow-counters index) );-> else modifc slow-counters
                )                                   ;---------------------modify slow-counters----------------------------;
            
           ]
        
      ;  [(list name n-items) ; name n-items
      ;    (cond  
      ;         ( (and (<= n-items ITEMS) (< (cdr(min-tt fast-counters)) (cdr(min-tt slow-counters))) ) 
      ;               (serve-help gone (cdr requests) (update (λ(C) ((add-to-counter name n-items)C) ) fast-counters (car (min-tt fast-counters))) slow-counters ) ;min-tt of fast  
      ;               )
      ;         ( (and (<= n-items ITEMS) (= (cdr(min-tt fast-counters)) (cdr(min-tt slow-counters))) ) 
      ;               (serve-help gone (cdr requests) (update (λ(C) ((add-to-counter name n-items)C) ) fast-counters (car (min-tt fast-counters))) slow-counters ) ;min-tt of fast  
      ;               )
      ;         (else (serve-help gone (cdr requests) fast-counters  (update (λ(C) ((add-to-counter name n-items)C) ) slow-counters (car (min-tt slow-counters))) )); go to min-tt of slow counters 
      ;        )                                     ;-------------------------------------------------------------------------------------------;
      ;   ]
      ;  [x ;(list x)
      ;   (if (> (cdr(min-et (append fast-counters slow-counters ))) x) ; nu iese niciun client 
      ;      (serve-help gone (cdr requests) (map (pass-time-through-counter x) fast-counters) (map (pass-time-through-counter x) slow-counters)) ; doar trece timpul
      ;       ;altfel trebuie sa fac timpul sa treaca + sa scot clienti -> sa i adaug in lista
      ;      (let* ((union (append fast-counters slow-counters))
      ;             (updated-gone (obtain-final-list gone x union)) )
      ;        ;(obtain-final-list null x union)
      ;        (serve-help updated-gone (cdr requests) (just-modify x fast-counters) (just-modify x slow-counters))
      ;        )
      ;      )
      ;   ]
        [(list 'close index) ; close index -> close & redistribuire 
          (if (< index (counter-index(car slow-counters))); daca idx ul e mai mic decat indexul primului counter din slow-counters-> stiu unde sa fac close                     
               (let* (( qf (counter-queue (obtain-counter fast-counters index)) ) ;coada care va trebui distribuita (completa)
                      ;(lista (stream->list (rotate (queue-left qf) (queue-right qf) empty-stream)))
                       (lista (append (stream->list (queue-left qf)) (reverse (queue-right qf))))
                      ) ;lista din coada 
                   
                 
                 (if (or (null? lista) (null? (cdr lista))) ;daca nu am ce sa distribui  (am un client) ( 0 clienti )
                     (serve-help gone (cdr requests) (update (λ(C) (struct-copy counter C [open 0]) )fast-counters index) slow-counters );doar inchid, fara sa modific tt pt ca nu am ce sa scad din el 
                     (let ( (distributed (distribute (get-rest-coada qf)
                                               (update (λ(C) (struct-copy counter C
                                                                          [tt (- (return-tt C) (sum-items qf))]
                                                                          [open 0]
                                                                          [queue (enqueue (top(counter-queue C)) empty-queue)]
                                                                          ) )fast-counters index)
                                               slow-counters)));FAST 
                      (serve-help gone (cdr requests) ;-> close(update) in fast-counters (modific & distribui )
                            (car distributed)
                            (cdr distributed)
                             )))
                )
             
                     
               (let* (( qs (counter-queue (obtain-counter slow-counters index)) ) ;coada care va trebui distribuita (completa)
                      ;(lists (stream->list (rotate (queue-left qs) (queue-right qs) empty-stream)) )
                       (lists (append (stream->list (queue-left qs)) (reverse (queue-right qs))))
                       ) ; lista 
                 
                 (if (or (null? lists) (null? (cdr lists))) ; daca nu am ce distribui (1 client ) or (0 clienti ) 
                      (serve-help gone (cdr requests) fast-counters (update (λ(C) (struct-copy counter C [open 0]) ) slow-counters index) );doar inchid
                      (let ((distrib (distribute (get-rest-coada qs)
                                          fast-counters
                                          (update (λ(C) (struct-copy counter C
                                                                     [tt (- (return-tt C) (sum-items qs))]
                                                                     [open 0]
                                                                     [queue (enqueue (top(counter-queue C)) empty-queue)]
                                                                     ) ) slow-counters index) )));SLOW
                       (serve-help gone (cdr requests) ;-> else close(update) in slow-counters (modific &distribui)
                            (car distrib) ; fast 
                            (cdr distrib) ; slow 
                            ))
                ))
             
             
               )  
         ]        
        [(list 'open index) ; open index -> just open
          (if (< index (counter-index(car slow-counters))); daca idx ul e mai mic decat indexul primului counter din slow-counters                      
                (serve-help gone (cdr requests) (update (λ(C) (struct-copy counter C [open 1]) )fast-counters index) slow-counters );-> modifc fast-counters
                (serve-help gone (cdr requests) fast-counters (update (λ(C) (struct-copy counter C [open 1]) ) slow-counters index) );-> else modifc slow-counters
                )  
         ]
        
         [(list name n-items) ; name n-items -> nu trebuie sa l pun la vreo casa closed ! 
          (let ((open-fast (filter (λ(C) (if (= 0 (counter-open C))
                                           #f
                                           #t
                                           )) fast-counters) )
                (open-slow (filter (λ(C) (if (= 0 (counter-open C)) ;closed
                                           #f
                                           #t
                                           )) slow-counters)))
            (cond
                ((null? open-fast)
                  (serve-help gone (cdr requests) fast-counters  (update (λ(C) ((add-to-counter name n-items)C) ) slow-counters (car (min-tt open-slow))) ))
                ((null? open-slow)
                  (serve-help gone (cdr requests) (update (λ(C) ((add-to-counter name n-items)C) ) fast-counters (car (min-tt open-fast))) slow-counters ))
                (else
                  (cond  
                     ( (and (<= n-items ITEMS) (< (cdr(min-tt open-fast)) (cdr(min-tt open-slow))) ) 
                        (serve-help gone (cdr requests) (update (λ(C) ((add-to-counter name n-items)C) ) fast-counters (car (min-tt open-fast))) slow-counters ) ;min-tt of fast  
                         )
                     ( (and (<= n-items ITEMS) (= (cdr(min-tt open-fast)) (cdr(min-tt open-slow))) ) 
                        (serve-help gone (cdr requests) (update (λ(C) ((add-to-counter name n-items)C) ) fast-counters (car (min-tt open-fast))) slow-counters ) ;min-tt of fast  
                         )
                     (else (serve-help gone (cdr requests) fast-counters  (update (λ(C) ((add-to-counter name n-items)C) ) slow-counters (car (min-tt open-slow))) )); go to min-tt of slow counters 
                      ))  )
              )
            
                                          
         ]
        [x ;(list x)
         (let ((merge (append fast-counters slow-counters))
              (has-clients (filter (λ(C)(if (queue-empty? (counter-queue C));daca coada e goala
                                     #f; nu l iau 
                                     #t; il iau daca are clienti 
                              )) (append fast-counters slow-counters ))  ) )
         (cond
            ((null? has-clients ) ; nu am nicio coada cu client-> toate cozile sunt goale->nu pot scoate clienti
                (serve-help gone (cdr requests) (map (pass-time-through-counter x) fast-counters) (map (pass-time-through-counter x) slow-counters)))  ; doar trece timpul
            
            ((> (cdr(min-et has-clients)) x)  ; nu iese niciun client pt ca timpul e prea mic 
                (serve-help gone (cdr requests) (map (pass-time-through-counter x) fast-counters) (map (pass-time-through-counter x) slow-counters))) ; doar trece timpul
            
            (else (let* ((union (append fast-counters slow-counters)) ;altfel trebuie sa fac timpul sa treaca + sa scot clienti -> sa i adaug in lista
                   (updated-gone (obtain-final-list gone x union)) )
              (serve-help updated-gone (cdr requests) (just-modify x fast-counters) (just-modify x slow-counters))
              ))
            ))
         ]
    
     
        )))
