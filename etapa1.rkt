#lang racket
(require racket/match)

(provide (all-defined-out))

(define ITEMS 5)

;; C1, C2, C3, C4 sunt case într-un magazin.
;; C1 acceptă doar clienți care au cumpărat maxim ITEMS produse
;; (ITEMS este definit mai sus).
;; C2 - C4 nu au restricții.
;; Considerăm că procesarea fiecărui produs la casă durează un minut.
;; Casele pot suferi întârzieri (delay).
;; La un moment dat, la fiecare casă există
;; 0 sau mai mulți clienți care stau la coadă.
;; Timpul total (tt) al unei case reprezintă
;; timpul de procesare al celor aflați la coadă,
;; adică numărul de produse cumpărate de ei +
;; întârzierile suferite de casa respectivă (dacă există).
;; Ex:
;; la C3 sunt Ana cu 3 produse și Geo cu 7 produse,
;; și C3 nu are întârzieri => tt pentru C3 este 10.


; Definim o structură care descrie o casă prin:
; - index (de la 1 la 4)
; - tt (timpul total descris mai sus)
; - queue (coada cu persoanele care așteaptă)
(define-struct counter (index tt queue) #:transparent)


; TODO 1 (10p)
; Implementați o funcție care întoarce o structură counter goală.
; tt este 0 si coada este vidă.
; Obs: la definirea structurii counter se creează automat
; o funcție make-counter pentru a construi date de acest tip
(define (empty-counter index)
  (make-counter index 0 '()))


; TODO 2 (10p)
; Implementați o funcție care crește tt-ul unei case
; cu un număr dat de minute.
(define (tt+ C minutes)         ;--------------------intoarce tt-------;
  (struct-copy counter C [tt (+ (match C [(counter index tt queue) tt]) minutes)]))


; TODO 3 (20p)
; Implementați o funcție care primește o listă nevidă 
; de case și întoarce o pereche dintre:
; - indexul casei (din listă) care are cel mai mic tt
; - tt-ul acesteia
; Obs: când mai multe case au același tt,
; este preferată casa cu indexul cel mai mic
; RESTRICȚII (20p):
;  - Folosiți recursivitate pe coadă.
(define (helper counters acc )
  (cond
    ((null? counters) acc ) ; base case , acc -> cea mai buna pereche gasita pana acum
    ((> (counter-tt (car counters)) (cdr acc))
          (helper (cdr counters) acc ));it s not better, i keep mine
    ((and (= (counter-tt (car counters)) (cdr acc)) (< (counter-index (car counters)) (car acc)))
           (helper (cdr counters) (cons (counter-index (car counters))  (counter-tt (car counters))) ) )
    ((and (= (counter-tt (car counters)) (cdr acc)) (> (counter-index (car counters)) (car acc) ))
           (helper (cdr counters) acc ) ) ;equal but mine is better so i keep mine
    (else (helper (cdr counters) (cons (counter-index (car counters)) (counter-tt (car counters))) )) ) ; smaller so i take their id 
            
  )
(define (min-tt counters) ; return (idx tt)
  (helper counters (cons (counter-index (car counters)) (counter-tt (car counters)) )) ) ;apelez counter 
                        ; C1-index                        C1-tt

; TODO 4 (20p)
; Implementați aceeași funcționalitate de mai sus,
; cu recursivitate pe stivă.
; RESTRICȚII (20p):
;  - Folosiți recursivitate pe stivă.
(define (min-tt-stack counters) ; (idx . tt ) 
   (cond
    ((null? counters ) (cons null null) ) ; base case
    ((null? (cdr counters)) (cons (counter-index (car counters))  (counter-tt (car counters))) )
    ((> (counter-tt (car counters)) (cdr (min-tt-stack (cdr counters))))
              (min-tt-stack (cdr counters))); mai mare keep going 
    ((and (= (counter-tt (car counters)) (cdr (min-tt-stack (cdr counters))) ) (< (counter-index (car counters)) (car (min-tt-stack (cdr counters))) ))
              (cons (counter-index (car counters))  (counter-tt (car counters)))  ) ;keep it 
    ((and (= (counter-tt (car counters)) (cdr (min-tt-stack (cdr counters))) ) (> (counter-index (car counters)) (car (min-tt-stack (cdr counters))) ))
             (min-tt-stack (cdr counters)) ) ;keep going 
    (else (cons (counter-index (car counters)) (counter-tt (car counters)))  ) ) ; smaller  , keep it 
  )            


; TODO 5 (10p)
; Implementați o funcție care adaugă o persoană la o casă.
; C = casa, name = numele persoanei,
; n-items = numărul de produse cumpărate
; Veți întoarce o nouă structură obținută prin așezarea perechii
; (name . n-items) la sfârșitul cozii de așteptare.
(define (add-to-counter C name n-items)
  (struct-copy counter C [tt (+ (counter-tt C) n-items)] [queue (append (counter-queue C) (list(cons name n-items)))]))
                                                         ;(name . n-items)

; TODO 6 (50p)
; Implementați funcția care simulează fluxul clienților pe la case.
; requests = listă de cereri care pot fi de 2 tipuri:
; - (<name> <n-items>) - așază persoana <name> la coadă la o casă
; - (delay <index> <minutes>) - întârzie casa <index> cu <minutes> minute
; C1, C2, C3, C4 = structuri corespunzătoare celor 4 case
; Sistemul procesează cererile în ordine, astfel:
; - așază persoana la casa cu tt minim la care are voie
;   (conform logicii implementate de min-tt)
; - când o casă suferă o întârziere, tt-ul ei crește
(define (serve requests C1 C2 C3 C4)
  
  ; Puteți să vă definiți aici funcții ajutătoare (define în define)
  ; - avantaj: aveți acces la variabilele
  ;   requests, C1, C2, C3, C4 fără a le retrimite ca parametri
  ; Puteți să vă definiți funcții ajutătoare în exteriorul lui "serve"
  ; - avantaj: puteți testa fiecare funcție imediat ce ați implementat-o
  ; Nu este obligatoriu să definiți funcții ajutătoare.
  (define (turn_counters A1 A2 A3 A4)
    (list A1 A2 A3 A4))
  
  (define (find-counter index)
  (cond
    ((= 1 index) C1)
    ((= 2 index) C2)
    ((= 3 index) C3)
    (else C4)
    ))

  (if (null? requests)
      (list C1 C2 C3 C4)
      (match (car requests)
        [(list 'delay index minutes)
         (cond
           ((= 1 index) (serve (cdr requests) (tt+ C1 minutes) C2 C3 C4 )) ; delay1
           ((= 2 index) (serve (cdr requests) C1 (tt+ C2 minutes) C3 C4 ) ) ; delay2 
           ((= 3 index) (serve (cdr requests) C1 C2 (tt+ C3 minutes) C4 ) ) ;delay3
           (else  (serve (cdr requests) C1 C2 C3 (tt+ C4 minutes)) ); delay4
           )]
        [(list name n-items)
          (if (and (= 1 (car (min-tt (turn_counters C1 C2 C3 C4)))) (<= n-items ITEMS)) ; min-tt->idx este 1 ? pot pune la 1 ?
              (serve (cdr requests) (add-to-counter C1 name n-items) C2 C3 C4)
              (cond
               ((= 2 (car (min-tt (cdr (turn_counters C1 C2 C3 C4)))) )
                         (serve (cdr requests) C1 (add-to-counter (find-counter (car (min-tt (cdr (turn_counters C1 C2 C3 C4))))) name n-items) C3 C4) )
               ((= 3 (car (min-tt (cdr (turn_counters C1 C2 C3 C4)))))
                         (serve (cdr requests) C1 C2 (add-to-counter (find-counter (car (min-tt (cdr (turn_counters C1 C2 C3 C4))))) name n-items) C4) )
               (else  (serve (cdr requests) C1 C2 C3 (add-to-counter (find-counter (car (min-tt (cdr (turn_counters C1 C2 C3 C4))))) name n-items)) ) ; it s 4 
               )
              )
         ])))
