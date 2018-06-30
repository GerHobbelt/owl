#| doc
Bisect makes a binary search on sorted data.
|#

;; todo: convert bisect range end to be denoted by returning the end instead of #false

(define-library (owl bisect)

   (export
      bisect             bisect-unsorted
      bisect-range       bisect-range-unsorted
   )

   (import
      (owl defmac)
      (owl math))

   (begin
      ;;;
      ;;; Search for first occurrence in a sorted range
      ;;;

      ; pos or it's close neighbours give the answer
      (define (bisect-fini op lo hi pos last)
         (cond
            ((= pos hi) last)
            ((op pos) ; cannot be lo since it was already tested
               (bisect-fini op lo hi (- pos 1) pos))
            (else
               (let ((next (+ pos 1)))
                  (cond
                     ((= next hi) last)
                     ((op next) next)
                     (else (bisect-fini op lo hi (+ next 1) last)))))))

      ; find the match or it's close neighbour by halving jump to correct direction
      (define (bisect-seek op lo hi pos step last)
         (if (eq? step 1)
            (bisect-fini op lo hi pos last)
            (if (op pos)
               (bisect-seek op lo hi (- pos step) (>> step 1) pos)
               (bisect-seek op lo hi (+ pos step) (>> step 1) last))))

      ; search first position in the range [lo .. hi-1] where (op n) becomes true in O(log_2(hi-lo)) steps
      (define (bisect op lo hi)
         (cond
            ((= hi lo) #false)
            ((> lo hi) (bisect op hi lo))
            ((op lo) lo) ; optional special case
            ((= hi (+ lo 1)) #false) ; ditto
            (else
               (let ((mid (>> (- hi lo) 1)))
                  (bisect-seek op lo hi (+ lo mid) (max 1 (>> mid 1)) #false)))))

      ; trivial O(n) version for unsorted data (or checking the other one)
      (define (bisect-unsorted op lo hi)
         (cond
            ((= lo hi) #false)
            ((op lo) lo)
            (else (bisect-unsorted op (+ lo 1) hi))))


      ;;;
      ;;; Search for range of a value in a sorted interval
      ;;;

      ; -> #false x #false | n x m, where n,m <- [lo .. hi-1] are the endpoints where (get i) = val

      (define (bisect-range get val lo hi)
         (let ((loc (bisect (λ (p) (>= (get p) val)) lo hi)))
            (if (and loc (= (get loc) val))
               (let ((hic (bisect (λ (p) (> (get p) val)) loc hi)))
                  (values loc (if hic (- hic 1) (- hi 1))))
               (values #false #false))))

      ; eww, more code in the naive one...
      (define (bisect-range-unsorted get val lo hi)
         (let loop ((pos lo) (first #false))
            (cond
               ((= pos hi)
                  (if first
                     (values first (- pos 1))
                     (values #false #false)))
               ((= val (get pos)) (loop (+ pos 1) (or first pos)))
               (first (values first (- pos 1)))
               (else (loop (+ pos 1) first)))))))

; note that bisect is more than just an array search. for example:
;
; (define (divide a b) (- (bisect (λ (q) (> (* q b) a)) 0 a) 1))
; (define (square n) (- (bisect (λ (q) (> (* q q) n)) 0 n) 1))
; (define (nth-root i n) (- (bisect (λ (q) (> (expt q n) i)) 0 i) 1)) ; actually hi would be around nbits(i)/n

