#| doc
Default Math

This library exports the usual arbitrary precision bignum arithmetic functions.
You can also use specific (owl math X) libraries if necessary.
|#

(define-library (owl math)

   (export

      ;; from (owl math integer>
      quotient << >>
      band bior bxor fx-width
      ncar ncdr fx-greatest
      zero? integer? truncate/
      even? odd? fixnum? ediv

      ;; from (owl math rational)
      numerator denominator gcd

      ;; full ones
      (exports (owl math complex))

      ;; extra stuff
      (exports (owl math extra)))

   (import
      (owl core)

      ;; integral, exported as such
      (only (owl math integer)
         quotient
         << >>
         band bior bxor
         fx-width
         ncar ncdr fx-greatest
         zero? integer? truncate/
         even? odd? fixnum? ediv)

      ;; rational, exported as such
      (only (owl math rational)
         numerator denominator gcd)

      ;; trailing versions with all cases
      (owl math complex)
      (owl math extra))

   (begin))
