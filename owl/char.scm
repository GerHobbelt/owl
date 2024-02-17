(define-library (owl char)

   (export
      char?
      char=?
      char<?
      char>?
      char<=?
      char>=?
      char->integer
      integer->char)

   (import
      (owl core)
      (owl math))

   (begin

      (define (char? obj)
         (eq? (type obj) type-fix+))

      (define char=? =)
      (define char<? <)
      (define char>? >)
      (define char<=? <=)
      (define char>=? >=)

      (define char->integer self)
      (define integer->char self)
))
