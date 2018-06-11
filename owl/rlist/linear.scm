(define-library (owl rlist linear)
   
   (import
      (owl defmac)
      (owl list)
      (owl list-extra))

   (export
      rnull
      rcons
      rget
      rcar
      rcdr
      rset
      rlen
      rlist
      rfold
      rfoldr
      riter
      riterr
      rmap
      rnull?
      rpair?
      list->rlist
      rlist->list)

   (begin
      (define rnull null)
      (define rcons cons)
      (define rget lget)
      (define rcar car)
      (define rcdr cdr)
      (define rset lset)
      (define (rlist . x) x)
      (define rfold fold)
      (define rfoldr foldr)
      (define (riter x) x)
      (define (riterr x) (reverse x))
      (define rmap map)
      (define rnull? null?)
      (define rpair? pair?)
      (define (list->rlist x) x)
      (define (rlist->list x) x)
      (define rlen length)
))
