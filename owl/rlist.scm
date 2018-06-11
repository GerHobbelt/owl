(define-library (owl rlist)
   (import
      (owl defmac)

      ;; alternative implementations
      (owl rlist lc-forest) ;; option 1
      ;(owl rlist old)      ;; option 2
      )

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

   (begin))
