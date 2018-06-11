(define-library (owl rlist)
   (import
      (owl defmac)

      ;; alternative implementations
      (owl rlist lc-forest) ;; fairly fast, can hold any data, fixnum offset
      ;(owl rlist old)      ;; fairly fast, can hold non-rlist-node -data, bignum offsets
      ;(owl rlist linear)   ;; reference library using regular lists. can bootstrap ol, barely
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
