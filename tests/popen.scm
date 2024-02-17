(import (owl io))
(import (owl lazy))

(lets ((r w (popen "echo halo salut")))
  (print (lcar (lines r))))

(lets ((r w (popen "cat -")))
  (print-to w "meow meow meow meow")
  (print (lcar (lines r))))
