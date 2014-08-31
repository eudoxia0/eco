(in-package :eco-test)

(def-suite asdf)
(in-suite asdf)

(test asdf
  (finishes (eco-template::test t "test" (list 1 2 3) nil))
  (finishes (eco-template::test2)))

(run! 'asdf)
