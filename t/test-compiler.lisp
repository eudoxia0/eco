
(in-package :eco-test)

(def-suite compiler)
(in-suite compiler)

(test compiler
  (signals error
    (progn
      (eco:compile-string
       "<% deftemplate error-test (hello x y z) () %>
<%= hello x y z%>abc
<% end %>")
      (eco-template::error-test "hello" 1 2 3)))

  (signals error
    (eco:compile-string
     "<% block %>missing end tag"))

  (progn
    (is (equal (eco-template::compiler-1 "input")
               "
inputabc
"))
    (is (equal (eco-template::compiler-2 "[input1]" "[input2]")
               "
[input1][input2]body
"))
    (is (equal (eco-template::compiler-3)
               "

word1word2body

"))
    (is (equal (eco-template::compiler-4)
               "
b
"))))

(run! 'compiler)
