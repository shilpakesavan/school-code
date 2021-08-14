# school-code

red-black-tree-code.py manages a self-balancing red-black binary tree which allows search, insertion, and deletion in O(log n) time

machine.scm is an RML (an assembly language simulator) in MIT Scheme (similar to Lisp) by forming a table which takes each register, operater, etc to its equivalent in Lisp. Then any line of RML code can be performed simply by "replacing" each part with its Lisp equivalent through table look-ups.

interpreter.scm implements "Lisp 2.0" by breaking it down into RML and then runs the RML through the RML simulator. Essentially it creates three layers: Lisp 2.0, which lies on top of RML, which lies on top of Lisp.

