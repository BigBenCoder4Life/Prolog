%
% CS 430 Module 14 Lab facts and rules
%
direct_ancestor(scheme,lisp).
direct_ancestor(commonlisp,lisp).
direct_ancestor(haskell,scheme).
direct_ancestor(java,smalltalk).
direct_ancestor(java,cpp).
direct_ancestor(cpp,c).
direct_ancestor(cpp,smalltalk).
direct_ancestor(smalltalk,simula).
direct_ancestor(simula,algol).
direct_ancestor(csharp,java).
direct_ancestor(c,algol).
direct_ancestor(pascal,algol).
direct_ancestor(ada,pascal).
direct_ancestor(algol,fortran).
ancestor(A,C) :- direct_ancestor(A,C).
ancestor(A,C) :- direct_ancestor(A,B), ancestor(B,C).
cousins(X,Y) :- ancestor(X,Z), ancestor(Y,Z). 
common_ancestor(X,Y,Z) :- ancestor(X,Z), ancestor(Y,Z). 

%
% TEST CASES
%
test_cousins :- cousins(java,cpp), \+cousins(java,lisp).
test_common_ancestor :- common_ancestor(java,cpp, smalltalk), \+common_ancestor(java,cpp, happy). 
test :- test_cousins, test_common_ancestor.