% This uses a 32 bit counter, that produces a representation error
% in case of overflow.

:- module(call_nth, [call_nth/2]).

:- use_module(library(structs),
         [new/2,
          dispose/1,
          get_contents/3,
          put_contents/3]).

:- meta_predicate(call_nth(0, ?)).
:- meta_predicate(call_nth1(0, +, ?)).

call_nth(Goal_0, Nth) :-
   new(unsigned_32, Counter),
   call_cleanup(call_nth1(Goal_0, Counter, Nth),
           dispose(Counter)).

call_nth1(Goal_0, Counter, Nth) :-
   nonvar(Nth),
   !,
   Nth \== 0,
   \+arg(Nth,s(1),2), % produces all expected errors
   call(Goal_0),
   get_contents(Counter, contents, Count0),
   Count1 is Count0+1,
   (  Nth == Count1
   -> !
   ;  put_contents(Counter, contents, Count1),
      fail
   ).
call_nth1(Goal_0, Counter, Nth) :-
   call(Goal_0),
   get_contents(Counter, contents, Count0),
   Count1 is Count0+1,
   put_contents(Counter, contents, Count1),
   Nth = Count1.
