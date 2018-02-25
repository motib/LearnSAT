% Copyright 2013 by M. Ben-Ari. GNU GPL. See copyright.txt.

/*
  Bounded model checking for semaphore
    Unsatisfiable (DPLL): units=25, decisions=12, conflicts=7
    Unsatisfiable (CDCL): units=19, decisions=4,  conflicts=3
    Unsatisfiable (NCB) : units=16, decisions=3,  conflicts=2

    Changing variable order so that waits are tried before signals:
      set_order([
        wso0,wso1,wso2,wsz0,wsz1,wsz2,wwo0,wwo1,wwo2,wwz0,
        sso0,sso1,sso2,ssz0,ssz1,ssz2,swo0,swo1,swo2,swz0,swz1,swz2]).
      Unsatisfiable (all modes): units=14, decisions=2, conflicts=2
*/

  :- ensure_loaded('../src/ops').
  :- ensure_loaded('../src/cnf').

to_clauses(A) :-
  to_internal(A, AI),
  cnf(AI, PW1),
  cnf_to_clausal(PW1, A2),
  write_clauses(A2).

/*
  Process P   Process Q
  wait(s)     wait(s)
  signal(s)   signal(s)

  States are PQSN, where
    P, Q are w or s for location counter of the process
    S is o or z for value of semaphore one or zero
    N is the time
  Signal of a semaphore whose value is 1 is not allowed
*/

generate :-
  tell('bmc-sem.pro'),
  write(":- use_module('../src/dpll').\nbmc :- dpll(\n"),
  to_clauses( 

  /* Correctness claim: both processes are not at signal */    
  (ssz0 v sso0  v  ssz1 v sso1  v  ssz2 v sso2) ^

  /* Initial state: (wait, wait, 1) */
  (wwo0 ^ ~wwz0 ^ ~swo0 ^ ~swz0 ^ ~wso0 ^ ~wsz0 ^ ~sso0 ^ ~ssz0) ^
  
  /* Transitions */
  ((wwo0 <-> swz1) + (wwo0 <-> wsz1)) ^
  (swo0 <-> ssz1) ^
  (swz0 <-> wwo1) ^
  (wso0 <-> ssz1) ^
  (wsz0 <-> wwo1) ^
  ((ssz0 <-> wso1) + (ssz0 <-> swo1)) ^
  
  ((wwo1 <-> swz2) + (wwo1 <-> wsz2)) ^
  (swo1 <-> ssz2) ^
  (swz1 <-> wwo2) ^
  (wso1 <-> ssz2) ^
  (wsz1 <-> wwo2) ^
  ((ssz1 <-> wso2) + (ssz1 <-> swo2))

  ),
  write('\n,_ ).\n'),
  told.
