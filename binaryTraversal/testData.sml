(* For the ML project *)

datatype 'a BT = empty | bTree of 'a * 'a BT * 'a BT ;

(* Don't use NIL for the empty tree as we want it for the empty list *) 


(* a new type -- X, to test the polymorhism on *) 
datatype X = A|B|C|D|E|F|G|H;



(*  TEST DATA - BINARY TREES   *)

val t1 =  bTree(1, 
	       bTree(2,bTree(3,empty, empty), bTree(4,empty, empty)),
	       bTree(5,bTree(6,empty, empty),bTree(7,empty, empty)));

val t2 =  bTree(A, 
	       bTree(B,bTree(D,empty, empty), bTree(E,empty, empty)),
	       bTree(C,bTree(F,empty, empty),bTree(G,empty, empty)));

val t3 =  bTree(1.22, bTree(2.33,empty, empty), bTree(3.44,empty,empty));

val t4 =  bTree("A", 
	       bTree("B",
		    bTree("C",bTree("E",empty,empty), empty), 
		    bTree("D",
			 bTree("F",empty,empty), 
			 bTree("G",bTree("H",empty,empty), empty))),
	       bTree("I",
		    bTree("J",empty, bTree("K",
				      bTree("L",empty, empty),
				      bTree("M",empty, empty))),
		    empty));

(* END END *)