(* X Datatype def *)
datatype X = A | B | C | D | E | F | G | H;

(* bTree Datatype def *)
datatype 'a BT = empty | bTree of 'a * ('a BT) * ('a BT);


(* TESTING DATUM *)
val t1 = bTree(1,
	       bTree(2,bTree(3,empty,empty), bTree(4,empty,empty)),
	       bTree(5,bTree(6,empty,empty), bTree(7,empty,empty)));

val t2 = bTree(A,
	       bTree(B,bTree(D,empty,empty), bTree(E,empty,empty)),
	       bTree(C,bTree(F,empty,empty), bTree(G,empty,empty)));

val t3 = bTree(1.22, bTree(2.33,empty,empty), bTree(3.44,empty,empty));

val t4 = bTree("A",
	       bTree("B",
		     bTree("C",bTree("E",empty,empty),empty),
		     bTree("D",
			   bTree("F",empty,empty),
			   bTree("G",
				 bTree("H",empty,empty),empty))),
	       bTree("I",
		     bTree("J",empty,bTree("K",
					   bTree("L",empty,empty),
					   bTree("M",empty,empty))),
		     empty));
		     
(* TREE TRAVERSAL FUNCTIONS *)
		     
fun preOrder  empty = nil
  | preOrder   (bTree (v,left,right)) = v::preOrder(left)@preOrder(right); 

fun inOrder   empty = nil
  | inOrder   (bTree (v,left,right))  = inOrder(left)@[v]@inOrder(right);

fun postOrder empty = nil
  | postOrder (bTree (v,left,right))  = postOrder(left)@postOrder(right)@[v];

(* PRINT FUNCTIONS *)

(* Function   printInt
   Goal:      to print an integer
   Parameter: Integer
   Return:    a printed integer
*)
fun printInt n = print(Int.toString n);

(* Function   printReal
   Goal:      to convert a real number to a string and print it
   Parameter: Real
   Return:    a printed real
*)
fun printReal n = print(Real.toString n);

(* Function   printX
   Goal:      to convert the enumerated X type to a string
   Parameter: X
   Return:    A printed X
*)
fun printX A = print "A"
  | printX B = print "B"
  | printX C = print "C"
  | printX D = print "D"
  | printX E = print "E"
  | printX F = print "F"
  | printX G = print "G"
  | printX H = print "H"; 

(* Indentation and Pretty Printing Functions *)
val indentLevel = 2;

(* Function   tab
   Goal:      to print a series of tabs. 
   Parameter: Integer
   Return:    nothing
 *)
fun tab n =
    let fun tabhelper 0 = ""
	  | tabhelper n = "  "^tabhelper(n-1)
    in print(tabhelper(n)) end;


(* Function   displaynode
   Goal:      to display the node based on some printing function
   Parameter: some value
   Parameter: the tabbing depth level
   Parameter: some function to apply to v
   Return:    nothing
*)
fun displaynode (v, depth, func) = (tab(depth);func(v);print("\n"));


(* Function   dash
   Goal:      to print the dash at some depth
   Parameter: Integer
   Return:    a dash prefixed by tabs
*)
fun dash n = displaynode("-", n, print); 

(* Function   displaytreeindent
   Goal:      to print the indented member
   Parameter: a bTree
   Parameter: the tabbing depth level
   Parameter: some  function to apply to v
   Return:    nothing
*)
fun displaytreeindent (empty, depth, func) = dash(depth)
  | displaytreeindent (bTree (v, empty, empty), depth, func) =
    displaynode(v, depth, func)
  | displaytreeindent (bTree (v, left, right), depth, func) =
    (displaynode(v,depth,func);
     displaytreeindent(left, depth+1, func);
     displaytreeindent(right, depth+1, func));

(* Function   displayTree
   Goal:      to print out the tree in a readable order
   Parameter: a bTree
   Parameter: some function to apply to v
   Return:    nothing
*)
fun displayTree(empty, func) = print("Empty Tree")
  | displayTree(bT, func) = displaytreeindent(bT, 0, func);





(* Technical Settings *)

Control.Print.printDepth := 200;

Control.Print.printLength := 200;

Control.polyEqWarn := false;


(* Runs *)

preOrder t1;
inOrder t1;
postOrder t1;
displayTree (t1, printInt);


preOrder t2;
inOrder t2;
postOrder t2;
displayTree(t2, printX);

preOrder t3;
inOrder t3;
postOrder t3;
displayTree(t3, printReal);


preOrder t4;
inOrder t4;
postOrder t4;
displayTree(t4, print);
