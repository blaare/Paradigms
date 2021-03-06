(* Name:          Timothy Wilson
   Class:         CSC 345
   Assignment:    ML Homework
   Description:   This file contains the functions flip, deleteIth, piglatinize,
                  and some extensive functions.
                  Each of these functions has documentation*) 


(* Problem 1: flip *)


(* Function   flip
   Goal:      To flip alternate elements of any list
   Parameter: some List to flip
   Return:    A list with the alternate elements flipped.
   ###############################################
   Usage:     - flip[1, 2, 3, 4, 5, 6];
              val it - [2, 1, 4, 3, 6, 5]: int list  
              - flip[1.0, 2.0, 3.0, 4.0, 5.0, 6.0];
              val it - [2.0, 1.0, 4.0, 3.0, 6.0, 5.0] : real list
*)
fun flip(nil)      = nil 
  | flip(x::y::xs) = [y, x] @ flip(xs)
  | flip(x::nil)   = [x];


(* Problem 2: deleteIth *)


(* Function   deleteIth
   Goal:      to delete the ith element of a list. If the length of the list is less than i,
              return the list.
   Parameter: some List 
   Parameter: i - some int, the index of the list to remove
   Return:    the list without the element
   ###############################################
   Usage:     - deleteIth([1,2,3,4,5,6],3);
	      val it = [1,2,4,5,6]: int list;
              - deleteIth([1,2,3,4,5,6,7],7);
	      val it = [1,2,3,4,5,6]:int list;
*)
fun deleteIth(nil, i)   = nil
  | deleteIth(_::xs, 1) = xs
  | deleteIth(x::xs, i) = x::deleteIth(xs, i-1);


(* Problem 3: piglatinize *)

(* Function   member
   Goal:      to return if some element e is in list L
   Parameter: e   - some element
   Parameter: some list
   Return:    true/false
   ###################################################
   Usage:     - member(#"a", [#"b",#"c",#"f",#"a"]);
              val it = true : bool
              - member(#"a", [#"b"]);
              val it = false : bool
*)
fun member(e, nil)   = false
  | member(e, x::xs) = e=x orelse member(e, xs); 

(* Function   isVowel
   Goal:      to determine if some character C is a vowel
   Parameter: c - some char
   Return:    true/false
   #################################################
   Usage:     - isVowel(#"a");
              val it = true : bool
              - isVowel(#"b");
              val it = false: bool
*)
fun isVowel(c:char) = member(c, [#"a", #"e", #"i", #"o", #"u",
				 #"A", #"E", #"I", #"O", #"U"]);


(* Function:  piglatinizer
   Goal:      to assist piglatinize in handling the exploded string
              by looking for vowels and appending non vowels to the end
              of the string
   Parameter: some list
   Return:    the list with a vowel at the first index.
   #################################################
   Usage:     - piglatinizer([#"s",#"t",#"r",#"i",#"p",#"e"]);
              val it = [#"i",#"p",#"e",#"s",#"t",#"r"] : char list
 *)
fun piglatinizer(nil)   = nil
  | piglatinizer(x::xs) = if isVowel(x) then x::xs else piglatinizer(xs@[x]); 


(* Function   piglatinizeController
   Goal:      to determine the suffix of piglatinizer and, if needed,
              put the character list through the piglatinizer.
   Parameter: some list
   Return:    the List in a modified order.
   #################################################
   Usage:     - piglatinizeController([#"s",#"t",#"r",#"i",#"p",#"e"]);
              val it = "ipestray" : string
*)
fun piglatinizeController(nil)   = ""
  | piglatinizeController(x::xs) = if isVowel(x) then implode(x::xs)^"yay"
                                   else implode(piglatinizer(x::xs))^"ay";



(* Function   piglatinize
   Goal:      to convert a word into pig latin by the following rules:
                 1. if it begins with a vowel, add to the end "yay"
                 2. otherwise, move non-vowels to the end of the string until
                    a vowel is at the front and then add to the end of the list.
   Parameter: s - some string
   Return:    a "piglatinized" version of the word.
   Note:      This function does not handle the following words:
                 1. that do notcontain a vowel a,e,i,o,u
	         2. that begin with y
	         3. whose only "vowel" is 'y'
   ##################################################
   Usage:     - piglatinize "able";
	      val it = "ableyay" : string
              - piglatinize "stripe";
	      val it = "ipestray : string
*)
fun piglatinize(s:string) = piglatinizeController(explode(s));

