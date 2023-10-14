﻿//
// Various functions for practicing F#
//

namespace practicingFSharp

module hw03 =

    let rec contains x L =
        match L with 
        | [] -> 0
        | head::tail when head = x -> 1
        | head::tail -> contains x tail
        

   //
   // NOTE: all types, functions in the module must be indented.
   //

   //
   // subset S L
   //
   // Returns true if S is a subset of L, false if not.
   // 
   // Examples: subset [3; 1; 2] [1; 4; 2; 0; 3] => yes
   //           subset [2; 1; 3] [1; 4; 0; 2; 8] => no
   //           subset [] []                     => yes
   //           subset [] [1; 2; 3]              => yes
   //           subset [1..1000] [1..1000]       => yes
   // 
   // NOTE: you can solve using tail-recursion, or using a
   // higher-order approach. Whatever you prefer. Beware
   // that List.length is an O(N) operation, since it 
   // traverses the entire list and counts the elements.
   // 
   // HINT: there are a variety of solutions. One idea
   // is write a helper function "contains e L" that 
   // returns true if e is an element of L, and false 
   // if not. Then build on that to define subset. This
   // leads to an O(N^2) solution, which is fine.
   // 

    let rec subset S L =
        if S = [] then 
            true
        else
            let result = List.map(fun x -> contains x L) S     //list.map, .reduce, .iter, .filter 
            let sum = List.reduce(fun y z -> y + z) result
            if sum = S.Length then 
                true
            else 
                false    
            



   //
   // delete_tr e L
   //
   // Deletes all occurrences of e from the list L, 
   // returning the new list. This version is written
   // recursively, using a helper function that is 
   // tail-recursive.
   //
   // Examples: delete_tr 3  [3; 1; 2]   => [1; 2]
   //           delete_tr 99 [99; 0; 99] => [0]
   //           delete_tr -2 []          => []
   //           delete_tr "cat" ["dog"]  => ["dog"]
   //           delete_tr 99999 [1..99999] => [1..99998]
   // 
   // NOTE: write a tail-recursive version using a helper
   // function; do not change the API of the original 
   // delete function. You'll also need to build the new
   // list efficiently; you can use List.rev if need be.
   //

    let delete_tr e L =
    // Define a tail-recursive helper function
        let rec delete_tr_helper e L acc =
            match L with
            | [] -> List.rev acc  // Reverse the accumulator and return it when L is empty
            | head::tail when head = e -> delete_tr_helper e tail acc  // Skip the current element if it equals e
            | head::tail -> delete_tr_helper e tail (head::acc)  // Add the current element to the accumulator if it does not equal e

        // Call the helper function with an initial accumulator of []
        delete_tr_helper e L []



   //
   // delete_ho e L
   //
   // Deletes all occurrences of e from the list L, 
   // returning the new list. This version uses a
   // higher-order approach.
   //
   // Examples: delete_ho 3  [3; 1; 2]   => [1; 2]
   //           delete_ho 99 [99; 0; 99] => [0]
   //           delete_ho -2 []          => []
   //           delete_ho "cat" ["dog"]  => ["dog"]
   //           delete_ho 99999 [1..99999] => [1..99998]
   //
    let delete_ho e L =
        List.filter (fun x -> x <> e) L



   //
   // examAverages LT
   //
   // Given a list of tuples of the form ("netid", [score;score;score;...]),
   // computes each netid's average score as a real number and returns a 
   // list of tuples of the form ("netid", average).
   //
   // Example:
   //   examAverages [("sdeitz2",[100;90;91]); ("psankar",[100;100;100;100;98])]
   //     => [("sdeitz2",93.66666667); ("psankar",99.6)]
   //
   // NOTE: F# offers a function List.average L that computes the average of
   // a list of real numbers.  However, the list of scores in the tuple are
   // integers, so in order to use List.average, you would first need to convert
   // the integers to reals --- List.map float L would work nicely here.
   //
   // You can solve using recursion, or higher-order, or both; tail recursion
   // is not necessary.
   //
    let examAverages LT =
    // Use List.map to apply a function to each element of LT
        List.map (fun (netid, scores) ->
            // Calculate the average score
            let avg = List.average (List.map float scores)
            // Return a tuple with the netid and the average score
            (netid, avg)
        ) LT



   //
   // pairwise L1 L2
   //
   // Given 2 lists L1 and L2, both the same length, merges the corresponding 
   // elements into pairs, returning a list of pairs.
   //
   // Example:
   //   pairwise [1;3;5;7] [10;20;30;40]
   //     => [(1,10); (3,20); (5,30); (7,40)] 
   //
   // You must solve using recursion; tail recursion is not necessary.
   // Note: there is a F# library function named zip that does this operation. 
   // You may not use that function in your solution. 
   //
    let rec pairwise L1 L2 =
        match L1, L2 with
        | [], [] -> []  // Base case: both lists are empty
        | head1::tail1, head2::tail2 -> (head1, head2) :: pairwise tail1 tail2  // Recursive case: pair the heads and recurse on the tails
        | _ -> failwith "Lists are not of the same length"  // Error case: lists have different lengths
