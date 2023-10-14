open System

// Main program for testing functions 

open practicingFSharp

//##################################################################
//
// main
//
// Compiles the simple program; the filename is input by the user.
//

[<EntryPoint>]
let main argv =
    printfn "** Starting **"
    printfn ""

    // Testing subset function
    let L1 = [1; 2; 3]
    let L2 = [1..5]
    printfn "L1: %A" L1
    printfn "L2: %A" L2
    printfn "Subset(L1,L2): %A" (hw03.subset L1 L2)

    // Testing delete_tr function
    let L3 = [3; 1; 2; 3; 4; 3]
    let e1 = 3
    printfn "\nL3: %A" L3
    printfn "Element to delete: %A" e1
    printfn "After delete_tr: %A" (hw03.delete_tr e1 L3)

    // Testing delete_ho function
    printfn "After delete_ho: %A" (hw03.delete_ho e1 L3)

    // Testing examAverages function
    let LT = [("sdeitz2",[100;90;91]); ("psankar",[100;100;100;100;98])]
    printfn "\nLT: %A" LT
    printfn "Exam averages: %A" (hw03.examAverages LT)

    // Testing pairwise function
    let L4 = [1;3;5;7]
    let L5 = [10;20;30;40]
    printfn "\nL4: %A" L4
    printfn "L5: %A" L5
    printfn "Pairwise(L4,L5): %A" (hw03.pairwise L4 L5)

    printfn "\n** Done **"
    0


