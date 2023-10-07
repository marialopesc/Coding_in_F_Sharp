// F# program to input a string and print out information
// about the # of top 10 letters in that string and substituting certain words


// Converts a string into a list of characters.
let explode (S:string) = 
  List.ofArray (S.ToCharArray())

// Converts a list of characters back into a string.
let implode (L:char list) = 
  new string(List.toArray L)

// Calculates the length of a list.
let rec customLength L = 
  match L with
  | [] -> 0
  | _::tail -> 1 + customLength tail

// Recursively calculates the length of a list.
let rec length L = 
  match L with
  | [] -> 0
  | _::tail -> 1 + length tail

// Recursively counts the occurrences of specific characters ('e', 't', 'a', 'o', 'i', 'n', 's', 'r', 'h', 'l') in a list.
let rec topTen L = 
  match L with
  | [] -> 0
  | head::tail when head = 'e' -> 1 + topTen tail
  | head::tail when head = 't' -> 1 + topTen tail
  | head::tail when head = 'a' -> 1 + topTen tail
  | head::tail when head = 'o' -> 1 + topTen tail
  | head::tail when head = 'i' -> 1 + topTen tail
  | head::tail when head = 'n' -> 1 + topTen tail
  | head::tail when head = 's' -> 1 + topTen tail
  | head::tail when head = 'r' -> 1 + topTen tail 
  | head::tail when head = 'h' -> 1 + topTen tail
  | head::tail when head = 'l' -> 1 + topTen tail
  | head::tail -> 0 + topTen tail

// Recursively counts the occurrences of a specific character in a list.
let rec matchChar L c =
  match L with
    | [] -> 0
    | head::tail when head = c -> 1 + matchChar tail c 
    | head::tail -> 0 + matchChar tail c 

// Iterates through a list of top letters and prints the count of each letter in the input list.
let topLettersCounter L = // holds the sentence
  let topTenLetters = ['a'; 'e'; 'h'; 'i'; 'l'; 'o'; 'n'; 'r'; 's'; 't']
  List.iter(fun x-> (printfn "%A: %A" x (matchChar L x)))topTenLetters  //iteraters

// Recursively substitutes occurrences of specific word patterns in a list with other patterns.
let rec substituteWords L L3 =
  match L with
  | a::b::c::tail when a='t' && b='h' && c='e' -> substituteWords tail (L3@['h';'e';'r'])
  | a::b::c::tail when a='r' && b='a' && c='t' -> substituteWords tail (L3@['h';'a';'t'])
  | a::b::c::tail when a='b' && b='o' && c='y' -> substituteWords tail (L3@['m';'a';'n'])
  | a::tail -> substituteWords tail (L3 @ [a])
  | [] -> L3

// Main function that takes user input and applies the above functions to display various outputs.
[<EntryPoint>]
let main argv =
  printfn "Starting"
  printfn ""

  printf("input> ")
  let input = System.Console.ReadLine()

  let L = explode input
  printfn "exploded: %A" L

  printfn ""
  let len = customLength L
  printfn "length of sentence: %A" len

  let num = topTen L
  printfn "# of top 10 letters: %A" num

  topLettersCounter L
  
  printfn ""
  let S = implode L
  printfn "imploded: %A" S
  printfn ""

  let substitutedL = substituteWords L [] 
  let substitutedS = implode substitutedL
  printfn "swap imploded: %A" substitutedS
  
  printfn ""
  printfn "Done"
  0
