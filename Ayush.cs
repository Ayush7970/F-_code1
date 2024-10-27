//
// F# program to ...
//
// Name: Ayush Bhardwaj
//
// Original author: Prof. Joe Hummel
// Modified by: Ellen Kidane
//

//
// explode:
//
// Given a string s, explodes the string into a list of characters.
// Example: explode "apple" => ['a';'p';'p';'l';'e']
//
let explode (S:string) =
 List.ofArray (S.ToCharArray())

//
// implode
//
// The opposite of explode --- given a list of characters, returns
// the list as a string. Example: implode ['t';'h';'e'] => "the"
//
let implode (L:char list) =
 new string(List.toArray L)

// function to calculate the list's length
let rec length L =
   match L with
   | [] -> 0
   | _::tail -> 1 + length tail

// function to calculate the count of the vowels
let rec numVowels L =
   let ifVowel charac =
       match charac with
       | 'a' | 'e' | 'i' | 'o' | 'u' -> true
       | _ -> false
   match L with
   | [] -> 0
   | hd::tl -> (if ifVowel hd then 1 else 0) + numVowels tl

// function to count the specific character's occurence
let rec counteach L para =
   match L with
   | [] -> 0
   | head :: tail when head = para -> 1 + counteach tail para
   | _ :: tail -> counteach tail para

// Function to run and count each character in the alphabet and display it
let rec counteach2 L para =
   match L with
   | [] -> 0
   | head :: tail when head = para -> 1 + counteach2 tail para
   | _ :: tail -> counteach2 tail para


// Function to count the characters
let rec repeating_char char (L: char list) acc =
   match L with
   | [] -> acc
   | hd :: tl when hd = char -> repeating_char char tl (acc + 1)
   | _ :: tl -> repeating_char char tl acc

// Function to find repeated characters in the input string
let redo (L: char list) =
   let letters charac =  charac <= 'z' && charac >= 'a'
   let list_pure = List.filter letters L
   list_pure
   |> List.groupBy (fun charac -> charac)
   |> List.filter (fun (char, repitation) -> 1 < (repeating_char char list_pure 0))
   |> List.map fst
   |> List.sort

// this Function checks whether a string is an isogram or not isogram
let isogram_true (L: char list) =
   let not_isogram_char = redo L
   match not_isogram_char with
   | [] -> printfn "The phrase is an isogram!"
   | _ ->
       printfn "The phrase is not an isogram."
       printfn "Repeated letters: %A" not_isogram_char
       printfn ""



// this Function produces a cipher text
let atabash_cipher (L: char list) =
   let alphabet = "abcdefghijklmnopqrstuvwxyz"
   let reversed_alphabet = "zyxwvutsrqponmlkjihgfedcba"
   let rec encode_char charac =
       match charac with
       | 'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g' | 'h' | 'i' | 'j' | 'k' | 'l' | 'm' | 'n' | 'o' | 'p' | 'q' | 'r' | 's' | 't' | 'u' | 'v' | 'w' | 'x' | 'y' | 'z' ->
           let index = alphabet.IndexOf(charac)
           reversed_alphabet.[index]
       | _ -> charac
   List.filter (fun c -> c <> ' ' && c <> '.' && c <> '?' && c <> '!' && c <> ',' && c <> ';' && c <> ':' && c <> '-' && c <> '(' && c <> ')' && c <> '[' && c <> ']' && c <> '{' && c <> '}' && c <> '\'' && c <> '"') L
   |> List.map encode_char



[<EntryPoint>]
let main argv =
 printfn "Starting..."
 printfn ""

 //
 // input string, output length and # of vowels:
 //
 printf("Enter your input> ")
 let input = System.Console.ReadLine()
 printfn ""

 let L = explode input
 printfn "Exploded: %A" L
 printfn ""

 let len = length L
 printfn "Length of phrase: %A" len
 printfn ""

 let num = numVowels L
 printfn "Number of vowels: %A" num
 printfn ""

 // calling the function for each vowel
 printfn("Number of each vowel in the phrase: ")
 let list2 = ['a';'e';'i';'o';'u']
 let printvowels var L =
     printfn "%A: %A" var (counteach L var)
 List.iter(fun c -> printvowels c L) list2

 // calling the function for each character
 printfn ""
 printfn("Number of each character in the phrase: ")
 let list3 = ['a';'b';'c';'d';'e';'f';'g';'h';'i';'j';'k';'l';'m';'n';'o';'p';'q';'r';'s';'t';'u';'v';'w';'x';'y';'z';'.';'?';'!';',';';';':';'-';'(';')';'[';']';'{';'}';'\'';'"']
 let printvowels2 var2 L =
     printfn "%A: %A" var2 (counteach2 L var2)
 List.iter(fun c2 -> printvowels2 c2 L) list3

 // calling the function for isogram
 let letters_inchar = explode input
 isogram_true letters_inchar

 // calling the function for cipher string
 let encoded_list = atabash_cipher letters_inchar
 let encoded_string = implode encoded_list
 printfn "Encoded phrase: \"%s\"" encoded_string
 

 let S = implode L
 printfn "Imploded: %A" S

 printfn ""
 printfn "Done!"
 0  // return 0 => success, much like C++