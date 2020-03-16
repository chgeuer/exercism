module Accumulate

let accumulate (func: 'a -> 'b) (input: 'a list): 'b list =
    // https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/lists
    // The use of the accumulator argument makes the function tail recursive.
    let rec loop list acc =
        match list with
        | head :: tail -> loop tail (func head :: acc)
        | [] -> List.rev acc
    loop input []
