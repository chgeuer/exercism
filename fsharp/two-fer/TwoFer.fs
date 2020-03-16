module TwoFer

let twoFer (input: string option): string = 
    match input with
    | None -> sprintf "One for %s, one for me." "you"
    | Some(name) -> sprintf "One for %s, one for me." name

let twoFer2 (input: string option): string = 
    match input with
    | None -> "you"
    | Some(name) -> name
    |> sprintf "One for %s, one for me."

let twoFer3 (input: string option): string =
    input
    |> Option.defaultValue "you"
    |> sprintf "One for %s, one for me."
