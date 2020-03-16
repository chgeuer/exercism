module Bob

open System.Text.RegularExpressions

let response (input: string): string =
    let i = input.Trim()
    let isEmpty = i.Length = 0
    let isQuestion = i.EndsWith("?")
    let isShouting = i = i.ToUpper() && i |> Regex("[\p{Ll}\p{Lu}]").IsMatch

    match (isEmpty, isShouting, isQuestion) with
    | (true, _, _) -> "Fine. Be that way!"
    | (_, true, true) -> "Calm down, I know what I'm doing!"
    | (_, true, _) -> "Whoa, chill out!"
    | (_, _, true) -> "Sure."
    | _ -> "Whatever."
