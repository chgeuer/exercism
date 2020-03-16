module Raindrops

let convert (number: int): string =
    let p (modulus, str) = if 0 = number % modulus then str else ""
    [
       (3, "Pling")
       (5, "Plang")
       (7, "Plong")
    ]
    |> List.map(p)
    |> String.concat ""
    |> function
       | "" -> sprintf "%d" number
       | plingPlangPlong -> plingPlangPlong
