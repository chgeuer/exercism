module Leap

let leapYear (year: int): bool =
   let d modulus = 0 = year % modulus
   match (d 4, d 100, d 400) with
   | (_, _, true) -> true
   | (_, true, _) -> false
   | (true, _, _) -> true
   | _ -> false
