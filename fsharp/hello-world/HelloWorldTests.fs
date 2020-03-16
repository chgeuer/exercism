// This file was auto-generated based on version 1.1.0 of the canonical data.

module HelloWorldTests

open FsUnit.Xunit
open Xunit

[<Fact>]
let ``Say Hi1 !``() = "Hello, World!" |> should equal HelloWorld.hello()

open HelloWorld

[<Fact>]
let ``Say Hi!``() = hello |> should equal "Hello, World!"

