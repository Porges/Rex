namespace Rex.Tests

open FsCheck.Xunit
open Xunit

open Rex.Regex
open Rex.Tests.Generators

[<Arbitrary(typeof<Instances>)>]
module ConstructorTests =

    [<Property(EndSize = 20)>]
    let ``Double negation is identity``(r : Regular<char>) =
        Not (Not r) = r
