module dojo.tests

open NUnit.Framework
open FsUnit
open Bowling

[<Test>]
let ``Given no rolls score is 0`` () =
    score ("".ToCharArray())
    |> should equal 0

[<Test>]
let ``Given one roll score equal to that roll`` () =
    score ("3-".ToCharArray())
    |> should equal 3

[<Test>]
let ``Given two rolls score equal to the sum of each roll`` () =
    score ("36".ToCharArray())
    |> should equal 9

[<Test>]
let ``Given one miss score equal to 0`` () =
    score ("--".ToCharArray())
    |> should equal 0

[<Test>]
let ``Given one miss and one score equal to the sum`` () =
    score ("-7".ToCharArray())
    |> should equal 7

[<Test>]
let ``Given two misses equal to the sum`` () =
    score ("--".ToCharArray())
    |> should equal 0

[<Test>]
let ``Given one strike equal to the strike value`` () =
    score ("X".ToCharArray())
    |> should equal 10

[<Test>]
let ``Given two frames with misses equal to 0`` () =
    score ("-- --".ToCharArray())
    |> should equal 0

[<Test>]
let ``Given two frames with one strike and one score and a miss equal to 10 plus double the score value`` () =
    score ("X 2-".ToCharArray())
    |> should equal 14

[<Test>]
let ``Given two frames with one strike and one score without a miss equal to 10 plus double the score value`` () =
    score ("X 24".ToCharArray())
    |> should equal 22

[<Test>]
let ``Given two frames with one strike and two misses equal to 10`` () =
    score ("X --".ToCharArray())
    |> should equal 10

[<Test>]
let ``Given two frames with one spare and two misses equal to 10`` () =
    score ("1/ --".ToCharArray())
    |> should equal 10
    
[<Test>]
let ``Given two frames with one spare and one score with one miss score equals to 10 plus duble the score`` () =
    score ("1/ 2-".ToCharArray())
    |> should equal 14

[<Test>]
let ``Given two frames with one spare and one score with no miss score equals to 10 plus double the first score plus second score`` () =
    score ("1/ 27".ToCharArray())
    |> should equal 21