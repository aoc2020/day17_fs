// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open day17.BaseTypes 
open day17.IO
open day17.Space 

// Define a function to construct a message to print
let from whom =
    sprintf "from %s" whom

[<EntryPoint>]
let main argv =
    let message = from "F#" // Call the function
    let cubes = readCubes "/Users/xeno/projects/aoc2020/day17_fs/input2.txt"
//    printfn "Cubes: %A" cubes
    let space = Space (cubes)
    let space6 = [|0..5|] |> Seq.fold (fun (s:Space) i -> s.iterate()) space
    // printfn "Space (0): %A" space
    let size = space6.numberOfActives () 
    let space1 = space.iterate ()
    // printfn "Space (1): %A" space1
    printfn "Space at 6 has %d active nodes" size 
    printfn "Hello world %s" message
    0 // return an integer exit code