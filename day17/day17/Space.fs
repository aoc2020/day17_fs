module day17.Space

open System
open day17.BaseTypes

let toMap (cubes:Cube[]) : Map<RawPosition,Cube> =
    cubes
    |> Seq.map (fun (cube:Cube) -> (cube.Position.raw,cube))
    |> Map.ofSeq 

type Space (cubes:Cube[],map:Map<RawPosition,Cube>) as self =
    override this.ToString () = sprintf "Space (%A)" cubes
    new (cubes:Cube[]) = Space(cubes,toMap(cubes))
    member this.hasCubeAt (pos:Position) : bool = map.ContainsKey pos.raw 
    member this.posIsUnmapped (pos:Position) : bool = map.ContainsKey pos.raw |> not  
    member this.expand () : Space =
        let newCubes = cubes
                       |> Seq.map (fun cube -> cube.neighbors ())
                       |> Seq.concat 
                       |> Seq.filter this.posIsUnmapped
                       |> Seq.map (fun pos -> (pos.raw,pos))
                       |> Map.ofSeq
                       |> Map.toSeq
                       |> Seq.map snd 
                       |> Seq.toArray 
                       |> Seq.map (fun pos -> Cube(OFF,pos))
        let allCubes = Seq.append cubes newCubes |> Seq.toArray 
        let newMap = allCubes |> Seq.map (fun (c:Cube) -> (c.Position.raw,c)) |> Map.ofSeq
        Space (allCubes,newMap)
        
    member this.valueAt (pos:Position) : CubeState =
        if map.ContainsKey pos.raw then
            map.[pos.raw].State
        else
            OFF
    
    member this.newState (cube:Cube) : CubeState =
        let isOn (s:Position) = this.valueAt s = ON  
        let litNeighbors = cube.neighbors () |> Seq.filter isOn |> Seq.length
        if cube.State = ON then
            if litNeighbors = 2 || litNeighbors = 3 then ON else OFF
        else
            if litNeighbors = 3 then ON else OFF

    member this.nextCube (cube:Cube) : Cube =
        Cube (this.newState cube,cube.Position)
        
    member this.update () : Space =
        let newCubes = cubes |> Seq.map this.nextCube |> Seq.toArray
        let newMap = toMap(cubes)
        Space (newCubes,newMap)

    member this.iterate () : Space =
        let expanded = this.expand ()
        expanded.update ()
        
    member this.numberOfActives () : int =
        cubes |> Seq.filter (fun (c:Cube) -> c.State = ON) |> Seq.length
