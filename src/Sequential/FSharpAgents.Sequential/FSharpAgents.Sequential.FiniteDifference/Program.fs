

let laplasjan (fi : int -> int -> double) i j h =
        let left = fi  (i - 1) j 
        let upper = fi  i (j + 1)
        let right = fi  (i+1) j 
        let down = fi  i  (j - 1) 
        let center = fi i j
        (left + upper + right + down - 4. * center) / (pown h 2)


[<EntryPoint>]
let main argv = 

    let rows = 4
    let cols = 5
    let h = 2.
    let mesh  = Array2D.zeroCreate<double> (rows + 2) (cols + 2)  // additional one point on left, one on up, one on down and one on rigth 
    
    
    for i in 1 .. cols do
        mesh.[rows, i] <- 100.

    printfn "%A" mesh 

    let get_mesh_value  i j =
        match (i, j) with
            | (i, j) when i < 1 || j < 1 -> 0.0 
            | (i, j) when i > rows || j > cols -> 0.0 
            | _ -> mesh.[i,j] 
    

    for t in 0 .. 1000 do 
        printfn "t=%d" t
        let centerOfHeat = mesh.[rows, cols/2]

        for i in 1 .. (rows ) do
            for j in 1 .. (cols )  do
                let vLap = laplasjan get_mesh_value i j h
                let orginValue = mesh.[i,j]
                mesh.[i,j] <- vLap + orginValue

        printfn "%A" mesh
        let newCenterOfHeat = mesh.[rows, cols/2]
        let error = newCenterOfHeat  / centerOfHeat       
        printfn "Current value: %e. Prev value was %e. Error: %e " newCenterOfHeat centerOfHeat error


    System.Console.ReadKey() |> ignore
    0 // return an integer exit code
