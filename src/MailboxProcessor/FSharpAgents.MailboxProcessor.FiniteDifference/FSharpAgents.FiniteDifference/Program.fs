
open System
open System.Collections.Generic

type Message = 
    | ReqValueAndReply of int *  AsyncReplyChannel<Reply> //forT 
    | ReqValue of int * int * int // fromI fromJ forT  
    | PrintInfo of int // forT
    | Respone of int * int * int * double  //fromI fromJ forT value //string * AsyncReplyChannel<Reply>
and Reply = 
//    | Failure of string
    | ResExist of int * int * int  * double 
    | ResCalculate of int * int * int  * double 

type PointAgent(rowNumber:int, colNumber:int, deltaH:double) =
    let mutable AgentMesh = Array2D.zeroCreate<PointAgent> 1 1
    let mutable Value  :double[]  = [|0.|]
    let mutable dictionaryValue = Map.empty<int,double>.Add(0,0.0)

    let mutable SendedRequestsToCalculate : (int*int*int) list = []
    let mutable ResponseWithValue : (int*int*int*double) list = []
    let timeoutForReceivingMessages = 10000000        
    let mutable clientResponse  = Map.empty<int,double>                    

    let GetLastT() = 
        let keys = dictionaryValue |> Map.toList |> List.map (fun (k,v)-> k)  
        keys |> List.max

    let GetValueForT(t) = 
        dictionaryValue.[t]

    let SendValue(i, j, t, value) = 
        AgentMesh.[i,j].Post(Respone(rowNumber,colNumber,t, value))
    
    let SendValueAfterCalculation () = 
        if SendedRequestsToCalculate.Length > 0 then
            let (_,_,forT) = SendedRequestsToCalculate |> List.maxBy (fun (i,j,t)-> t)
            if GetLastT() >= forT then
                let value = GetValueForT(forT)
                let reqToCalculateNow,reqToCalculateLater =  SendedRequestsToCalculate |> List.partition (fun (i,j,t) -> t = forT)
                reqToCalculateNow |>  
                    List.iter (fun (i,j,t) ->SendValue (i, j, t, value) )
                
                SendedRequestsToCalculate  <- reqToCalculateLater 
                

    let TrySendRequestValue (forI, forJ, forT) =
        let exist = ResponseWithValue |> List.exists (fun (i,j,t,v)-> i=forI && j=forJ && t=forT )
        if exist = false then 
            let maxCols = Array2D.length2 AgentMesh
            let maxRows = Array2D.length1 AgentMesh
            if forI < maxRows  && forI >= 0 && forJ < maxCols && forJ >= 0 then
                AgentMesh.[forI, forJ].Post(ReqValue(rowNumber,colNumber,forT))     
         

    let SendRequestToNeighbors (forT) =      
        TrySendRequestValue  ((rowNumber + 1), colNumber, forT)
        TrySendRequestValue  (rowNumber, (colNumber - 1), forT)
        TrySendRequestValue  (rowNumber, (colNumber + 1), forT)
        TrySendRequestValue  ((rowNumber - 1), colNumber, forT)
        
    
    let GetRequiredRsponsesNumber() = 
        let maxCol = Array2D.length2 AgentMesh
        let maxRow = Array2D.length1 AgentMesh
        let lastColNum = (maxCol) - 1
        let lastRowNum = (maxRow) - 1
        
        match (rowNumber,colNumber) with
            | (row,col) when row = 0 && col = 0 && maxCol = 1 && maxRow = 1 -> 1
            | (row,col) when (row = 0 && maxRow = 1) || (col = 0 && maxCol = 1) -> 1 
            | (0,0) -> 2
            | (row,col) when (row = lastRowNum && col =0 ) || (row = 0 && col =lastColNum ) -> 2
            | (lastRowNum, lastColNum ) -> 2
            | (0,_) | (_,0)-> 3
            | (row,col) when row = lastRowNum || col= lastColNum -> 3
            | (_,_) -> 4
            

    let GetOldestResponseWithValueToCalculate() =
        if ResponseWithValue .Length > 0 then
            ResponseWithValue 
            |> List.map (fun (i,j,t,v) -> t)  
            |> List.toSeq 
            |> Seq.distinct  
            |> Seq.toList
            |> List.sort
            |> Some
        else
            None

    let GetAllResponsesForT (forT) =
         ResponseWithValue |> List.filter (fun (i,j,t,v)-> t= forT ) 
    
    let CalculateFromResponses (forT) = // it calculating T(n+1) with parameter n from T(n) not T(n+1) 
        let responsesToCalculate, responsesToGo = ResponseWithValue |> List.partition (fun (i,j,t,v)-> t= forT )
        ResponseWithValue <- responsesToGo
        if dictionaryValue.ContainsKey(forT)  && dictionaryValue.ContainsKey(forT + 1) =false then 
            let sum = responsesToCalculate |> List.sumBy (fun (i,j,t,v)-> v)
            let prevValue = dictionaryValue.[forT]
            let newValue = (sum  - 4. * prevValue  ) / (pown deltaH 2)
            dictionaryValue <- dictionaryValue.Add(forT + 1, newValue) 
       
    let TryCalculateForOldersResponse()  = 
        let oldersResponse = GetOldestResponseWithValueToCalculate()
        if oldersResponse .IsSome then 
            oldersResponse.Value |> List.iter (fun t->
                let reponses = GetAllResponsesForT t
                let requiredResponsesNumber = GetRequiredRsponsesNumber()   
                if reponses.Length = requiredResponsesNumber  then
                    CalculateFromResponses t 
                else
                    SendRequestToNeighbors t 
            )

    let agent = new MailboxProcessor<Message>( fun inbox ->async{
                while true do    
                    try
                        let! message = inbox.Receive(timeoutForReceivingMessages)
                     
                        match message with 
                            | ReqValueAndReply (t, reply) when GetLastT()  > t ->  
                                reply.Reply(ResExist( rowNumber, colNumber, t, GetValueForT(t)))
                            
                            | ReqValueAndReply (t, reply)  ->  
                                printfn "TODO"
                                //clientResponse.Add(
                                //reply.Reply(ResExist( rowNumber, colNumber, t, Value.[t]))
                            

                            | ReqValue (fromI, fromJ, forT) when GetLastT() > forT -> 
                                 SendValue(fromI, fromJ, forT, GetValueForT(forT))
                        
                            | ReqValue (fromI, fromJ, forT) ->
                                let exist =  SendedRequestsToCalculate |> List.exists (fun (i,j,t)-> i=fromI && j=fromJ && t=forT)
                                if exist = false then  
                                    SendedRequestsToCalculate <- (fromI, fromJ, forT) :: SendedRequestsToCalculate
                        
                            | Respone (fromI, fromJ, forT, withValue) ->
                                let exist =  ResponseWithValue |> List.exists (fun (i,j,t,v)-> i=fromI && j=fromJ && t=forT && v=withValue)
                                if exist= false then 
                                    ResponseWithValue <- (fromI,fromJ, forT, withValue) :: ResponseWithValue    
                        
                            | PrintInfo(forT) when GetLastT() > forT ->
                                printfn "[%d,%d] = %f" rowNumber colNumber (GetValueForT(forT))
                        
                            | PrintInfo(forT) ->
                                printfn "[%d,%d] trying to print value for T= %d" rowNumber colNumber forT
                                SendRequestToNeighbors (forT - 1)
                        
                        TryCalculateForOldersResponse()      
                        SendValueAfterCalculation()
                        SendRequestToNeighbors (GetLastT()  )                    

                    with
                    | :?  TimeoutException as  TimeoutException ->
                        printfn "Timeout Exception"
                        SendRequestToNeighbors (GetLastT()  + 1)

                    | exc -> printfn "Some error. Message: %s" exc.Message
                    
            })

    
    member this.SetAgentMesh (agentMesh: PointAgent[,]) = 
        AgentMesh <- agentMesh

    member this.SetInitialValue(initialValue:double) =
       dictionaryValue <- Map.empty<int,double>.Add(0,initialValue)
       
    member this.Start(?agentMesh: PointAgent[,], ?initialValue:double) = 
        if agentMesh.IsSome then
            this.SetAgentMesh(agentMesh.Value)
        if initialValue.IsSome then
           this.SetInitialValue(initialValue.Value)
            
        agent.Start()      

    member this.PostAndAsyncReply = agent.PostAndReply
    member this.Post = agent.Post

[<EntryPoint>]
let main argv = 

    let rows = 4
    let cols = 4
    let deltaH = 5.
    let agentMesh = Array2D.init<PointAgent> rows cols (fun i -> fun j -> new PointAgent(i,j,deltaH))
    

    let PrintMesh (forT:int) =
        let mutable valueMesh = Array2D.create rows cols 0.0
        for i = 0 to (rows - 1) do
            for j = 0 to (cols - 1) do
                let reply = agentMesh.[i,j].PostAndAsyncReply(fun rep -> ReqValueAndReply(forT,rep))
                let value = match reply with 
                    | ResExist(_,_,_,v) -> v 
                valueMesh.[i,j] <- value
        printfn "%A" valueMesh
        valueMesh


    for i = 0 to (rows - 1) do
        for j = 0 to (cols - 1) do
            agentMesh.[i,j].SetAgentMesh(agentMesh)
            agentMesh.[i,j].SetInitialValue(2.1)

    
    for j =0 to (cols - 1) do 
        agentMesh.[(rows - 1),0].SetInitialValue(100.)

    for i = 0 to (rows - 1) do
        for j = 0 to (cols - 1) do
            agentMesh.[i,j].Start()
    
    System.Threading.Thread.Sleep(10000)
    for t = 0 to 2 do
        PrintMesh t |> ignore
        



    

    
    System.Console.ReadKey() |> ignore
    0 // return an integer exit code
