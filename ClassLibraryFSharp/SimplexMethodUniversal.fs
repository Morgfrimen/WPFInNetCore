module private SimplexMethodUniversal

    let mutable private MatrixA:float[,] = null
    let mutable private VecB:float[] = null
    let mutable private VecC:float[] = null
    let mutable private Z:float = 0.0
    let mutable private W:float =  0.0
    let mutable private D : float[] = null
    let mutable private Базис : int32 = -1
    let mutable private Опор : int32 = -1
    let mutable private Eps : int32 = 5


    //let levVecC() =
    //    if VecC.Length <> D.Length then
    //        let mutable newVecC = Array.create D.Length 0.0
    //        for i in 0..VecC.Length-1 do
    //            newVecC.[i] <- VecC.[i]
    //        VecC <- newVecC                        


    let Пункт10И16 (A,B,C,z,w,d,баз(*,Оп*)(*,z0,w0*)): System.Collections.ArrayList =
           MatrixA <- A
           VecB <- B
           VecC <- C
           Z<-z
           W<-w
           D<-d
           Базис<-баз
           //Опор<-Оп
           //W0<-w0
           //Z0<-z0
           if VecC.Length-1 <Базис then
               let result = 
                  new System.Collections.ArrayList ()
               result.Add(MatrixA)|>ignore
               result.Add(VecB)|>ignore
               result.Add(VecC)|>ignore
               result.Add(Z)|>ignore
               result.Add(W)|>ignore
               result.Add(D)|>ignore
               result.Add(Опор)|>ignore
               result.Add(Базис)|>ignore
               result
           else
               let NewBOpor:float[] = [|for i in 0..VecB.Length-1 do if MatrixA.[Базис,i]>0.0 then System.Math.Round(VecB.[i]/MatrixA.[Базис,i],Eps) else System.Double.PositiveInfinity|]
               let BOpor = Array.min NewBOpor
               //printfn "Матрица А"
               //printfn "%A" MatrixA
               Опор<-Array.findIndex (fun elem->elem = BOpor ) NewBOpor
               //11
               VecB<-[|for i in 0..VecB.Length-1 do if i<>Опор then System.Math.Round(VecB.[i] - BOpor * MatrixA.[Базис,i],Eps) else NewBOpor.[i]|]
               //12
               //printfn"Было:%A" MatrixA
               for i in 0..MatrixA.GetLength(0)-1 do
                   if i<>Базис then
                       MatrixA.[i,Опор]<- System.Math.Round(MatrixA.[i,Опор]/MatrixA.[Базис,Опор],Eps)
               //13
               for i in 0..MatrixA.GetLength(0)-1 do
                   for j in 0..MatrixA.GetLength(1)-1 do
                       if i <> Базис && j <> Опор then
                           MatrixA.[i,j]<- System.Math.Round(MatrixA.[i,j] - MatrixA.[i,Опор]*MatrixA.[Базис,j],Eps)
               
               //14
               for i in 0..VecC.Length-1 do
                   if i<>Базис then
                       VecC.[i] <- System.Math.Round(VecC.[i] - MatrixA.[i,Опор] * VecC.[Базис],Eps)
               for i in 0..D.Length-1 do
                   if i <> Базис then
                       D.[i]<- System.Math.Round(D.[i] - MatrixA.[i,Опор]*D.[Базис],Eps)
               //15
               Z <- System.Math.Round(Z + VecC.[Базис] * VecB.[Опор],Eps)
               W <- System.Math.Round(W + D.[Базис] * VecB.[Опор],Eps)
               //printfn "Значение целевой функции: %F" Z
               //printfn "Значение новой целевой функции: %F" W
               //printfn "А занчение В : %F  D= %F C = %F" VecB.[Опор]  D.[Базис] VecC.[Базис]
               //16
               VecC.[Базис] <- 0.0
               D.[Базис] <- 0.0
               MatrixA.[Базис,Опор]<- 1.0
               for i in 0..MatrixA.GetLength(1)-1 do
                   if i <> Опор then
                       MatrixA.[Базис,i] <- 0.0
               //printfn"Стало:%A" MatrixA
               let result = 
                    new System.Collections.ArrayList ()
               result.Add(MatrixA)|>ignore
               result.Add(VecB)|>ignore
               result.Add(VecC)|>ignore
               result.Add(Z)|>ignore
               result.Add(W)|>ignore
               result.Add(D)|>ignore
               result.Add(Опор)|>ignore
               result.Add(Базис)|>ignore
               result
           
             