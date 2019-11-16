namespace ClassLibraryFSharp

module public Logical=
    let mutable private MatrixA:float32[,] = null
    let mutable private Znak:string[] = null
    let mutable private VecB:float32[] = null
    let mutable private VecC:float32[] = null

    //тут нужно пихать list с индексами переменных без базисных переменных
    let Пункт5 arrayList : list<int>=
        printfn arrayList
        if arrayList.Length == MatrixA.GetLength(1) then
            printfn "Все круто"
        else 




    //неравенства в равенства
    let private Пунк2 arraysZnak =
        MatrixA<-SimplexMethod.MatrixA
        Znak<-SimplexMethod.Znak
        VecB<-SimplexMethod.VecB
        VecC<-SimplexMethod.VecC
        for znak in 0..Znak.Length-1 do
                   if Znak.[znak]="<" then 
                       let mutable newMatrixA = 
                            Array2D.create (MatrixA.GetLength(0)+1) (MatrixA.GetLength(1)) 0.0f
                       Array2D.blit MatrixA 0 0 newMatrixA 0 0 (MatrixA.GetLength(0)) (MatrixA.GetLength(1))
                       Array2D.set newMatrixA (newMatrixA.GetLength(0)-1) znak 1.0f
                       MatrixA<-newMatrixA
                   elif Znak.[znak]=">" then 
                       let mutable newMatrixA = 
                           Array2D.create (MatrixA.GetLength(0)+1) (MatrixA.GetLength(1)) 0.0f
                       Array2D.blit MatrixA 0 0 newMatrixA 0 0 (MatrixA.GetLength(0)) (MatrixA.GetLength(1))
                       Array2D.set newMatrixA (newMatrixA.GetLength(0)-1) znak -1.0f
                       MatrixA<-newMatrixA
        SimplexMethod.MatrixA <- MatrixA
        printfn "Отыграл пункт 2"
        
    let private Пунк3 arrayMatrixA =
        let mutable countX:int = 0
        let mutable indexBazM = List.empty
        for row in 0..MatrixA.GetLength(0)-1 do
            //let mutable count:int = 1
            let mutable rows = [|for i in 0..MatrixA.GetLength(1)-1 -> MatrixA.[row,i]|]
            printfn "Пункт 3 - строка %d : %A" row rows
            for i in 0..rows.Length-1 do
               if Array.sum rows = rows.[i] && Array.sum rows >0.0f 
               then 
                   printfn "Базисная переменная %d" (row+1)
                   countX<-countX+1
                   indexBazM<-countX::indexBazM
        if countX=MatrixA.GetLength(0) then printfn "Количество базисных переменных равно количеству X"
        else 
            printfn "Требуется универсальный симплекс метод"
            Пункт5 indexBazM
        printfn "Отыграл Пункт 3"    

   
        

    

    let StartProgram ():unit =
        Пунк2 Znak
        Пунк3 MatrixA
    



