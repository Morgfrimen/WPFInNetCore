namespace ClassLibraryFSharp

module public Logical=
    let mutable private MatrixA:float32[,] = null
    let mutable private Znak:string[] = null
    let mutable private VecB:float32[] = null
    let mutable private VecC:float32[] = null
    let mutable private W:float32 =  0.0f
    let mutable private D : float32[] = null
    let mutable private Базис : int32 = -1

    //Вспомогательные функции, по сути велосипед, ибо Array2D.copy работает не так, как ожидается
    let private array2DCopy (oldArray:float32[,] , newArray: float32[,]) : float32[,]=
        for i in 0..oldArray.GetLength(0)-1 do
            for j in 0..oldArray.GetLength(1)-1 do
                newArray.[i,j]<-oldArray.[i,j]
        newArray
    
    let private arrayCopyFloat (oldArray:float32[] , newArray: float32[]) : float32[]=
        for i in 0..oldArray.GetLength(0)-1 do
            newArray.[i]<-oldArray.[i]
        newArray

    let private arrayCopyInt (oldArray:int32[] , newArray: int32[]) : int32[]=
        for i in 0..oldArray.GetLength(0)-1 do
            newArray.[i]<-oldArray.[i]
        newArray
    //TODO: Для проверки разбить большие пункты как нужно - создать нормальную иеррархию
    //TODO: Пункт 10 допилить
    let Пункт9 ():unit =
        if D.[Базис] >= 0.0f then printfn "Переход к пункту 17 - обычный симплекс метод"
        else printfn "Универсальные симплекс метод - переход к пункту 10"
        


    let Пунк8 ():unit =
        let Dmin = 
            let mutable DNew:array<float32> =
                Array.copy D
            Базис <- Array.findIndex (fun elem->elem = Array.min DNew ) DNew
            Array.min DNew
        printfn "Базис первый в массиве D = %d , а минимальное значение: %F" Базис Dmin
        Пункт9()


    //тут нужно пихать list с индексами переменных без базисных переменных
    let Пункт5 (arrayList : int[]):unit=
        let mutable notBazicOgr = []
        let arraysListing = arrayCopyInt (arrayList ,(Array.create (MatrixA.GetLength(1)) 0))
        let matrixX = [|for i in 0..MatrixA.GetLength(1)-1 do i|]
        if arrayList.Length <> MatrixA.GetLength(1) then
            printfn "Все не круто, нужно добавлять базисные переменные"
            for i in matrixX do
                if arraysListing.[i] <> matrixX.[i] then
                    notBazicOgr <- matrixX.[i]::notBazicOgr    
            let mutable newMatrixA = Array2D.create (MatrixA.GetLength(0)+notBazicOgr.Length) (MatrixA.GetLength(1)) 0.0f
            newMatrixA<-array2DCopy (MatrixA ,newMatrixA)
            for i in 0..notBazicOgr.Length-1 do
                newMatrixA.[MatrixA.GetLength(0)+i,notBazicOgr.[i]]<-1.0f
            MatrixA<-newMatrixA
        printfn "Новая матрица А :" 
        printfn "%A" MatrixA
        for i in notBazicOgr do
            W<-W+VecB.[i]
        W <- -W
        printfn "Сумма W = %F" W
        D <- Array.create (MatrixA.GetLength(0)) 0.0f
        for n in 0..MatrixA.GetLength(0)-notBazicOgr.Length-1 do
            for m in notBazicOgr do
                D.[n]<- D.[n] - MatrixA.[n,m]
        printfn "Новый вектор D: %A" D
        Пунк8()

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
        let mutable indexBazM = Array.empty
        for row in 0..MatrixA.GetLength(0)-1 do
            //let mutable count:int = 1
            let mutable rows = [|for i in 0..MatrixA.GetLength(1)-1 -> MatrixA.[row,i]|]
            printfn "Пункт 3 - строка %d : %A" row rows
            for i in 0..rows.Length-1 do
               if Array.sum rows = rows.[i] && Array.sum rows >0.0f 
               then 
                   printfn "Базисная переменная %d" (row+1)
                   countX<-countX+1
                   indexBazM<-Array.append indexBazM [|i|]
        if countX=MatrixA.GetLength(0) then printfn "Количество базисных переменных равно количеству X"
        else 
            printfn "Требуется универсальный симплекс метод"
            Пункт5 indexBazM
        printfn "Отыграл Пункт 3" 

    //[<EntryPoint>]
    let StartProgram ():unit =
        Пунк2 Znak
        Пунк3 MatrixA
        //0
        
    



