//namespace ClassLibraryFSharp

//TODO: Оптимизировать??? (уменьшить объём памяти, напрмиер индекты можно вообще в byte впихнуть)


module public Logical
    open System
    open SimplexMethodUniversal

    let mutable private MatrixA:float[,] = null
    let mutable private Znak:string[] = null
    let mutable private VecB:float[] = null
    let mutable private VecC:float[] = null
    let mutable private Z:float = 0.0
    let mutable private W:float =  0.0
    let mutable private D : float[] = null
    let mutable private Базис : int32 = -1
    let mutable private Опор : int32 = -1
    let mutable private Eps : int32 = 5
    let mutable internal ArrayBazResult : int32[] = null
    //let mutable private W0 : float =0.0
    //let mutable private Z0:float=0.0
    let mutable private countX:int = 0


    //Вспомогательные функции, по сути велосипед, ибо Array2D.copy работает не так, как ожидается
    let private array2DCopy (oldArray:float[,] , newArray: float[,]) : float[,]=
        for i in 0..oldArray.GetLength(0)-1 do
            for j in 0..oldArray.GetLength(1)-1 do
                newArray.[i,j]<-oldArray.[i,j]
        newArray
    
    let private arrayCopyFloat (oldArray:float[] , newArray: float[]) : float[]=
        for i in 0..oldArray.GetLength(0)-1 do
            newArray.[i]<-oldArray.[i]
        newArray

    let private arrayCopyInt (oldArray:int32[] , newArray: int32[]) : int32[]=
        for i in 0..oldArray.GetLength(0)-1 do
            newArray.[i]<-oldArray.[i]
        newArray
    //TODO: Для проверки разбить большие пункты как нужно - создать нормальную иеррархию
    //TODO: Пункт 10 допилить
   

       
        
    let mutable private resultUniversal:System.Collections.ArrayList = null

    let Пункт9 ():bool =
        if D.[Базис] >= 0.0 && W<=0.0 then //ибо float кидается погрешностью и происходит зацикливание
            //printfn "Переход к пункту 17 - обычный симплекс метод"
            false
        else
            let res = SimplexMethodUniversal.Пункт10И16(MatrixA,VecB,VecC,Z,W,D,Базис(*,Опор*)(*,Z0,W0*))
            
            let checkW = W
            if res.Count=0 || checkW = (res.[4]:?>float) then 
                false
            else
                resultUniversal<-res
                MatrixA<-(res.[0]:?>float[,])
                VecB<-(res.[1]:?>float[])
                VecC<-(res.[2]:?>float[])
                Z<-(res.[3]:?>float)
                W<-(res.[4]:?>float)
                D<-(res.[5]:?>float[])
                Опор<-(res.[6]:?>int32)
                Базис<-(res.[7]:?>int32)
                Array.set ArrayBazResult Опор (Базис+1)
                true
            
            

    let rec Пункт8 ():System.Collections.ArrayList =
        let Dmin = 
            let mutable DNew:array<float> =
                Array.copy D
            Базис <- Array.findIndex (fun elem->elem = Array.min DNew ) DNew
            Array.min DNew
        //printfn "Базис первый в массиве D = %d , а минимальное значение: %F" Базис Dmin
        if Пункт9()=true then
            Пункт8()
        else
            //printfn "Из пункта 8 : обычный симплкс-метотод"
            //printfn "Результирующие базисы: %A " ArrayBazResult
            //printfn "А результаты: %A" VecB
            SimpleSimplexMethod.Пункт17И25(MatrixA,VecB,VecC,Z,ArrayBazResult) //пока игнор


    //тут нужно пихать list с индексами переменных без базисных переменных
    let Пункт5 (arrayList : int[]):System.Collections.ArrayList=
        let mutable notBazicOgr = []
        let arraysListing = arrayCopyInt (arrayList ,(Array.create (MatrixA.GetLength(1)) 0))
        let matrixX = [|for i in 0..MatrixA.GetLength(1)-1 do i|]
        if arrayList.Length <> MatrixA.GetLength(1) then
            for i in matrixX do
                if arraysListing.[i] <> matrixX.[i] then
                    notBazicOgr <- matrixX.[i]::notBazicOgr    
            let mutable newMatrixA = Array2D.create (MatrixA.GetLength(0)+notBazicOgr.Length) (MatrixA.GetLength(1)) 0.0
            newMatrixA<-array2DCopy (MatrixA ,newMatrixA)
            for i in 0..notBazicOgr.Length-1 do
                newMatrixA.[MatrixA.GetLength(0)+i,notBazicOgr.[i]]<-1.0
            MatrixA<-newMatrixA
            ArrayBazResult <- Array.create (MatrixA.GetLength(1)) 0
        //printfn "Новая матрица А :" 
        //printfn "%A" MatrixA
        for i in notBazicOgr do
            W <- W+VecB.[i]
        //printfn "Сумма W = %F" W
        D <- Array.create (MatrixA.GetLength(0)) 0.0
        for n in 0..MatrixA.GetLength(0)-notBazicOgr.Length-1 do
            for m in notBazicOgr do
                D.[n]<- D.[n] - MatrixA.[n,m]
        //printfn "Новый вектор D: %A" D
        Пункт8()

    //неравенства в равенства
    let private Пунк2 arraysZnak =
        for znak in 0..Znak.Length-1 do
                   if Znak.[znak]="<" then 
                       let mutable newMatrixA = 
                            Array2D.create (MatrixA.GetLength(0)+1) (MatrixA.GetLength(1)) 0.0
                       Array2D.blit MatrixA 0 0 newMatrixA 0 0 (MatrixA.GetLength(0)) (MatrixA.GetLength(1))
                       Array2D.set newMatrixA (newMatrixA.GetLength(0)-1) znak 1.0
                       MatrixA<-newMatrixA
                       countX<-countX+1
                   elif Znak.[znak]=">" then 
                       let mutable newMatrixA = 
                           Array2D.create (MatrixA.GetLength(0)+1) (MatrixA.GetLength(1)) 0.0
                       Array2D.blit MatrixA 0 0 newMatrixA 0 0 (MatrixA.GetLength(0)) (MatrixA.GetLength(1))
                       Array2D.set newMatrixA (newMatrixA.GetLength(0)-1) znak -1.0
                       MatrixA<-newMatrixA
        //printfn "Отыграл пункт 2"
        
    let private Пунк3 arrayMatrixA =
        let mutable indexBazM = Array.empty
        for row in 0..MatrixA.GetLength(0)-1 do
            //let mutable count:int = 1
            let mutable rows = [|for i in 0..MatrixA.GetLength(1)-1 -> MatrixA.[row,i]|]
            //printfn "Пункт 3 - строка %d : %A" row rows
            for i in 0..rows.Length-1 do
               if Array.sum rows = rows.[i] && Array.sum rows >0.0 
               then 
                   //printfn "Базисная переменная %d" (row+1)
                   countX<-countX+1
                   indexBazM<-Array.append indexBazM [|i|]
                   //ArrayBazResult.[i]<- i+1
        if countX=MatrixA.GetLength(0) then 
            //printfn "Количество базисных переменных равно количеству X"
            ArrayBazResult <- Array.create (MatrixA.GetLength(1)) 0
            SimpleSimplexMethod.Пункт17И25(MatrixA,VecB,VecC,Z,ArrayBazResult) //пока игнор
        else 
            //printfn "Требуется универсальный симплекс метод"
            Пункт5 indexBazM
        //printfn "Отыграл Пункт 3" 

    //[<EntryPoint>]
    let public StartProgram (A,Zn,B,C):System.Collections.ArrayList =
        MatrixA<-A
        Znak<-Zn
        VecB<-B
        VecC<-C
        Z<-0.0
        W<-0.0
        D<-null
        ArrayBazResult<-null
        Пунк2 Znak
        Пунк3 MatrixA
        //0
        
 



