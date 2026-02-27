open System

let rec inputInt () = 
    printf "Введите индекс: "
    match System.Int32.TryParse( Console.ReadLine() ) with
    | ( true, convertInt ) when convertInt >= 0 -> convertInt
    | _ -> 
        printfn "Ошибка, повторите ввод"
        inputInt ()

let addEl inpList addVal :list<string>  = // функция добавления элементов в список
    inpList @ [addVal]

let merLists firList secList = 
    firList @ secList

let rec findEl inpList findVal index = 
    match inpList with
        | [] -> -1
        | head :: tail when head = findVal -> index
        | _ :: tail -> findEl tail findVal (index + 1)

let rec delEl inpList delVal = 
    match inpList with 
        | [] -> []
        | head :: tail when head = delVal -> tail
        | head :: tail -> head :: delEl tail delVal

let rec getValList inpList index = 
     match inpList with
        | [] -> []
        | head :: tail -> 
            if index = 0 then [head]
            else getValList tail (index - 1) 

let rec createListAuto inpList addVal count = 
    let valInStr = string(addVal)
    if count > 0 then
        createListAuto (inpList @ [valInStr]) (addVal + 1)  (count - 1)
    else 
        inpList

let rec createList inpList = 
    printf "Введите добавляемое значение(0 - конец): "
    let addVal = Console.ReadLine()
    match addVal with
    | "0" -> inpList
    | _ -> createList (inpList @ [addVal])

let rec selectOpt inpList=
    printfn "\nCписок: %A" inpList
    printfn "1 - создание нового списка автоматически"
    printfn "2 - создание нового списка вручную"
    printfn "3 - добавление элемента в список"
    printfn "4 - удаление элемента по значению"
    printfn "5 - поиск элемента по значению"
    printfn "6 - сцепка двух списков"
    printfn "7 - получение элемента по индексу(нумерация с нуля)"
    printfn "0 - выход"
    printf "Выберите операцию: "
    let option = Console.ReadLine()
    match option with
    | "1" -> selectOpt (createListAuto [] 0 3)
    | "2" -> selectOpt (createList [])
    | "3" -> 
        printf "Введите добавляемое значение: "
        selectOpt (addEl inpList (Console.ReadLine()))
    | "4" -> 
        printf "Введите удаляемое значение: "
        selectOpt(delEl inpList (Console.ReadLine()))
    | "5" ->
        printf "Введите значение индекс которого хотите найти: "
        let findVal = Console.ReadLine()
        let findInd = findEl inpList findVal 0
        if findInd <> -1 then 
            printf "Индекс этого значения: %i" findInd
        else 
            printf "Значение не найдено"
        selectOpt inpList

    | "6" ->
        let secList = createListAuto [] 1 4
        printf "Второй список(создан автоматически): %A" secList
        selectOpt (merLists inpList secList)
    | "7" ->
        let index = inputInt ()
        let valEl = getValList inpList index
        printf "Элемент с индексом %i: %A" index valEl
        selectOpt inpList
    | "0" ->
        0
    | _ ->
        printfn "Ошибка выбора"
        selectOpt inpList

        


        

[<EntryPoint>]
let main args = 

    selectOpt (createListAuto [] 0 5)