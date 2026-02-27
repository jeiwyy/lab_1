//Сформировать список из последовательно вводимых слов.
open System

let rec inputWords inpList =
    printf "Введите слово (пустая строка - выход): "
    let inpStr = Console.ReadLine()
    match inpStr with
    | "" -> inpList
    | _ ->
        inputWords (inpList @ [inpStr])

[<EntryPoint>]
let main args = 
    printfn "Результат: %A" (inputWords [])
    0