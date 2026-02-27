
open System

let rec inputInt () = 
    printf "Введите натуральное число: "
    match System.Int32.TryParse(Console.ReadLine()) with
    | (true, convertInt) when convertInt > 0 -> convertInt
    | _ -> 
        printfn "Ошибка, повторите ввод"
        inputInt ()

let rec findMax ourInt maxVal = 
    match ourInt with
    | 0 -> maxVal
    | _ ->
        if maxVal < ourInt%10 then
            findMax (ourInt / 10) (ourInt % 10)
        else findMax (ourInt / 10) maxVal

[<EntryPoint>]
let main args = 
    printfn "Максимальная цифра: %i" (findMax (inputInt ()) 0)
    0