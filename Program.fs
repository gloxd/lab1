/// Модуль для работы с числами и списками
module NumberOperations

open System

/// ========== СОБСТВЕННЫЕ ФУНКЦИИ ДЛЯ РАБОТЫ СО СПИСКАМИ ==========

/// Добавление элемента в начало списка
let addToBeginning element list = element :: list

/// Добавление элемента в конец списка
let addToEnd element list =
    list @ [element]

/// Удаление первого вхождения элемента из списка
let removeFirst element list =
    let rec removeRec acc remaining =
        match remaining with
        | [] -> List.rev acc  // Элемент не найден, возвращаем исходный список
        | head :: tail when head = element -> (List.rev acc) @ tail
        | head :: tail -> removeRec (head :: acc) tail
    
    removeRec [] list

/// Удаление всех вхождений элемента из списка
let removeAll element list =
    list |> List.filter (fun x -> x <> element)

/// Удаление элемента по индексу
let removeAtIndex index list =
    if index < 0 || index >= List.length list then
        failwithf "Индекс %d вне диапазона (0..%d)" index (List.length list - 1)
    else
        let beforeIndex = list |> List.take index
        let afterIndex = list |> List.skip (index + 1)
        beforeIndex @ afterIndex

/// Поиск индекса первого вхождения элемента
let findIndex element list =
    let rec findRec currentIndex remaining =
        match remaining with
        | [] -> None  // Элемент не найден
        | head :: _ when head = element -> Some currentIndex
        | _ :: tail -> findRec (currentIndex + 1) tail
    
    findRec 0 list

/// Проверка наличия элемента в списке 
let contains element list =
    match findIndex element list with
    | Some _ -> true
    | None -> false

/// Сцепка двух списков (конкатенация)
let concatenate list1 list2 =
    list1 @ list2

/// Получение элемента по индексу
let getAtIndex index list =
    if index < 0 then
        failwithf "Индекс %d не может быть отрицательным" index
    else
        let rec getRec currentIndex remaining =
            match remaining with
            | [] -> failwithf "Индекс %d вне диапазона (максимальный индекс: %d)" index (currentIndex - 1)
            | head :: _ when currentIndex = index -> head
            | _ :: tail -> getRec (currentIndex + 1) tail
        
        getRec 0 list

/// Получение элемента по индексу (безопасная версия - возвращает Option)
let tryGetAtIndex index list =
    if index < 0 then
        None
    else
        let rec getRec currentIndex remaining =
            match remaining with
            | [] -> None
            | head :: _ when currentIndex = index -> Some head
            | _ :: tail -> getRec (currentIndex + 1) tail
        
        getRec 0 list

/// ========== ОСНОВНЫЕ ЗАДАНИЯ ==========

/// Задание 1: Формирование списка из чисел, противоположных вводимым значениям
let getOppositeNumbers (numbers: float list) : float list =
    List.map (fun x -> -x) numbers

/// Задание 2: Подсчет количества цифр в записи натурального числа
let countDigits (number: uint64) : int =
    if number = 0UL then 1
    else
        let rec countDigitsRec (n: uint64) (acc: int) : int =
            if n = 0UL then acc
            else countDigitsRec (n / 10UL) (acc + 1)
        countDigitsRec number 0

/// ========== ФУНКЦИИ ДЛЯ ВВОДА ==========

/// Функция для ввода списка чисел с клавиатуры 
let rec inputNumberList () : float list =
    printfn "Введите числа через пробел (например: 5 -3.5 10.2 0 -7.8)"
    printfn "(или введите 'q' для выхода):"
    let input = Console.ReadLine()
    
    if input = "q" then
        []
    else
        let numbers =
            input.Split(' ', StringSplitOptions.RemoveEmptyEntries)
            |> Array.choose (fun s ->
                // Пробуем распарсить как double 
                match Double.TryParse(s, Globalization.NumberStyles.Any, Globalization.CultureInfo.InvariantCulture) with
                | (true, n) -> Some n
                | _ ->
                    // Пробуем распарсить с текущей культурой (для запятой как разделителя)
                    match Double.TryParse(s, Globalization.NumberStyles.Any, Globalization.CultureInfo.CurrentCulture) with
                    | (true, n) -> Some n
                    | _ -> 
                        printfn "'%s' не является числом и будет пропущено" s
                        None)
            |> Array.toList
        
        if List.isEmpty numbers then
            printfn "Не введено ни одного корректного числа"
            inputNumberList()
        else
            numbers

/// Функция для ввода одного натурального числа
let rec inputNaturalNumber () : uint64 =
    printfn "Введите натуральное число (целое положительное или 0):"
    let input = Console.ReadLine()
    
    match UInt64.TryParse(input) with
    | (true, n) -> n
    | _ ->
        printfn "Ошибка: '%s' не является натуральным числом. Попробуйте снова." input
        inputNaturalNumber()

/// ========== ДЕМОНСТРАЦИЯ СОБСТВЕННЫХ ФУНКЦИЙ ==========

let demonstrateListFunctions () =
    printfn "\n========== ДЕМОНСТРАЦИЯ СОБСТВЕННЫХ ФУНКЦИЙ ДЛЯ РАБОТЫ СО СПИСКАМИ ==========\n"
    
    // Создаем исходный список 
    let originalList = [10.0; 20.0; 30.0; 40.0; 50.0; 20.0]
    printfn "Исходный список: %A\n" originalList
    
    // Демонстрация добавления элементов
    printfn "--- Добавление элементов ---"
    let withAddedToStart = addToBeginning 5.0 originalList
    printfn "Добавление 5.0 в начало: %A" withAddedToStart
    
    let withAddedToEnd = addToEnd 60.0 originalList
    printfn "Добавление 60.0 в конец: %A\n" withAddedToEnd
    
    // Демонстрация удаления элементов
    printfn "--- Удаление элементов ---"
    let afterRemoveFirst = removeFirst 20.0 originalList
    printfn "Удаление первого вхождения 20.0: %A" afterRemoveFirst
    
    let afterRemoveAll = removeAll 20.0 originalList
    printfn "Удаление всех вхождений 20.0: %A" afterRemoveAll
    
    let afterRemoveAtIndex = removeAtIndex 2 originalList
    printfn "Удаление элемента с индексом 2: %A\n" afterRemoveAtIndex
    
    // Демонстрация поиска
    printfn "--- Поиск элементов ---"
    match findIndex 30.0 originalList with
    | Some idx -> printfn "Индекс первого вхождения 30.0: %d" idx
    | None -> printfn "Элемент 30.0 не найден"
    
    match findIndex 100.0 originalList with
    | Some idx -> printfn "Индекс первого вхождения 100.0: %d" idx
    | None -> printfn "Элемент 100.0 не найден"
    
    printfn "Содержит ли список 40.0? %b" (contains 40.0 originalList)
    printfn "Содержит ли список 99.0? %b\n" (contains 99.0 originalList)
    
    // Демонстрация сцепки списков
    printfn "--- Сцепка списков ---"
    let listA = [1.0; 2.0; 3.0]
    let listB = [4.0; 5.0; 6.0]
    printfn "Список A: %A" listA
    printfn "Список B: %A" listB
    printfn "Сцепка A и B: %A\n" (concatenate listA listB)
    
    // Демонстрация получения элемента по индексу
    printfn "--- Получение элементов по индексу ---"
    try
        let element = getAtIndex 3 originalList
        printfn "Элемент с индексом 3: %A" element
    with e -> printfn "Ошибка: %s" e.Message
    
    let safeElement = tryGetAtIndex 10 originalList
    match safeElement with
    | Some value -> printfn "Элемент с индексом 10: %A" value
    | None -> printfn "Элемент с индексом 10 не существует"

/// ========== ДЕМОНСТРАЦИЯ ОСНОВНЫХ ЗАДАНИЙ ==========

let demonstrateOppositeNumbers () =
    printfn "\n=== Задание 1: Формирование списка из противоположных чисел ===\n"
    
    let numbers = inputNumberList()
    
    if not (List.isEmpty numbers) then
        let oppositeNumbers = getOppositeNumbers numbers
        printfn "\nИсходный список: %A" numbers
        printfn "Список с противоположными числами: %A\n" oppositeNumbers
    else
        printfn "Задание 1 пропущено (список не введен)\n"

let demonstrateCountDigits () =
    printfn "\n=== Задание 2: Подсчет количества цифр в натуральном числе ===\n"
    
    let number = inputNaturalNumber()
    let digitCount = countDigits number
    
    printfn "Число %d содержит %d цифр\n" number digitCount

/// ========== ГЛАВНОЕ МЕНЮ ==========

let rec mainMenu () =
    printfn "\n==================== ГЛАВНОЕ МЕНЮ ===================="
    printfn "1. Задание 1: Противоположные числа (поддержка дробных)"
    printfn "2. Задание 2: Подсчет цифр в числе"
    printfn "3. Выполнить оба задания"
    printfn "4. Демонстрация собственных функций для работы со списками"
    printfn "5. Выход"
    printfn "======================================================"
    printf "Выберите действие (1-5): "
    
    match Console.ReadLine() with
    | "1" ->
        demonstrateOppositeNumbers()
        mainMenu()
    | "2" ->
        demonstrateCountDigits()
        mainMenu()
    | "3" ->
        demonstrateOppositeNumbers()
        demonstrateCountDigits()
        mainMenu()
    | "4" ->
        demonstrateListFunctions()
        mainMenu()
    | "5" ->
        printfn "Программа завершена. До свидания!"
        0
    | _ ->
        printfn "Неверный ввод. Пожалуйста, выберите числа 1, 2, 3, 4 или 5"
        mainMenu()

[<EntryPoint>]
let main argv =
    Console.OutputEncoding <- Text.Encoding.UTF8
    Console.InputEncoding <- Text.Encoding.UTF8
    
    printfn "Программа для работы с числами и списками"
    
    // Проверяем, есть ли аргументы командной строки
    if argv.Length > 0 then
        printfn "Обнаружены аргументы командной строки:"
        
        // Распрасить аргументы как числа для первого задания
        let argsAsNumbers =
            argv
            |> Array.choose (fun arg ->
                // Распарсить как double
                match Double.TryParse(arg, Globalization.NumberStyles.Any, Globalization.CultureInfo.InvariantCulture) with
                | (true, n) -> Some n
                | _ ->
                    match Double.TryParse(arg, Globalization.NumberStyles.Any, Globalization.CultureInfo.CurrentCulture) with
                    | (true, n) -> Some n
                    | _ -> None)
            |> Array.toList
        
        if not (List.isEmpty argsAsNumbers) then
            printfn "Исходный список из аргументов: %A" argsAsNumbers
            printfn "Противоположные числа: %A" (getOppositeNumbers argsAsNumbers)
            
            // Также демонстрируем работу собственных функций на этих данных
            printfn "\nДемонстрация собственных функций на введенных данных:"
        else
            printfn "Не удалось распознать числа в аргументах командной строки\n"
    
    // Запускаем интерактивное меню
    mainMenu()