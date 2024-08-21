whatIsQuickSort = "quick sort is a sorting algorithm that sorts items with a divide and conquer method that selects a pivot, and compare all other items to it placing them in a higher and lower list on either side which it is concatonated it to depending on asc or dsc, then quicksort is performed on those sublists until the base case of an empty list. Its quality."

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort [a | a <- xs, a > x]
    in smallerSorted ++ [x] ++ biggerSorted