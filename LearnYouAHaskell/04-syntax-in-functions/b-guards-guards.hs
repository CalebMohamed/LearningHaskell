howDoGuardsDifferFromPatterns = "patterns allow you to deconstruct an input and then reference those deconstructed parts, whereas guards test a specific condition and then run the body: func x; | x < y = body; | x > z = body; | otherwise = body, the pattern goes first then the guards, but you don't put an = after the pattern, the equals are in the guards"
whatIsTheGuardOtherwiseFor = "the otherwise guard is to catch all cases where the data does not fit the conditions, if all the guards fall through then it goes to the next pattern so in light of the previous example func x:xs; | x < y..."
whatIfIHaveACommonCalculationAcrossGuards = "do reduce redundancy you can just name them and after the guards write 'where name = calculation', names defined like this have a local scope and don't 'pollute the namespace' if you want to define multiple names then indent all of them with one where. you can also use pattern matching here. These are called bindings. You can also bind function definitions too :O"