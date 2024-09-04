//варианты 9, 10, 11
let e = 2.71828
let eps = 0.0000000001

let rec dichotomy f a b = 
    let middle = (a + b) / 2.
    if f(middle)<eps then
        middle
    else
        if (f(a)*f(b))<0. then
            if (f(a) * f(middle))<0. then
                dichotomy f a middle
            else
                dichotomy f middle b
        else
            middle

let rec iterations phi x0 =
    if abs(x0 - phi(x0))<eps then
        phi(x0)
    else
        iterations phi (phi(x0))
    

let newthon f f' x0 = 
    let phi x = x - f(x)/f'(x)
    iterations phi x0

// Решите 3 уравнения (начиная со своего номера варианта) с использованием 3-х методов
let f1 x = x ** 2. - log(1. + x) - 3.
let f2 x = 2. * x * sin(x) - cos(x)
let f3 x = (e ** x) + sqrt(1. + (e ** (x * 2.))) - 2.

let f1' x = 2. * x - (1. / ( 1. + x))
let f2' x = 2. * x * cos(x) + 3. * sin(x)
let f3' x = ((e ** x) * sqrt(1. + (e ** (2. * x))) + (e ** (2. * x))) / (sqrt((e ** (2. * x)) + 1.))

let phi1 x = x - f1(x) / f1'(x)
let phi2 x = x - f2(x) / f2'(x)
let phi3 x = x - f3(x) / f3'(x)     

let main = 
    printfn "%10.5f  %10.5f  %10.5f" (dichotomy f1 2. 3.) (iterations phi1 2.) (newthon f1 f1' 3.)
    printfn "%10.5f  %10.5f  %10.5f" (dichotomy f2 0.4 1.) (iterations phi2 0.4) (newthon f2 f2' 1.)
    printfn "%10.5f  %10.5f  %10.5f" (dichotomy f3 -1. 0.) (iterations phi3 -1.) (newthon f3 f3' 0.)

 