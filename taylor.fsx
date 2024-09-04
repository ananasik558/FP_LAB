// вариант 9

// Series -(1 + 2/3) -(1 + 2/(3 ^ 2))x - ... -(1 + 2/(3^(n + 1)))x^n

// function ((3. * x - 5.) / (x * x - 4. * x + 3))
let f x = ((3. * x - 5.) / (x * x - 4. * x + 3.))

let a = 0.0
let b = 0.5
let n = 10
let eps = 0.001

let my_abs a =
    if a>=0. then
        a
    else
        -a

let rec my_pow a deg =
    if deg<=0. then 1.
    else a * (my_pow a (deg - 1.))

let taylor_naive x eps =
    let rec taylor_nai sum step x eps = 
        let cur = -(1. + 2./(my_pow 3. (step + 1.))) * (my_pow x step)
        if (my_abs cur) < eps then
            sum + cur
        else
            sum + (taylor_nai cur (step + 1.) x eps)
    taylor_nai 0. 0. x eps

let taylor_naive_step x eps =
    let rec taylor_nai_step step x eps = 
        let cur = -(1. + 2./(my_pow 3. (step + 1.))) * (my_pow x step)
        if (my_abs cur)  < eps then
            1.
        else
            step + (taylor_nai_step (step + 1.) x eps)
    taylor_nai_step 0. x eps

let taylor x eps =
    let rec taylor_r sum step prev x eps =
        let cur = prev / (1. + 2./(my_pow 3. step)) * x * (1. + 2./(my_pow 3. (step + 1.)))
        if (my_abs cur) < eps then
            sum + cur
        else
            sum + (taylor_r cur (step + 1.) cur x eps)
    let first = -5. / 3.
    taylor_r first 1. first x eps

let taylor_step x eps =
    let rec taylor_r_step step prev x eps =
        let cur = prev * x * (1. + 2./(my_pow 3. (step + 1.))) / (1. + 2./(my_pow 3. step))
        if (my_abs cur) < eps then
            1.
        else
            step + (taylor_r_step (step + 1.) cur x eps)
    let first = -5./3.
    taylor_r_step 1. first x eps

let main =
   printfn "   x \t f(x) \t dumbTaylor\t n \t smartTaylor \t n"
   for i=0 to n do
     let x = a+(float i)/(float n)*(b-a)
     printfn "%5.2f  %10.6f  %10.6f   %3.1f   %10.6f   %3.1f" x (f x) (taylor_naive x eps) (taylor_naive_step x eps) (taylor x eps) (taylor_step x eps)


main