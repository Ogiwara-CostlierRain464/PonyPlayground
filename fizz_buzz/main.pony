use "collections"

actor Main
    new create(env: Env) =>
        try
            let n = env.args(1)?.usize()?
            for i in Range(0, n) do
                env.out.print(
                    try
                        fizz_buzz(i)?
                    else
                        i.string()
                    end
                    )
            end
        end


    fun fizz_buzz(i: USize): String ? =>
        if (i % 15) == 0 then
            "FizzBuzz"
        elseif (i % 5) == 0 then  
            "Fizz"
        elseif (i % 3) == 0 then
            "Buzz"
        else
            error
        end                  
    