use "http"

actor Main
    new create(env: Env) =>
        try
            let client = HTTPClient(env.root as AmbientAuth, None)
            let url = URL.build("http://www.example.com")?
            let request = Payload.request("GET", url)
            request("User-Agent") = "Pony"
            client(consume request, recover val _Factory(env.out) end)?
        else
            env.out.print("failed")
        end    

class _Factory
    let _out: OutStream

    new create(out: OutStream) =>
        _out = out

    fun box apply(session: HTTPSession tag): HTTPHandler ref =>
        _HTTPHandler(_out)


class _HTTPHandler is HTTPHandler
    let _out: OutStream

    new create(out: OutStream) =>
        _out = out

    fun ref chunk(data: (String | Array[U8 val] val)) =>
        _out.print("chunk!")   