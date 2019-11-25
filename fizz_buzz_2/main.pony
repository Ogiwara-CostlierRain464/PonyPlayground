// cf. https://gist.github.com/timyates/8e2cb554d3df5bc136e2#file-fizzbuzz-pony-L13

type FbVal is (U64, (String|None))
type FbNext is (FbActor | None)
  
actor FbActor
  var _env  : Env
  var _mod  : U64
  var _word : String
  var _next : FbNext

  new create(env: Env, mod: U64, word : String, next: FbNext = None) =>
    _env  = env
    _mod  = mod
    _word = word
    _next = next
    
  fun _decorate(num: FbVal) : FbVal =>
    if (num._1 % _mod) == 0 then
      match num
      | (var num':U64, var txt':String) =>
        (num', txt' + _word)
      else
        (num._1, _word)
      end  
    else
      num
    end    
    
  be send(num: FbVal) =>
    var modified:FbVal = _decorate(num)
    match _next
    | var next': FbActor =>
      next'.send(modified)
    else
      match modified
      | (var num':U64, var txt':String) =>
        _env.out.print(txt')
      | (var num':U64, None) =>
        _env.out.print(num'.string())
      end
    end

actor Main
  new create(env: Env) =>
    var buzz: FbActor = FbActor.create(env, 5, "Buzz")
    var fizz: FbActor = FbActor.create(env, 3, "Fizz", buzz)
    var count: U64 = 1
    while count <= 15 do
      var init: FbVal = (count, None)
      fizz.send(init)
      count = count + 1
    end

