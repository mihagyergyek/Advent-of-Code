module String = struct
  include Stdlib.String

  let startswith prefix hay =
    let l_p = String.length prefix in
    if l_p > String.length hay then false else String.sub hay 0 l_p = prefix

  let rec repeat s = function n when n <= 0 -> "" | n -> s ^ repeat s (n - 1)

  (** Pridobljeno: https://batsov.com/articles/2022/10/24/ocaml-tips-converting-a-string-to-a-list-of-characters/ *)
  let explode_string s = List.init (String.length s) (String.get s)
end
