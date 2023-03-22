module MyRouter

type Page =
    | Login
    | Home
    | BoQ

let parseUrl = function
    | [ ] -> Login
    | [ "boq" ] -> BoQ
    | [ "login" ] -> Login
    | _ -> Login
