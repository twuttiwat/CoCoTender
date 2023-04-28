module MyRouter

type Page =
    | Login
    | Home
    | BoQ

let parseUrl = function
    | [ ] -> Login
    | [ "secured" ] -> Home
    | [ "boq" ] -> BoQ
    | [ "login" ] -> Login
    | _ -> Login
