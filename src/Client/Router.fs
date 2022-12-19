module MyRouter

type Page =
    | Home
    | BoQ

let parseUrl = function
    | [ ] -> Home
    | [ "boq" ] -> BoQ
    | _ -> Home
