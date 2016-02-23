// --------------------------------------------------------------------------------------
// Minimal Suave.io server!
// --------------------------------------------------------------------------------------

#r "packages/Suave/lib/net40/Suave.dll"
open Suave
open Suave.Http
open Suave.Http.Applicatives
open Suave.Http.Files
open Suave.Http.Redirection
open Suave.Http.RequestErrors
open Suave.Http.Successful
open Suave.Types
open Suave.Web

let mutable id = 0
let mutable todos = []

let getTodos () = 
    todos
    |> List.map (fun (t) -> sprintf "{ \"id\": %d, \"text\": \"%s\" }" (fst t) (snd t))
    |> String.concat ","
    |> sprintf "{ \"todos\": [ %s ] }"
    
let add text =
    match text with
    | Some(t) ->
        let next = id + 1
        id <- next
        todos <- (id, t) :: todos
    | None -> ()
        
let remove id = 
    let removed =
        todos
        |> List.filter (fun t -> (fst t) <> id)
    todos <- removed

let app : WebPart = 
    choose
        [
            GET >>= choose
                [
                    path "/" >>= file ("index.html")
                    path "/todos" >>= request (fun req -> OK (getTodos()))
                    pathScan "/static/%s" (fun (filename) -> file (sprintf "./static/%s" filename))
                ]
            POST >>= choose
                [
                    path "/todos" >>= request (fun req -> add (req.formData "text") ; OK "")
                ]
            DELETE >>= choose
                [
                    pathScan "/todos/%d" (fun (id) -> remove id ; OK "")
                ]
        ]

// If you prefer to run things manually in F# Interactive (over running 'build' in 
// command line), then you can use the following commands to start the server
#if TESTING
// Starts the server on http://localhost:8083
let config = { defaultConfig with homeFolder = Some __SOURCE_DIRECTORY__ }
let _, server = startWebServerAsync config app
let cts = new System.Threading.CancellationTokenSource()
Async.Start(server, cts.Token)
// Kill the server (so that you can restart it)
cts.Cancel()
#endif