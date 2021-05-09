module Program = 

    open Microsoft.Extensions.Hosting
    open Giraffe
    open Giraffe.GiraffeViewEngine

    let createHostBuilder args =
        Host.CreateDefaultBuilder(args)

    [<EntryPoint>]
    let main argv =
        createHostBuilder(argv).Build().Run()
        0
