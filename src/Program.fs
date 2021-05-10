module Program = 

    open System
    open Microsoft.AspNetCore.Hosting
    open Microsoft.Extensions.Hosting
    open Microsoft.Extensions.DependencyInjection
    open Microsoft.AspNetCore.Builder
    open Routing
    open Handler
    open Giraffe

    let configureApp (webHostContext: WebHostBuilderContext) (app: IApplicationBuilder) =
        let webApp =
            GET >=> choose [
                    route "/ping" >=> text "pong"
                    route "/fish" >=> text "fish"
                    routexp "/escher(/.*)" escherHandler
            ]
        app.UseGiraffe webApp

    let configureServices (webHostContext: WebHostBuilderContext) (services: IServiceCollection) =
        services
            .AddGiraffe()
            |> ignore

    let createHostBuilder args =
        Host.CreateDefaultBuilder(args)
            .ConfigureWebHostDefaults(fun webBuilder -> 
                webBuilder
                    .Configure(configureApp)
                    .ConfigureServices(configureServices)
                |> ignore
            )

    [<EntryPoint>]
    let main argv =
        createHostBuilder(argv).Build().Run()
        0