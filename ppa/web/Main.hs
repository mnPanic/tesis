{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- \| Haskell module declaration

-- | Haskell language pragma
module Main where

-- \| Miso framework import
import Miso
import Miso.String hiding (words)

import Language.Javascript.JSaddle.Warp as JSaddle
import Network.Wai qualified as Wai
import Network.Wai.Application.Static qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.WebSockets (defaultConnectionOptions)

import Args (parseTerms)
import Extractor.Extractor (extractWitnessCtx)
import PPA.Certifier (certify, checkContext)
import PPA.PPA (Context, sizeC)
import PPA.Parser (parseProgram', parseTerm)
import PrettyShow (PrettyShow (prettyShow))
import Result (Result)
import Text.Printf (printf)

-- | Type synonym for an application model
data Model = Model
    { inputText :: MisoString
    , theorem :: MisoString
    , terms :: MisoString
    , result :: MisoString
    }
    deriving (Show, Eq)

-- | Sum type for application events
data Action
    = AddOne
    | Check
    | Extract
    | TheoremEntered MisoString
    | TermsEntered MisoString
    | TextEntered MisoString
    | SubtractOne
    | NoOp
    | SayHelloWorld
    deriving (Show, Eq)

runApp :: JSM () -> IO ()
runApp f = do
    putStrLn "Web server running on 0.0.0.0:8000..."
    Warp.runSettings (Warp.setPort 8000 $ Warp.setTimeout 3600 Warp.defaultSettings)
        =<< JSaddle.jsaddleOr defaultConnectionOptions (f >> syncPoint) app
  where
    app :: Wai.Application
    app req =
        case Wai.pathInfo req of
            ["style.css"] -> Wai.staticApp (Wai.defaultWebAppSettings ".") req
            ["favicon.ico"] -> Wai.staticApp (Wai.defaultWebAppSettings ".") req
            _ -> JSaddle.jsaddleApp req

-- | Entry point for a miso application
main :: IO ()
main = runApp $ startApp App{..}
  where
    initialAction = SayHelloWorld -- initial action to be executed on application load
    model = Model{inputText = "", result = "", theorem = "", terms = ""} -- initial model
    update = updateModel -- update function
    view = viewModel -- view function
    events = defaultEvents -- default delegated events
    subs = [] -- empty subscription list
    mountPoint = Nothing -- mount point for application (Nothing defaults to 'body')
    logLevel = Off -- used during prerendering to see if the VDOM and DOM are in sync (only applies to `miso` function)

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel action m =
    case action of
        -- AddOne ->noEff (m + 1)
        -- SubtractOne ->noEff (m - 1)
        NoOp ->
            noEff m
        TextEntered t -> noEff (m{inputText = t})
        TheoremEntered t -> noEff (m{theorem = t})
        TermsEntered t -> noEff (m{terms = t})
        Check -> doCheck m
        Extract -> doExtract m
        SayHelloWorld ->
            m <# do consoleLog "Hello World" >> pure NoOp

doCheck :: Model -> Effect Action Model
doCheck m@Model{inputText = program} = case parseAndCheck program of
    Right ctx -> noEff m{result = "OK!"}
    Left err -> noEff m{result = ms $ "Failed\n" ++ err}

doExtract :: Model -> Effect Action Model
doExtract m@Model{inputText = rawProgram, theorem = theorem, terms = rawTerms} =
    case mapM parseTerm (words $ fromMisoString rawTerms) of
        Left err -> noEff m{result = ms $ "Parsing terms: " ++ err}
        Right parsedTerms ->
            case parseAndCheck rawProgram of
                Left err -> noEff m{result = ms err}
                Right ctx ->
                    case extractWitnessCtx ctx (fromMisoString theorem) parsedTerms of
                        Left err -> noEff m{result = ms err}
                        Right (ctxT, ctx', t, f) -> do
                            case checkContext ctx' of
                                Left err -> noEff m{result = ms err}
                                Right _ -> noEff m{result = ms (printf "OK! Extracted witness: %s of formula: %s" (show t) (show f) :: String)}

parseAndCheck :: MisoString -> Result Context
parseAndCheck rawProgram = do
    prog <- parseProgram' "" (fromMisoString rawProgram)
    ctx <- certify prog
    checkContext ctx
    return ctx

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel x@Model{result = result} =
    div_
        [class_ "root"]
        [ button_
            [onClick AddOne]
            [text "+"]
        , button_
            [onClick SubtractOne]
            [text "-"]
        , text "Enter program"
        , div_
            [class_ "input"]
            [ textarea_ (onInput TextEntered : [class_ "input-text"]) [text ""]
            ]
        , button_ [onClick Check] [text "check"]
        , text "Theorem"
        , div_
            [class_ "input"]
            [ textarea_ (onInput TheoremEntered : [class_ "input-text"]) [text ""]
            ]
        , text "Terms"
        , div_
            [class_ "input"]
            [ textarea_ (onInput TermsEntered : [class_ "input-text"]) [text ""]
            ]
        , button_ [onClick Extract] [text "extract"]
        , text (ms result)
        , link_
            [ rel_ "stylesheet"
            , type_ "text/css"
            , href_ "style.css"
            ]
        , link_
            [ rel_ "icon"
            , href_ "favicon.ico"
            ]
        ]