module TestArgs where

import Args (Args (..), Path (..), parseArgs)

import Test.HUnit (
    Counts,
    Test,
    runTestTT,
    test,
    (~:),
    (~?=),
 )

main :: IO Counts
main = do runTestTT testArgs

-- main = runTestWithNames "" testCertifier

testArgs :: Test
testArgs =
    test
        [ "unknown command"
            ~: test
                [ parseArgs [] ~?= Left "empty args"
                , parseArgs ["pepe"] ~?= Left "invalid command 'pepe', supported: check, extract"
                ]
        , "check" ~: testParseCheck
        , "extract" ~: testParseExtract
        ]

testParseCheck :: Test
testParseCheck =
    test
        [ parseArgs ["check", "somefile.ppa"]
            ~?= Right
                ArgsCheck{input = File "somefile.ppa", output = Nothing}
        , parseArgs ["check", "somefile.ppa", "--out", "out"]
            ~?= Right
                ArgsCheck
                    { input = File "somefile.ppa"
                    , output = Just $ File "out"
                    }
        , parseArgs ["check", "somefile.ppa", "-o", "out"]
            ~?= Right
                ArgsCheck
                    { input = File "somefile.ppa"
                    , output = Just $ File "out"
                    }
        , parseArgs ["check", "somefile.ppa", "-o", "-"]
            ~?= Right
                ArgsCheck
                    { input = File "somefile.ppa"
                    , output = Just Std
                    }
        , parseArgs ["check"] ~?= Left "check: empty args"
        , parseArgs ["check", "f", "out"] ~?= Left "check: invalid args"
        ]

testParseExtract :: Test
testParseExtract =
    test
        [ parseArgs ["extract"] ~?= Left "extract: empty args"
        , parseArgs ["extract", "--out", "a"] ~?= Left "extract: expected file, not arg '--out'"
        , parseArgs ["extract", "somefile.ppa"]
            ~?= Left "extract: no theorem specified"
        , parseArgs ["extract", "somefile.ppa", "--theorem", "thm"]
            ~?= Right
                ArgsExtract
                    { input = File "somefile.ppa"
                    , output = Nothing
                    , theorem = "thm"
                    , terms = []
                    }
        , parseArgs ["extract", "somefile.ppa", "-t", "thm"]
            ~?= Right
                ArgsExtract
                    { input = File "somefile.ppa"
                    , output = Nothing
                    , theorem = "thm"
                    , terms = []
                    }
        , parseArgs ["extract", "somefile.ppa", "-t", "thm", "--terms", "a", "b", "c"]
            ~?= Right
                ArgsExtract
                    { input = File "somefile.ppa"
                    , output = Nothing
                    , theorem = "thm"
                    , terms = ["a", "b", "c"]
                    }
        , parseArgs ["extract", "somefile.ppa", "-t", "thm", "-ts", "a", "b", "c"]
            ~?= Right
                ArgsExtract
                    { input = File "somefile.ppa"
                    , output = Nothing
                    , theorem = "thm"
                    , terms = ["a", "b", "c"]
                    }
        , parseArgs
            [ "extract"
            , "somefile.ppa"
            , "-t"
            , "thm"
            , "-ts"
            , "a"
            , "b"
            , "c"
            , "--out"
            , "out"
            ]
            ~?= Right
                ArgsExtract
                    { input = File "somefile.ppa"
                    , output = Just $ File "out"
                    , theorem = "thm"
                    , terms = ["a", "b", "c"]
                    }
        , parseArgs
            [ "extract"
            , "somefile.ppa"
            , "-t"
            , "thm"
            , "-ts"
            , "a"
            , "b"
            , "c"
            , "--out"
            , "-"
            ]
            ~?= Right
                ArgsExtract
                    { input = File "somefile.ppa"
                    , output = Just Std
                    , theorem = "thm"
                    , terms = ["a", "b", "c"]
                    }
        , parseArgs
            [ "extract"
            , "somefile.ppa"
            , "-ts"
            , "a"
            , "b"
            , "c"
            , "--out"
            , "-"
            , "-t"
            , "thm"
            ]
            ~?= Right
                ArgsExtract
                    { input = File "somefile.ppa"
                    , output = Just Std
                    , theorem = "thm"
                    , terms = ["a", "b", "c"]
                    }
        , parseArgs
            [ "extract"
            , "somefile.ppa"
            , "-ts"
            , "a"
            , "b"
            , "c"
            , "-o"
            , "-"
            , "-t"
            , "thm"
            ]
            ~?= Right
                ArgsExtract
                    { input = File "somefile.ppa"
                    , output = Just Std
                    , theorem = "thm"
                    , terms = ["a", "b", "c"]
                    }
        ]
