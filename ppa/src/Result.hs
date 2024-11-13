module Result (Result, wrapR) where

type Result a = Either String a

wrapR :: String -> Result a -> Result a
wrapR _ r@(Right _) = r
wrapR msg (Left err) = Left (msg ++ ": " ++ err)
