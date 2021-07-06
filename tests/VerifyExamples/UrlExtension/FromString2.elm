module VerifyExamples.UrlExtension.FromString2 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import UrlExtension exposing (..)







spec2 : Test.Test
spec2 =
    Test.test "#fromString: \n\n    fromString \"file:///home/user/example.png\"\n    --> Just { fragment = Nothing, host = \"\", path = \"/home/user/example.png\", port_ = Nothing, protocol = FILE, query = Nothing }" <|
        \() ->
            Expect.equal
                (
                fromString "file:///home/user/example.png"
                )
                (
                Just { fragment = Nothing, host = "", path = "/home/user/example.png", port_ = Nothing, protocol = FILE, query = Nothing }
                )