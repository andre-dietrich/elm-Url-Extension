module VerifyExamples.UrlExtension.FromStringWithProtocol1 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import UrlExtension exposing (..)







spec1 : Test.Test
spec1 =
    Test.test "#fromStringWithProtocol: \n\n    fromStringWithProtocol [\"ipfs\", \"custom\"] \"ipfs://bafybeiemxf5abjwjbikoz4mc3a3dla6ual3jsgpdr4cjr3oz3evfyavhwq/wiki/Vincent_van_Gogh.html\"\n    --> Just { fragment = Nothing, host = \"bafybeiemxf5abjwjbikoz4mc3a3dla6ual3jsgpdr4cjr3oz3evfyavhwq\", path = \"/wiki/Vincent_van_Gogh.html\", port_ = Nothing, protocol = CUSTOM \"ipfs\", query = Nothing }" <|
        \() ->
            Expect.equal
                (
                fromStringWithProtocol ["ipfs", "custom"] "ipfs://bafybeiemxf5abjwjbikoz4mc3a3dla6ual3jsgpdr4cjr3oz3evfyavhwq/wiki/Vincent_van_Gogh.html"
                )
                (
                Just { fragment = Nothing, host = "bafybeiemxf5abjwjbikoz4mc3a3dla6ual3jsgpdr4cjr3oz3evfyavhwq", path = "/wiki/Vincent_van_Gogh.html", port_ = Nothing, protocol = CUSTOM "ipfs", query = Nothing }
                )