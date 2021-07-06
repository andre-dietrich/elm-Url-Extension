module UrlExtension exposing
    ( Protocol(..), Url
    , fromString, fromStringWithProtocol, toString
    , parse
    )

{-| This is a simple wrapper for `elm/url` that allows to deal with custom protocols, next to `http` and `https`.
It does not take into account the different standards for `file`, `ftp`, `etc`, it simply allows to define different
protocols, while still relying on the `elm/url` parsing methods.

@docs Protocol, Url

@docs fromString, fromStringWithProtocol, toString

@docs parse

-}

import Url
import Url.Parser


{-| To don't interfere with the original `Url.Protocol` type, this type is defined in uppercase.
-}
type Protocol
    = HTTP
    | HTTPS
    | FILE
    | FTP
    | CUSTOM String


{-| This is actually only an replacement for the `Url.Url` type, the only difference is the internal usage of a separate `Protocol` type.
-}
type alias Url =
    { protocol : Protocol
    , host : String
    , port_ : Maybe Int
    , path : String
    , query : Maybe String
    , fragment : Maybe String
    }


{-| This is a shortcut for `fromStringWithProtocol`, which uses only use the `ftp` and `file` protocol.
If you want to define your own custom protocols for internal routing use `fromStringWithProtocol`.

    fromString "file:///home/user/example.png"
    --> Just { fragment = Nothing, host = "", path = "/home/user/example.png", port_ = Nothing, protocol = FILE, query = Nothing }

    fromString "file://C:/home/user/example.png"
    --> Just { fragment = Nothing, host = "C:", path = "/home/user/example.png", port_ = Nothing, protocol = FILE, query = Nothing }

    fromString "ftp://internet.address.edu/path/file.gz"
    --> Just { fragment = Nothing, host = "internet.address.edu", path = "/path/file.gz", port_ = Nothing, protocol = FTP, query = Nothing }

-}
fromString : String -> Maybe Url
fromString =
    fromStringWithProtocol []


{-| Use this, if you want to use more protocols, such as `ipfs`, `dweb`, or something `custom`

    fromStringWithProtocol
        ["ipfs", "custom"]
        "ipfs://bafybeiemxf5abjwjbikoz4mc3a3dla6ual3jsgpdr4cjr3oz3evfyavhwq/wiki/Vincent_van_Gogh.html"
    --> Just { fragment = Nothing, host = "bafybeiemxf5abjwjbikoz4mc3a3dla6ual3jsgpdr4cjr3oz3evfyavhwq", path = "/wiki/Vincent_van_Gogh.html", port_ = Nothing, protocol = CUSTOM "ipfs", query = Nothing }

    fromStringWithProtocol
        ["ipfs", "custom"]
        "custom://RouteTo/http://example.com"
    --> Just { fragment = Nothing, host = "RouteTo", path = "/http://example.com", port_ = Nothing, protocol = CUSTOM "custom", query = Nothing }

-}
fromStringWithProtocol : List String -> String -> Maybe Url
fromStringWithProtocol custom str =
    case getProtocol custom str of
        Just HTTP ->
            str
                |> Url.fromString
                |> Maybe.map (copy HTTP)

        Just HTTPS ->
            str
                |> Url.fromString
                |> Maybe.map (copy HTTPS)

        Just FILE ->
            if String.startsWith "file:///" str then
                str
                    |> String.replace "file:///" "file://xlou83lsOk8hzg/"
                    |> fromString_ 4 FILE
                    |> Maybe.map (\url -> { url | host = "" })

            else
                str
                    |> fromString_ 4 FILE

        Just FTP ->
            fromString_ 3 FTP str

        Just (CUSTOM protocol) ->
            fromString_
                (String.length protocol)
                (CUSTOM protocol)
                str

        _ ->
            Nothing


fromString_ : Int -> Protocol -> String -> Maybe Url
fromString_ i protocol =
    String.dropLeft i
        >> (++) "http"
        >> Url.fromString
        >> Maybe.map (copy protocol)


{-| Use this to turn your Url back into a string:

    { fragment = Nothing
    , host = "bafybeiemxf5abjwjbikoz4mc3a3dla6ual3jsgpdr4cjr3oz3evfyavhwq"
    , path = "/wiki/Vincent_van_Gogh.html"
    , port_ = Nothing
    , protocol = CUSTOM "ipfs"
    , query = Nothing
    }
    |> toString
    --> "ipfs://bafybeiemxf5abjwjbikoz4mc3a3dla6ual3jsgpdr4cjr3oz3evfyavhwq/wiki/Vincent_van_Gogh.html"

-}
toString : Url -> String
toString url =
    url
        |> copy Url.Http
        |> Url.toString
        |> String.dropLeft 4
        |> (++) (protocolToString url.protocol)


protocolToString : Protocol -> String
protocolToString protocol =
    case protocol of
        HTTP ->
            "http"

        HTTPS ->
            "https"

        FILE ->
            "file"

        FTP ->
            "ftp"

        CUSTOM str ->
            str


getProtocol : List String -> String -> Maybe Protocol
getProtocol custom str =
    case
        str
            |> String.split ":"
            |> List.head
            |> Maybe.map String.toLower
    of
        Just "http" ->
            Just HTTP

        Just "https" ->
            Just HTTPS

        Just "file" ->
            Just FILE

        Just "ftp" ->
            Just FTP

        Just protocol ->
            if
                custom
                    |> List.map String.toLower
                    |> List.member protocol
            then
                Just (CUSTOM protocol)

            else
                Nothing

        _ ->
            Nothing


{-| Replace the protocol only. Works in both directions, from `Url` -> `UrlExtension` and `UrlExtension` -> `Url`.
-}
copy :
    protocol
    ->
        { protocol : a
        , host : String
        , port_ : Maybe Int
        , path : String
        , query : Maybe String
        , fragment : Maybe String
        }
    ->
        { protocol : protocol
        , host : String
        , port_ : Maybe Int
        , path : String
        , query : Maybe String
        , fragment : Maybe String
        }
copy protocol url =
    { protocol = protocol
    , host = url.host
    , port_ = url.port_
    , path = url.path
    , query = url.query
    , fragment = url.fragment
    }


{-| Use this function to wrap your custom protocol into `Url.Http`, so that you can run your parsers from the Url.Parser module.
-}
parse : Url.Parser.Parser (a -> a) a -> Url -> Maybe a
parse parser =
    copy Url.Http >> Url.Parser.parse parser
