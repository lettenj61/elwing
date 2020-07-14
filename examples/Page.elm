module Page exposing (main)

import Html
import Html.Attributes as Attrs
import Markup exposing (..)


main =
    markup_ options [] <|
        [ embed awsmCss
        , section "Greetings" <|
            [ p
                [ t "This is my first paragraph."
                , u """
                    I always was, and am, try very hard to understand what effective UI is.

                    There is a big separation between native and web.
                    """
                , aYaml
                ]
            , p
                [ t "You will find useful information at "
                , ref .github (t "GitHub.")
                ]
            , blockquote
                [ t "Arthur C. Jerkin"
                ]
            , p <|
                w
                    """
                    This is very long, embedded paragraph.

                    Show must go on.
                    Show must go on.
                    """
            , p
                [ t "The magnitudes would appear in following order:"
                ]
            , ol
                [ li <| w "DOM is complex"
                , li <| w "Silence is golden"
                , li <| [ t "Sometimes you get", em (t "emotional") ]
                , nested <|
                    ul
                        [ li <| w "Inside another list"
                        ]
                ]
            ]
        , section "Kana, Kanji and Kana" <|
            [ p
                [ t "全国2億7000万人の" , ref .elmLang (t "Elm") , t "ユーザーのみなさま、こんにちは。"
                , q "Elm", t "で開発したマークアップ DSL です。どうぞ熱いうちにお召し上がりください。"
                , t "おっと、舌をやけどしないように気を付けて！"
                ]

            , highlight "elm" <|
                """
import Markup exposing (..)

main =
    markup [] <|
        [ section "Hello, world!" <|
            [ p <| w
                "This is my new blog."
            ]
        ]
                """
            ]
        ]


options =
    { defaults
        | refs = Just refs
        , content = Html.main_
        , section = Html.section
    }


refs =
    { github = "https://github.com/"
    , elmLang = "https://elm-lang.org/"
    , top = "#"
    }


awsmCss =
    Html.node "link"
        [ Attrs.rel "stylesheet"
        , Attrs.href "https://unpkg.com/awsm.css/dist/awsm.min.css"
        ]
        []

aYaml =
    abbr
        "YAML Ain't Markup Language"
        (t "YAML")