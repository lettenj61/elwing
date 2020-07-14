module Elwing.Markup exposing
    ( markup, markup_, defaults
    -- inline texts
    , t, u, w, em, kbd, small, strike
    , strong, q, ref, link, abbr, code
    -- paragraphs
    , p, blockquote
    -- code blocks
    , highlight, noHighlight
    -- list items
    , nested, ol, ul, li
    -- grouped blocks
    , section, section_, subsection
    -- definition lists
    , def, dl
    -- external HTML
    , embed
    )

import Html exposing (Attribute, Html)
import Html.Attributes as Attrs


type Text msg refs
    = Text String
    | Marked (List (Html msg) -> Html msg) (Text msg refs)
    | Anchor String (Text msg refs)
    | Ref (refs -> String) (Text msg refs)


type Heading
    = H2 String
    | H3 String


type ListOrder
    = Ordered
    | Unordered


type ListItem msg refs
    = ListItem (List (Text msg refs))
    | Nested (Markup msg refs)


type alias Def msg refs =
    { name : String
    , description : List (Text msg refs)
    }


type Markup msg refs
    = Paragraph (List (Text msg refs))
    | Code (Maybe String) String
    | Quote (List (Text msg refs))
    | Block (Maybe Heading) (List (Markup msg refs))
    | Li ListOrder (List (ListItem msg refs))
    | DefList (List (Def msg refs))
    | Embed (Html msg)
    | Separator


type alias Options msg refs =
    { targetBlank : Bool
    , permalink : Bool
    , refs : Maybe refs
    , makeId : String -> String
    , content : List (Attribute msg) -> List (Html msg) -> Html msg
    , section : List (Attribute msg) -> List (Html msg) -> Html msg
    }



-- MARKUP


defaults : Options msg refs
defaults =
    { targetBlank = True
    , permalink = False
    , refs = Nothing
    , makeId = makeIdString
    , content = Html.div
    , section = Html.div
    }


markup : List (Attribute msg) -> List (Markup msg refs) -> Html msg
markup =
    markup_ defaults


markup_ : Options msg refs -> List (Attribute msg) -> List (Markup msg refs) -> Html msg
markup_ options attrs content =
    options.content
        attrs
        (List.map (viewMarkup options) content)


t : String -> Text msg refs
t =
    Text << concatStrings identity


u : String -> Text msg refs
u =
    Text << concatStrings String.trim


w : String -> List (Text msg refs)
w input =
    normalizeTokens input
        |> List.map Text


q : String -> Text msg refs
q inner =
    code (t inner)


kbd : String -> Text msg refs
kbd key =
    Marked (Html.kbd []) (t key)


small : Text msg refs -> Text msg refs
small =
    Marked (Html.small [])


strong : Text msg refs -> Text msg refs
strong =
    Marked (Html.strong [])


strike : Text msg refs -> Text msg refs
strike =
    Marked (Html.s [])


code : Text msg refs -> Text msg refs
code =
    Marked (Html.code [])


em : Text msg refs -> Text msg refs
em =
    Marked (Html.em [])


abbr : String -> Text msg refs -> Text msg refs
abbr title =
    Marked (Html.abbr [ Attrs.title title ])


ref : (refs -> String) -> Text msg refs -> Text msg refs
ref =
    Ref


link : String -> Text msg refs -> Text msg refs
link =
    Anchor


p : List (Text msg refs) -> Markup msg refs
p =
    Paragraph


blockquote : List (Text msg refs) -> Markup msg refs
blockquote =
    Quote


highlight : String -> String -> Markup msg refs
highlight lang body =
    Code (Just lang) body


noHighlight : String -> Markup msg refs
noHighlight =
    Code Nothing


section : String -> List (Markup msg refs) -> Markup msg refs
section title =
    Block (Just <| H2 title)


section_ : List (Markup msg refs) -> Markup msg refs
section_ =
    Block Nothing


subsection : String -> List (Markup msg refs) -> Markup msg refs
subsection title =
    Block (Just <| H3 title)


ol : List (ListItem msg refs) -> Markup msg refs
ol =
    Li Ordered


ul : List (ListItem msg refs) -> Markup msg refs
ul =
    Li Unordered


li : List (Text msg refs) -> ListItem msg refs
li =
    ListItem


nested : Markup msg refs -> ListItem msg refs
nested =
    Nested


dl : List (Def msg refs) -> Markup msg refs
dl =
    DefList


def : String -> List (Text msg refs) -> Def msg refs
def =
    Def


embed : Html msg -> Markup msg refs
embed =
    Embed


hr : Markup msg refs
hr =
    Separator



-- VIEW


viewMarkup : Options msg refs -> Markup msg refs -> Html msg
viewMarkup options mrk =
    case mrk of
        Paragraph ts ->
            viewTexts options ts

        Quote ts ->
            viewQuote options ts

        Code maybeLang code0 ->
            viewCodeBlock maybeLang code0

        Block maybeHeading ms ->
            viewBlock options maybeHeading ms

        Li ord tss ->
            viewList options ord tss

        DefList defs ->
            viewDefs options defs

        Embed html ->
            html

        Separator ->
            Html.hr [] []


viewTexts : Options msg refs -> List (Text msg refs) -> Html msg
viewTexts options ts =
    Html.p
        []
        (List.map (viewTextChild options) ts)


viewTextChild : Options msg refs -> Text msg refs -> Html msg
viewTextChild options tx =
    case tx of
        Text r ->
            Html.text r

        Marked wrapper inner ->
            wrapper [ viewTextChild options inner ]

        Anchor href inner ->
            viewAnchor options href (viewTextChild options inner)

        Ref refToUrl inner ->
            case options.refs of
                Just ref_ ->
                    viewAnchor options (refToUrl ref_) (viewTextChild options inner)

                Nothing ->
                    viewTextChild options inner


viewQuote : Options msg refs -> List (Text msg refs) -> Html msg
viewQuote options ts =
    Html.blockquote [] (List.map (viewTextChild options) ts)


viewAnchor : Options msg refs -> String -> Html msg -> Html msg
viewAnchor { targetBlank } href inner =
    let
        linkAttr =
            if targetBlank then
                [ Attrs.href href
                , Attrs.target "_blank"
                , Attrs.rel "noopener noreferrer"
                ]

            else
                [ Attrs.href href
                ]
    in
    Html.a linkAttr [ inner ]


viewCodeBlock : Maybe String -> String -> Html msg
viewCodeBlock maybeLang code0 =
    let
        hlClass =
            case maybeLang of
                Just lang ->
                    [ Attrs.class <| "language-" ++ lang ]

                Nothing ->
                    []
    in
    Html.pre
        []
        [ Html.code
            hlClass
            [ Html.text code0 ]
        ]


viewList : Options msg refs -> ListOrder -> List (ListItem msg refs) -> Html msg
viewList options lo tss =
    let
        el =
            case lo of
                Ordered ->
                    Html.ol

                Unordered ->
                    Html.ul
    in
    el [] (List.map (viewListItem options) tss)


viewListItem : Options msg refs -> ListItem msg refs -> Html msg
viewListItem options child =
    Html.li [] <|
        case child of
            ListItem items ->
                List.map (viewTextChild options) items

            Nested mrk ->
                [ viewMarkup options mrk ]


viewBlock : Options msg refs -> Maybe Heading -> List (Markup msg refs) -> Html msg
viewBlock options maybeHeading ms =
    let
        makeHead el title =
            ( Attrs.id <| options.makeId title 
            , el [] [ Html.text title ]
            )

        ( id, heading ) =
            maybeHeading
                |> Maybe.map
                    (\h ->
                        case h of
                            H2 title ->
                                makeHead Html.h2 title

                            H3 title ->
                                makeHead Html.h3 title
                    )
                |> Maybe.withDefault
                    ( Attrs.class ""
                    , Html.text ""
                    )

        body =
            List.map (viewMarkup options) ms
    in
    options.section
        [ id ]
        (heading :: body)


viewDefs : Options msg refs -> List (Def msg refs) -> Html msg
viewDefs options defs =
    let
        toChildren =
            List.concatMap
                (\{ name, description } ->
                    [ Html.dt
                        []
                        [ Html.text name ]
                    , Html.dd
                        []
                        [ viewTexts options description ]
                    ]
                )
    in
    Html.dl [] (toChildren defs)



-- UTILITIES


normalizeTokens : String -> List String
normalizeTokens =
    String.trim >> String.lines >> List.map String.trim


concatStrings : (String -> String) -> String -> String
concatStrings f many =
    many
        |> String.lines
        |> List.map f
        |> String.join " "


makeIdString : String -> String
makeIdString string =
    string
        |> String.trim
        |> String.words
        |> String.join "_"
        |> String.filter
            (\c ->
                Char.isAlphaNum c || c == '-' || c == '_'
            )
        |> String.toLower
