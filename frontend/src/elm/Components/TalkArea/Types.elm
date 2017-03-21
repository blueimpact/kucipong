module Components.TalkArea.Types exposing (..)

{-| Types for `Components.TalkArea`

# Talk structure itself

@docs TalkBlock
@docs TalkBody
@docs TalkParagraph

# Extra information about the talk
@docs Speaker (..)
@docs Feeling (..)
@docs ParagraphTag (..)
-}


{-| A record representing a series of messages by same speaker.
-}
type alias TalkBlock =
  { speaker : Speaker
  , feeling : Maybe Feeling
  , balloons : List TalkBody
  }


{-| Representing a talk balloon
-}
type alias TalkBody =
  List TalkParagraph


{-| Representing a paragraph on a talk balloon
-}
type alias TalkParagraph =
  { ptag : ParagraphTag
  , value : String
  }


{-| Owner of a talk balloon
-}
type Speaker
  = AI
  | User


{-| Feeling of the owner
-}
type Feeling
  = FeelNormal
  | FeelBad
  | FeelGood


{-| Which meanings of the paragraph?
-}
type ParagraphTag
  = PlainParagraph
  | AnnotationParagraph
  | ImportantParagraph
  | TitleParagraph
  | SubParagraph
  | ImageParagraph
