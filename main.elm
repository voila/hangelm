import Html exposing (Html, Attribute, text, h1, div, button, input, span)
import Html.App exposing (beginnerProgram)
import Html.Attributes exposing (type', placeholder, style)
import Html.Events exposing (onInput, onClick)
import String as S
import Char as C
import Svg exposing (svg, line, circle, rect)
import Svg.Attributes exposing 
  (width, height, viewBox, x, y, x1, y1, x2, y2, stroke, strokeWidth, cx, cy, r)

main =
  beginnerProgram { 
           model = model, view = view, update = update }

-- MODEL
type Screen = GetWord | PlayGame

alphabet =
  List.map C.fromCode [97 .. 122]
  
model0 = { 
    input = ""
  , word = ""
  , screen = GetWord
  , letters = []
  , mistakes = 0 -- goes up to 11
  }

model = model0

-- UPDATE
type Msg = NewInput String 
         | NewWord String 
         | NewLetter Char 
         | Mistake Char
         | NewGame

update msg model =
  case msg of 
    NewWord w ->
      { model | word = w, screen = PlayGame }
    NewInput s ->
      { model | input = s }
    NewLetter l ->
      { model | letters = l :: model.letters }
    Mistake l ->
      { model | mistakes = model.mistakes + 1, letters = l :: model.letters }
    NewGame ->
      model0

---- Drawing

drawLine (x1', y1') (x2', y2') =
  line
    [ stroke      "black"
    , strokeWidth "3"
    , x1          (toString x1')
    , x2          (toString x2')
    , y1          (toString y1')
    , y2          (toString y2')
    ] []
    
drawCircle (cx', cy', r') =
    circle [ cx (toString cx'), cy (toString cy'), r (toString r') ] []

drawHangman m =
  case m of
    0 -> [] 
    1 -> drawLine (0, 200) (100, 200) :: drawHangman 0
    2 -> drawLine (50, 200) (50, 0) :: drawHangman 1
    3 -> drawLine (50, 0) (150, 0) :: drawHangman 2
    4 -> drawLine (50, 50) (100, 0) :: drawHangman 3
    5 -> drawLine (150, 0) (150, 20) :: drawHangman 4
    6 -> drawCircle (150, 35, 15) :: drawHangman 5
    7 -> drawLine (150, 50) (150, 100) :: drawHangman 6
    8 -> drawLine (150, 60) (170, 90):: drawHangman 7
    9 -> drawLine (150, 60) (130, 90) :: drawHangman 8
    10 -> drawLine (150, 100) (170, 140) :: drawHangman 9
    11 -> drawLine (150, 100) (130, 140) :: drawHangman 10
    _ -> []

drawing model = 
  svg
      [ width "200", height "200", viewBox "0 0 200 200" ]
      (drawHangman model.mistakes)

reveal w ls =
  div [ revealStyle ]
    (List.map (\c ->  
    span [] [ text (
      if List.member c ls then (S.fromChar c) else "_") ]
      ) (S.toList w))
  
lettersDiv w ls = 
  let 
    letters_left = List.filter (\c -> not (List.member c ls)) alphabet
  in  
    div [ lettersStyle ] 
      (List.map (\c -> 
          span [onClick (selectLetter w c)] 
                [text (S.fromChar c)]
        ) letters_left)

selectLetter w c =
  if not (List.member c (S.toList w))
  then Mistake c
  else NewLetter c

result model = 
   let 
     newGame = div [] [ button [ onClick NewGame ] [ text "New Game" ] ]
   in
     div [ resultStyle ] (
       if List.all (\c -> List.member c model.letters) (S.toList model.word) 
       then [ text "You won!", newGame ]
       else 
         if model.mistakes == 11 
         then [ text "You lost!", newGame ]
         else [])

view model =
 case model.screen of
  GetWord ->
    div []  
    [ 
      h1 [] [text "Hangman"]
    , input [ type' "password",
      placeholder "Type the word to guess", 
      onInput NewInput ] []
    , button [ onClick (NewWord model.input) ] [ text "Start" ]
    , div [] [ text (S.reverse model.word) ]
    ]
  PlayGame ->
    div [] 
    [
       h1 [] [text "Hangman"]
     , drawing model
     , reveal model.word model.letters
     , lettersDiv model.word model.letters
     , result model
    ]
    
-- Styles

resultStyle =
  style [
    ("margin", "1em")   
  , ("fontSize", "4em")
  , ("textAlign", "center")
  , ("color", "red")  
  ]
    
revealStyle =
  style [
    ("margin", "1em")
  , ("fontSize", "4em")
  , ("letterSpacing", "15px")
  ]
  
lettersStyle = 
  style [
    ("margin", "1em")
  , ("fontSize", "2em")
  , ("letterSpacing", "15px")
  ]
