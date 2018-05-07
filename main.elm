import Html exposing (..)
import Random exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Window
import Time exposing (Time)
import Keyboard
import Debug exposing (..)

main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
    }

--MODEL
type alias Block = { x:Int, y:Int}
type alias Snake = List Block
type Direction = Left | Down | Up | Right
type Key = LeftArrow | DownArrow | UpArrow | RightArrow | Space | NoKey
type alias Prey = Maybe Block
type alias Model =
    { dimensions : Window.Size
    , snake : Snake
    , direction : Direction
    , prey : Prey
    , isSnakeDead : Bool
    , isGamePaused : Bool
    , metrics : GameMetrics   
    }
type alias GameMetrics = {
    score : Int
    ,level : Int
    ,preyCaught : Int
}
type Msg = Tick Time | KeyPressed Key | NewPreyChance Int | NewPreyPosition (Int, Int)


--UPDATE
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Tick time -> case model.isGamePaused of
            True -> (model, Cmd.none)
            False -> ((model, Cmd.none) 
                |> checkIfOutOfBoundary 
                |> checkIfSnakeRanIntoTail
                |> updateSnake 
                |> updatePrey)
        KeyPressed key -> 
            (updateDirection key model, Cmd.none)
        NewPreyChance chance -> 
            if (chance == 1) then
                -- create new prey
                    (model, Random.generate NewPreyPosition randomPointGenerator)
            else -- don't create a new prey
                    (model, Cmd.none)
        NewPreyPosition (x,y) -> 
            ({model | prey = Maybe.Just (Block x y)}, Cmd.none)
                    
checkIfOutOfBoundary : (Model, Cmd Msg) -> (Model, Cmd Msg)                     
checkIfOutOfBoundary (model, cmd) = case model.isSnakeDead of
    True -> --do nothing
        (model, cmd)
    False ->
        let head = snakeHead model.snake
            t = Debug.log "head x = " head.x
            isSnakeOutOfBoundary = if(head.x < 0 || head.x > 49 || head.y < 0 || head.y > 49) then
                True 
            else False
            x = Debug.log  "is snake out of boundary " isSnakeOutOfBoundary
        in ({model | isSnakeDead = isSnakeOutOfBoundary}, cmd)

checkIfSnakeRanIntoTail : (Model, Cmd Msg) -> (Model, Cmd Msg)
checkIfSnakeRanIntoTail (model, cmd) = case model.isSnakeDead of
    True -> --do nothing
        (model, cmd)
    False -> 
        let head = snakeHead model.snake
            tail = snakeTail model.snake
            didSnakeRanIntoTail = if(List.any (\a -> a == head) tail) then True else False
        in ({model | isSnakeDead = didSnakeRanIntoTail}, cmd)
        
randomPointGenerator : Generator (Int, Int) 
randomPointGenerator = pair (int 0 (49)) (int 0 (49))

randomChanceOfPrey : Generator Int
randomChanceOfPrey = (Random.int 1 6)
    

updatePrey : (Model, Cmd Msg) -> (Model, Cmd Msg)
updatePrey (model, cmd) = case model.isSnakeDead of
    True ->
       (model, cmd) 
    False ->
        case model.prey of
            Maybe.Just p -> 
                (model,  cmd)
            Maybe.Nothing -> 
                    -- create new prey at randon location, random creation chance
                (model, Random.generate NewPreyChance randomChanceOfPrey)
                

updateDirection : Key -> Model -> Model
updateDirection key model = 
    case key of
        LeftArrow -> if(model.direction /= Right) then
            {model  | direction = Left }
            else model
        DownArrow -> if(model.direction /= Up) then
            {model  | direction = Down }
            else model
        UpArrow -> if(model.direction /= Down) then
            {model  | direction = Up }
            else model
        RightArrow -> if(model.direction /= Left) then
            {model  | direction = Right }
            else model
        Space -> {model | isGamePaused =  not model.isGamePaused}
        NoKey -> model

updateSnake : (Model, Cmd Msg) -> (Model, Cmd Msg)
updateSnake (model, cmd) = case model.isSnakeDead of
    True ->
       (model, cmd) 
    False ->
        -- move snake based on direction
        let head = snakeHead model.snake
            headU = case model.direction of
                Left -> { head | x = head.x - 1}
                Down -> { head | y = head.y + 1}
                Up -> { head | y = head.y - 1}
                Right -> { head | x = head.x + 1}
            
            -- if headU and prey coincide, pery is eaten
            modelU = if (isPreyCaught headU model.prey) then
                -- remove current prey from the game
                -- update the metrics prey caught count, level and score
                {model | prey = Maybe.Nothing , snake = headU :: model.snake,
                metrics =  GameMetrics (model.metrics.score + model.metrics.level * 10) 
                (model.metrics.level + (if model.metrics.preyCaught /= 0 && rem model.metrics.preyCaught 10 == 0 then 1 else 0 )) 
                (model.metrics.preyCaught + 1)}
            else
                {model | snake = headU :: (List.take ( (List.length model.snake) - 1 ) model.snake)}

        in
            (modelU, cmd)


isPreyCaught : Block -> Prey -> Bool
isPreyCaught head prey =
    case prey of
        Maybe.Just preyblock -> head == preyblock
        Maybe.Nothing -> False

snakeHead : Snake -> Block
snakeHead snake = List.head snake |> Maybe.withDefault (Block 10 10)

snakeTail: Snake -> List Block
snakeTail snake = List.drop 1 snake

blockSize : Int
blockSize = 10
backgroundColor : Svg.Attribute Msg
backgroundColor = fill "grey"

renderBackground : Svg Msg 
renderBackground = 
    rect[Svg.Attributes.width (toString (50 * blockSize)), Svg.Attributes.height (toString (50 * blockSize))
        , backgroundColor][]

renderBlock : Block  -> Svg Msg
renderBlock block  = 
    let (strX, strY) = (toString (block.x * blockSize), toString (block.y * blockSize))
    in rect[x strX, y strY, Svg.Attributes.width (toString blockSize)
        , Svg.Attributes.height (toString blockSize), fill "blue", rx "0.2"][]

renderSnake : Snake -> List(Svg Msg) 
renderSnake snake = 
    List.map renderBlock snake

renderPrey : Prey -> List(Svg Msg)
renderPrey prey = 
    case prey of
        Maybe.Just block ->
            let (strX, strY) = (toString (block.x * blockSize), toString (block.y * blockSize))
            in [rect[x strX, y strY, Svg.Attributes.width (toString blockSize)
                , Svg.Attributes.height (toString blockSize), fill "red", rx "0.9"][]]
        Maybe.Nothing -> []

view : Model -> Html Msg
view model =
    let (scaledWidth, scaledHeight) = scale model.dimensions
    in 
        div[][
            div[][Html.text "Welcome to snake game in Elm. Excuse my artistic skills."
                ,Html.br[][]
                ,Html.text " Arrows for direction and space to pause/unpause"
                ,div[][Html.text (toString model.metrics)]]
            ,svg[Svg.Attributes.width scaledWidth, Svg.Attributes.height scaledHeight]
                ([renderBackground] ++ renderPrey model.prey ++ renderSnake model.snake )
        ]

--SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [Time.every ((1000 / toFloat  model.metrics.level) * Time.millisecond) Tick, Keyboard.downs keyDown]

keyDown : Keyboard.KeyCode -> Msg
keyDown keyCode = 
    case keyCode of
        32 -> KeyPressed Space
        37 -> KeyPressed LeftArrow
        38 -> KeyPressed UpArrow
        39 -> KeyPressed RightArrow
        40 -> KeyPressed DownArrow
        _ -> KeyPressed NoKey
            

--INIT
init : (Model, Cmd Msg)
init = 
    (
        {
            dimensions = Window.Size (50 * blockSize) (50 * blockSize)
            , snake = [Block 10 13, Block 10 12, Block 10 11, Block 10 10]
            , direction = Down
            , prey = Just (Block 34 23)
            , isSnakeDead = False
            , isGamePaused = False
            , metrics = GameMetrics 0 1 0
            }
        , Cmd.none)



scale : Window.Size -> ( String, String )
scale size =
    let
        toPixelStr =
            \i -> round i |> toString

        ( fWidth, fHeight ) =
            ( toFloat size.width, toFloat size.height )

        ( scaledX, scaledY ) =
            if fWidth > fHeight then
                ( fHeight / fWidth, 1.0 )
            else
                ( 1.0, fWidth / fHeight )
    in
        ( toPixelStr (fWidth * scaledX), toPixelStr (fHeight * scaledY) )