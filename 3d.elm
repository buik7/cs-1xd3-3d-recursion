-- VVV DO NOT MODIFY THESE VVV
-- (Unless you know what you're doing)
import Set exposing (Set)
import Plane3d

-- Data type for representing different kinds of modules
type ModuleType
  = Housing
  | Medical
  | Recreation
  | Custom (Object BodyCoordinates)

-- Each node stores its ID, position on the global coordinate system, and its module type.
type alias Node =
  { id : Int
  , position : Point3d Meters WorldCoordinates
  , nodeType : ModuleType
  }

-- A City is a graph made up of nodes and their connections.
-- Connections are represented as a set of IDs, to remove redundant connections.
type alias City = List (Node, Set Int)

-- This function loops through the given cityGraph and renders the nodes as modules.
buildModules : City -> Model -> List (Body PhysicsData)
buildModules cityGraph model =
  List.map
    ( \ (node, _) ->
      let
        pos = (Point3d.xCoordinate node.position, Point3d.yCoordinate node.position, Point3d.zCoordinate node.position)
      in
        case node.nodeType of
          Housing ->
            house model
              |> usePhysics "houseModule"
              |> addTag "housing"
              |> startAtPos pos
          Medical ->
            medical model
              |> usePhysics "medModule"
              |> addTag "medical"
              |> startAtPos pos
          Recreation ->
            recreation model
              |> usePhysics "recModule"
              |> addTag "recreation"
              |> startAtPos pos
          Custom obj ->
            obj
              |> usePhysics "customModule"
              |> addTag "customModule"
              |> startAtPos pos
    )
    cityGraph

-- This function loops through the given cityGraph
-- and renders the connections between nodes.
-- It is considerably more complex than the function
-- for rendering the nodes themselves.
buildConnections : City -> Model -> List (Body PhysicsData)
buildConnections cityGraph model =
  -- Loop through the list and render connections
  List.map
    ( \ (node, connections) ->
      let
        -- Get the list of nodes connected to this one
        connectedNodes =
          List.filter
            ( \ (otherNode, _) ->
              Set.member otherNode.id connections
            )
            cityGraph
            |> List.map ( \ (otherNode, _) -> otherNode)
        -- Extract positions. Could probably have been combined
        -- with the above local variable
        connectedPositions =
          List.map
            ( \ otherNode ->
              otherNode.position
            )
            connectedNodes
        -- The physics library is very finicky with local and global coordinate systems
        -- This just converts the global node positions to a local one.
        bodyPos = node.position |> Point3d.relativeTo Frame3d.atOrigin
        -- A list of directions from this node to its connected nodes
        directions =
          List.map
            ( \ pos ->
              Maybe.withDefault Direction3d.z
                (Direction3d.from bodyPos (pos |> Point3d.relativeTo Frame3d.atOrigin))
            )
            connectedPositions
        -- Distances between this node and its connected nodes
        -- Distances are adjusted somewhat to allow for the arrow to show up
        lengths =
          List.map
            ( \ pos ->
              Point3d.distanceFrom node.position pos
                |> Quantity.minus (Length.meters 1)
            )
            connectedPositions
        -- Zip the direction and length lists together
        arrowData =
          List.map2 Tuple.pair directions lengths
      in
        group3D
          ( List.map
              ( \ (dir, len) ->
                arrowStartingAt
                  bodyPos -- Arrow starts at this node...
                  dir -- ...and points to a connected node
                  { radius = Length.centimeters 10, length = len }
                  |> matte Color.white
                  -- Connections shouldn't cause collisions due to a bug
              )
              arrowData
          )
          |> noCollide
          |> usePhysics ("node" ++ String.fromInt node.id ++ "connections")
    )
    cityGraph

-- Combines buildModules and buildConnections
buildCity cityGraph model =
  buildModules cityGraph model ++ buildConnections cityGraph model

-- Function to make creating nodes a bit easier.
-- You technically don't need to use it, but you might find it helpful.
buildNode : Int -> Point3d Meters WorldCoordinates -> ModuleType -> Node
buildNode id pos moduleType =
  { id = id
  , position = pos
  , nodeType = moduleType
  }
-- ^^^ DO NOT MODIFY THESE ^^^
  
-- VVV YOU MAY MODIFY THESE VVV

-- These will be used to actually render the different module types.
-- If you change the name or type signature (e.g. by adding parameters),
-- you will also need to modify any references to them, so be careful.
house model =
  group3D
    [ box 150 100 70
        |> matte Color.white,
      customPolygon [(-50,0),(50,0),(0,30),(-50,0)]
        |> matte Color.white
        |> rotate3D 0 (degrees 90) (degrees 90)
        |> move3D (-75,0,35),
      customPolygon [(-50,0),(50,0),(0,30),(-50,0)]
        |> matte Color.white
        |> rotate3D 0 (degrees 90) (degrees 90)
        |> move3D (75,0,35),
      polyCylinder [(-50,0),(0,30),(50,0),(60,0),(0,40),(-60,0),(-50,0)] 160 model.meshStore
        |> plastic Color.blue 0.3
        |> rotate3D 0 (degrees 90) (degrees 90)
        |> move3D (-80,0,35),
      square3D 30
        |> matte Color.blue
        |> rotate3D 0 0 (degrees 90)
        |> move3D (35,-51,0),
      square3D 30
        |> matte Color.blue
        |> rotate3D 0 0 (degrees 90)
        |> move3D (-35,-51,0),
      square3D 30
        |> matte Color.blue
        |> rotate3D 0 0 (degrees 90)
        |> move3D (35,51,0),
      square3D 30
        |> matte Color.blue
        |> rotate3D 0 0 (degrees 90)
        |> move3D (-35,51,0),
      rectangle3D 50 30
        |> matte Color.blue
        |> rotate3D (degrees 90) 0 0
        |> move3D (-76,0,-10)
    ]

medical model = group3D
                [box 50 80 100
                    |> matte Color.grey
                    |> move3D(0,0, 50)
                , box 30 50 50
                    |> matte Color.grey
                    |> move3D(0,0, 85)
                  ,window|> move3D (-25,0,70)
                  ,window|> move3D (-25,15,70)
                  ,window|> move3D (-25,-15,70)
                  ,window|> move3D (-25,-30,70)
                  ,window|> move3D (-25,30,70)
                  ,window|> move3D (-25,0,50)
                  ,window|> move3D (-25,15,50)
                  ,window|> move3D (-25,-15,50)
                  ,window|> move3D (-25,-30,50)
                  ,window|> move3D (-25,30,50)
                  
                  ,window|> move3D (25,0,70)
                  ,window|> move3D (25,15,70)
                  ,window|> move3D (25,-15,70)
                  ,window|> move3D (25,-30,70)
                  ,window|> move3D (25,30,70)
                  ,window|> move3D (25,0,50)
                  ,window|> move3D (25,15,50)
                  ,window|> move3D (25,-15,50)
                  ,window|> move3D (25,-30,50)
                  ,window|> move3D (25,30,50)
                  
                   ,window|> rotate3D 0 -300 0|> move3D (-15,-40,70)
                   ,window|> rotate3D 0 -300 0|> move3D (0,-40,70)
                   ,window|> rotate3D 0 -300 0|> move3D (15,-40,70)
                   ,window|> rotate3D 0 -300 0|> move3D (-15,-40,50)
                   ,window|> rotate3D 0 -300 0|> move3D (0,-40,50)
                   ,window|> rotate3D 0 -300 0|> move3D (15,-40,50)
                   
                   ,window|> rotate3D 0 -300 0|> move3D (-15,40,70)
                   ,window|> rotate3D 0 -300 0|> move3D (0,40,70)
                   ,window|> rotate3D 0 -300 0|> move3D (15,40,70)
                   ,window|> rotate3D 0 -300 0|> move3D (-15,40,50)
                   ,window|> rotate3D 0 -300 0|> move3D (0,40,50)
                   ,window|> rotate3D 0 -300 0|> move3D (15,40,50)
                  
                  ,box 1 70 15
                      |> matte Color.white 
                      |> move3D (-25,0,90)
                  ,box 1 70 15
                      |> matte Color.white 
                      |> move3D (25,0,90)
                 ,door
                 ,door|> move3D (50,0,0)
                 ,cross model
                 ,cross2 model
                ] |> scale3D 2
window = 
        group3D
           [box 1 15 15
                |> matte Color.lightBlue 
     ,cylinder 0.75 (15 + 1.375)
        |> matte Color.black
        |> move3D (0, -7.5, 0)
    , cylinder 0.75 (15 + 1.375)
        |> matte Color.black
        |> move3D (0, 7.5, 0)
    , cylinder 0.75 (15 + 1.375)
        |> matte Color.black
        |> rotate3D 0 0 (degrees 90)
        |> move3D (0, 0, 7.5)
    , cylinder 0.75 (15 + 1.375)
        |> matte Color.black
        |> rotate3D 0 0 (degrees 90)
        |> move3D (0, 0, -7.5)
        ]
door = group3D
           [box 1 15 30
                      |> matte Color.brown 
                      |> move3D (-25,10,15)
             ,box 1 15 30
                      |> matte Color.brown 
                      |> move3D (-25,-6,15) 
             ]
crosspoints = [(-9.874,-9.6),(0.5485,-10.14),(0.5485,0.2742),(9.8742,-0.274),(10.422,10.148),(0.5485,10.697),(0,20.571),(-9.874,20.571),(-10.42,9.6),(-19.74,10.148),(-19.2,0.2742),(-9.874,-0.274),(-9.874,-9.6),(-9.874,-9.6)]
cross model = polyCylinder crosspoints 1 model.meshStore|>plastic Color.red 0.3|>scale3D 0.5|> rotate3D -300 0 0|>move3D (-26,0,88)
cross2 model = polyCylinder crosspoints 1 model.meshStore|>plastic Color.red 0.3|>scale3D 0.5|> rotate3D -300 0 0|>move3D (26,0,88) 
    
recreation model =
  group3D
    [ 
      cylinder 4 50
        |> matte Color.darkBrown
        |> move3D (0, 0, 25)
    , cone 30 30
        |> matte Color.darkGreen
        |> move3D (0, 0, 10)
    , cone 25 30
        |> matte Color.darkGreen
        |> move3D (0, 0, 25)
    , cone 20 30
        |> matte Color.darkGreen
        |> move3D (0, 0, 40)
    , water model
    ] |> scale3D 2.5
    
water model = 
  let
    useTexture = getTexture model.textureLoader.textures
  in
    group3D [ polygon3D 15 30
                |> textured (useTexture "water") 0.5 0.5
                |> move3D (0,0,1)]

-- This is your city.
-- For the Underwater Recursion lab, remember that it should be algorithmically generated.
{-
myCity model =
  [ (buildNode 0 (Point3d.meters 3 3 5) Housing, Set.fromList [1])
  , (buildNode 1 (Point3d.meters 8 8 5) Recreation, Set.fromList [0, 2, 3])
  , (buildNode 2 (Point3d.meters 8 3 5) Medical, Set.fromList [0, 1])
  , (buildNode 3 (Point3d.meters 5 5 1) (Custom (house model)), Set.fromList [1])
  ]
-}

numModules = 5

myCity model =
  cityLine model numModules
  
cityLine model n =
  let
    actualN =
      if n >= 3 then
        n
      else
        3
  in
    cityLineAux model actualN [] actualN
    
cityLineAux model n currentCity max = 
    if n < 1 then
      currentCity
    else
      cityLineAux
        model
        (n - 1)
        ( ( buildNode n (Point3d.meters (getX n) (getY n) 0) (getModule n)
          , Set.fromList (getConnection n 1 max) 
          ) :: currentCity
        )
        max 


getConnection n min max = 
   if n == min then [n+1, n+2] 
   else 
     if n == max then 
       if modBy 2 max == 0 then [n-1] 
       else [n-1,n-2]
     else 
       if modBy 2 n == 0 then [n-1, n+1] 
       else  
           if n + 1 == max then [n-2,n-1,n+1] 
           else [n-2, n-1, n+1 , n+2]
           
getModule n = 
  if modBy 4 n == 1 then Housing 
  else 
    if modBy 4 n == 3 then Recreation 
    else Medical 
    
getPos n = 
  if modBy 2 n == 1 then (toFloat (2*(n-1)), 0, 0) 
  else (toFloat (2*n - 2), (sqrt 3) * 2, 0)  
  
getX n =
  if modBy 2 n == 1 then toFloat (2*(n-1))
  else toFloat (2*n - 2)
  
getY n = 
  if modBy 2 n == 1 then 0
  else (sqrt 3) * (-2)
  
distance x1 y1 z1 x2 y2 z2 = 
  sqrt ((x2-x1)*(x2-x1) + (y2-y1)*(y2-y1) + (z2-z1)*(z2-z1))
  
warning x2 y2 z2 n =
  let
    x1 = getX n
    y1 = getY n
    dist = distance x1 y1 0 x2 y2 z2
  in 
    if n == 0 then text ""  else
    if dist < 5 then text "WARNING! YOU ARE NEAR THE BUILDING" 
    else warning x2 y2 z2 (n-1)

-- Put your physics objects here!
myPhysicsObjects model =
    let
        useTexture = getTexture model.textureLoader.textures
    in
        [ 
        ] ++ buildCity (myCity model) model

-- Put your update functions here! Try using "pushIf" or "updateIf"
myPhysicsUpdates : Model -> List (Body PhysicsData -> Body PhysicsData)
myPhysicsUpdates model =
    [ 
    ]

{-| Define your submarine here. Note that animations will not work. -}
submarineEntity : Model -> Object BodyCoordinates
submarineEntity model =
    group3D
        [ ellipsoid 100 25 25 model.meshStore
            |> metallic Color.lightGrey 0.3
        , polyCylinder [(0,0),(30,0),(30,10),(0,10),(0,0)] 35 model.meshStore
            |> metallic Color.lightGrey 0.3
            |> move3D (10,-5,0)
        , ellipsoid 10 38 2 model.meshStore
            |> metallic Color.lightGrey 0.3
            |> move3D (50,0,10)
        , ellipsoid 10 30 2 model.meshStore
            |> metallic Color.lightGrey 0.3
            |> move3D (-90,0,0)
            |> rotateX3D 48
        , ellipsoid 10 30 2 model.meshStore
            |> metallic Color.lightGrey 0.3
            |> move3D (-90,0,0)
            |> rotateX3D -48
        , ring 5 2 model.meshStore
            |> metallic Color.lightGrey 0.3
            |> rotateY3D 89.5
            |> move3D (-101,0,0)
        , ring 9 2 model.meshStore
            |> metallic Color.lightGrey 0.3
            |> rotateY3D 89.5
            |> move3D (90,0,0)
        , ring 15 2 model.meshStore
            |> metallic Color.lightGrey 0.3
            |> rotateY3D 89.5
            |> move3D (75,0,0)
        , texturedCylinder 1 22 model.meshStore
            |> metallic Color.lightGrey 0.3
            |> move3D (18,0,25)
        , texturedCylinder 1 15 model.meshStore
            |> metallic Color.lightGrey 0.3
            |> move3D (25,0,25)
        , ring 20 2 model.meshStore
            |> metallic Color.lightGrey 0.3
            |> rotateY3D 89.5
            |> move3D (-50,0,0)
        ]
        
subStartingPos : (Length, Length, Length)
subStartingPos = (Length.meters 0, Length.meters 0, Length.meters 10)

-- I don't recommend turning this on for these labs.
useGravity = False

-- Set this to true show position, velocity, and force visualizers for ALL bodies
showPhysicsVisualizers = False

-- Distance that the camera moves when you click the buttons in the top right corner
cameraMoveDistance = Length.centimeters 25

-- move / edit the light
light =
    pointLight
        { position = Point3d.centimeters 0 0 100    -- position of the light
        , chromaticity = Light.sunlight             -- the colour of the light (see https://package.elm-lang.org/packages/ianmackenzie/elm-3d-scene/latest/Scene3d-Light#Chromaticity)
        , intensity = LuminousFlux.lumens 10000     -- how intense the light is
        }
showLight = True -- whether to show the light ball or not

-- Use "loadTexture [name] [url]" to load in texture images from the Internet!
-- Give each one a unique name.
-- You can list many of them!
myTextures = 
    [ loadTexture "water" "https://sharyuwu.github.io/image/texture1.png"
    ]

-- Usage: `svgTexture "name" "name`, where shape is any 2D shape or group
-- Give each one a unique name.
-- You can list many of them!
svgTextures =
    [ svgTexture "squares" squares
    ]

-- SVG textures are 50 by 50
squares =
    group
    [
        square 25 |> filled purple |> move (12.5,12.5)
    ,   square 25 |> filled orange |> move (-12.5,12.5)
    ,   square 25 |> filled purple |> move (-12.5,-12.5)
    ,   square 25 |> filled orange |> move (12.5,-12.5)
    ]



-- Put your 2D shapes here, and they will be overlayed on top of the screen!
overlay : Model -> List (Shape Msg)
overlay model =
    [ submarineHUD model
    ]

-- This is the Heads-Up Display for the submarine.
submarineHUD model =
  let
    -- Set up some useful variables and data
    subMaybe = getBodyByName "submarine" model.world
    colour = hsl (degrees 155) 1 0.2
    colourHighlight = hsl (degrees 180) 1 0.3
    subPos =
      subMaybe
        |> Maybe.map Body.frame
        |> Maybe.withDefault Frame3d.atOrigin
        |> Frame3d.originPoint
    subX =
      subMaybe
        |> Maybe.map (Body.frame)
        |> Maybe.withDefault Frame3d.atOrigin
        |> Frame3d.xDirection
    subY =
      subMaybe
        |> Maybe.map (Body.frame)
        |> Maybe.withDefault Frame3d.atOrigin
        |> Frame3d.yDirection
    subZ =
      subMaybe
        |> Maybe.map (Body.frame)
        |> Maybe.withDefault Frame3d.atOrigin
        |> Frame3d.zDirection
    screenWidth =
      toFloat (unwrapQ model.width)
    screenHeight =
      toFloat (unwrapQ model.height)
    -- Define HUD elements
    hudVelocity = 
      let 
        velocity1 = case subMaybe of
                      Nothing -> {x=0,y=0,z=0}
                      Just a -> Vector3d.toRecord Speed.inMetersPerSecond (Body.velocity a)
        velocity2 = sqrt (velocity1.x * velocity1.x + velocity1.y * velocity1.y + velocity1.z * velocity1.z)
      in 
        
        text ("VELOCITY: " ++ Round.round 2 velocity2 ++ "m/s")
          |> customFont "Audiowide"
          |> centered
          |> size 16
          |> filled colour
          

    
    hudWarning = 
      let 
        x2 = 
          subPos
            |> Point3d.xCoordinate
            |> Quantity.plus (Length.meters 1)
            |> Length.inMeters
        y2 = 
          subPos
            |> Point3d.yCoordinate
            |> Quantity.plus (Length.meters 1)
            |> Length.inMeters
        z2 = 
          subPos
            |> Point3d.zCoordinate
            |> Quantity.plus (Length.meters 1)
            |> Length.inMeters
      in 
        warning x2 y2 z2 numModules
          |> customFont "Audiowide"
          |> centered
          |> size 20
          |> filled red
    
    hudAltitude =
      let
        altitude =
          subPos
            |> Point3d.zCoordinate
            -- The actual floor is 1 meter below the origin.
            |> Quantity.plus (Length.meters 1)
            |> Length.inMeters
      in
        text ("ALTITUDE: " ++ Round.round 2 altitude ++ "m")
          |> customFont "Audiowide"
          |> centered
          |> size 16
          |> filled colour
    hudPitch =
      let
        sidePlane =
          SketchPlane3d.unsafe
            { originPoint = subPos
            , xDirection =
                subX
                  |> Direction3d.projectOnto Plane3d.xy
                  |> Maybe.withDefault subX
            , yDirection = Direction3d.z
            }
        angle =
          subX
            |> Direction3d.azimuthIn sidePlane
            |> Angle.inRadians
        thickness = solid 2
        radius = 75
      in
        group
          [ circle radius
              |> outlined thickness colour
          , polygon [(0, 65), (-15, -30), (0, 0), (15, -30), (0, 65)]
              |> filled colour
              |> rotate (angle - (degrees 90))
          , triangle 10
              |> filled colour
              |> rotate (degrees 180)
              |> move (radius - 5, 0)
          , triangle 10
              |> filled colour
              |> rotate (degrees -90)
              |> move (0, radius - 5)
          , triangle 10
              |> filled colour
              |> rotate (degrees 90)
              |> move (0, -radius + 5)
          , text "UP"
              |> customFont "Audiowide"
              |> centered
              |> size 24
              |> filled colourHighlight
              |> move (0, radius + 10)
          , text "PITCH"
              |> customFont "Audiowide"
              |> centered
              |> size 16
              |> filled colour
              |> move (0, -radius - 20)
          ]
    -- Essentially a compass
    hudYaw =
      let
        angle =
          subX
            |> Direction3d.azimuthIn SketchPlane3d.xy
            |> Angle.inRadians
        thickness = solid 2
        radius = 75
      in
        group
          [ circle radius
              |> outlined thickness colour
          , polygon [(0, 65), (-15, -30), (0, 0), (15, -30), (0, 65)]
              |> filled colour
              |> rotate angle
          , triangle 10
              |> filled colour
              |> rotate (degrees -90)
              |> move (0, radius - 5)
          , text "N"
              |> customFont "Audiowide"
              |> centered
              |> size 24
              |> filled colourHighlight
              |> move (0, radius + 10)
          , text "YAW"
              |> customFont "Audiowide"
              |> centered
              |> size 16
              |> filled colour
              |> move (0, -radius - 20)
          ]
    -- Reading the roll value is considerably more difficult than yaw.
    -- Here, we are drawing a 2D plane facing forward, calculating the angle by
    -- getting the azimuth of the submarine Y-direction's projection onto that plane
    hudRoll =
      let
        -- A plane whose normal direction is the sub's forward direction
        forwardPlane =
          SketchPlane3d.unsafe
            { originPoint =
                subPos
            , xDirection =
                subX
                  |> Direction3d.rotateAround Axis3d.z (Angle.degrees 90)
            , yDirection =
                Direction3d.z
            }
        -- The angle used for the roll meter is just the horizontal angle of the direction vector
        -- on the forward-facing plane
        angle =
          subY
            |> Direction3d.azimuthIn forwardPlane
            |> Angle.inRadians
            |> negate
        thickness = solid 2
        radius = 150
      in
        group
          [ group -- The roll meter that rotates with the submarine.
              [ circle radius
                  |> outlined thickness colour
                  |> clip (rectangle (radius * 3) (radius * 2) |> ghost |> move (0, radius))
              , line (-radius - 25, 0) (-radius + 25, 0)
                  |> outlined thickness colour
              , line (-radius - 20, -10) (-radius + 20, -10)
                  |> outlined thickness colour
              , line (radius - 25, 0) (radius + 25, 0)
                  |> outlined thickness colour
              , line (radius - 20, -10) (radius + 20, -10)
                  |> outlined thickness colour
              , triangle 10
                  |> filled colour
                  |> rotate (degrees 90)
                  |> move (0, radius + 5)
              , triangle 10
                  |> filled colour
                  |> rotate (degrees -90)
                  |> move (0, -radius - 5)
              , line (-25, -radius) (25, -radius)
                  |> outlined thickness colour
              , line (-20, -radius + 10) (20, -radius + 10)
                  |> outlined thickness colour
              ]
              |> rotate angle
          -- A static indicator so you know where the neutral state is.
          , group 
              [ triangle 10
                  |> filled colour
                  |> rotate (degrees -90)
              , line (-25, 0) (25, 0)
                  |> outlined thickness colour
                  |> move (0, 5)
              ]
              |> move (0, radius + 25)
          , text "ROLL"
              |> customFont "Audiowide"
              |> centered
              |> size 16
              |> filled colour
              |> move (0, radius + 40)
          ]
  in
    group
      [ hudAltitude
          |> move (0, screenHeight / 2 - 50)
      , hudPitch
          |> move (-screenWidth / 2 + 150, -screenHeight / 2 + 150)
      , hudYaw
          |> move (screenWidth / 2 - 150, -screenHeight / 2 + 150)
      , hudRoll
      , hudWarning
      , hudVelocity
          |> move (screenWidth / 2 - 100, screenHeight / 2 - 50)
      ]

skyboxType =
    Skybox.URLSkybox textureBottom textureTop textureSide1 textureSide2 textureSide3 textureSide4

textureBottom : String
textureBottom =
    "Put an image URL here!"

textureTop : String
textureTop =
    "Put an image URL here!"

textureSide1 : String
textureSide1 =
    "Put an image URL here!"

textureSide2 : String
textureSide2 =
    "Put an image URL here!"

textureSide3 : String
textureSide3 =
    "Put an image URL here!"

textureSide4 : String
textureSide4 =
    "Put an image URL here!"
