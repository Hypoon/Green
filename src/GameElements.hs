module GameElements
(
) where

import qualified Data.Map.Strict as Map
import Data.Time

data GameLocation
data GameColor
data Asset

data GameVar = VarString String
             | VarInt Integer
             | VarReal Double
             | VarBool Bool
             | VarGameObject GameObject
             | VarGameObjectType GameObjectType
             | VarLoc Location
             | VarRegion Region
             | VarGameColor GameColor
             | VarAsset Asset
             | VarDate UTCTime
             | VarDuration NominalDiffTime
             | VarList (List [GameVar])
             | VarGameEvent GameEvent

data GameObjectType = GameObjectType { goTypeID     :: String 
                                     , goTypeParams :: Map.Map String GameVar
                                     }

-- Include global variables as GameObjects?
data GameObject = GameObject { goID     :: String
                             , goType   :: GameObjectType
                             , goParams :: Map.Map String GameVar
                             }



-- Perhaps just another GameObject?
data UserSelectionPrompt = UserSelectionPrompt { promptID    :: String
                                               , description :: String
                                               , options     :: [UserSelectionOptions]
                                               }
data UserSelectionOption = FixedUserSelectionOption String
                         | VariableUserSelectionOption String

data UserSelection

data GameEvent = GameEvent Cause Effect
data Cause = Immediate
           | IC InteractionCause
           | WC WorldCause
data InteractionCause = UserInteract GameObject
                      | UserSelect UserSelection
data WorldCause = DateReached UTCTime
                | RepeatEvery UTCTime NominalDiffTime
                | ObjectChanges GameObject
data Effect = MultiEffects [Effect]
            | CreateEvent GameEvent
            | DestroyEvent GameEvent
            | ConditionalEffect Condition Effect
            | CreateObject GameObject
            | DestroyObject GameObject
            | ChangeObject GameObject String GameVar

data Evaluable v = Literal v | EvaluableExpression (Expression v)
data Expression v = EString StringExpression 
                  | EInt IntExpression
                  | EReal RealExpression
                  | EBool BoolExpression
                  | EGO GameObjectExpression
                  | EGOT GameObjectTypeExpression
                  | ELoc LocationExpression
                  | ERegion RegionExpression
                  | EGameColor GameColorExpression
                  | EAsset AssetExpression
                  | EDate DateExpression
                  | EDuration DurationExpression
                  | EGameEvent GameEventExpression
data BoolExpression = Equals GameVar GameVar
                    | LessThan GameVar GameVar
                    | GreaterThan GameVar GameVar
                    | LessThanEqual GameVar GameVar
                    | GreaterThanEqual GameVar GameVar
                    | ObjectHasParameter GameObject String GameVar
                    | LocationInRegion Location Region
                    |
