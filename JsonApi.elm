module JsonApi exposing (..)
import Json.Encode as Encode exposing (object)
import Json.Decode as Decode exposing (andThen, string, map2, list, maybe, int, Decoder, succeed, at)
import HttpBuilder exposing (RequestBuilder, withHeaders)
import Http
import String


-- DECODING


root : Decoder a -> Decoder a
root =
  at ["data"]


attribute : String -> Decoder a -> Decoder a
attribute name decoder =
  at ["attributes", name] decoder


decodeId : Decoder (Maybe String)
decodeId =
  at ["id"] (maybe string) 


readString : Maybe String -> Decoder (Maybe Int)
readString payload =
  case payload of
    Nothing -> succeed Nothing
    Just s -> succeed <| Result.toMaybe (String.toInt s)


{-| Specify a relationship attribute that is expected in the incoming payload -}
-- TODO shouldn't this take a tuple as its first parameter to match the interface of the elm object library?
-- TODO could the syntax be shortened in some way to make it easier to use, like "<-R"?
relationship : String -> Decoder a -> Decoder a
relationship name decoder =
  at ["relationships", name, "data"] decoder


listDecoder : Decoder a -> Decoder (List a)
listDecoder decoder =
  root <| list decoder


-- ENCODING


{-| Creates the correct payload for POST requests to a JSON API endpoint -}
encodePost : String 
 -> List (String, Encode.Value) 
 -> List (String, Encode.Value) 
 -> Encode.Value
encodePost objType atts rels =
  [jsonApiType objType, attributes atts, relationships rels] |> dataEncoder


{-| Creates the correct payload for PATCH requests to a JSON API endpoint -}
encodePatch : String 
 -> Int 
 -> List (String, Encode.Value) 
 -> List (String, Encode.Value) 
 -> Encode.Value
encodePatch objType id atts rels =
  [jsonApiType objType, encodeId id, attributes atts, relationships rels] |> dataEncoder
 

{-| Encodes the root "data" node of a JSON API conformant payload -}
dataEncoder : List (String, Encode.Value) -> Encode.Value
dataEncoder forWrapping =
  object [
    ("data", (object forWrapping))
    ]
    

{-| Encodes the "type" attribute of a JSON API conformant payload -}
jsonApiType : String -> (String, Encode.Value)
jsonApiType name =
  ("type", Encode.string name)

      
{-| Encodes the "attribute" node of a JSON API conformant payload -}
attributes : List (String, Encode.Value) -> (String, Encode.Value)
attributes atts =
  ("attributes", (object atts))


{-| Encodes the "relationships" node of a JSON API conformant payload -}
relationships : List (String, Encode.Value) -> (String, Encode.Value)
relationships rels = 
  ("relationships", (object rels))


{-| Encodes the "id" attribute of a JSON API conformant payload -}
encodeId : Int -> (String, Encode.Value)
encodeId i =
  ("id", Encode.int i)


{-| Adds the correct headers for JSON API conformant request to an Http.Request object -}
requiredHeaders : RequestBuilder a -> RequestBuilder a
requiredHeaders request =
  request |> withHeaders [ ("content-type", "application/vnd.api+json")
                         , ("accept", "application/vnd.api+json")
                         ]
