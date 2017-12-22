module JsonApi exposing (..)

{-| Helpers for encoding and decoding JSON Api payloads in elm. This is a convenience
package, loaded into elm-package for convenience reasons (i.e. to make them
available in several of my programs). Not all aspects of JSON Api are implemented.

@docs root, attribute, decodeId, relationship, listDecoder

@docs encodePost, encodePatch, dataEncoder, jsonApiType, attributes, relationships, encodeId, requiredHeaders

-}

import Http
import HttpBuilder exposing (RequestBuilder, withHeaders)
import Json.Decode as Decode exposing (Decoder, andThen, at, int, list, map2, maybe, string, succeed)
import Json.Encode as Encode exposing (object)
import String


-- DECODING


{-| Access the root "data" object of a JsonApi payload
-}
root : Decoder a -> Decoder a
root =
    at [ "data" ]


{-| Decode an atribute
-}
attribute : String -> Decoder a -> Decoder a
attribute name decoder =
    at [ "attributes", name ] decoder


{-| Decode an id
-}
decodeId : Decoder (Maybe String)
decodeId =
    at [ "id" ] (maybe string)


{-| Specify a relationship attribute that is expected in the incoming payload
-}
relationship : String -> Decoder a -> Decoder a
relationship name decoder =
    at [ "relationships", name, "data" ] decoder


{-| Decode a payload where objects are delivered in a list
-}
listDecoder : Decoder a -> Decoder (List a)
listDecoder decoder =
    root <| list decoder



-- ENCODING


{-| Creates the correct payload for POST requests to a JSON API endpoint
-}
encodePost :
    String
    -> List ( String, Encode.Value )
    -> List ( String, Encode.Value )
    -> Encode.Value
encodePost objType atts rels =
    [ jsonApiType objType, attributes atts, relationships rels ] |> dataEncoder


{-| Creates the correct payload for PATCH requests to a JSON API endpoint
-}
encodePatch :
    String
    -> Int
    -> List ( String, Encode.Value )
    -> List ( String, Encode.Value )
    -> Encode.Value
encodePatch objType id atts rels =
    [ jsonApiType objType, encodeId id, attributes atts, relationships rels ] |> dataEncoder


{-| Encodes the root "data" node of a JSON API conformant payload
-}
dataEncoder : List ( String, Encode.Value ) -> Encode.Value
dataEncoder forWrapping =
    object
        [ ( "data", object forWrapping )
        ]


{-| Encodes the "type" attribute of a JSON API conformant payload
-}
jsonApiType : String -> ( String, Encode.Value )
jsonApiType name =
    ( "type", Encode.string name )


{-| Encodes the "attribute" node of a JSON API conformant payload
-}
attributes : List ( String, Encode.Value ) -> ( String, Encode.Value )
attributes atts =
    ( "attributes", object atts )


{-| Encodes the "relationships" node of a JSON API conformant payload
-}
relationships : List ( String, Encode.Value ) -> ( String, Encode.Value )
relationships rels =
    ( "relationships", object rels )


{-| Encodes the "id" attribute of a JSON API conformant payload
-}
encodeId : Int -> ( String, Encode.Value )
encodeId i =
    ( "id", Encode.int i )


{-| Adds the correct headers for JSON API conformant request to a RequestBuilder object
-}
requiredHeaders : RequestBuilder a -> RequestBuilder a
requiredHeaders request =
    request
        |> withHeaders
            [ ( "content-type", "application/vnd.api+json" )
            , ( "accept", "application/vnd.api+json" )
            ]
