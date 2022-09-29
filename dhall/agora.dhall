let Prelude = ./Prelude.dhall

let JSON = Prelude.JSON

let ScriptHash = Text

let IntegerSchema
    : Type
    = {}

let DatumSchema = JSON.Type

let Field
    : Type
    = { mapKey : Text, mapValue : JSON.Type }

let field =
      \(name : Text) ->
      \(value : JSON.Type) ->
        { mapKey = name, mapValue = value }

let list =
      \(values : DatumSchema) ->
        JSON.object
          [ { mapKey = "type", mapValue = JSON.string "List" }
          , { mapKey = "elements", mapValue = values }
          ]

let constr =
      \(tag : Integer) ->
      \(fields : List Field) ->
        JSON.object
          [ { mapKey = "type", mapValue = JSON.string "Constr" }
          , { mapKey = "tag", mapValue = JSON.integer tag }
          , { mapKey = "fields", mapValue = JSON.object fields }
          ]

let address =
      JSON.object [ { mapKey = "type", mapValue = JSON.string "address" } ]

let value = JSON.object [ { mapKey = "type", mapValue = JSON.string "value" } ]

let Effect
    : Type
    = { name : Text
      , description : Text
      , scriptHash : ScriptHash
      , datumSchema : DatumSchema
      }

in  { ScriptHash
    , Effect
    , DatumSchema
    , IntegerSchema
    , constr
    , field
    , list
    , address
    , value
    }
