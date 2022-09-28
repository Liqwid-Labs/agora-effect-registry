-- let Prelude = ./Prelude.dhall
let ScriptHash = Text

let IntegerSchema
    : Type
    = { foo : Text }

let ByteStringSchema
    : Type
    = {}

let DatumSchema = < I : IntegerSchema | B : ByteStringSchema >

let Effect
    : Type
    = { name : Text
      , description : Text
      , scriptHash : ScriptHash
      , datumSchema : DatumSchema
      }

in  { ScriptHash, Effect, DatumSchema, IntegerSchema }
