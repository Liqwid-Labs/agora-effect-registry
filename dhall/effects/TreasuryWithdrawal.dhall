let agora = ../agora.dhall

let effect
    : agora.Effect
    = { name = "TreasuryWithdrawal"
      , description = "Withdraw funds from the treasury"
      , scriptHash = "foo"
      , datumSchema = agora.DatumSchema.I { foo = "hello" }
      }

in  effect
