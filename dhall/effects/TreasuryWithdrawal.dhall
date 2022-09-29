let agora = ../agora.dhall

let effect
    : agora.Effect
    = { name = "TreasuryWithdrawal"
      , description = "Withdraw funds from the treasury"
      , scriptHash = "foo"
      , datumSchema =
          agora.constr
            +0
            [ agora.field
                "receivers"
                ( agora.list
                    ( agora.constr
                        +0
                        [ agora.field "receiver" agora.address
                        , agora.field "amount" agora.value
                        ]
                    )
                )
            ]
      }

in  effect
