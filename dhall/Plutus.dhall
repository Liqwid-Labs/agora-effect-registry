-- Representation of the PlutusCore.Data `Data` type.
let Prelude = ./Prelude.dhall

let Types =
      let PlutusConstr =
            \(PlutusData : Type) -> { tag : Integer, fields : List PlutusData }

      let PlutusMap =
            \(PlutusData : Type) -> List { _1 : PlutusData, _2 : PlutusData }

      let PlutusList = \(PlutusData : Type) -> List PlutusData

      let PlutusI = Integer

      let PlutusB = Integer

      let PlutusData
          : Type
          = forall (PlutusData : Type) ->
            forall (PlutusConstr : PlutusConstr PlutusData -> PlutusData) ->
            forall (PlutusMap : PlutusMap PlutusData -> PlutusData) ->
            forall (PlutusList : PlutusList PlutusData -> PlutusData) ->
            forall (PlutusI : PlutusI -> PlutusData) ->
            forall (PlutusB : PlutusB -> PlutusData) ->
              PlutusData

      in  { PlutusConstr, PlutusMap, PlutusList, PlutusI, PlutusB, PlutusData }

let PlutusData =
      let PlutusConstr
          : Types.PlutusConstr Types.PlutusData -> Types.PlutusData
          = \(constr : Types.PlutusConstr Types.PlutusData) ->
            \(PlutusData : Type) ->
            \(PlutusConstr : Types.PlutusConstr PlutusData -> PlutusData) ->
            \(PlutusMap : Types.PlutusMap PlutusData -> PlutusData) ->
            \(PlutusList : Types.PlutusList PlutusData -> PlutusData) ->
            \(PlutusI : Types.PlutusI -> PlutusData) ->
            \(PlutusB : Types.PlutusB -> PlutusData) ->
              PlutusConstr
                ( constr
                  with fields =
                      Prelude.List.map
                        Types.PlutusData
                        PlutusData
                        ( \(field : Types.PlutusData) ->
                            field
                              PlutusData
                              PlutusConstr
                              PlutusMap
                              PlutusList
                              PlutusI
                              PlutusB
                        )
                        constr.fields
                )

      let PlutusMap
          : Types.PlutusMap Types.PlutusData -> Types.PlutusData
          = \(map : Types.PlutusMap Types.PlutusData) ->
            \(PlutusData : Type) ->
            \(PlutusConstr : Types.PlutusConstr PlutusData -> PlutusData) ->
            \(PlutusMap : Types.PlutusMap PlutusData -> PlutusData) ->
            \(PlutusList : Types.PlutusList PlutusData -> PlutusData) ->
            \(PlutusI : Types.PlutusI -> PlutusData) ->
            \(PlutusB : Types.PlutusB -> PlutusData) ->
              PlutusMap
                ( Prelude.List.map
                    { _1 : Types.PlutusData, _2 : Types.PlutusData }
                    { _1 : PlutusData, _2 : PlutusData }
                    ( \ ( kvp
                        : { _1 : Types.PlutusData, _2 : Types.PlutusData }
                        ) ->
                        { _1 =
                            kvp._1
                              PlutusData
                              PlutusConstr
                              PlutusMap
                              PlutusList
                              PlutusI
                              PlutusB
                        , _2 =
                            kvp._2
                              PlutusData
                              PlutusConstr
                              PlutusMap
                              PlutusList
                              PlutusI
                              PlutusB
                        }
                    )
                    map
                )

      let PlutusList
          : Types.PlutusList Types.PlutusData -> Types.PlutusData
          = \(xs : Types.PlutusList Types.PlutusData) ->
            \(PlutusData : Type) ->
            \(PlutusConstr : Types.PlutusConstr PlutusData -> PlutusData) ->
            \(PlutusMap : Types.PlutusMap PlutusData -> PlutusData) ->
            \(PlutusList : Types.PlutusList PlutusData -> PlutusData) ->
            \(PlutusI : Types.PlutusI -> PlutusData) ->
            \(PlutusB : Types.PlutusB -> PlutusData) ->
              PlutusList
                ( Prelude.List.map
                    Types.PlutusData
                    PlutusData
                    ( \(v : Types.PlutusData) ->
                        v
                          PlutusData
                          PlutusConstr
                          PlutusMap
                          PlutusList
                          PlutusI
                          PlutusB
                    )
                    xs
                )

      let PlutusI
          : Types.PlutusI -> Types.PlutusData
          = \(i : Types.PlutusI) ->
            \(PlutusData : Type) ->
            \(PlutusConstr : Types.PlutusConstr PlutusData -> PlutusData) ->
            \(PlutusMap : Types.PlutusMap PlutusData -> PlutusData) ->
            \(PlutusList : Types.PlutusList PlutusData -> PlutusData) ->
            \(PlutusI : Types.PlutusI -> PlutusData) ->
            \(PlutusB : Types.PlutusB -> PlutusData) ->
              PlutusI i

      in  { PlutusConstr, PlutusMap, PlutusList, PlutusI }

in  { Types, PlutusData }
