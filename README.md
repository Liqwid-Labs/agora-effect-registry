# agora-effect-registry

This repository contains metadata for Agora effects. It contains information about how effect datums are structured, documentation about them, and versioning information.

## Effect datum structure

Effect schemas are each a JSON file containing information about a particular effect script. The top level fields identify and document this effect. An example might look like this:

```json
{
    "meta": {
        "name": "MintNFT",
        "description": "Mints a single NFT based on the provided token name. This NFT is uniquely identified by the UTXO from which the GAT is burned is spent."
    },
    "scriptHash": "b00f0ebdd6ef98c82f8534b2040cd454c3cf3601b908ce212d43b74b",
    "datumSchema": { 
        "meta": {
            "name": "tokenName",
            "description": "The token name of the NFT to mint."
        },
        "type":"string"
    }
}
```

`meta` fields each are not important to the actual functioning of the script nor its datum, but instead are useful for frontends to provide information to the user, as well as general documentation that can be placed.

`scriptHash` identifies the actual script which this particular schema belongs to. This is done as a script hash. 

`datumSchema` is the schema that the _datums_ that this script receives in UTXOs (as a result of a proposal passing) should follow. This is the bread and butter of this repo, as it is valuable to prevent wrong use of the script and risk potential loss of funds. 

# agora-effect-registry-server

The server provided by this repository is useful for querying the registry and computing validated datums from it.

## Endpoints

### `GET /info`

This will give you information about the server and list out the available scripts.

**Example response:**

```json
{
  "hostedEffectDatumSchemas": [
    {
      "name": "TreasuryWithdrawal",
      "scriptHash": "581caabbccddeeff11223344556677889900aabbccddeeff11223344"
    },
    {
      "name": "MintNFT",
      "scriptHash": "b00f0ebdd6ef98c82f8534b2040cd454c3cf3601b908ce212d43b74b"
    }
  ],
  "revision": "main@674ccf3fc3a978a164405398b22c14ef1d04c1f2"
}
```

### `GET /effects/:name`

Queries the raw schema file from those that live in `/effects`.

**Example response:**

```json
{
    "meta": {
        "name": "MintNFT",
        "description": "Mints a single NFT based on the provided token name. This NFT is uniquely identified by the UTXO from which the GAT is burned is spent."
    },
    "scriptHash": "b00f0ebdd6ef98c82f8534b2040cd454c3cf3601b908ce212d43b74b",
    "datumSchema": { 
        "meta": {
            "name": "tokenName",
            "description": "The token name of the NFT to mint."
        },
        "type":"string"
    }
}
```

### `POST /encodeEffectDatum/:scriptHash`

Encode a datum that follows a particular effect schema into its hex-encoded CBOR representation. This endpoint also ensures the datum passed matches that of the schema it should be checked against, throwing an error if it doesn't.

**Example request:**

```sh
curl \
    -X POST \
    localhost:3838/encodeEffectDatum/babbccddeeff11223344556677889900aabbccddeeff112233445566 \
    -H 'Content-Type: application/json' \
    -d '{ "type": "integer", "value": 42 }'
```

**Example response:**

```json
{"cborDatum":"182a"}
```

# Addendum: Schema types

When writing a datum schema, a number of different building blocks are available to you. These will be listed below.

### `integer` and `bytes`

**Example structure**:

An integer (`I` in `PlutusCore.Data`):

```json
{ "type": "integer" }
```

A bytestring (`B` in `PlutusCore.Data`):

```json
{ "type": "bytes" }
```

These two types are primitive types that contain no subvalues inside of the schema. In other words: they are leaves in the schema tree.

### `constr`

A constr tag (`Constr` in `PlutusCore.Data`). This tags a particular struct with a integer tag. This is particularly useful in combination with `oneOf`, to create tagged unions.

**Example structure**:

```json
{ "type": "constr", "tag": ..., "fields": [..., ...] }
```

### `list`

A homogeneous list (`List` in `PlutusCore.Data`). The `elements` field is yet another datum schema type, which represents the type of all elements of this list. Keep in mind that the `elements` field is a _single_ type, as opposed to a list of types. This is slightly unintuitive due to the naming. See it as when you write `List a`, `a` is a single type to which all elements must conform to.

**Example structure**:

```json
{ "type": "list", "elements": ... }
```

### `shapedList`

**Example structure**:

Represents a _heterogeneous_ list (also `List` in `PlutusCore.Data`). Unlike `list`, this may contain different types for each element, but they are in a particular predetermined order, as given by the `elements` field.

```json
{ "type": "shapedList", "elements": [..., ...] }
```

When representing a tuple of two integers, we could for example encode it like so:

```json
{
  "type": "shapedList",
  "elements": [
    { "type": "integer" },
    { "type": "integer" }
  ]
}
```

In many ways, this is similar to how `constr` works, but without the tag.

### `map`

A map (`Map` in `PlutusCore.Data`). This maps `keys` to `values`. Both are homogeneous in their types, so they are a single subschema, similarly to `list`.

**Example structure**:

```json
{
  "type": "map",
  "keys": ...,
  "values": ...
}
```

### `oneOf`

`oneOf` allows encoding variants into the schema. This will usually be used in combination with `constr` in order to encode tagged unions, but can also encode other variants.

**Example structure**:

```json
{ "type": "oneOf", "options": [..., ...] }
```

---

Additionally, the following singleton schema types are available which can be used to represent frequently used plutus types:

- `plutus/Address`
- `plutus/Value`
- `plutus/Credential`
- `plutus/Hash32`
- `plutus/Hash28`

Each can be encoded with additional metadata just like all other types.
