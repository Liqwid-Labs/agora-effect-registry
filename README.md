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
      "scriptHash": "581caabbccddeeff11223344556677889900aabbccddeeff112233445566"
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
TODO
```
