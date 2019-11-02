# Gugugu

Gugugu is a non-opinionated data serialization and RPC (Remote Procedure Call)
framework.
*Non-opinionated* means gugugu assumes very little on your implementation.
You can serialize your data with JSON, XML... or your own serialization format,
and communicate with any protocol you like.

The definition syntax is a strict subset of Haskell.

```haskell
module Hello where


-- The content after double-dash is ignored.

fold :: FoldRequest -> IO Int32

data FoldRequest
  = FoldRequest
    { values  :: List Int32
    , initial :: Int32
    , op      :: Operation
    }

data Operation
  = Add
  | Mul
```

## Build Status

|  All    |       | [![Build Status](https://dev.azure.com/cosmiafu/gugugu/_apis/build/status/gugugu?branchName=master)](https://dev.azure.com/cosmiafu/gugugu/_build) |
|---------|-------|----------------------------------------------------------------------------------------------------------------------------------------------------|
| Linux   | amd64 | ![Linux Status](https://dev.azure.com/cosmiafu/gugugu/_apis/build/status/gugugu?branchName=master&jobName=build&configuration=build%20linux)       |
| macOS   | amd64 | ![macOS Status](https://dev.azure.com/cosmiafu/gugugu/_apis/build/status/gugugu?branchName=master&jobName=build&configuration=build%20darwin)      |
| Windows | amd64 | ![Windows Status](https://dev.azure.com/cosmiafu/gugugu/_apis/build/status/gugugu?branchName=master&jobName=build&configuration=build%20win32)     |
