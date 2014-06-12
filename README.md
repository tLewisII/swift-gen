# swift-gen

Generate boilerplate swift with a simple DSL!

*You can get pretty far without macros!*

[Download Binary (OS X x86)] **Coming soon!**

## Example

```
-- GeoJSON
data GeoJSON = GeoJSON { _type :: String
                       , features :: Array Feature }
  deriving (Printable, Equatable, JSON)

data Feature = Feature { _type :: String
                       , geometry :: Array Coordinate
                       , properties :: Dictionary String String }
  deriving (Printable, Equatable, JSON)

data Coordinate = Coordinate { _type :: String
                             , coordinates :: Array Double }
  deriving (Printable, Equatable, JSON)

```

## Usage

```
swift-gen file.swift.gen
```

This produces the file `file.swift`. This can also be used from XCode.

A field name that starts with an underscore, the underscore is removed.
This lets you write `type` as `_type` so the code is valid syntax.

## Implemented

Standard library:

- `Equatable`
- `Printable`
- `Hashable`
- `Comparable`

swiftz:

- Value boxing when required (via `Box`)
- `JSON`
- `Dataable`
- `Lens`

todo:

- A curried `create`
- swift thrift
- void types?
- quasi quote method bodies
- ArrayLiteralConvertible
