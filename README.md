# swift-gen

Generate Swift structs / enums with protocol implementations!

*You can get pretty far without macros!*

[Download Binary (OS X x86)] **Coming soon!**

## Example

Define your structs / enums in a simple DSL and ask protocols
to be derived for you by the `deriving` keyword.

```haskell
-- GeoJSON
data GeoJSON = GeoJSON { _type :: String
                       , features :: Array Feature }
  deriving (Printable, Equatable)

data Feature = Feature { _type :: String
                       , geometry :: Array Coordinate
                       , properties :: Dictionary String String }
  deriving (Printable, Equatable)

data Coordinate = Coordinate { _type :: String
                             , coordinates :: Array Double }
  deriving (Printable, Equatable)

```

Now run `swift-gen file.swift.gen` and it produces a definition of the
structs and enums with Printable and Equatable implemented!

```swift
struct GeoJSON : Printable, Equatable, JSON {
  let type: String
  let features: Array<Feature>
  var description: String {
    get {
      return "GeoJSON(\(type), \(features))"
    }
  }
}
func==(lhs: GeoJSON, rhs: GeoJSON) -> Bool {
  return lhs.type == rhs.type && lhs.features == rhs.features
}
// ... the same for Feature and Coordinate
```

Let's see this in action:
```swift
println(GeoJSON(type: "Feature", features: [Feature(type: "Coordinate", geometry: [Coordinate(type: "fixed", coordinates: [2.0, 4.0])], properties: ["foo": "bar"])]))
// prints GeoJSON(Feature, [Feature(Coordinate, [Coordinate(fixed, [2.0, 4.0])], [foo: bar])])
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

## TODO / Help wanted

- Standard library
  - `Hashable`
  - `Comparable`
- swiftz
  - Value boxing when required (via `Box`)
  - `JSON`
  - `Dataable`
  - `Lens`
- Switch to `language-swift-quote`
- XCode integration tutorial
- A curried `create`
- thrift generation
- void types?
- quasi quote method bodies
- ArrayLiteralConvertible
