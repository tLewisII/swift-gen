# swift-gen

Generate Swift structs / enums with protocol implementations!

*You can get pretty far without macros!*

[Download Binary (OS X x64)](https://github.com/maxpow4h/swift-gen/releases/tag/v0.1.0)

## Example

Define your structs / enums in a simple DSL and ask protocols
to be derived for you by the `deriving` keyword.

```haskell
-- GeoJSON
data GeoJSON = GeoJSON { _type :: String
                       , features :: Array Feature }
  deriving (Printable, Equatable, JSON)

data Feature = Feature { _type :: String
                       , geometry :: Coordinate
                       , properties :: Dictionary String String }
  deriving (Printable, Equatable, JSON)

data Coordinate = Coordinate { _type :: String
                             , coordinates :: Array Double }
  deriving (Printable, Equatable, JSON)
```

Now run `swift-gen file.swift.gen` and it produces definitions of the
structs and enums with Printable and Equatable generated!

```swift
struct GeoJSON : Printable, Equatable, JSON {
  let type: String
  let features: Array<Feature>
  var description: String {
    get {
      return "GeoJSON(\(type), \(features))"
    }
  }
  static func fromJSON(x: JSValue) -> GeoJSON? {
    var vtype: String?
    var vfeatures: Array<Feature>?
    switch x {
      case let .JSObject(d):
        vtype = d["type"] >>= JString.fromJSON
        vfeatures = d["features"] >>= JArray<Feature, Feature>.fromJSON
        if (vtype && vfeatures) {
          return GeoJSON(type: vtype!, features: vfeatures!)
        } else {
          return nil
        }
      default: return nil
    }
  }
  func toJSON(x: GeoJSON) -> JSValue {
    return JSValue.JSObject(["type": .JSString(x.type)
            , "features": .JSArray(x.features.map({ $0.toJSON($0) }))])
  }
}
func==(lhs: GeoJSON, rhs: GeoJSON) -> Bool {
  return lhs.type == rhs.type && lhs.features == rhs.features
}
// ... similarly for the Feature and Coordinate structs
```

Let's see this in action:
```swift
let location = GeoJSON(type: "FeatureCollection"
                      ,features: [
                        Feature(type: "Feature"
                               ,geometry: Coordinate(type: "Point"
                                                    ,coordinates: [2.0, 4.0])
                               ,properties: ["foo": "bar"])])

println(location)
// GeoJSON(FeatureCollection, [Feature(Feature
//                                    ,Coordinate(Point, [2.0, 4.0]), [foo: bar])])

println(location.toJSON(location))
// {"type":"FeatureCollection"
//  ,"features":[
//     {"geometry":
//       {"type":"Point"
//       ,"coordinates":[2.0, 4.0]}
//       ,"properties":{"foo":"bar"}
//     ,"type":"Feature"}]}
```

## Usage

```
swift-gen file.swift.gen
```

A field name that starts with an underscore, the underscore is removed.
This lets you write `type` as `_type` so the code is valid syntax.

## Implemented

Standard library:

- `Equatable`
- `Printable`

Swiftz:

- `JSON` for a limited set of types

## TODO / Help wanted

- Standard library
  - `Hashable`
  - `Comparable`
- swiftz
  - Value boxing when required (via `Box`)
  - `JSON` for all types
  - `Dataable`
  - `Lens`
- Switch to `language-swift-quote`
- XCode integration tutorial
- A curried `create`
- thrift generation
- void types?
- quasi quote method bodies
- ArrayLiteralConvertible
