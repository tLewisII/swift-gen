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
    return JSValue.JSObject(["type": .JSString(x.type), "features": .JSArray(x.features.map({ $0.toJSON($0) }))])
  }
}
func==(lhs: GeoJSON, rhs: GeoJSON) -> Bool {
  return lhs.type == rhs.type && lhs.features == rhs.features
}

struct Feature : Printable, Equatable, JSON {
  let type: String
  let geometry: Array<Coordinate>
  let properties: Dictionary<String, String>
  var description: String {
    get {
      return "Feature(\(type), \(geometry), \(properties))"
    }
  }
  static func fromJSON(x: JSValue) -> Feature? {
    var vtype: String?
    var vgeometry: Array<Coordinate>?
    var vproperties: Dictionary<String, String>?
    switch x {
      case let .JSObject(d):
        vtype = d["type"] >>= JString.fromJSON
        vgeometry = d["geometry"] >>= JArray<Coordinate, Coordinate>.fromJSON
        vproperties = d["properties"] >>= JDictionary<String, JString>.fromJSON
        if (vtype && vgeometry && vproperties) {
          return Feature(type: vtype!, geometry: vgeometry!, properties: vproperties!)
        } else {
          return nil
        }
      default: return nil
    }
  }
  func toJSON(x: Feature) -> JSValue {
    return JSValue.JSObject(["type": .JSString(x.type), "geometry": .JSArray(x.geometry.map({ $0.toJSON($0) })), "properties": .JSObject(x.properties.mapValues({ jstring.toJSON($0.1) }))])
  }
}
func==(lhs: Feature, rhs: Feature) -> Bool {
  return lhs.type == rhs.type && lhs.geometry == rhs.geometry && lhs.properties == rhs.properties
}

struct Coordinate : Printable, Equatable, JSON {
  let type: String
  let coordinates: Array<Double>
  var description: String {
    get {
      return "Coordinate(\(type), \(coordinates))"
    }
  }
  static func fromJSON(x: JSValue) -> Coordinate? {
    var vtype: String?
    var vcoordinates: Array<Double>?
    switch x {
      case let .JSObject(d):
        vtype = d["type"] >>= JString.fromJSON
        vcoordinates = d["coordinates"] >>= JArray<Double, JDouble>.fromJSON
        if (vtype && vcoordinates) {
          return Coordinate(type: vtype!, coordinates: vcoordinates!)
        } else {
          return nil
        }
      default: return nil
    }
  }
  func toJSON(x: Coordinate) -> JSValue {
    return JSValue.JSObject(["type": .JSString(x.type), "coordinates": .JSArray(x.coordinates.map({ jdouble.toJSON($0) }))])
  }
}
func==(lhs: Coordinate, rhs: Coordinate) -> Bool {
  return lhs.type == rhs.type && lhs.coordinates == rhs.coordinates
}
