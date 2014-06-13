struct GeoJSON : Printable, Equatable {
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

struct Feature : Printable, Equatable {
  let type: String
  let geometry: Array<Coordinate>
  let properties: Dictionary<String, String>
  var description: String {
    get {
      return "Feature(\(type), \(geometry), \(properties))"
    }
  }
}
func==(lhs: Feature, rhs: Feature) -> Bool {
  return lhs.type == rhs.type && lhs.geometry == rhs.geometry && lhs.properties == rhs.properties
}

struct Coordinate : Printable, Equatable {
  let type: String
  let coordinates: Array<Double>
  var description: String {
    get {
      return "Coordinate(\(type), \(coordinates))"
    }
  }
}
func==(lhs: Coordinate, rhs: Coordinate) -> Bool {
  return lhs.type == rhs.type && lhs.coordinates == rhs.coordinates
}

enum List<A> : Printable, Equatable {
  case Cons(A, List<A>)
  case Nil()
  var description: String {
    get {
      switch self {
    case let (.Cons(a1, a2)):
      return "Cons(\(a1), \(a2))"
    case (.Nil):
      return "Nil()"
    }
    }
  }
}
func==<A: Equatable>(lhs: List<A>, rhs: List<A>) -> Bool {
  switch (lhs, rhs) {
    case let (.Cons(la1, la2), .Cons(ra1, ra2)):
      return la1 == ra1 && la2 == ra2
    case (.Nil, .Nil):
      return true
  }
}

enum Map<K, V> : Printable, Equatable {
  case Empty()
  case Map(K, V, Map<K, V>)
  var description: String {
    get {
      switch self {
    case (.Empty):
      return "Empty()"
    case let (.Map(a1, a2, a3)):
      return "Map(\(a1), \(a2), \(a3))"
    }
    }
  }
}
func==<K: Equatable, V: Equatable>(lhs: Map<K, V>, rhs: Map<K, V>) -> Bool {
  switch (lhs, rhs) {
    case (.Empty, .Empty):
      return true
    case let (.Map(la1, la2, la3), .Map(ra1, ra2, ra3)):
      return la1 == ra1 && la2 == ra2 && la3 == ra3
  }
}

struct Thing : Printable, Equatable {
  let a1: String
  let a2: Foo
  var description: String {
    get {
      return "Thing(\(a1), \(a2))"
    }
  }
}
func==(lhs: Thing, rhs: Thing) -> Bool {
  return lhs.a1 == rhs.a1 && lhs.a2 == rhs.a2
}

struct Box : Printable, Equatable {

  var description: String {
    get {
      return "Box()"
    }
  }
}
func==(lhs: Box, rhs: Box) -> Bool {
  return true
}
