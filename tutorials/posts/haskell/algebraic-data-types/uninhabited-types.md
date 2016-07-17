```haskell
data Kilometer
```

```haskell
data Void = Void !Void
```
This does not terminate.

Invent a new set of types to fill in GADT type parameters.
Tag a value at the type level.

```haskell
data Distance unit where
  Miles      :: Double -> Distance Miles
  Kilometers :: Double -> Distance Kilometers

addDistances :: Distance unit -> Distance unit -> Distance unit
addDistances (Miles x) (Miles y) = Miles $ x + y
addDistances (Kilometers x) (Kilometers y) = Kilometers $ x + y
-- addDistances (Kilometers x) (Miles y) = Kilometers $ x + y -- "Type error: cannot match expected type "Distance Kilometers" with actual type "Distance Miles"
```

Use DataKinds to give them their own kind rather than *
type promoted to kind

```
data DistanceUnit
  = Kilometer
  | Mile
  | Yard
  | Meter
  | Foot
  | Inch
  | Centimeter
  | Millimeter
  ...
  | Angstrom
```

```
data Distance (unit :: DistanceUnit) where
  Kilometers :: Double -> Distance Kilometer
  ...
```
`::` on the type variable is a kind signature, DataKinds means that the type/value
structure of `DistanceUnit` is reflected as a kind/type structure `DistanceUnit`.
the types are unihabited because all inhabited types are in `*`.

Without DataKinds `DistanceUnit` is just `type`, not a `kind`.

kind of `Maybe` is `* -> *`
