---
title: Haskell Libraries ekg-core
---

All metrics have a name
a way to get the metrics current value

### Store

Mutable metric

System.Metric http://hackage.haskell.org/package/ekg-core-0.1.1.1/docs/src/System-Metrics.html#Store

- Counter: non-negative, monotonically increasing integer-valued metric.
- Gauge: integer-valued metric.
- Label: text metric.
- Distribution: product type of mean, variance, count, sum, min and max.
- Group: an action to be executed any time one of the metrics computed from the value it returns needs to be sampled.
 sample groups of metrics together, consistent view of several metrics, sampling metrics together is more efficient.

IORef is a container of a mutable value. 
stateMetrics is a HashMap of Text keys to Either a MetricSampler or a GroupId (Int).
stateGroups is a integer keys to 

efficient implementation of maps (dictionaries) from integer keys to values.
[Data.IntMap.Strict](https://hackage.haskell.org/package/containers-0.5.10.2/docs/Data-IntMap-Strict.html)

The most important type in ekg-core is `Store`, located in `System.Metrics`.
```haskell
newtype Store = Store { storeState :: IORef State }

data State = State
     { stateMetrics :: !(M.HashMap T.Text (Either MetricSampler GroupId))
     , stateGroups  :: !(IM.IntMap GroupSampler)
     , stateNextId  :: {-# UNPACK #-} !Int
     }
     
type GroupId = Int

data GroupSampler = forall a. GroupSampler
     { groupSampleAction   :: !(IO a)
     , groupSamplerMetrics :: !(M.HashMap T.Text (a -> Value))
     }
     
data MetricSampler = CounterS !(IO Int64)
                   | GaugeS !(IO Int64)
                   | LabelS !(IO T.Text)
                   | DistributionS !(IO Distribution.Stats)
```


register let's you add a sample to an existing Store value.
```haskell
register :: T.Text
         -> MetricSampler
         -> Store
         -> IO ()
register name sample store = do
    atomicModifyIORef (storeState store) $ \ state@State{..} ->
        case M.member name stateMetrics of
            False -> let !state' = state {
                               stateMetrics = M.insert name
                                              (Left sample)
                                              stateMetrics
                             }
                     in (state', ())
            True  -> alreadyInUseError name
```

`Distribution` is a metric for tracking events
```haskell
newtype Distribution = Distribution { unD :: Array Stripe }

data Stripe = Stripe
    { stripeFp    :: !(ForeignPtr CDistrib)
    }

data CDistrib = CDistrib
    { cCount      :: !Int64
    , cMean       :: !Double
    , cSumSqDelta :: !Double
    , cSum        :: !Double
    , cMin        :: !Double
    , cMax        :: !Double
    , cLock       :: !Int64  -- ^ 0 - unlocked, 1 - locked
    }
```

haskell version of Distribution, you can use `read :: Distribution -> IO Stats`
-- | Distribution statistics
```haskell
data Stats = Stats
    { mean     :: !Double  -- ^ Sample mean
    , variance :: !Double  -- ^ Biased sample variance
    , count    :: !Int64   -- ^ Event count
    , sum      :: !Double  -- ^ Sum of values
    , min      :: !Double  -- ^ Min value seen
    , max      :: !Double  -- ^ Max value seen
    } deriving (Eq, Show)
```