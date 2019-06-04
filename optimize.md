### Optimization opportunities

- memoize methods like
  `implicit def isoCSizePrim[Val](implicit eVal: Elem[Val]): Iso[CSizePrimData[Val], CSizePrim[Val]]`
  
- Use Slicing to optimize dataSize computations out of cost: (Int, Long) => Int function.
   This make sense when sizes are not used in cost computation.
   
### HISTORY (done)

- Avoid caching of elements for non-generic types (segmentElement)
- Avoid creating Fold nodes for CostedOption.get,isDefined (remove CostedFoldExtractors.IsGet)
