## A list of hard-fork changes

Please describe here all changes which may lead to hard fork (HF for short).

**Pull requests based on the next HF branch should be rejected, 
if they contain HF change, which is not described here**.

### Hard-fork changes in v0.7.0 (since v0.6.0)

1. Removed RW rule `case IsNumericToLong(Def(IsNumericToInt(x))) if x.elem == LongElement => x`
  This is a bug, but its fix may lead to hard-fork.
  Example: 
  The follwing expression `MaxLong.toInt.toLong == MaxLong`
  with this rule will evaluate to `true`, 
  without this rule will throw ArithmeticException.
  MaxLong here can be any Long which is larger than MaxInt.
  With this rule the expression becomes `MaxLong == MaxLong`
  
 2. Removed RW rule `case CM.map(CM.map(_xs, f: RFunc[a, b]), _g: RFunc[_,c]) =>`.
 Such kind of transformations in general don't preserve expression equivalence 
 in a strict (Call-By-Value) language. 
 Having such rule is another bug, which is safe by itself, but cannot
 be fixed without HF.

 3. CReplColl.updated (bug fixed) 
 Added index boundary check throwing IndexOutOfBoundsException.
 This check is necessary to preserve Coll.updated contract.
