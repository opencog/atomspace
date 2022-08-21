
Load Ordinary CSV Tables
========================
The code here is able to load "delimiter-separated values" (DSV,
or CSV, TSV for comma and tab separators) from a file. This are
just very conventional tables.

Each column from a DSV file is read in and placed into an Atomese
Values on an indicated Atom. Atomese Values are vectors (of floats,
bools, strings). Each Value holds one column from the dataset.

Basically, this just gets CSV data into the AtomSpace, where it
becomes easy for Atomese programs to act on them, i.e. to use them
as input for some kind of data stream processing.

The features (columns) specified in ignore_features will be omitted
from the representation.

Example
-------
For example, a CSV dataset like this:
```
   o, i1, i2, i3, i4
   1, 0, 0, 3.3, "foo"
   0, 1, 0, 4.4, "bar"
```
will be loaded as key-value pairs on the `anchor` Atom.

The column names will be loaded under a "well known key":
```
   (Predicate "*-column-keys-*")
```
This key will point at a value holding a list of all of the
column-keys in the table:
```
   (LinkValue
      (Predicate "o")
      (Predicate "i1")
      (Predicate "i2")
      (Predicate "i3")
      (Predicate "i4"))
```
Then, under each key, there will a column of values:
```
   (Predicate "o") (BoolValue 1 0)
   (Predicate "i1") (BoolValue 0 1)
   (Predicate "i2") (BoolValue 0 0)
   (Predicate "i3") (FloatValue 3.3 4.4)
   (Predicate "i4") (StringValue "foo" "bar")
```
