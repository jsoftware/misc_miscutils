cocurrent 'z'
require 'misc/miscutils/langexten'

NB. Routines for keyed lists (lists of key ; data [, data])

NB. y is list of key ; data
NB. If x is given, it is the list of key indices
NB. Result is the (boxed) keys
keyskld =: ({"1"_)
keyskl =: 0&keyskld : keyskld f.

NB. y is list of key ; data
NB. If x is given, it is the list of data columns
NB. Result is the (boxed) data items only, using the shape of x for each row
datakld =: ({"1)
datakl =: 1&datakld : datakld f.

NB. x is set of keys, y is keyed list, u is key columns
NB. Records with those keys are deleted
delkl_colsu =: 1 : '-.@:((e.!.0)~ xuvy (m&keyskld)) # ]'
delkl =: 0 delkl_colsu f.

NB. x is set of keys, y is keyed list, u is column numbers of key
NB. Records with those keys are kept, the others are deleted
keepkl_colsu =: 1 : '(e.!.0)~ xuvy (m&keyskld)   #  ]'
keepkl =: 0 keepkl_colsu f. NB. default version with key in position 0

NB. x is set of keys, y is keyed list, u is column numbers of key
NB. Result is index of x into }: keys, but _1 if there is no match
indexkl_colsu =: 1 : '(#@])  (((_1"_)`(I.@:=)`])})  (m&keyskld uy_vx (i.!.0))'
indexkl =: 0 indexkl_colsu f. NB. default version with key in position 0

NB. x is set of keys, y is keyed list, u is column numbers of key
NB. Result is 1 if x is in the list
inkl_colsu =: 1 : '(e.!.0) xuvy (m&keyskld)'
inkl =: 0 inkl_colsu f.

NB. x is a (list of) boxed key value
NB. y is an n,m $ array of key ; data
NB. u is default value (verb or noun)
NB. v is (key columns;data columns)
NB. Result is BOXED requested columns, default if not found
getkl_defu_colsv =: 2 : '(<@{. n)&keyskld uy_vx (i.!.0) ((<@{: n)&{@{ :: (u"_)"0 _) ]'
getklu_defu_colsv =: 2 : '[: > u getkl_defu_colsv n'
NB. m is key column(s), n is data column(s).  x is key(s), y is kl.  x must be found
getkl =: 2 : '<@(;&n)@(m&{"1 uy_vx i.) { ]'
getklu =: 2 : '[: > u getkl v'
NB. Default value is {:y, key is column 0, return column 1 (items have rank 0)
getkl1 =: (<_1;1)&{@]  getkl_defu_colsv (0;1) f.
NB. Default value is u, key is column, return column 1
getkl1d =: getkl_defu_colsv (0;1)
getklu1d =: 1 : '[: > u getkl1d'
NB. Default value is u, key is column, return other columns
getkld =: getkl_defu_colsv (0;<<0)
getklud =: 1 : '[: > u getkld'

NB. x is a list of boxed key value (may be scalar if key is rank 1)
NB. y is an n,m $ array of key ; data
NB. u is key columns, v is columns to return
NB. Result is the list of all boxed lists matching the key
allgetkl_colsuv =: 2 : '(n&datakld)@(((e.!.0)~ xuvy (m&keyskld)) # ])'
allgetkl =: 0 allgetkl_colsuv (<<<0) f.

NB. y is a list of key ; data, x is list of key columns (default 0)
NB. Result is the input, with duplicate keys removed (first one of multiples survives)
nubkl =: (#~ ~:@:(0&keyskld)) : (] #~ ~:@:keyskld) f.

NB. y is list of key ; data, x is list of columns to return (default 1)
NB. result is raze of one column of the data with the keys removed.  Default is first column
razekl =: (1&$:) : (; @: ({"1)) f.

NB. Conjunction: apply u to column number n of y, but only in records with keys in x
applykl =: 2 : '(((u&.>) @: (n&{)@])`(n"_)`] }) ^: ({. uy_vx e.) "_ 1'

NB. Conjunction: u is a predicate, v is a (possibly list of) column numbers
NB. u is applied to the list of (boxed) selected elements of y and the entire x (if any)
NB. Result is only those items which produce a nonzero predicate
cullkl =: 2 : '(u bivalent (n&{"1)   #   ])   ifany'