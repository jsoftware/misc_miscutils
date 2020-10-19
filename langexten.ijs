cocurrent 'z'
NB. language extensions

NB. Type y on the terminal
display =: (i.0 0)"_ ((1!:2) 2:)


NB. Conjunction: u unless y is empty; then v
butifnull =: 2 : 'v"_`u@.(*@#@])'
NB. Alternative form without gerund.  This turns out to be slower
NB. butifnull =: [. ^: ((] *@#) ` ((]."_) ^: ((] 0&=@#)`])))

NB. Conjunction: u unless x is empty; then v
butifxnull =: 2 : 'v"_`u@.(*@#@[)'

NB. Empties: select one according to context
NIL =: ''   NB. required argument to niladic function
NILRET =: ''   NB. return from function with no explicit result
NILRETQUIET =: i. 0 0  NB. return to J, without printing
NB. verb to return empty list, to discard the actual returned value
null =: ''"_

NB. Adverb.  Do u, but skip it if y is null
ifany =: ^: (*@#@])
NB. Same if x is nonnull
ifanyx =: ^: (*@#@[)

NB. bivalent =: [. ^: (1:`(].@]))  NB. u v y if monad, x u (v y) if dyad
NB. u v y if monad, x u (v y) if dyad
bivalent =: 2 : 'u^:(1:`(]v))'

NB. Logical connectives - evaluates right to left and avoids
NB.  unnecessary evaluations
and =: 2 : 'u@[^:] v'
or =: 2 : 'u@[^:(-.@]) v'

NB. Like e. dyadic, but using tolerant comparison.  Faster on lists with identical first elements
in0 =: e.!.0

NB. Like i. dyadic, but using tolerant comparison.  Faster on lists with identical first elements
index0 =: i.!.0

NB. Adverb.  Apply u and join the results end to end
endtoend =: 1 : ';@:(<@u)'
NB. Adverb.  Apply u on keys, join results end-to-end
keyetoe =: 1 : ';@:(<@u/.)'
NB. Conjunction, like Cut but put results end-to-end
cutetoe =: 2 : ';@:(<@u;.n)'

NB. Conjunctions to use for readability
xuvy =: 2 : 'u v'
yuvx =: 2 : '(u v)~'
uy_vx =: 2 : 'v~ u'
ux_vy =: 2 : '(v~ u)~'
vy_ux =: 2 : 'u~ v'
vx_uy =: 2 : '(u~ v)~'

NB. m is name of noun; n is default value; result is the value of
NB. the noun if it is defined, otherwise n
butifundef =: 2 : 'if. 0 > 4!:0 <m do. n else. ". m end.'

NB. y is anything; result is a ist of 0 y s
noneof =: ($0)&(]"0 _)

NB. Adverb.  Monad, converts y to rank u by adding axes
enrank =: 1 : ',: ^: ((0&>.)@(m&-)@#@$)'

NB. x is column number; we open column x of y
ocol =: >@:({"1)

NB. like {., but limited to the length of y
leading =: ((<. #) {. ])"0 _
NB. like - ux_vy {., but limited to the length of y
trailing =: (-@(<. #) {. ])"0 _

NB. Adverb.  Install x in the path given by m
store =: 1 : 0
:
if. #m do. (< x (}.m)store ({.m){::y) ({.m)} y else. x end.
)

NB. Adverb.  Applies u within boxes of y, without changing boxing of x
iny =: 1 : '<@(u xuvy >)"_ 0'

NB. Atomic-representation utilities
NB. y is string name of verb, result is AR of verb
arverb =: <@,
NB. y is noun, result is AR of noun
arnoun =: <@((,'0')&(,&<))
NB. (adverb) m is conjunction (in string form), x is AR, y is AR
NB. Result is AR of x m y
arconj =: 1 : '<@((,&<~ ,)&m)@,"0'
NB. y is a list of 3 ARs, result is AR of their fork
arfork =: <@((,'3')&(,&:<))
NB. x is AR, y is AR, result is AR for (x y)
arhook =: <@((,'2')&(,&:<))@,"0

NB. Adverb.  m describes an item to be fetched, as
NB. spec[!spec]...[!]
NB.  where spec is
NB.  [prefix?]expr
NB.   where prefix is a character string which (boxed) becomes
NB.     the prefix, or a number, which tells how many trailing
NB.     items of the incoming prefix to keep.  Default is _, meaning
NB.     keep the whole prefix
NB.   and expr is an expression which is evaluated to give a selector.
NB.     This selector (after being boxed) is applied to the input at that
NB.     level.  All '^' and '^:' appearing in the input are replaced by the current
NB.     path, razed with '_' after each item.  If '^:' appears in the
NB.     input, the current path is cleared after all the replacements.
NB.     After that, the path is extended by adding the token following the
NB.     first '^' or '^:', if there is one.
NB.  The specs are processed left-to-right.  When '!' is encountered,
NB.  the result of the selection is unboxed (and subsequent selections are
NB.  applied to each atom after it is unboxed)
NB. Ex: 'IUNIT?^:ORDER!^:HISTORY!^STATUS!^STS' fstr

NB. x is one unboxed spec string, y is incoming path (a list of boxed strings), result is path to use here
inputpath =. ( ({.~ i.&'?') ux_vy ((<@[) ` (0&". ux_vy trailing) @. (*./@(e.&'0123456789')@[)) ) ^:('?'&e.@[)
NB. x is one unboxed spec string, y is path to use here, result is path to use at next level
outputpath =. ( ;: ux_vy ( -.@((<'^:')&e.) ux_vy #  ,  ({~ >:@(i.&1)@(e.&((,'^');'^:')))@[ ) ) ^: ('^'&e.@[)
NB. x is one boxed spec string, y is (accumulated path[;garbage])
NB. Result is (path for next level;path to use at this level)
accumpath =. ([ (outputpath ,&< ]) inputpath)&>  {.
NB. y is spec string (boxed in pieces), result is prefix string to use at each level
buildpaths =. ;@:(,&'_'&.>)&.>@:({:"1)@:((accumpath/\.)&.(,&(<0$a:)))&.|.
NB. x is boxed pieces of the spec string, y is list of boxed paths
NB. result is x with substitutions made for ^ and ^:
substpath =. (<;._2@(,&'^')@(#~ -.@(|.!.0)@('^:'&E.))@((}.~ >:@(i.&'?'))^:('?'&e.))@> ux_vy (<@;@}:@,@,.))"0
NB. y is the spec string, result is selection info, with substitutions
NB. performed (and '!' indicating unboxing)
brktospecs =. (substpath buildpaths) @ (<;.1~ (+. |.!.1)@('!'&=))
NB. y is the spec string.  Result is
NB. a list of ARs, one for each spec and one for each !
arofspecs =. ('&' arconj&(arverb '{')@arnoun@(".&.>))`((arverb '>')"_) @. ((<,'!')&-:) "0 @: brktospecs
NB. Convert each component to an AR, and then roll up the ARs.  We go from
NB. the right-hand end, but we add each element to the left of the accumulated
NB. AR, so this has the effect of reversing the order and left-grouping the
NB. result
fstrar =: ('@' arconj)~/ @: arofspecs f.
fstr =: 1 : '(fstrar m)`:6'


NB. Conjunction.  Apply u at the cell indicated by n
applyintree =: 2 : 0
if. #n do. ((u applyintree (}.n)) L:_1 ({.n){y) ({.n)} y else. u y end.
:
NB. The rank is s,0 where s is the surplus of x-rank over y-rank.  This causes
NB. the cells of y to be matched up with the largest appropriate blocks x  This
NB. is necessary because it is impossible to change the shape of the values being modified
if. #n do. (x u applyintree (}.n) L:_ _1"(0 (>.,[) x -&(#@$) a) (a =. ({.n){y)) ({.n)} y else. x u y end.
)

NB. y is character string list of entry-point names
NB. x is the level number at which we should publish the entry points (default _1, 'z')
NB. we publish these names in the locale at position x in the path
publishentrypoints =: 3 : 0
_1 publishentrypoints y
:
NB. The rhs of the assigment below interprets the names as gerunds
path =. '_' (,,[) x {:: (<,'z') ,~^:(-.@*@#@]) 18!:2 current =. 18!:5 ''
l =. ,&path^:('_'&~:@{:)&.> ;: y
r =. ,&('_' (,,[) > current)@(({.~ i:&'_')@}:^:('_'&=@{:))&.> ;: y
NB. The gerund assignment requires more than one name, so duplicate the last:
NB.?lintmsgsoff
('`' , ;:^:_1 (, {:) l) =: (, {:) r
NB.?lintmsgson
)

NB. 18!:4 without side effects
setlocale =: 18!:4

NB. Cuts
onpiecesbetweenm =: 2 : '(u ;._1)@:(n&,)'
onpiecesbetweend =: 2 : '(u>)"_1 <;._1@(n&,)'
onpiecesbetween =: 2 : '(u onpiecesbetweenm n) : (u onpiecesbetweend n)'
onpiecesusingtail =: 1 : 'u ;._2'

NB. Conjunction.  u is verb, n (or [x] v y) is arg to { to select s = desired portion of y
NB. The result of x u s (if dyad) or u s (if monad) replaces s
onitem =: 2 : '(u bivalent (n&{)) n} ]'
onitemm =: 2 : 'n}~ u@:(n&{)'
onitemd =: 2 : '(u (n&{)) n} ]'

NB. Debugging support
NB. conjunction: execute u after displaying n
afterdisplaying =: 2 : 'u [ display@(n"_)'

NB. Initialize a global, but not if it's already been initialized
NB. Example: 'name' initifundef 5
initifundef =: (, ('_'&([,],[))@(>@(18!:5)@(0&$)) ) ux_vy ((4 : '(x) =: y')^:(0:>(4!:0)@<@[))

NB. Timing
ts =: 6!:2 , 7!:2@]

NB. conjunction: execute u, counting time.  n is a descriptive string
showtime =: 2 : 0
display 'Starting ' , n
starttime =. 6!:0 NIL
u y
if. 0: display 'Exiting ' , n , ' time=' , ": 0 12 30 24 60 60 #. (6!:0 NIL) - starttime do. end.
:
display 'Starting ' , n
starttime =. 6!:0 NIL
x u y
if. 0: display 'Exiting  ' , n , ' time=' , ": 0 12 30 24 60 60 #. (6!:0 NIL) - starttime do. end.
)

NB. List the combinations of x things taken from y things
comb =: [: ; [: (,.&.> <@;\.)/  >:@-~ [\ i.@]

NB. associative power: like ^: but uses repeated doubling
NB. u is applied between v copies of y
NB. requires y > 0
apow =: 2 : 0
(v"_ y) u/@(u~@]^:(I.@|.@#:@[))"0 _ y
)

NB. Conjunction: we use this for things that may need to be 'rank' if J
NB. starts reexecuting frequently, but are " till then.  The nature of these things
NB. must be that they perform I/O, so we inhibit them if they are null
rnk =: 2 : 'u"v ifany'

NB. Conjunction: x if y is nonzero, otherwise nullverb
butonlyif =: 2 : 0
if. n do. u else. ($0)"_ end.
:
if. n do. u else. ($0)"_ end.
)

FormalLevel =: 2 : 0
 m=. 0{ 3&$&.|. n
 ly=. L. y  if. 0>m do. m=.0>.m+ly end.
 if. m>:ly do. u y else. u FormalLevel m&.> y end.
   :
 'l r'=. 1 2{ 3&$&.|. n
 lx=. L. x  if. 0>l do. l=.0>.l+lx end.
 ly=. L. y  if. 0>r do. r=.0>.r+ly end.
 b=. (l,r)>:lx,ly
 if.     b-: 0 0 do. x    u FormalLevel(l,r)&.> y
 elseif. b-: 0 1 do. x    u FormalLevel(l,r)&.><y
 elseif. b-: 1 0 do. (<x) u FormalLevel(l,r)&.> y
 elseif. 1       do. x u y
 end.
)

FormalFetch =: >@({&>/)@(<"0@|.@[ , <@]) " 1 _
