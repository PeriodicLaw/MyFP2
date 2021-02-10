### syntax

#### statements

```
stmt   := let name [: term]? = term
        | data name : term where [split name : term]*
        | syntax [symbol | name]+ = term
        | eval term
        | import name[.name]+
name   := [[alphabet]] [[char]]*     // except keywords
symbol := [[non-alphabet]] [[char]]* // except -> => ( ) { } : | =
split  := "|"
```

#### term

```
term   := [func | symbol]+
        | lambda [name | (name : term) | {name} | {name : term}] term
        | case term of [split [name | {name}]+ => term]*
func   := apply -> func
        | ([name]+ : term) -> func
        | {[name]+ [: term]?} -> func
apply  := atomic atomic
        | atomic {term}
atomic := name
        | (term)
```


