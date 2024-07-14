
# `read_edm_parser`

read_edm_parser


## Description

Allow to read data from edm parsed dataset, see examples


## Usage

```r
read_edm_parser(inpt, to_find_v = c())
```


## Arguments

Argument      |Description
------------- |----------------
`inpt`     |     is the input dataset
`to_find_v`     |     is the vector containing the path to find the data, see examples


## Examples

```r
print(read_edm_parser("(ok(ee:56))(ok(oui(rr((rr2:6)(rr:5))))(oui(bb(rr2:1)))(ee1:4))",
to_find_v = c("ok", "oui", "rr", "rr2")))

[1] "6"

print(read_edm_parser("(ok(ee:56))(ok(oui(rr((rr2:6)(rr:5))))(oui(bb(rr2:1)))(ee1:4))", to_find_v = c("ok", "ee")))

[1] "56"

print(read_edm_parser("(ok(ee:56))(ok(oui(rr((rr2:6)(rr:5))))(oui(bb(rr2:1)))(ee1:4))", to_find_v = c("ee")))

[1] "56"
```


