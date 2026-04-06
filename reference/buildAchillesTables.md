# Create achilles tables

Create achilles tables

## Usage

``` r
buildAchillesTables(cdm, achillesId = NULL)
```

## Arguments

- cdm:

  A cdm_reference object

- achillesId:

  A vector of achilles ids. You can also use "minimal", "default" or
  "all" to point to a subset of analyses. Or also "person", "observation
  period", ... to run the analyses refering to a certain category.

## Value

The cdm_reference object with the achilles tables populated.
