# Corrects speeds to Fristch-Carlson constraints, recursively.

Internal function. Not intended for external use.

## Usage

``` r
correct_speeds_fun(m_0, deltas)
```

## Arguments

- m_0:

  A numeric vector of initial slopes (observed velocities)

- deltas:

  A numeric vector of initial FC delta values

## Value

A numeric vector of m_0 adjusted to FC constraints
