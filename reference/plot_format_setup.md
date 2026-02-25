# Function to set up plot formats.

Intended for internal use only.

## Usage

``` r
plot_format_setup(plotting_df, attribute_input, attribute_type, attribute_name)
```

## Arguments

- plotting_df:

  DF for plotting, either trips or features

- attribute_input:

  The user input value for the attribute (e.g., outline_input =
  veh_outline)

- attribute_type:

  The type of attribute being constructed (e.g., "outline")

- attribute_name:

  The name of the attribute (e.g., "veh_outline")

## Value

List with: 1) new plotting_df, 2) show_legend, 3) attribute_by, and 4)
attribute_vals
