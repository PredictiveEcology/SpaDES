# Code style guide:

This is adapted from Hadley Wickham's [Advanced R Style Guide](http://adv-r.had.co.nz/Style.html), but does not always follow it.

## Notation and naming

### File names

- Use descriptive lowercase filenames.
- Use dashes (instead of underscores or periods).
- Use `.R` as the file extension.
- If files need to be run in a specific sequence, use a number prefix

        # good
        plotting-functions.R
        0-prepare-workspace.R
        1-load-data.R
        2-run-analysis.R
        
        # bad
        PLOT_functions.r
        a.r

### Object names

- Variable and object names should be concise and descriptive *nouns*.
- Functions and methods should be named using concise and descriptive *verbs*.
- Avoid using the names of existing variables and functions.
- Use lowerCamelCase.
- Do not use periods nor underscores.
- Use `TRUE` and `FALSE` instead of `T` and `F`.

        # good
        beetleSurvival
        findArea()
        
        # bad
        mean.vals
        to_extract_mean_raster_value_and_prob_cross_pathway_roads()

## Syntax

### Spacing

- Use spaces around operators (*e.g.*, `+`, `-`, `=`, `<-`). One exception: do not use spaces around `=` when assigning an argument to a function.
- Don't use spaces around `:`, `::`, and `:::`. (Should this also apply to `:=` in `data.table`?)
- Spacing can be used to improve alignment/readability.

### Curly braces

- `{` should never be on its own line (it should end the line; never begin it).
- `}` should always be on its own line unless followed by `else`.
- Use curly braces for `if else` statements.
- Code inside curly braces should be indented.

### Line length and indentation

- Try to limit lines to 80 characters.
- Indent using 2 spaces. Only use spaces (no tabs).
- Spacing can be used to improve alignment/readability.

### Assignment

- Use `<-` instead of `=`.

## Organization

### Comments and documentation

- Use comments to explain *why* (and if the code is complicated or uses shorthand, *what*) the code is doing.
- Use a single `# ` (with trailing space) to denote commented lines.
- Document every class, function, etc. you write.
- Use `#' ` to denote documentation blocks (see `roxygen2` documentation: `vignette("rd", package = "roxygen2")`).
