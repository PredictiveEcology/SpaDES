# NetLogoR

<details>

* Version: 0.3.11
* GitHub: https://github.com/PredictiveEcology/NetLogoR
* Source code: https://github.com/cran/NetLogoR
* Date/Publication: 2022-08-17 07:30:02 UTC
* Number of recursive dependencies: 112

Run `revdep_details(, "NetLogoR")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
      Running ‘test-all.R’
     ERROR
    Running the tests in ‘tests/test-all.R’ failed.
    Last 13 lines of output:
          equals, is_less_than, not
      
      > test_check("NetLogoR")
      Loading required package: NetLogoR
      Loading required package: raster
      Loading required package: sp
      [ FAIL 19 | WARN 14 | SKIP 1 | PASS 915 ]
      
      ══ Skipped tests ═══════════════════════════════════════════════════════════════
      • On CRAN (1)
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      Error in utils::packageVersion("cli") : there is no package called 'cli'
      Calls: test_check ... style_hyperlink -> .rlang_cli_has_cli -> <Anonymous>
      Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘fastshp’
    ```

# SpaDES.addins

<details>

* Version: 0.1.3
* GitHub: https://github.com/PredictiveEcology/SpaDES.addins
* Source code: https://github.com/cran/SpaDES.addins
* Date/Publication: 2021-06-11 08:40:12 UTC
* Number of recursive dependencies: 139

Run `revdep_details(, "SpaDES.addins")` for more info

</details>

## Newly broken

*   checking for code/documentation mismatches ... WARNING
    ```
    Error: package or namespace load failed for ‘SpaDES.addins’ in find.package(package, lib.loc, verbose = verbose):
     there is no package called ‘raster’
    Call sequence:
    6: stop(msg, call. = FALSE, domain = NA)
    5: value[[3L]](cond)
    4: tryCatchOne(expr, names, parentenv, handlers[[1L]])
    3: tryCatchList(expr, classes, parentenv, handlers)
    2: tryCatch({
           attr(package, "LibPath") <- which.lib.loc
           ns <- loadNamespace(package, lib.loc)
    ...
               paste(" in", deparse(cc)[1L])
           else ""
           msg <- gettextf("package or namespace load failed for %s%s:\n %s", 
               sQuote(package), P, conditionMessage(e))
           if (logical.return && !quietly) 
               message(paste("Error:", msg), domain = NA)
           else stop(msg, call. = FALSE, domain = NA)
       })
    1: library(package, lib.loc = lib.loc, character.only = TRUE, v
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘devtools’ ‘rstudioapi’
      All declared Imports should be used.
    ```

