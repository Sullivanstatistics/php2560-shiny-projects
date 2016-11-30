## Shiny Apps Created by Students in PHP 2560


This is a collection of Shiny Apps created by students in PHP 2560. You can see all of these apps at:
`https://www.sullivanstatistics.com/shiny/php2560`

To run the examples locally, you can install the **shiny** package in R, and
use the function `runGithub()`. For example, to run the example `Emulator`:

```R
if (!require('shiny')) install.packages("shiny")
shiny::runGitHub("php2560-shiny-projects", "sullivanstatistics", subdir = "Emulator")
```

Or you can clone or download this repository, and use run
`shiny::runApp("Emulator")`.
