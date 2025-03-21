# Sidebot (R Edition)

This is a demonstration of using an LLM to enhance a data dashboard written in Shiny [r-chatbot-clinical](https://y7xfuy-chaoyang-ye.shinyapps.io/r-chatbot-clinical/).

To run locally, you'll need to create an `.Renviron` file in the repo root with `GEMINI_API_KEY=` followed by a valid GEMINI API key. Or if that environment value is set some other way, you can skip the `.Renviron` file.

Then run:

```r
pak::pak(c("bslib", "DBI", "dplyr", "duckdb", "fastmap", "fontawesome",
  "ggplot2", "ggridges", "here", "plotly", "reactable", "shiny",
  "ellmer", "shinychat"))
```

(Note that [{ellmer}](https://github.com/hadley/ellmer) and [{shinychat}](https://github.com/jcheng5/shinychat) are highly experimental and their APIs may change.)

## Warnings and limitations

This app sends at least your data schema to a remote LLM. As written, it also permits the LLM to run SQL queries against your data and get the results back. Please keep these facts in mind when dealing with sensitive data.
