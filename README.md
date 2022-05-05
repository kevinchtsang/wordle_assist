# Wordle Assistant
Tool to help your Wordle games. Type in your word attempts and change the colours to match (grey - no match, yellow - in word, green - correct position) by clicking on the tiles.

You can use this app by running the following in R.

```
shiny::runGitHub( "wordle_assist", "kevinchtsang")
```

![Example](/wordle_assist_example1.png?raw=true)

## Word of Caution
The tool has been build with the Scrabble dictionary available via the [`words`](https://cran.r-project.org/web/packages/words/index.html) package, which may not match exactly to the Wordle dictionary you are playing with.