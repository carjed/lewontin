# A scientometric analysis of Lewontin, 1972

This repository contains code and underlying data for the analyses performed in [**citation pending**]()

- `bibliometrics.R` is a script used to retrieve literature citation data and perform our bibliometric analyses. Data are primarily sourced from the free-to-use [Semantic Scholar API](https://api.semanticscholar.org), but for certain analyses we also rely on [OpenCitations](https://opencitations.net) and the [Scopus journal taxonomy](https://www.scopus.com/home.uri), in addition to some edge cases requiring manual inspection of data on [Google Scholar](https://scholar.google.com).

- `scrape_tweets.R` is a script used to periodically retrieve the most recent "dark citation" tweets mentioning "Lewontin". Because the Twitter API only retains tweet data for the last ~10-14 days, this was automated via a cron job over a 9-month data collection period. Due to Twitter's limitations on data sharing, we are unable to share the full dataset as collected, and are restricted to sharing only the status IDs, which must then be "rehydrated" by each end user with their own Twitter API key. Consequently, any tweets in our original dataset that have since been deleted (or where the user account has been deleted or suspended) will not be included when this pipeline is re-run by other users.

- `load_tweets.R` is a script used to perform our altmetric analyses on the Twitter data we collected.