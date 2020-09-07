# myFacebook
Simple helper functions for reading your Facebook data download into R

This is a package which I developed from the code I used in [my blog post series "What does Facebook actually know about me?"](https://medium.com/@chris.brownlie).

It is a series of get_* functions which allow you to read in the data from Facebooks data download quickly and easily.

To download your data from Facebook (on desktop website) go to Settings -> "Your Facebook Information" -> "Download your information". Download all your information in JSON format. When ready to download, download and extract the root folder and store it in your R project directory.

## Disclaimer
This package was originally only for personal use in developing analysis for my blog. As such I have not even remotely tested the package other than checking it worked for me. I also have not written functions for every piece of data included in the Facebook data download. You should also note that some functions discard data which I personally was not interested in.
If there is interest in this package I will develop and refine it further but as of now there is no plans to do so.

## Installation

myFacebook is not available on CRAN and I do not plan on submitting it. Install using 
```
install.packages("devtools")
devtools::install_github("chrisbrownlie/myFacebook")
```

## Usage

There are many different functions which simply read in a specific part of your facebook data e.g.
```
get_posts()

get_comments()

get_likes_and_reactions()

get_ad_interests()

get_information_used_for_recommendations()
```

All functions take at least one argument 'folder', which is simply the name of the main folder extracted from your Facebook data download. For me this was simply called "data" and as such this is the default value for the argument.
