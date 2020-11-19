# KAIST CS564(Big Data Analytics Using R) Term Project Team 8

## Example Crawled data files
### File name: TYPE_yymmdd_HHMMSS.json
* TYPE
  * keys: Target instagram posts that includes the hashtag '#제주'
  * output: Fetched result of target instagram posts
  * hashtags: Fetched result that only has at least one hashtag in the post content
  * locations: Fetched result that only has the location tag
* yymmdd & HHMMSS: Crawled date and time

### JSON file format
```
[
  {
    "key": address of the instagram post,
    "datetime": uploaded datetime,
    "author": author,
    "caption": instagram post content,
    "hashtags": [
      hashtags
    ],
    "location": location tag
  }
]
```
