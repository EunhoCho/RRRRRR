# KAIST CS564(Big Data Analytics Using R) Term Project Team 8

## Folders
* data: Instagram crawled data
* dataframes: Dataframes that are converted from instagram crawled data
* figures: Result figures
* instagram-crawler: Instagram Crawler
* loc2coord: Codes for matching location to coordinates & address
* map_data: Data of Jeju given by government
* processed-data: Processed data through clustering, etc. 
* Presentation: presentation file (.pdf) of the team proeject

## Example Crawled data files in `/data`
### File name: TYPE_yymmdd_HHMMSS.json
* TYPE
  * keys: Target instagram posts that includes the hashtag 'а╕аж'
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
