NCAA Basketball Stats Scraper
Updates by Meyappan 

Some formats have changed since Rodrigo recently made this python code. The hyperlink formats have changed so I edited them for that as well as emphasizing the parser to use in Beautiful Soup. 

I also added R code to clean up the data. Some of the R code was taken from Rodrigo's old R code. I have updated it for some other new stats like AST/TO ratio, and PER. Unfortunately, to calculate PER you need to account for pace through the accounting of possesions. I was not able to find possession stats on the NCAA website. I also aimed to make the code a bit more efficient, used sapply and apply in general instead of for loops. I hope this helps. 

Version: 1.2 

=============================
Author: Rodrigo Zamith  
Version: 1.1


Usage
-----
First, edit the scraper settings in `scrapersettings.py`. In particular, be sure to change the two variables at the top, `academic_year` and `year_index`, using the information provided in that file. You can also set what kind of data you'd like saved, and where you'd like it saved.

Then, execute either `ncaab_stats_scraper.sh` or `ncaab_stats_scraper.bat`, depending on your operating system. Alternatively, you can just execute the python files, preferably in this order:

1. create_team_mappings.py
2. create_schedule_mappings.py
3. create_player_mappings_and_agg_stats.py
4. create_ind_stats.py


Requirements
------------
This script requires Python, as well as the urllib2 and BeautifulSoup libraries.


License
--------
This script is licensed under the Mozilla Public License Version 2.0 (see LICENSE file in root folder). TL;DR: feel free to use it commercially, modify it, and distribute it, provided you disclose both the source code and any modifications you make to it.
