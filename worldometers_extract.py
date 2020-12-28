import datetime
import os
import re
import urllib
from bs4 import BeautifulSoup
import pandas as pd
from urllib import request
import time
import sys
import numpy as np

SYNONYMS_DICT = {'Country': 'Country,Other',
                 'Country, Territory ': 'Country,Other',
                 'Country, Other': 'Country,Other',
                 'Cases': 'TotalCases',
                 ' Total Cases': 'TotalCases',
                 'Deaths': 'TotalDeaths',
                 ' Total Deaths': 'TotalDeaths',
                 'Total Recovered': 'TotalRecovered',
                 'Total Cured ': 'TotalRecovered',
                 'Total Critical': 'Serious,Critical',
                 'Total Severe': 'Serious,Critical',
                 'Serious,  Critical ': 'Serious,Critical',
                 "Change Today": "NewCases",
                 'New Today ': "NewCases",
                 '##  Cases ': "NewCases",
                 'New Cases ': "NewCases",
                 'Feb 9,10  Cases ': "NewCases",
                 'Feb 10,11  Cases ': "NewCases",
                 '## ##  Cases ': "NewCases",
                 'Change (cases)  ': "NewCases",
                 '##  Deaths  ': "NewDeaths",
                 'Feb 10,11  Deaths  ': "NewDeaths",
                 'Feb 9,10  Deaths  ': "NewDeaths",
                 'NewDeaths ': "NewDeaths",
                 'Change (deaths) ': "NewDeaths",
                 '## ##  Deaths  ': "NewDeaths",
                 'Tot\xa0Deaths/1M pop': 'Deaths/1M pop',
                 "Today's Deaths  ": "NewDeaths"}

ROWS_TO_DROP = ["Total:", "World", "", "Asia", "North America", "Europe", "South America", "Africa", "Oceania"]

AMOUNT_OF_TRIES = 30

SLEEP_TIME = 10

DATE = 1
DATE_REMOVE = '[A-Z,a-z]{3} [0-9]{1,2} '
NONE_NUM_CHAR = "[^0-9]"
COLUMNS = ['Country,Other', 'TotalCases', 'NewCases', 'TotalDeaths', 'NewDeaths',
           'TotalRecovered', 'ActiveCases', 'Serious,Critical',
           'Tot Cases/1M pop', 'Deaths/1M pop', 'TotalTests',
           'Tests/1M pop']


def get_web(url):
    """
    creates a beautiful soup object from a given url
    """
    web = None
    i = 0
    while web is None or web is False:
        web = get_site_html(url)
        i += 1
        if i > 1:
            print("going to sleep")
            time.sleep(SLEEP_TIME)
        if i > AMOUNT_OF_TRIES:
            return None
    return web


def get_site_html(url):
    try:
        # pretend to be Firefox
        req = urllib.request.Request(url,
                                     headers={'User-Agent': 'Mozilla/5.0 (X11; Linux x86_64) '
                                                            'AppleWebKit/537.36 (KHTML, like Gecko) '
                                                            'Chrome/61.0.3163.79 '
                                                            'Safari/537.36'})
        with urllib.request.urlopen(req) as url_file:
            url_byte = url_file.read()
    except urllib.request.HTTPError as e:  # HTTP status code
        print(e.__str__())
        return False
    except urllib.request.URLError as e:  # General Error
        print(e.__str__())
        return False
    except OSError as e:  # Vague Error
        print(e.__str__())
        return False
    except Exception as e:  # Anything
        print(e.__str__())
        return False
    try:
        url_string = url_byte.decode(encoding='latin1').encode(
            encoding='utf-8')
    except UnicodeDecodeError as e:
        print(e.__str__())
        return False
    except Exception as e:
        print(e.__str__())
        return False
    return BeautifulSoup(url_string, "html.parser")


def get_table(web):
    """
    Parse the html into a dataframe
    """
    table = web.find_all('table')[-1]
    table = table.find_all_next("tr")
    cols = [i.text.replace('\n', "") for i in table[0].find_all('th')]
    df = pd.DataFrame(columns=cols)

    for i in range(1, len(table[1:])):
        row = pd.Series([i.text for i in table[i].find_all('td')], index=cols)
        df = df.append(row, ignore_index=True)
    return df


def filter_and_check_columns(table):
    # the following line changes the columns name to the default name and get rid of unnecessary columns
    table.columns = [re.sub(DATE_REMOVE, '## ', x) for x in table.columns]
    #  debugging print: prints columns which are not in the wanted output DF before being converted
    # print([i for i in table.columns if i not in COLUMNS])
    new_col = [i if i not in SYNONYMS_DICT.keys() else SYNONYMS_DICT[i] for i in table.columns]
    table.columns = new_col
    to_drop = set(table.columns.tolist()) - set(COLUMNS)
    #  debugging print: prints columns which are dropped from the table <good to know if a col's name was changed>
    # print(to_drop)
    table = table.drop(to_drop, axis=1)

    # drop rows with no information or rows with no needed data (continents & sums)
    for col, data in table.iteritems():
        table[col] = data.str.replace("\n", "")
    drop_mask = table[table.columns[0]].isin(ROWS_TO_DROP)
    table = table.loc[np.logical_not(drop_mask)]
    return table


def process_table(table: pd.DataFrame):
    table = filter_and_check_columns(table)
    mini_table = table[table.columns[1:]].replace(NONE_NUM_CHAR, '', regex=True)
    table.loc[:, table.columns[1:]] = mini_table

    # adds missing columns if needed (with null information, just for unification of all tables)
    for i in COLUMNS[-3:]:  # columns with identification of '-1' if there is no information
        if i not in table.columns.tolist():
            table[i] = -1
        else:
            mask = table[i].str.len() == 0
            table.loc[mask, i] = -1

    to_add = [i for i in COLUMNS if i not in table.columns.tolist()]
    for i in to_add:  # columns with identification of NaN if there is no information
        if i not in table.columns:
            table[i] = np.nan
    table = table[COLUMNS]
    return table


def check_difference(cur_date, prev_date):
    prev_df = pd.read_csv("data/worldometer/%s.csv" % prev_date)
    cur_df = pd.read_csv("data/worldometer/%s.csv" % cur_date)
    country_mask = cur_df['Country,Other'].isin(prev_df['Country,Other'])
    fil_df = cur_df.iloc[country_mask.tolist()]
    fil_cur = fil_df.sort_values('Country,Other', axis=0).reset_index(drop=True).fillna(0)
    fil_prev = prev_df.sort_values('Country,Other', axis=0).reset_index(drop=True).fillna(0)

    columns = ['TotalDeaths', 'TotalCases', 'TotalRecovered']
    verify = fil_cur[columns] - fil_prev[columns]
    mask = verify >= 0
    if mask.all(axis=None):
        return True
    index = np.argwhere(np.logical_not(mask.to_numpy())).tolist()
    for row, col in index:
        country = fil_cur['Country,Other'][row]
        colum = columns[col]
        diff = abs(int(verify.iloc[row, col]))
        print("%s difference in %s of %s " % (diff, colum, country))


if __name__ == '__main__':
    prev = None
    for d in pd.date_range(start="2020-01-29", end=datetime.datetime.today()):
        i = d.strftime("%m%d")
        year = d.strftime("%Y")
        fname = 'data/worldometer/' + year + i + ".csv"
        if not os.path.isfile(fname):
            print(year+i)
            url = "https://web.archive.org/web/%s%s/https://www.worldometers.info/coronavirus/" % (year, i)
            # on this specific date, there is a problem with the default address
            if i == "0318":
                url = "https://web.archive.org/web/20200318234401/https://www.worldometers.info/coronavirus/"
            web = get_web(url)
            df = get_table(web)
            df = process_table(df)
            df['Date'] = year + i
            df.to_csv(fname)
            check_difference(year+i, prev)
        prev = year+i
