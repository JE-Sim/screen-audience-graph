import urllib.request
import json
import datetime
import os
import sys
import io
sys.stdout = io.TextIOWrapper(sys.stdout.detach(), encoding = 'utf-8')
sys.stderr = io.TextIOWrapper(sys.stderr.detach(), encoding = 'utf-8')


def set_date():
    if 'lastUpdate.txt' in os.listdir("C:/Users/정은/Desktop/API"):
        with open("C:/Users/정은/Desktop/API/lastUpdate.txt", 'r') as d:
            lastUpdate = d.read()
        d = datetime.date(int(lastUpdate[0:4]), int(lastUpdate[5:7]), int(lastUpdate[8:10]))
        start_key = datetime.date.toordinal(d)
        lastWeek = int(lastUpdate[11:])
    else:
        start = datetime.date(2017,1,1)
        start_key = datetime.date.toordinal(start)
        lastWeek = 0
    today = datetime.date.today()
    end_key = today.toordinal()
    return start_key, end_key, lastWeek

#date_key should be datetime.date.toordinal type
def web_scrapping(date_key, data_type):
    start_key, end_key, lastWeek = date_key
    with open("C:/Users/정은/Desktop/방학프로젝트/API/영화진흥위원회API_key.txt", 'r') as k:
        key = k.read()
    # key = "7e0e2b120d9895aea092b9baa6d5074f"
    if data_type == "daily":
        url_key = "searchDailyBoxOfficeList.json?key=" + key
        seq = 1
    elif data_type == "weekly":
        url_key = "searchWeeklyBoxOfficeList.json?key=" + key + "&weekGb=0"
        seq = 7

    total = {}
    for i in range(start_key, end_key, seq):
        day_week = str(datetime.date.fromordinal(i)).replace("-", "")
        url = "http://www.kobis.or.kr/kobisopenapi/webservice/rest/boxoffice/" + url_key + "&targetDt=" + day_week
        req = urllib.request.Request(url)
        data = urllib.request.urlopen(req).read().decode("UTF-8")
        dict_data= json.loads(data)['boxOfficeResult']

        year = datetime.date.fromordinal(i).year
        if i == start_key:
            total[year] = []
        elif year != last_year:
            total[year] = []
        total[year].append(dict_data)
        last_year = year
    return total


def check_recent_data(date_key, total, data_type):
    start_key, end_key, lastWeek = date_key
    year = datetime.date.fromordinal(end_key).year
    if data_type == "daily":
        print(str(datetime.date.fromordinal(start_key)) + " / " + str(datetime.date.fromordinal(end_key)))
        if start_key != end_key:
            print("Not recent daily data!")
            return(True)
        else :
            print("Recent daily data!")
            return(False)
    elif data_type == "weekly":
        if len(total) < 1:
            currentWeek = lastWeek
        elif len(total[year]) < 2:
            currentWeek = lastWeek
        elif lastWeek == 0:
            currentWeek = total[year][-2]['yearWeekTime']
        elif len(total[year][-1]) < 3:
            currentWeek = total[year][-2]['yearWeekTime']
        else:
            currentWeek = total[year][-1]['yearWeekTime']
        print(str(lastWeek) + " / " + str(currentWeek))
        i = open("C:/Users/정은/Desktop/API/lastUpdate.txt", "w")
        lastUpdate = str(datetime.date.fromordinal(end_key) )+ " " + str(currentWeek)
        i.write(lastUpdate)
        i.close()
        if currentWeek != lastWeek:
            print("Not recent weekly data!")
            return(True)
        else:
            print("Recent weekly data")
            return(False)


def convert_csv(total, data_type):
    import csv
    years = list(total.keys())
    listType = str(data_type + "BoxOfficeList")
    rough_data = total.get(years[0])

    if data_type == "daily":
        key_data = rough_data[0][listType][0].keys()
        colnames = list(key_data)
        colnames.insert(0, 'date')
    elif data_type == "weekly":
        key_data = rough_data[0][listType][0].keys()
        colnames = list(key_data)
        colnames.insert(0, 'week')
        colnames.insert(1, 'showRange')

    for y in years:
        mode = 'a' if y == datetime.date.today().year else 'w'
        address = "C:/Users/정은/Desktop/API/csv/" + str(y) + "_" + data_type + "BoxOffice.csv"
        h = open(address, mode, encoding='EUC-KR', newline='')
        wr = csv.writer(h)
        wr.writerow(colnames)
        for i in range(0, len(total[y])):
            for j in range(0, len(total[y][i][listType])):
                rows = list(total[y][i][listType][j].values())
                if data_type == "daily":
                    date = total[y][i]['showRange'][0:8]
                    rows.insert(0, date)
                elif data_type == "weekly":
                    week = total[y][i]['yearWeekTime']
                    showRange = total[y][i]['showRange']
                    rows.insert(0, week)
                    rows.insert(1, showRange)
                wr.writerow(rows)
        h.close()


def api_update():
    date_key = set_date()
    Type = ('daily', 'weekly')
    for data_type in Type:
        total = web_scrapping(date_key, data_type)
        if check_recent_data(date_key, total, data_type):
            print(data_type + " data Update Complete")
            convert_csv(total, data_type)
