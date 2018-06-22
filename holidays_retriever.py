from workalendar.europe import UnitedKingdom
from workalendar.asia import HongKong

cal=UnitedKingdom()
f = open('holidays.csv','w+')
for year in range(2016,2019):
    result = dict(cal.holidays(year))
    for date_str in result:
        print(date_str)
        f.write('UK,')
        f.write(str(date_str)+'\n')

cal=HongKong()
for year in range(2016,2019):
    result = dict(cal.holidays(year))
    for date_str in result:
        print(date_str)
        f.write('HK,')
        f.write(str(date_str)+'\n')


f.close()

##HongKong()
