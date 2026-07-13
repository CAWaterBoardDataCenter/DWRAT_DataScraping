

from datetime import datetime


now = datetime.now()

print(now.date())
print(now.time())


print(datetime.now())

print(datetime.now().year)

print(datetime.now().month)

print(datetime.now().day)

print(datetime.now().hour)

print(datetime.now().minute)

print(datetime.now().second)

with open(str(now.date()) + "_" + str(now.hour) + str(now.minute) + str(now.second) + ".txt", 'w') as f:
    f.write('File generated at ' + str(now) + '!')