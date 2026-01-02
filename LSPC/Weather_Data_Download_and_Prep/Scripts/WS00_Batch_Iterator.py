# This script is used when running through the entire process for every watershed

# Each iteration of the loop will correspond to a different watershed

# In each step, this script will update 'WS01_Set_Parameters.py' and run the other scripts



#### SETUP ####

import subprocess



#### PROCEDURE ####

# The number specified in range() sets the number of watersheds to run through (starting from the first one)
for i in range(2):

    with open('Scripts/WS01_Set_Parameters.py', 'r', encoding = 'utf-8') as ws_file:
        
        fileLines = ws_file.readlines()

        for j in range(len(fileLines)):
            
            if 'wsIndex =' in fileLines[j]:
                
                fileLines[j] = 'wsIndex = ' + str(i + 1) + '\n'

                break
        
    with open('Scripts/WS01_Set_Parameters.py', 'w', encoding = 'utf-8') as ws_file:

        ws_file.writelines(fileLines)


    # iterRes = subprocess.run("python Scripts/Demo_Script_1.py")

    # print(iterRes.stdout.decode('utf-8'))

    # print(iterRes.stderr.decode('utf-8'))

    # iterRes = subprocess.run("python Scripts/Demo_Script_2.py")

    # print(iterRes.stdout.decode('utf-8'))

    # print(iterRes.stderr.decode('utf-8'))


    iterRes = subprocess.run("cd Scripts\\ && .\\Run_Both_Demo_#1_and_#2.bat", shell = True, capture_output = True)


    if iterRes.stderr != b'':
        print('\n\n')
        print(iterRes.stderr.decode('utf-8'))
        raise ValueError('\n\nErrors were encountered in the batch script (iteration ' + i + ')')
    
    else:
        print('\n\n')

        print(iterRes.stdout.decode('utf-8'))

        print('\n\n')
