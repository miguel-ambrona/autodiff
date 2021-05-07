#!/usr/bin/env python3

from os import listdir
from os.path import isfile, join
from subprocess import Popen, PIPE

COLORS = { 'green' : '\033[92m', 'yellow' : '\033[93m',
           'red' : '\033[91m', 'purple' : '\033[95m', 'std' : '\033[0m' }

constructions_path = "./constructions/"
constructions  = [f for f in listdir(constructions_path) if isfile(join(constructions_path, f))]
constructions.sort()

# Timeout in seconds:
T = 100

def getTime(output):
    time = ""
    for l in output.splitlines():
        if "Solved in" in str(l):
            time = str(l)[9:]
    return time

for h in ["1","2"]:

    print("\n\nAnalyzing files from './constructions/ with heuristic " + h + ":\n")
    for c in constructions:

        p = Popen(["../../search.native", h, constructions_path + c], stdin=PIPE, stdout=PIPE, stderr=PIPE)
        padding = " " * (20 - len(c))

        try:
            output, err = p.communicate(timeout = T)

        except:
            p.kill()
            print(COLORS['purple'] + c + padding + COLORS['yellow'] + "Timeout\ttime:  > " + str(T) + " s" + COLORS['std'])
            continue

        found = "false"
        time = None
        lines = output.splitlines()
        for i in range(len(lines)):
            l = lines[i]
            if b"Found attack:" in l:
                found = l[14:]
                break

        time = getTime(output)

        (color, result) = ('green', 'Found!\t') if found == b'true' else ('red','Not found\t')
        print(COLORS['purple'] + c + padding + COLORS[color] + result + "time: " + str(time)[2:-1] + COLORS['std'])
