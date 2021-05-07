#!/usr/bin/env python3

from os import listdir
from os.path import isfile, join
from subprocess import Popen, PIPE

COLORS = { 'green' : '\033[92m', 'yellow' : '\033[93m',
           'red' : '\033[91m', 'purple' : '\033[95m', 'std' : '\033[0m' }

def getTime(output):
    time = ""
    for l in output.splitlines():
        if "Solved in" in str(l):
            time = str(l)[12:-1]
    return time


def test(expected, folder):

    print ("\n\nAnalyzing files from '%s:\n" % folder)
    files = [f for f in listdir(folder) if isfile(join(folder, f))]
    files.sort()

    for att in files:
        p = Popen(['../../test.native', folder + att], stdin=PIPE, stdout=PIPE, stderr=PIPE)
        try:
            output, err = p.communicate()
        except:
            p.kill()
            continue

        (color, result) = ('green','Ok') if expected in str(output) else ('red','Failure')
        (color, result) = ('yellow', result + " (W)") if b'WARNING' in output else (color, result)

        time = getTime(output)

        print( '{:<40s} {:<16s} {:<20s}'.format(COLORS['purple'] + att,  COLORS[color] + '\t' + result + COLORS['std'], time))


if __name__ == '__main__':

    test('There does not exist a simulator for the distinguisher', './attacks/')
    test('The following assignment represents a valid simulator for the distinguisher', './must-fail/')
