#!/usr/bin/python

import os
import struct
import sys
import getopt
import csv

from itertools import zip_longest

def write_to_file(xdata, ydata, outputfile):
    # Ensure the output directory exists
    os.makedirs(os.path.dirname(outputfile), exist_ok=True)
    
    # Writes output to file
    with open(outputfile, 'w', newline='') as fp:
        a = csv.writer(fp, delimiter=',')
        # Writes header
        h = ['Field_G', 'Intensity']
        a.writerow(h)
        for i in range(len(xdata)):
            s1 = xdata[i]
            s2 = ydata[i]
            r = [s1, s2]
            a.writerow(r)

def generate_ydata(inputfile):
    # Extracts the y axis data from the input file
    ydata = []
    with open(inputfile, 'rb') as inh:
        indata = inh.read()
    for i in range(0, len(indata), 8):
        pos = struct.unpack('>d', indata[i:i+8])
        ydata.append(pos[0])
    return ydata

def generate_xdata(paramfile):
    # Extracts the x axis data from the parameter file
    with open(paramfile, 'r') as fin:
        for line in fin:
            p = line[0:4]
            if p == 'XPTS':
                xpoints = int(line[5:])
            elif p == 'XMIN':
                xmin = float(line[5:])
            elif p == 'XWID':
                xwid = float(line[5:])

    xmax = xmin + xwid
    xsampling = xwid / xpoints

    xdata = [xmin + (xsampling * (k - 1)) for k in range(1, xpoints)]
    return xdata

def test_files(inputfile, outputfile):
    errors = 0
    # Test if input and output files have been specified properly
    if not inputfile:
        print("Oh no! You haven't specified an input file!")
        print("conv.py -i inputfile -o outputfile")
        sys.exit(2)
    elif not outputfile:
        print("Oh no! You haven't specified an output file!")
        print("conv.py -i inputfile -o outputfile")
        sys.exit(2)

    # Test if input file exists
    if not os.path.isfile(inputfile):
        print("Input file doesn't exist!")
        sys.exit(2)

    fileName, fileExtension = os.path.splitext(inputfile)
    paramfile = fileName + ".DSC"

    # Test if param file exists
    if not os.path.isfile(paramfile):
        print("Parameter file doesn't exist!")
        sys.exit(2)

    # Test if is DTA file or not
    if fileExtension != ".DTA":
        print("Not a DTA file!")
        sys.exit(2)

    return errors

def process_file(inputfile, outputfile):
    if test_files(inputfile, outputfile) == 0:
        fileName, fileExtension = os.path.splitext(inputfile)
        paramfile = fileName + ".DSC"
        xdata = generate_xdata(paramfile)
        ydata = generate_ydata(inputfile)
        write_to_file(xdata, ydata, outputfile)

def main(argv):
    inputfolder = ''
    outputfolder = ''
    doall = 0

    # Get command line arguments
    try:
        opts, args = getopt.getopt(argv, "hi:o:a", ["ifolder=", "ofolder="])
    except getopt.GetoptError:
        print("Oh no! You didn't put in enough arguments or something!")
        sys.exit(2)
    for opt, arg in opts:
        if opt == '-h':
            print("conv.py -i inputfolder -o outputfolder")
            sys.exit()
        elif opt in ("-i", "--ifolder"):
            inputfolder = arg
        elif opt in ("-o", "--ofolder"):
            outputfolder = arg
        elif opt == "-a":
            doall = 1

    if doall == 1:
        for root, dirs, files in os.walk(inputfolder):
            for filename in files:
                if filename.endswith(".DTA"):
                    inputfile = os.path.join(root, filename)
                    outputfile = os.path.join(outputfolder, filename.replace(".DTA", ".csv"))
                    process_file(inputfile, outputfile)
    else:
        print("Please specify the -a option to process all files in the folder.")

if __name__ == "__main__":
    main(sys.argv[1:])