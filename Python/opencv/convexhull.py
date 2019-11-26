#!bin/python3
import cv2 as cv
import random as rng
import numpy as np
import argparse

parser = argparse.ArgumentParser()
parser.add_argument("-i", action='store_false', help="disable rendering inner contours")
parser.add_argument("--feed", "-f", type=int, nargs='?', help="source device", default=0)
parser.add_argument("--thresh", "-t", type=int, nargs='?', help="Canny lower threshhold", default=100)

args = parser.parse_args()

rng.seed(12345)
threshhold = args.thresh
feed = cv.VideoCapture(args.feed)
while True:
    ret, frame = feed.read()
    cv.imshow('feed', frame)

    gray = cv.cvtColor(frame, cv.COLOR_BGR2GRAY)

    canny = cv.Canny(gray, threshhold, 2 * threshhold)
    contours, _ = cv.findContours(canny, cv.RETR_TREE, cv.CHAIN_APPROX_SIMPLE)

    hull_l = []
    for i in range(len(contours)):
        hull  = cv.convexHull(contours[i])
        hull_l.append(hull)

    drawing = np.zeros((*canny.shape[:2], 3), dtype=np.uint8)
    for i in range(len(contours)):
        col = (rng.randint(0, 256), rng.randint(0, 256), rng.randint(0, 256))
        if args.i:
            cv.drawContours(drawing, contours, i, col)
        cv.drawContours(drawing, hull_l, i, col)
    cv.imshow('hull', drawing)

    if cv.waitKey(1) & 0xFF == ord('q'):
        break
feed.release()
cv.destroyAllWindows()