#!bin/python3
import cv2 as cv
import random as rng
import numpy as np
from collections import defaultdict

LABELS = defaultdict(lambda: circularHandler, [
    (0, lambda *_: "<missing>"),
    (1, lambda *_: "<missing>"),
    (2, lambda *_: "<missing>"),
    (3, lambda *_: "Triangle"),
    (4, lambda *_: "Rect"),
    (5, lambda *_: "Pentagon"),
    (6, lambda *_: "Hexagon")
])

def standardWindow(name):
    cv.namedWindow(name, cv.WINDOW_NORMAL)
    cv.resizeWindow(name, 600, 600)

def getpoly(contour):
    epsilon = 0.01 * cv.arcLength(contour, True)
    return cv.approxPolyDP(contour, epsilon, True)

def circularHandler(shape, thresh):
    x,y,w,h = cv.boundingRect(shape)
    subthresh = thresh[x:x+w, y:y+h]
    lines = cv.HoughLines(subthresh, 1, np.pi/180, 200)
    if lines is not None:
        return "Semicircular"
    else:
        return "Circle"

def classify(shape, thresh):
    n = len(shape)
    return LABELS[n](shape, thresh)

def _main(img):
    gray = cv.cvtColor(img, cv.COLOR_BGR2GRAY)
    _, thresh = cv.threshold(gray, 127, 255, cv.THRESH_BINARY)
    # dialation helps smaller polygons
    dialated = cv.dilate(thresh, np.ones((5, 5), dtype=np.uint8), iterations=1)
    edges = cv.Canny(dialated, 1, 254)

    contours, _ = cv.findContours(edges, cv.RETR_EXTERNAL, cv.CHAIN_APPROX_SIMPLE)
    drawing = img.copy()
    for contour in contours:
        shape = getpoly(contour)
        x,y,w,h = cv.boundingRect(shape)
        cv.rectangle(drawing, (x, y), (x+w, y+h), (255, 0, 0), thickness=2)

        text = classify(shape, thresh)
        M = cv.moments(contour)
        cx = int(M['m10'] / M['m00'])
        cy = int(M['m01'] / M['m00'])
        cv.circle(drawing, (cx, cy), 2, (0, 0, 255), thickness=-1)
        cv.putText(drawing, text, (int(cx - (w/2)), cy), cv.FONT_HERSHEY_SIMPLEX, 1, (0, 255, 0), thickness=2)

    return drawing

if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("target", nargs='?', default="./shapes_nocolor.jpg")
    args = parser.parse_args()

    img = cv.imread(args.target)
    overlayed = _main(img)

    standardWindow("shapes")
    cv.imshow("shapes", overlayed)
    cv.waitKey(0)
    cv.destroyAllWindows()