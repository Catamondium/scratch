#!bin/python3
import cv2 as cv
import random as rng
import numpy as np

def _main(feedid = 0, threshhold = 100, use_inner = False):
    rng.seed(12345)
    threshhold = args.thresh
    feed = cv.VideoCapture(feedid)
    while True:
        _, frame = feed.read()
        cv.imshow('feed', frame)

        gray = cv.cvtColor(frame, cv.COLOR_BGR2GRAY)

        edges = cv.Canny(gray, threshhold, 2 * threshhold)
        contours, _ = cv.findContours(edges, cv.RETR_TREE, cv.CHAIN_APPROX_SIMPLE)

        hull_l = []
        for contour in contours:
            hull  = cv.convexHull(contour)
            hull_l.append(hull)

        drawing = np.zeros((*edges.shape[:2], 3), dtype=np.uint8)
        for i, contour in enumerate(contours):
            col = (rng.randint(0, 256), rng.randint(0, 256), rng.randint(0, 256))
            if use_inner:
                cv.drawContours(drawing, [contour], -1, col)
            cv.drawContours(drawing, hull_l, i, col)
        cv.imshow('hull', drawing)

        if cv.waitKey(1) & 0xFF == ord('q'):
            break
    feed.release()
    cv.destroyAllWindows()

if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("-i", "--inner", action='store_false', help="disable rendering inner contours")
    parser.add_argument("--feed", "-f", type=int, nargs='?', help="source device", default=0)
    parser.add_argument("--thresh", "-t", type=int, nargs='?', help="Canny lower threshhold", default=100)

    args = parser.parse_args()
    _main(args.feed, args.thresh, args.inner)