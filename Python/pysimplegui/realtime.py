#!/usr/bin/env python3

import PySimpleGUI as sg
from time import time
from itertools import starmap
from pprint import pprint

class CTimer:
    """
    Change logging timer
    """
    def __init__(self, begin, blacklist=[]):
        self.prev = begin # init opening state
        self.ptime = time() # opening time
        self.log = [] # change log
        self.blacklist = blacklist
    
    def __call__(self, nstate):
        # Ignore events that cannot sustain
        if nstate in self.blacklist:
            return

        if self.prev != nstate:
            ntime = time() # retain new time
            self.log.append((self.prev, nstate, self.ptime, ntime))
            self.ptime = ntime # current level time
            self.prev = nstate
    
    def get(self):
        """
        Get exhaustive change log
        """
        return self.log
    
    @staticmethod
    def _convert(p, n, pt, nt):
        # Convert into nstate,delta form
        return p, nt - pt
    
    def presses(self):
        """
        get state elapses (state, delta)
        """
        yield from starmap(self._convert, self.log)


if __name__ == "__main__":
    layout = [[sg.RealtimeButton('rtime'), sg.RealtimeButton('realtime')], [sg.Quit(bind_return_key=1)]]
    w = sg.Window('timings', layout)
    
    # Exclude unsustained events
    timer = CTimer(sg.TIMEOUT_KEY, ['Quit', None])
    while True:
        event, _ = w.read(timeout=0)
        timer(event)
        if event in ('Quit', None):
            print("EXIT")
            break

    pprint(list(timer.presses()))
    