#!/usr/bin/env python3
from pathlib import Path
from zipfile import ZipFile

myzip = Path(__file__).parent  # assuming is Zipapp
print(f"ZIP ROOT: {myzip}")
with ZipFile(myzip, 'r') as root:  # Open the archive
    with root.open('resource') as res:  # Read 'resource' member
        print(res.read().decode())
