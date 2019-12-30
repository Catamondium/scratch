#!/usr/bin/env python3
from common import *
import asyncio as aio
from typing import Tuple
from sys import argv


async def main(nick, msg):
    reader, writer = await aio.open_unix_connection(ADDR)
    writer.write(f"/register {nick}\n".encode('utf-8'))
    writer.write(f"{msg}\n".encode('utf-8'))
    while True:
        msg = await reader.readline()
        if not msg:
            break
        print(msg.decode('utf-8'), end='')

nick = argv[1]
msg = argv[2]
loop = aio.get_event_loop()
loop.run_until_complete(main(nick, msg))