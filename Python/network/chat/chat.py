#!/usr/bin/env python3
from common import *
import asyncio as aio
from os import unlink

lock = aio.Lock()
clients = {}

def iscmd(data) -> bool:
    return data[0] == '/'

async def reg_handler(nick, reader, writer):
    async with lock:
        clients[nick] = (reader, writer)

async def othercast(nick, msg):
    async with lock:
        for n, (r, w) in clients.items():
            if n != nick:
                w.write(msg.encode('utf-8'))

async def handler(reader: aio.StreamReader, writer: aio.StreamWriter):
    nickreg = await reader.readline()
    nickreg = nickreg.decode('utf-8')
    if iscmd(nickreg):
        try:
            stuff = nickreg.split(' ', maxsplit=1)
            if stuff[0][1:] != 'register':
                return
            nick = stuff[1].strip()
            await reg_handler(nick, reader, writer)
            print(f"{nick} registered")
        except:
            return

    while True:
        msg = await reader.readline()
        msg = msg.decode('utf-8')
        if not msg:
            print(f"{nick} left")
            break
        await othercast(nick, f"{nick}: {msg}")


async def main():
    await aio.start_unix_server(handler, path=ADDR)
try:
    unlink(ADDR)
except:
    pass

try:
    loop = aio.get_event_loop()
    loop.run_until_complete(main())
    loop.run_forever()
except KeyboardInterrupt:
    print("Service stopped")