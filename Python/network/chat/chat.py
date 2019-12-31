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
        if nick in clients:
            raise LookupError
        clients[nick] = (reader, writer)


async def othercast(nick, msg):
    async with lock:
        for n, (r, w) in clients.items():
            if n != nick:
                w.write(msg.encode(ENC))


async def handler(reader: aio.StreamReader, writer: aio.StreamWriter):
    nickreg = await reader.readline()
    nickreg = nickreg.decode(ENC)
    if iscmd(nickreg):
        try:
            com, rawnick = nickreg.split(' ', maxsplit=1)
            if com[1:] != 'register':
                writer.write(b"[SERV] Failure to register.\n")
                writer.close()
                return
            nick = rawnick.strip()
            await reg_handler(nick, reader, writer)
            print(f"{nick} registered")
        except:
            writer.write(b"[SERV] Bad registration.\n")
            writer.close()
            return

    while True:
        msg = await reader.readline()
        msg = msg.decode(ENC)
        if not msg:
            print(f"{nick} left")
            break
        await othercast(nick, f"{nick}: {msg}")


async def main():
    server = await aio.start_unix_server(handler, path=ADDR)
    await server.wait_closed()

try:
    unlink(ADDR)
except FileNotFoundError:
    pass

try:
    loop = aio.get_event_loop()
    loop.run_until_complete(main())
    loop.run_forever()
except KeyboardInterrupt:
    print("Service stopped")
finally:
    for task in aio.Task.all_tasks():
        task.cancel()
    loop.run_until_complete(loop.shutdown_asyncgens())
    loop.close()
