#!/usr/bin/env python3
import tarfile as tf
from cmd import Cmd
from pathlib import Path
from typing import Tuple, List
from trie import Trie, TPath
from copy import deepcopy
from functools import wraps
from inspect import signature, Parameter, _empty
from shlex import split as shsplit
from itertools import islice
import sys
import os


def perr(msg: str = "An error occured"):
    """
    Prints string when an exception occurs, passes exception
    """
    def outer(f):
        @wraps(f)
        def deco(*args, **kwargs):
            try:
                return f(*args, **kwargs)
            except:
                print(msg)
        return deco
    return outer


def lexed(f):
    """
    apropos: allows method do_* = f, to be written as a normal method
    instead of f(self, line)

    Cmd do_* lexing decorator
    tokenizes input line, then constructs according to annotations
    the result is passed onto method 'f'
    ignores surplus arguments post-tokenization
    """
    sig = signature(f)
    @wraps(f)
    def deco(obj, line):
        toks = shsplit(line)[::-1]
        args = ()
        for name, param in sig.parameters.items():
            if name == 'self':
                # Exclude obj, handled eariler
                continue

            if param.kind == Parameter.POSITIONAL_ONLY:
                args += (param.annotation(toks.pop()),)
            elif param.kind == Parameter.POSITIONAL_OR_KEYWORD:
                if toks:
                    args += (param.annotation(toks.pop()),)
                else:
                    args += (param.default,)
            elif param.kind == Parameter.VAR_POSITIONAL:
                def ctor(x): return x
                if param.annotation.__name__ == 'Tuple':
                    # Ellipsis varargs
                    ctor = param.annotation.__args__[0]
                elif param.annotation != _empty:
                    ctor = param.annotation
                args += tuple(map(ctor, toks[::-1]))
        return f(obj, *args)
    return deco


class Tarcmd(Cmd):
    intro = "Tar explorer shell"
    postfix = "(Tar) > "
    prompt = postfix
    tree = Trie()

    environ = dict()
    pwd = []

    def do_mount(self, target: Path):
        """Mount a new tar archive"""
        self.environ['file'] = target
        self.environ['pwd'] = '/'
        with tf.open(target) as f:
            for info in f:
                self.tree[info.name.split('/')] = info

    @lexed
    def do_ls(self, path: TPath = TPath('.')):
        """List members"""
        solved = path.parts(deepcopy(self.pwd))
        p = Path('/'.join(solved or []))
        n = None
        try:
            n = self.tree.getNode(solved)

            results = [n, *n.children.values()]
            results = [(i.value.name, i.value) for i in results if i.isLeaf]

            for r, _ in sorted(results, key=lambda t: t[0]):
                relative = Path(r).relative_to(p)
                print(str(relative))
        except:
            print(f"No such path: {n or str(path)}")

    @lexed
    def do_env(self):
        """List environment variables"""
        for k, v in self.environ.items():
            print(f"{k} = {v}")

    @lexed
    def do_cd(self, path: TPath = TPath('.')):
        """Change working directory"""
        npwd = path.parts(deepcopy(self.pwd))
        self.pwd = npwd
        self.environ['pwd'] = '/' + '/'.join(npwd)

    def do_exit(self, *args):
        """Exit tarcmd"""
        sys.exit()

    def do_quit(self, *args):
        """Exit tarcmd"""
        sys.exit()

    def do_debug(self, *args):
        print(f"pwd: {self.pwd}")
        self.tree.print()

    @perr("Invalid/Insufficient arguments")
    @lexed
    def do_lextest(self, tpath: TPath, spath: Path, *rest: Tuple[(int, ...)]):
        print(f"TAR: {tpath} : {type(tpath)}")
        print(f"SYS: {spath} : {type(spath)}")
        print(f"*rs: {rest} : {type(rest)}, {type(rest[0])}")

    def postcmd(self, *args):
        self.prompt = f"{self.environ['pwd']} {self.postfix}"


if __name__ == "__main__":
    from argparse import ArgumentParser, FileType
    parser = ArgumentParser()
    parser.add_argument("tar", type=Path)
    args = parser.parse_args()
    cmd = Tarcmd()
    cmd.do_mount(args.tar)
    try:
        cmd.cmdloop()
    except KeyboardInterrupt:
        pass
