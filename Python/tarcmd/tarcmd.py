#!/usr/bin/env python3
import tarfile as tf
from cmd import Cmd
from pathlib import Path
from typing import Tuple, List
from trie import Trie, TPath
from copy import deepcopy
from functools import wraps
from inspect import signature, Parameter
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
    Cmd do_* lexing decorator
    ignores surplus arguments post-tokenization
    Currently only tokenizes # FIXME
    """
    sig = signature(f)
    @wraps(f)
    def deco(obj, line):
        toks = shsplit(line)[::-1]
        args = ()
        print(sig.parameters)
        for name, param in sig.parameters.items():
            if name == 'self':
                # Exclude obj, handled eariler
                continue

            if param.kind in (Parameter.POSITIONAL_ONLY, Parameter.POSITIONAL_OR_KEYWORD):
                args += (toks.pop(),)
            elif param.kind == Parameter.VAR_POSITIONAL:
                args += tuple(toks[::-1])
        return f(obj, *args)
    return deco


class Tarcmd(Cmd):
    intro = "Tar explorer shell"
    postfix = "(Tar) > "
    prompt = postfix
    tree = Trie()

    environ = dict()
    pwd = []

    def refresh_prompt(self):
        self.prompt = f"{self.environ['pwd']} {self.postfix}"

    def do_mount(self, target: Path):
        """Mount a new tar archive"""
        self.environ['file'] = target
        self.environ['pwd'] = '/'
        self.refresh_prompt()
        with tf.open(target) as f:
            for info in f:
                self.tree[info.name.split('/')] = info

    def do_ls(self, path: str, verbose=False):
        """List members"""
        solved = TPath(path).parts(deepcopy(self.pwd))
        p = Path('/'.join(solved or []))
        n = None
        try:
            print(f"Resolved: {solved}")
            n = self.tree.getNode(solved)

            results = [n, *n.children.values()]
            results = [(i.value.name, i.value) for i in results if i.isLeaf]

            for r, _ in sorted(results, key=lambda t: t[0]):
                relative = Path(r).relative_to(p)
                print(str(relative))
        except:
            print(f"No such path: {n or str(path)}")

    def do_env(self, *args):
        """List environment variables"""
        for k, v in self.environ.items():
            print(f"{k} = {v}")

    def do_cd(self, path: str):
        """Change working directory"""
        npwd = TPath(path).parts(deepcopy(self.pwd))
        self.pwd = npwd
        self.environ['pwd'] = '/' + '/'.join(npwd)
        self.refresh_prompt()

    def do_exit(self, *args):
        """Exit tarcmd"""
        sys.exit()

    def do_quit(self, *args):
        """Exit tarcmd"""
        sys.exit()

    def do_debug(self, *args):
        print(f"pwd: {self.pwd}")
        self.tree.print()

    @perr("Insufficient arguments")
    @lexed
    def do_lextest(self, tpath: TPath, spath: Path, *rest: Tuple[(int, ...)]):
        print(f"TAR: {tpath} : {type(tpath)}")
        print(f"SYS: {spath} : {type(spath)}")
        print(f"*rs: {rest} : {type(rest)}")


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
