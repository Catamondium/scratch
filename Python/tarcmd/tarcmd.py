#!/usr/bin/env python3
import tarfile as tf
from cmd import Cmd
from pathlib import Path
from typing import Tuple, List
from trie import Trie, TPath
from subprocess import run
import sys
import os
from tempfile import TemporaryDirectory
from cmdext import *


class LazyTmpDir:
    def __init__(self, *args, **kwargs):
        self.args = args
        self.kwargs = kwargs
        self.ret = None

    def __call__(self) -> TemporaryDirectory:
        if self.ret is None:
            self.ret = TemporaryDirectory(*self.args, **self.kwargs)
            return self.ret
        else:
            return self.ret

    def cleanup(self):
        if self.ret:
            self.ret.cleanup()


class Tarcmd(Cmd):
    intro = "Tar explorer shell"
    postfix = "(Tar) > "
    prompt = postfix
    tree = Trie()

    environ = dict()
    pwd = []
    tmp = LazyTmpDir(prefix='Tarcmd-')

    def mount(self, target: Path):
        """Mount a new tar archive"""
        self.environ['file'] = target
        self.environ['pwd'] = '/'
        with tf.open(target) as f:
            for info in f:
                self.tree[info.name.split('/')] = info
        self.postcmd()

    def extract(self, prefix: TPath) -> Path:
        rtmp = self.tmp()
        tpath = Path(rtmp.name)
        entry = prefix.parts(self.pwd)
        with tf.open(self.environ['file']) as f:
            submembers = self.tree.prefixSearch(entry)
            stuff = ((self.tree[sm], tpath / Path('/'.join(sm)))
                     for sm in submembers)
            for subentry, path in stuff:
                fobj = f.extractfile(subentry)
                if fobj is None:
                    continue
                os.makedirs(path.parent, exist_ok=True)
                with open(path, mode='w+b') as target:
                    target.writelines(fobj.readlines())
        return tpath / Path('/'.join(entry))

    def cleanup(self):
        self.tmp.cleanup()

    @lexed
    def do_mount(self, target: Path):
        """Mount a new tar archive"""
        self.mount(target)

    @lexed
    def do_ls(self, path: TPath = TPath('.')):
        """List members"""
        solved = path.parts(self.pwd)
        p = Path('/'.join(solved or []))
        try:
            n = self.tree.getNode(solved)
            results = [n.ch] + [x for x in n.children]
            for r in sorted(x for x in results if x != []):
                try:
                    relative = Path(r).relative_to(p)
                except ValueError:
                    relative = Path(r)
                if str(relative) == n.ch:
                    continue
                print(str(relative))
        except KeyError:
            print(f"No such path: {str(p)}")

    @lexed
    def do_env(self):
        """List environment variables"""
        for k, v in self.environ.items():
            print(f"{k} = {v}")

    @lexed
    def do_cd(self, path: TPath = TPath('.')):
        """Change working directory"""
        npwd = path.parts(self.pwd)
        if npwd in self.tree:
            self.pwd = npwd
            self.environ['pwd'] = '/' + '/'.join(npwd)
        else:
            print("No such path")

    @perr("Invalid/Insufficient arguments")
    @lexed
    def do_lextest(self, tpath: TPath, spath: Path, *rest: Tuple[(int, ...)]):
        print(f"TAR: {tpath} : {type(tpath)}")
        print(f"SYS: {spath} : {type(spath)}")
        print(f"*rs: {rest} : {type(rest)}, {type(rest[0])}")

    @perr("Process interrupted")
    @lexed
    def do_openin(self, command: str, *tars: Tuple[(TPath, ...)]):
        """
        openin command [tars, ...]
        run command *tars
        tars are temporarily extracted
        """
        rcmd = (command,)
        for t in tars:
            rcmd += (str(self.extract(t)),)
        run(rcmd)

    # TODO completion
    def completedefault(self, text, line, begidx, endidx):
        import shlex
        from inspect import signature, Parameter, _empty

        # Part 1, context accumulation
        command, *middle, subject = shlex.split(line)
        method = getattr(self, 'do_' + command)
        params = signature(method).parameters
        nth = min(len(middle), len(params) - 1)
        target = lextype(list(params.values())[nth])

        # Part 2, Path/TPath expansion
        if target == TPath:
            prefix = '/'.join(TPath(subject).parts(self.pwd))
            options = ('/'.join(k) for k in self.tree.keys())

            return [x for x in options if x.startswith(prefix)]
        return []

    def do_exit(self, *args):
        """Exit tarcmd"""
        self.cleanup()
        sys.exit()

    def do_quit(self, *args):
        """Exit tarcmd"""
        self.do_exit()

    def do_debug(self, *args):
        print(f"pwd: {self.pwd}")
        self.tree.print()

    def do_shell(self, *args):
        """
        Run command in Bash subshell
        """
        status = os.system(*args)
        if status != 0:
            print(f"Shell exited w/ status {status}")

    def postcmd(self, *args):
        self.prompt = f"{self.environ['pwd']} {self.postfix}"


if __name__ == "__main__":
    from argparse import ArgumentParser, FileType
    parser = ArgumentParser()
    parser.add_argument("tar", type=Path)
    args = parser.parse_args()
    cmd = Tarcmd()
    cmd.mount(args.tar)
    try:
        cmd.cmdloop()
    except KeyboardInterrupt:
        pass
