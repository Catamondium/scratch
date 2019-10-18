#!/usr/bin/env python3
import tarfile as tf
from cmd import Cmd
from pathlib import Path
from typing import Tuple, List, Optional as Maybe
from trie import Trie
from copy import deepcopy
import sys
import os


class TPath:
    """Trie path, submit to Trie w/ #parts"""

    def __init__(self, string: str):
        self._unsolved = string
        self.solved = None
        self._pending = []

    def __div__(self, npart):
        if self.solved:
            self.solved.append(npart)
        else:
            self._pending.append(npart)

    def __repr__(self):
        if self.solved:
            return f"TPath({self.solved})"
        else:
            return f"TPath(\'{self._unsolved}\')"

    def parts(self, pwd) -> Maybe[List[str]]:
        if self.solved:
            return self.solved

        if not self._unsolved or self._unsolved == '.':
            return pwd

        p = Path(self._unsolved)
        parts = [*p.parts]
        resolved = []

        try:
            origin = parts[0]
            if origin in ('~', '/'):
                parts = parts[1:]
            else:
                resolved = pwd
        except IndexError:
            pass

        for part in parts:
            if part in ('.', '~'):
                # Ignore null move
                continue
            elif part == '..' and resolved:
                # Remove previous, obeying root
                resolved.pop()
            else:
                resolved.append(part)
        if self._pending:
            resolved += self._pending
            self._pending = None
        self.solved = resolved
        return self.solved


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

    # @lexed # TODO
    # def do_lextest(self, tpath: TPath, spath: Path, *rest: Tuple[(int, ...)]):
    #    print(f"TAR: {tpath}")
    #    print(f"SYS: {spath}")
    #    print(f"*rs: {rest}")


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
