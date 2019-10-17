#!/usr/bin/env python3
import tarfile as tf
from cmd import Cmd
from pathlib import Path
from typing import List, Optional as Maybe
from trie import Trie
from copy import deepcopy
import sys
import os

"""
TODO

PARSING
    class decorator?
    tokenize by spaces, escaped by '\'

    marking decorators to distinguish?
    globbing support:
        tar context
            fnmatch
        'mixed' context (during extractions, appends etc.)
            decorator provides arity / 'types'?
            glob.glob
    'real' effects: delegate parsing to shell
        cd
        shell runs

multiple files, append?
perms: chown, chmod, chgrp(?)
editing: edit, mv, rm, add
    lazy TemporaryDirectory?
    might get complicated tracking changes
        heterogenous trie values?

    add:
        create temporary; either duplicate name or extension preserving
        load tmp into $EDITOR, overridable?
        keep tmp until making changes, or commit to BytesIO/TarInfo?
"""


class Tarcmd(Cmd):
    intro = "Tar explorer shell"
    postfix = "(Tar) > "
    prompt = postfix
    tree = Trie()

    environ = dict()
    pwd = []

    def refresh_prompt(self):
        self.prompt = f"{self.environ['pwd']} {self.postfix}"

    def resolve(self, path: str = '.') -> Maybe[List[str]]:
        """Resolve TAR paths"""
        if not path or path == '.':
            return deepcopy(self.pwd)

        p = Path(path)
        parts = [*p.parts]
        resolved = []

        try:
            origin = parts[0]
            if origin in ('~', '/'):
                parts = parts[1:]
            else:
                resolved = deepcopy(self.pwd)
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

        return resolved

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
        solved = self.resolve(path)
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
        npwd = self.resolve(path)
        self.pwd = npwd
        self.environ['pwd'] = '/' + '/'.join(npwd)
        self.refresh_prompt()

    def do_debug(self, *args):
        print(f"pwd: {self.pwd}")
        self.tree.print()

    def do_exit(self, *args):
        """Exit tarcmd"""
        sys.exit()

    def do_quit(self, *args):
        """Exit tarcmd"""
        sys.exit()


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
