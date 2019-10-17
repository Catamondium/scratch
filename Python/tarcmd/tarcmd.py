#!/usr/bin/env python3
import tarfile as tf
from cmd import Cmd
from pathlib import Path
from trie import Trie
import sys

"""
TODO

lazy TemporaryDirectory
tokenize by spaces, escaped by '\'

globbing support:
	tar context
		fnmatch
	'real' context (during extractions, appends etc.)
		glob.glob
'real' effects:
	cd
	shell runs

cd & cd environ
multiple files, append?
columnize ls (reparse stringio redirect?)
perms: chown, chmod, chgrp(?)
editing: edit, mv, rm, add
    might get complicated tracking changes
        heterogenous trie values?

    add:
        create temporary; either duplicate name or extension preserving
        load tmp into $EDITOR, overridable?
        keep tmp until making changes, or commit to BytesIO/TarInfo?
"""


class Tarcmd(Cmd):
    intro = "Tar explorer shell"
    postfix = "T> "
    prompt = postfix
    tartree = Trie()
    environ = dict()

    def do_mount(self, target):
        """Mount a new tar archive"""
        self.environ['file'] = target
        self.environ['pwd'] = '/'
        with tf.open(target) as f:
            for info in f:
                self.tartree[info.name.split('/')] = info

    def do_ls(self, path, verbose=False):
        """List members"""

        p = Path(path)
        n = None
        try:
            # TODO, pull into method and fix this shit!
            if not path.strip():
                n = self.tartree.getNode(None)
            elif p == '.':
                raise NotImplementedError
            elif p.is_absolute():
                n = self.tartree.getNode(str(p)[1:].split('/'))
            else:
                n = self.tartree.getNode(str(p).split('/'))

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

    def do_cd(self, path):
        # TODO
        pass

    def do_debug(self, *args):
        """Display underlying trie"""
        self.tartree.print()


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
