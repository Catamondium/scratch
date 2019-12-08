#!/usr/bin/env rakudo

grammar Lisp {
    rule TOP { \s* <expr>* % \s+ }
    token ident { <[a..zA..Z\+\\\*]>+ }

    # Sexprs
    proto token expr {*}
    token expr:<ident> { <ident> }
    rule expr:<fcall> {\( <ident> <expr>* % \s+ \)}
    token expr:<literal> { <literal> }

    # Literals
    proto token literal {*}
    token literal:<int> { \d+ }
}

class LispActions {
    has %.syms = Hash.new; # global symbol table
    method TOP($/) {
        $/.make: [.made for $/<expr>]
    }

    # Sexprs
    method expr:<fcall>($/) {
        my $ret;
        given $/<ident> {
                when '+' {$ret = [+] (.made for $/<expr>)}
                when '*' {$ret = [*] (.made for $/<expr>)}
            }
        $/.make($ret)
    }

    method ident($/) {
        make ~$/
    }

    method expr:<literal>($/) {
        $/.make: $/<literal>.made
    }

    # Literals
    method literal($/) {
        $/.make: ~$/
    }

    method literal:<int>($/) {
        $/.make: $($/)
    }

    # Declarations

}

my $content = q:to/DATA/;
(+ 1 2 (* 2 4))
(* 5 5)
DATA

my $m = Lisp.parse($content, actions => LispActions.new);
say $m;
say $m.made;