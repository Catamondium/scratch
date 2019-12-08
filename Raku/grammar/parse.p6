#!/usr/bin/env rakudo

grammar Stanza {
    token ws { <!ww> \h* }
    token identifier  { \w+ }
    token text { <inlinetext> | <blocktext> }
    token inlinetext { <[\w\h]>+ }
    token blocktext { <[\w\s]>+ }
    rule kvpair { \s* <key=identifier> ':' <value=inlinetext> \n+ }
    token section {
        <kvpair>*
        <text>?
        \s* '##' \s*
    }
 
    token TOP {
        <section>*
    }
}
 
my $contents = q:to/EOI/;
  key : val    
riribubuvbtuvbtuvbtubu ugb utbgutb gutbgu tb   
eivruvbruuru urbubgubgur b ru bur b   
##
key2: val2
##
EOI

my Match $m = Stanza.parse($contents);
say $m