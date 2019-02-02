#!/usr/bin/perl6
use strict;

given "foo\n" {
	.print;
}

say "apldburbvuerbv";
my @arr = 0..9;

# sigil( ) casts;

my $z = $(2..4); # 'range' object, stored verbatim
my %w = @("key"=>"val");

say %w.WHAT;

sub fact(Int $n) {
	my $curr = 1;
	for @(1..$n) -> $i {
		$curr *= $i;
	}
	return $curr;
}

print fact(5) ~ "\n";
print fact(0) ~ "\n";

# Wrapped debug decorator
sub debug_wrap(&f) {
	&f.wrap: sub (|args) {
		my \ret := callwith(|args);
		say "{&f.name}({\args.gist}) -> {\ret.perl}";
		ret;
	}
}

# 'Trait' form using debug_wrap
multi sub trait_mod:<is>(Routine $f, :$debugging!) {
	debug_wrap($f);
}

sub x($i=55) is debugging {$i;};
x();

debug_wrap(&fact);
fact(5);
