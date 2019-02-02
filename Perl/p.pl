#!/usr/bin/perl
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

print $_ = [*] 1..5;
