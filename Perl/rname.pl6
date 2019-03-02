#!/usr/bin/perl6
use strict;
use worries;

sub sFiles(@seq, Int $width) {
	my &matcher = -> IO $f {
		my Str $reS = "{$f.dirname}-\\d\{{$width}\}.*";
		$reS .= subst(/'/'/, '\/', :g);
		return ($f.basename ~~ /$reS/).Bool;
	};

	say "running";
	## stops printing from here 
	my @compliant = grep(&matcher, @seq);
	@compliant .= sort(-> IO $v {$v.basename});
	say @compliant;
	say "compliants";

	my @incompliant = grep(-> $f { ! &matcher($f) }, @seq);
	say @incompliant.WHAT.WHAT;
	say "incompliants";

#	return @compliant.append(@incompliant);
}

sub rname(@targets) {
	my @files = grep({$^a ~~ :f}, @targets);
	my Int $width = floor(log10 @files.elems) + 1;
	sFiles(@files, $width);
	@files .= sort(-> IO $v {$v.basename});
	for 0..@files.elems-1 {
		my IO $file = @files[$^a];
		my Str $ext = $file.extension;
		$ext = $ext eq "" ?? "" !! "." ~ $ext;
		my Str $dest = sprintf("%s-%0*d%s", $file.dirname, $width, $^a, $ext);
		#say "{$file.basename} -> {$dest}";
	}
	my @dirs  = grep({$^a ~~ :d}, @targets);
	@dirs.hyper.map(-> IO $d {rname dir($d)});
}

sub MAIN(+@targets) {
	for @targets -> $t {
		my @stuff = dir($t.IO.resolve).flat;
		rname @stuff;
	}
}