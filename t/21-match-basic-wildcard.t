#! /usr/bin/env perl
use strict;
use warnings;
use Test::More;

use_ok 'Path::Match'
	or BAIL_OUT;

my @patterns= qw(
	*.txt
	*.csv
	*.*
	*/README
	t/*
);
my $matcher= Path::Match->new(\@patterns);
my @tests= (
	[ 'a.txt'    => [ $patterns[0], ['a'] ], [ $patterns[2], ['a','txt'] ] ],
	[ 'a.csv'    => [ $patterns[1], ['a'] ], [ $patterns[2], ['a','csv'] ] ],
	[ 't/README' => [ $patterns[4], ['README'] ], [ $patterns[3], ['t'] ] ],
);
#local $Web::ConServe::PathMatch::DEBUG= sub { warn "$_[0]\n" };
for (@tests) {
	my ($path, @expected)= @$_;
	my @actual;
	ok( !$matcher->search($path, sub { push @actual, [@_]; 0 }), 'search = false' );
	is_deeply( \@actual, \@expected, "path $path" )
		or diag explain \@actual;
}

done_testing;
