#! /usr/bin/env perl
use strict;
use warnings;
use Test::More;

use_ok 'Path::Match'
	or BAIL_OUT;

subtest path_with_capture => sub {
	my @patterns= (
		'/foo/*',
		'/foo/*/bar',
		'/fo*/bar',
		'/foo*/baz',
	);
	my $matcher= Path::Match->new(patterns => \@patterns);
	my @tests= (
		[ '/foo'       => () ],
		[ '/foo/1'     => [ $patterns[0], [1] ] ],
		[ '/foo/1/bar' => [ $patterns[1], [1] ] ],
		[ '/foo/bar'   => [ $patterns[0], ['bar'] ], [ $patterns[2], ['o'] ] ],
		[ '/foo2/bar'  => [ $patterns[2], ['o2'] ] ],
		[ '/foo2/baz'  => [ $patterns[3], ['2'] ] ],
		[ '/foo/'      => () ],
		[ '/foo//bar'  => () ],
	);
	for (@tests) {
		my ($path, @expected)= @$_;
		my @actual;
		$matcher->search($path, sub { push @actual, [@_]; 0 });
		is_deeply( \@actual, \@expected, "path $path" )
			or diag explain \@actual;
	}
};

subtest wildcard_nuances => sub {
	my @patterns= (
		'/foo/*.*',
		'/foo/*',
		'/foo/**/json',
		'/foo/**/*',
		'/bar/**',
		'/bar/**/x/**/y',
		'/bar/**/d/*',
		'/bar/**/t*',
	);
	#local $Path::Match::DEBUG= sub { warn @_, "\n" };
	my $matcher= Path::Match->new(patterns => \@patterns);
	my @tests= (
		[ '/'            => () ],
		[ '/foo'         => () ],
		[ '/foo/3'       => 
			[ $patterns[1], [3] ],
			[ $patterns[3], ['',3] ],
		],
		[ '/foo/3/'      => () ],
		[ '/foo/3//'     => () ],
		[ '/foo/3/xyz'   => [ $patterns[3], [3,'xyz'] ] ],
		[ '/foo/3/json'  =>
			[ $patterns[2], [3] ],
			[ $patterns[3], [3,'json'] ]
		],
		[ '/foo/index.html' =>
			[ $patterns[0], ['index','html'] ],
			[ $patterns[1], ['index.html'] ],
			[ $patterns[3], ['', 'index.html'] ],
		],
		[ '/bar'         => [ $patterns[4], [''] ] ],
		[ '/bar/'        => [ $patterns[4], [''] ] ],
		[ '/bar/z'       => [ $patterns[4], ['z'] ] ],
		[ '/bar/z/'      => [ $patterns[4], ['z/'] ] ],
		[ '/bar/1/x/2/y' =>
			[ $patterns[5], [1,2] ],
			[ $patterns[4], ['1/x/2/y'] ],
		],
		[ '/bar/1/a/x/2/y' =>
			[ $patterns[5], ['1/a',2] ],
			[ $patterns[4], ['1/a/x/2/y'] ],
		],
		[ '/bar/1/x/2/1/2/3/y' =>
			[ $patterns[5], [1,'2/1/2/3'] ],
			[ $patterns[4], ['1/x/2/1/2/3/y'] ],
		],
		[ '/bar/t' =>
			[ $patterns[7], [ '', '' ] ],
			[ $patterns[4], [ 't' ] ],
		],
		[ '/bar/42/d/'   => [ $patterns[4], ['42/d/'] ] ],
		[ '/bar/d'       => [ $patterns[4], ['d'] ] ],
		[ '/bar/d/'      => [ $patterns[4], ['d/'] ] ],
		[ '/bar/d/2'     =>
			[ $patterns[6], ['',2] ],
			[ $patterns[4], ['d/2'] ],
		],
	);
	for (@tests) {
		my ($path, @expected)= @$_;
		my @actual;
		$matcher->search($path, sub { push @actual, [@_]; 0 });
		is_deeply( \@actual, \@expected, "path $path" )
			or diag explain \@actual;
	}
};

done_testing;
