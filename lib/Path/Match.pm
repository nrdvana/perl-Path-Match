package Path::Match;
use strict;
use warnings;
require Carp;
require Scalar::Util;
our $DEBUG; #= sub { warn @_; };

# ABSTRACT: Match a path against many glob-like patterns, efficiently
# VERSION

=head1 SYNOPSIS

  # Example: iterating filesystem while ignoring patterns
  
  my $ignore_patterns= Path::Match->new([
    'a/*/b',
    'a/*/c',
    '**/b',
    '*/*/b',
  );
  for (@paths) {
    next if $ignore_patterns->matches($_);
    ...
  }
  
  # Example: dispatching web requests

  my $router= Path::Match->new([
    { pattern => '/album/*',        ... },
    { pattern => '/artist/*',       ... },
    { pattern => '/artist/*/album', ... },
    { pattern => '**/favicon.ico',  ... },
  ]);
  sub dispatch($uri) {
    $router->search($uri => sub($match, @captures) {
      return 0 unless ...      # decide whether you want this match, or try the next
      $match->{action}->(...)  # dispatch however you like
      return 1;                # end search
    });
  }

=head1 DESCRIPTION

This module is an optimized algorithm for comparing a path efficiently to multiple glob-like
patterns (the rsync variety where C<*> matches within one directory and C<**> matches multiple
directories deep).  The matching occurs in a tree-style recursive search so that it can
efficiently handle arbitrary numbers of paths, making it suitable for use as a URL router in
a web app.  It also has the ability to find I<all> matches, or just the best one, or iterate
them in a priority-based order.

This module only deals with paths, and only with fairly basic wildcards.  However it is easy to
build more advanced matching for HTTP routers by iterating all the path-matches found by this
module while performing further matching, such as regex-checks on the captured components,
header checks, or HTTP method checks.  HTTP routing almost always uses basic path notations to
divide up the request-space, so this module can serve as an efficient first step to narrow the
list of potential controller actions.

=head2 Patterns

The patterns should be familiar to anyone who has used C<< rsync --exclude >>, but in short,
C<'*'> captures any string that doesn't contain a path separator, and C<**> matches any
string including path separators.  The single-star capture may occur more than once in a single
path component, such as C<'/a*b*xyz/'>.  As a special case, if C<**> is bounded by path
separators (or end of string) it may match "zero path components", such as C<'a/**/b'> matching
C<'a/b'>, C<'a/**'> matching C<'a'>, or C<'**/b'> matching C<'/b'>.

=head2 Pattern Instances

For maximum flexibility, you may provide the patterns to this module in a variety of forms.
The thing you provide to the module will be the same thing you get back from the search method.

=over 25

=item C<$pattern>

A simple scalar containing the pattern notation.

=item C<< [ $pattern, ... ] >>

An arrayref whose first element is the pattern notation.

=item C<< { pattern => $pattern, ... } >>

A hashref containing the key C<'pattern'>.

=item C<< $pattern= $obj->pattern() >>

An object with a 'pattern' attribute.

=back

=head2 Match Priority

Pattern matching gives priority to:

=over

=item 1

Longest matching prefix / literal matches

    /foo/bar/baz

=item 2

Single-star captures followed by extra pattern before '/'

    /foo/b*r/baz

=item 3

Single-star captures

    /foo/b*/baz

=item 4

Double-star captures followed by extra pattern

    /foo/b**/baz

=item 5

Double-star captures

    /foo/b**

=back

All ties are resolved by which rule came first.  Duplicate paths are allowed, and will be
iterated in order given.

=cut

# The nodes are optimized as arrayrefs, rather than hashes or objects.
# These constants are the "attribute names" of the arrayrefs.
use constant {
	NODE_LEAF_PATHS       => 0,
	NODE_BACKTRACK        => 1,
	NODE_PATTERNS_SET     => 2,
	NODE_COMPILED         => 3,
	NODE_FIRST_PATTERN    => 4,
	NODE_PATTERN_REGEX    => 0,
	NODE_PATTERN_ORDER    => 1,
	NODE_PATTERN_SUBNODES => 2,
	NODE_PATTERN_ACTIONS  => 3,
};

=head1 CONSTRUCTOR

=head2 new

  $path_match= Path::Match->new( %attributes );
                       ...->new( \%attributes );
                       ...->new( \@patterns );

The constructor takes a hashref or key/value list of attributes.  For convenience, you may pass
an arrayref of the L</patterns> attribute as the only parameter.

=cut

sub new {
	my $class= shift;
	my $self= @_ != 1? { @_ }
		: @_ == 1 && ref $_[0] eq 'ARRAY'? { patterns => $_[0] }
		: { %{$_[0]} };
	bless $self, $class;
	$self->{_tree}= [];
	$self->_add_patterns_to_tree($self->{_tree}, $self->{patterns})
		if $self->{patterns};
	$self;
}

=head1 ATTRIBUTES

=head2 patterns

An arrayref of patterns.  See L</Patterns> and L</Pattern Instances>.

The attribute may be assigned a new arrayref, but modifying the existing arrayref has no effect.
Use L</add_pattern> to add additional patterns and re-build the search tree.

Note that elements from this list are what L</search> delivers to the callback, so if you are
building something elaborate like a HTTP router, you can use structured items like
C<< { pattern => $p, method => 'GET', action => $coderef } >> to hold all the rest of the
decision-making details needed by your callback.

=cut

sub patterns {
	$_[0]->set_patterns($_[1]) if @_ > 1;
	$_[0]{patterns}
}

sub set_patterns {
	my ($self, $p)= @_;
	my $tree= [];
	$self->_add_patterns_to_tree($tree, $p);
	$self->{patterns}= $p;
	$self->{_tree}= $tree;
}

=head1 METHODS

=head2 add_pattern

  $path_match->add_pattern($pattern);

Add one element to L</patterns>.

=cut

sub add_pattern {
	my ($self, $pattern)= @_;
	$self->_add_patterns_to_tree($self->{_tree}, $pattern);
	$self->{_tree}[NODE_COMPILED]= 0;
	1;
}

=head2 matches

  my $bool= $path_match->matches( $path )

Returns true if any pattern matches the path.

=cut

sub matches {
	my ($self, $path)= @_;
	return $self->_search_tree($self->{_tree}, $path, sub { 1 });
}

=head2 search

  $path_match->search( $path, sub($pattern, $captures) {
    ...
  });

Find the pattern (or object) which best matches C<$path>.  C<$captures> is an arrayref of the
portions of the string that matched the wildcards.

Multiple patterns might match, and the callback will be called for each until it returns a true
value.  This allows you to iterate only as much of the sequence as needed.  The sequence of
nodes passed to the callback is given by L</Match Priority> listed above.

The search method returns true if the callback returns true at any point, and false otherwise.

=cut

sub search {
	my ($self, $path, $callback)= @_;
	return $self->_search_tree($self->{_tree}, $path, $callback, []);
}

# Nodes test the remainder of the path vs. a list of regexes
# [
#    \@actions_ending_here,
#    $backtrack_path_key,
#    \%patterns_set, (temporary)
#    @patterns,
# ]
# pattern: [ $regex, \%subnodes, \@actions ]

sub _add_patterns_to_tree {
	my ($self, $root, $patterns)= @_;
	# Tree up the actions according to prefix
	for my $pat_obj (ref $patterns eq 'ARRAY'? @$patterns : ( $patterns )) {
		my $pattern= !ref $pat_obj? $pat_obj
			: ref $pat_obj eq 'ARRAY'? $pat_obj->[0]
			: ref $pat_obj eq 'HASH'? $pat_obj->{pattern}
			: Scalar::Util::blessed($pat_obj) && $pat_obj->can('pattern')? $pat_obj->pattern
			: "$pat_obj";
		defined $pattern && length $pattern
			or Carp::croak("Invalid pattern: $pat_obj");

		my $node= $root;
		my $from_subnodes;
		my (@capture_names, $prefix, $wild);
		$DEBUG->("considering pattern '$pattern'") if $DEBUG;
		my @parts= split m,(\*+[^/]*),, $pattern;
		for my $p (0..$#parts) {
			my $pathpart= $parts[$p];
			# If it is a literal string, then ...
			if (substr($pathpart,0,1) ne '*') {
				# If it is the final part, list this action under NODE_LEAF_PATHS
				if ($p == $#parts) {
					if ($from_subnodes) {
						push @{ $from_subnodes->{$pathpart}[NODE_LEAF_PATHS]{''} }, $pat_obj;
					} else {
						push @{ $node->[NODE_LEAF_PATHS]{$pathpart} }, $pat_obj;
					}
				}
				# Else step into a sub-node.  If at root, step into '' pattern's sub-nodes
				else {
					$from_subnodes //= $node->[NODE_PATTERNS_SET]{''}[NODE_PATTERN_SUBNODES] //= {};
					$node= $from_subnodes->{$pathpart} //= [];
				}
			}
			# Else if it is not a double-star capture...
			elsif ($pathpart !~ /\*\*/) {
				my $pattern= $node->[NODE_PATTERNS_SET]{$pathpart} //= [];
				$pattern->[NODE_PATTERN_ORDER] //= keys %{$node->[NODE_PATTERNS_SET]};
				# prepare to enter sub-node on next loop
				$from_subnodes= ($pattern->[NODE_PATTERN_SUBNODES] //= {});
				# But if this was the end, then record the action as ''->''
				push @{ $from_subnodes->{''}[NODE_LEAF_PATHS]{''} }, $pat_obj
					unless $p < $#parts;
			}
			# Else a double star capture.  No further sub-nodes are possible.
			else {
				# Special handling for terminating '**' with no other match text
				if ($pathpart eq '**' and $p == $#parts) {
					# if $wild is '**' and previous char is '/', the wildcard can match
					# end-of-string one character sooner
					if ($p && substr($parts[$p-1],-1) eq '/') {
						my $earlier_node= $from_subnodes->{substr($parts[$p-1],0,-1)} //= [];
						my $pattern= $earlier_node->[NODE_PATTERNS_SET]{''} //= [];
						$pattern->[NODE_PATTERN_ORDER] //= keys %{$earlier_node->[NODE_PATTERNS_SET]};
						push @{ $pattern->[NODE_PATTERN_ACTIONS] }, $pat_obj;
					}
				}
				my $remainder= join('', @parts[$p..$#parts]);
				my $pattern= $node->[NODE_PATTERNS_SET]{$remainder} //= [];
				$pattern->[NODE_PATTERN_ORDER] //= keys %{$node->[NODE_PATTERNS_SET]};
				push @{ $pattern->[NODE_PATTERN_ACTIONS] }, $pat_obj;
				last;
			}
		}
	}
	$root->[NODE_COMPILED]= 0;
	$DEBUG->("tree is:", $root) if $DEBUG;
	return $root;
}

sub _compile_tree {
	my ($self, $node, $prefix)= @_;
	# The patterns that come first are anything with single-star in them followed by '*'
	# followed by anything with '**' in them followed by '**', followed by ''.
	# Otherwise patterns are preserved in the order they were seen.
	my @patterns= sort {
			($a eq '') cmp ($b eq '')
			or ($a eq '**') cmp ($b eq '**')
			or ($a =~ /\*\*/) cmp ($b =~ /\*\*/)
			or ($a eq '*') cmp ($b eq '*')
			or $node->[NODE_PATTERNS_SET]{$a}[NODE_PATTERN_ORDER] <=> $node->[NODE_PATTERNS_SET]{$b}[NODE_PATTERN_ORDER]
		}
		keys %{$node->[NODE_PATTERNS_SET]};
	# for each pattern, convert it to the form
	#  [ $regex, \%subnodes, \@actions ]
	$#$node= NODE_FIRST_PATTERN - 1; # clear any previously built items
	for my $pattern (@patterns) {
		my $pat_item= $node->[NODE_PATTERNS_SET]{$pattern};
		push @$node, $pat_item;
		# If the pattern includes subnodes:
		if ($pat_item->[NODE_PATTERN_SUBNODES]) {
			my $re_text= !length $pattern? '^'
				: $pattern eq '*'? '^([^/]+)'
				: do {
					my @parts= split /(\*)/, $pattern;
					'^'.join('', map { $_ eq '*'? '([^/]+)' : "\Q$_\E" } grep length, @parts);
				};
			# sort keys by length
			my @keys= sort keys %{ $pat_item->[NODE_PATTERN_SUBNODES] };
			$re_text .= '('.join('|', map "\Q$_\E", reverse @keys).')';
			$pat_item->[NODE_PATTERN_REGEX]= qr/$re_text/;
			
			# Find every case of a longer string which also has a prefix, and record the fallback
			my %seen;
			for my $key (@keys) {
				# Recursive to sub-nodes
				$self->_compile_tree($pat_item->[NODE_PATTERN_SUBNODES]{$key}, $key);
				$seen{$key}++;
				
				# then check for prefixes, to set up backtracking linked list
				for (map substr($key, 0, $_), reverse 1..length($key)-1) {
					if ($seen{$_}) {
						$pat_item->[NODE_PATTERN_SUBNODES]{$key}[NODE_BACKTRACK]= $_;
						last;
					}
				}
			}
		}
		# else its a wildcard.  but '' is a special case of '**'
		elsif ($pattern eq '') {
			$pat_item->[NODE_PATTERN_REGEX]= qr/^()$/;
		}
		else {
			# convert pattern to regex
			# \Q\E add escapes to "/", which is inconvenient below, so gets handled specifically here
			my @parts= split m{ ( \*\*? | / ) }x, $pattern;
			my $re_text= '^'.join('', map { $_ eq '/'? '/' : $_ eq '*'? '([^/]+)' : $_ eq '**'? '(.*?)' : "\Q$_\E" } @parts).'$';
			# WHEE!  Processing regular expressions with regular expressions!
			# "/**/" needs to match "/" and "/**" needs to match ""
			$re_text =~ s, / \( \. \* \? \) ( / | \$ ) ,(?|/(.*?)|())$1,xg;
			# If prefix ends with '/', then "**/" can also match ""
			$re_text =~ s,\^ \( \. \* \? \) / ,(?|(.*?)/|()),x
				if substr($prefix, -1) eq '/';
			$pat_item->[NODE_PATTERN_REGEX]= qr/$re_text/;
		}
	}
	$node->[NODE_COMPILED]= 1;
}

sub _search_tree {
	my ($self, $node, $path, $callback, $captures)= @_;
	$self->_compile_tree($node) unless $node->[NODE_COMPILED];
	my $actions;
	# Step 1, quickly dispatch any static path, or exact-matching wildcard prefix
	$DEBUG->("test '$path' vs constant (".join(', ', map "'$_'", keys %{$node->[NODE_LEAF_PATHS]}).')') if $DEBUG;
	if ($node->[NODE_LEAF_PATHS] && ($actions= $node->[NODE_LEAF_PATHS]{$path})) {
		$DEBUG->("  checking ".@$actions." actions") if $DEBUG;
		$callback->($_, $captures) && return 1
			for @$actions;
	}
	# Step 2, check for a path that we can capture a portion of, or otherwise match with a regex.
	# Node contains a list of patterns, each with a regex, an optional subtree, and optional actions.
	# $opt: [ qr/.../, \%subtree, @actions ];
	for my $pat (@{$node}[NODE_FIRST_PATTERN..$#$node]) {
		$DEBUG->("test '$path' vs pattern ".$pat->[NODE_PATTERN_REGEX]) if $DEBUG;
		if (my @cap= ($path =~ $pat->[NODE_PATTERN_REGEX])) {
			# If the option has sub-paths, then descend into one of those
			if ($pat->[NODE_PATTERN_SUBNODES]) {
				my $prefix= pop @cap;
				my $from_pos= $-[-1];
				# The regex captures the longest prefix, but there might be shorter prefixes
				# that yield a match.  The prefixes are recorded as ->[NODE_BACKTRACK], so
				# loop through them like a linked list.
				while (defined $prefix) {
					my $subnode= $pat->[NODE_PATTERN_SUBNODES]{$prefix}
						or die "BUG: invalid path tree (no '$prefix' subtree when '$path' matches ".$pat->[NODE_PATTERN_REGEX].")";
					my $remainder= substr($path, $from_pos+length($prefix));
					$DEBUG->((@cap? "captured ".join('', map "'$_' ", @cap).", " : '')."descend into '$prefix'") if $DEBUG;
					$self->_search_tree($subnode, $remainder, $callback, [ @$captures, @cap ]) && return 1;
					
					$DEBUG->("backtrack to ".($subnode->[NODE_BACKTRACK]//'previous node')) if $DEBUG;
					$prefix= $subnode->[NODE_BACKTRACK];
				}
			}
			# Else the regex does not represent a prefix, and we have finished
			# a match, and need to check all the actions.
			else {
				@cap= ( @$captures, @cap );
				$DEBUG->("  checking ".@{$pat->[NODE_PATTERN_ACTIONS]}." actions") if $DEBUG;
				$callback->($_, \@cap) && return 1
					for @{$pat->[NODE_PATTERN_ACTIONS]};
			}
		}
	}
	# No match, but might need to backtrack to a different wildcard from caller
	return undef;
}

1;

=head1 SEE ALSO

Was it worth publishing this module?  Despite the massive number of similar-goal
modules on CPAN, this module actually fills a niche that wasn't already solved:
using an B<optimized algorithm> (not brute force) to match one path against
multiple patterns and B<return all the matches> while also B<ranking them> by
which is the "best" match.  It also B<doesn't depend on a larger framework> to
operate.  This is also one of the only modules that can act as a path router
as well as a B<generic shell-glob comparator>.

Here are the others you can find on CPAN:

=over 20

=item L<Text::Glob>

Match a string against a regex compiled from a shell-glob pattern.  Only compares one pattern
at a time.

=item L<FastGlob>

Match a shell-glob pattern against files in the real filesystem.

=item L<URI::Router>

Claims to be the fastest of all HTTP path routers.  Appears to use the best algorithm of any
module.  Requires XS.  Can't be used for generic shell glob tests.

=item L<Path::Map>

Maps a URL to a matching handler.  Small, fast, few dependencies.  Supports named-captures.
Doesn't support duplicate paths or content after wildcard.

=item L<Router::PathInfo>

Dispatch a URL to a set of rules.  Based on Plack; supports PATH_INFO and REQUEST_METHOD
matching, and filesystem lookup of static files.  Optimized by cache and tree lookup.

=item L<Path::AttrRouter>

Match a path to routes defined in Controllers via method attributes.  Based on Mouse.
Optimized by tree lookup.

=item L<Path::Pygmy>

Match a path to a route and vice versa.  Supports named captures, and reverse mapping.
No dependencies.  Optimized by tree lookup.  Does not support multiple matches or match by
http-method.

=item L<Path::Router>

Dispatch a URL by comparing to every rule in a list.  Supports named-captures and Moose types.
Based on Moo.  Optimized by code generation (but still C<< O(n) >> ).

=item L<HTTP::Router>

Match a HTTP method and path to a set of rules.  Based on Hash::AsObject.  Minor optimization
using path depth.

=item L<Path::Dispatcher>

Dispatch a URL by comparing to every rule in a list.  Large number of matching options.
Based on Moo and Type::Tiny.  No optimization.

=item L<Router::Simple>

Match a PSGI request to a set of rules.  Based on Class::Accessor::Lite.  No optimization.

=item L<Router::Dumb>

Match a path to a set of rules.  Based on Moose.  Minor optimization using path depth.

=item L<Router::R3>

Match a path to a set of rules.  Extremely fast, via prefix-trie in XS.  Supports string and
digit captures.

=item L<Router::XS>

Match a path to a set of rules.  Extremely fast, via prefix-trie in XS.  Faster than Router::R3
but uses a global route list.

=back

