package Path::Match;
use strict;
use warnings;
require Carp;
require Scalar::Util;
our $DEBUG= undef;#sub { use DDP; warn join(' ', map +(ref? &np($_) : $_), @_)."\n"; };
use 5.026;
use Time::HiRes 'time';
use Moo;
BEGIN {
	unless (eval { require Sub::Util; Sub::Util->import('set_subname','subname'); 1 }) {
		*set_subname= sub { $_[1] };
		*subname= sub { "(Install Sub::Util for better diagnostics)" };
	}
	{ package Path::Match::_Matcher;
		use overload '""' => sub { Path::Match::subname(shift) };
	}
}

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

=head1 CONSTRUCTOR

=head2 new

  $path_match= Path::Match->new( %attributes );
                       ...->new( \%attributes );
                       ...->new( \@patterns, %attributes );

The constructor takes a hashref or key/value list of attributes.  For convenience, you may pass
an arrayref of the L</patterns> attribute as the only parameter.

=cut

sub new {
	my $class= shift;
	my $self= @_ >= 1 && ref $_[0] eq 'ARRAY'? { patterns => @_ }
		: ref $_[0] eq 'HASH'? { %{$_[0]} }
		: { @_ };
	$self->{syntax}{separator}= delete $self->{sep} if exists $self->{sep};
	$self->{syntax}{expansion}= delete $self->{exp} if exists $self->{exp};
	$self->{syntax}{escape}= delete $self->{esc} if exists $self->{esc};
	$self->{syntax}{anychar}= delete $self->{any} if exists $self->{any};
	bless $self, $class;
	$self->set_syntax($self->{syntax} ||= {});
	$self->set_patterns($self->{patterns} ||= []);
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
	$self->{patterns}= $p;
#	$self->{_trie}= {};
#	$self->_add_patterns_to_trie;
	$self;
}

=head2 syntax

A collection of options describing the grammar of the shell-glob patterns.

Using the defaults as an example:

  { separator   => '/',
    expansion   => [ '{', ',', '}' ],
    charset     => [ '[', ']' ],
    anychar     => '?',
    escape      => '\\',
    part_wild   => '*',
    path_wild   => '**',
  }

Each component above may be a string or a regex-ref.  If the field is not given, it receives
the default above.  If the field is set to C<undef>, that part of the syntax is disabled.
(You can't disable the path_separator, as that's fundamental to the algorithm)

=head2 set_syntax

Same as C<< $obj->syntax($new_syntax); >>

Be sure to set the value of the whole syntax attribute rather than making changes to the
internals of the current attribute, or else it won't trigger a re-compile.

=cut

our %syntax_defaults= (
	separator   => '/',
	escape      => '\\',
	expansion   => [ '{', ',', '}' ],
	charset     => [ '[', ']' ],
	anychar     => '?',
	part_wild   => '*',
	path_wild   => '**',
);

sub syntax {
	$_[0]->set_syntax($_[1]) if @_ > 1;
	$_[0]{syntax};
}
sub set_syntax {
	my ($self, $syn)= @_;
	my @unknown= grep !exists $syntax_defaults{$_}, keys %$syn;
	Carp::carp("Unknown syntax components ".join(', ', @unknown))
		if @unknown;
	for (keys %syntax_defaults) {
		next if exists $syn->{$_};
		$syn->{$_}= ref $syntax_defaults{$_} eq 'ARRAY'? [ @{ $syntax_defaults{$_} } ] : $syntax_defaults{$_};
	}
	Carp::croak("separator must be defined")
		unless length $syn->{separator};
	$self->{syntax}= $syn;
	delete $self->{_compiled_matchers};
	delete $self->{_glob_parser};
	$self;
}

sub _perform_expansion {
	return shift; # TODO
}
sub _translate_pwild {
	my $spec= shift;
	# Leading slash is meaningless because the "current directory" is essentially "/"
	shift @$spec if ref $spec->[0] && $spec->[0][0] eq '/';
	# Trailing slash is meaningless because there is no test for whether a path part is a directory
	pop @$spec if ref $spec->[-1] && $spec->[-1][0] eq '/';
	for (my $i=0; $i < @$spec; $i++) {
		if (ref $spec->[$i] && $spec->[$i][0] eq '**') {
			# If '**' follows a /, convert it to '/**'
			if ($i > 0 && ref $spec->[$i-1] && $spec->[$i-1][0] eq '/') {
				splice(@$spec, $i, 1);
				$spec->[--$i][0]= '/**';
			}
			# If '**' is followed by '/', convert it to '**/'
			if ($i < $#$spec && ref $spec->[$i+1] && $spec->[$i+1][0] eq '/') {
				splice(@$spec, $i+1, 1);
				$spec->[$i][0] .= '/';
			}
		}
	}
	$spec;
}
sub _glob_parser { $_[0]{_glob_parser} ||= $_[0]->_build__glob_parser }
sub _build__glob_parser {
	my $syn= shift->syntax;
	my ($sep, $esc, $any, $wild, $pwild)=
		map { !defined $_? '' : ref eq 'Regexp'? $_ : quotemeta }
		@{$syn}{qw( separator escape anychar part_wild path_wild )};
	my @exp= map { ref eq 'Regexp'? $_ : quotemeta } @{ $syn->{expansion} || [] };
	my @charset= map { ref eq 'Regexp'? $_ : quotemeta } @{ $syn->{charset} || [] };
	
	my $re= '';
	$re .= <<CODE if length $esc;
		| $esc (.) (?{ \@parts && !ref \$parts[-1]? (\$parts[-1] .= \$1) : (push \@parts, \$1) })
		| $esc \\z (?{ die "Encountered escape at end of string"; })
CODE
	$re .= <<CODE;
		| $sep (?{ push \@parts, ["\\/"] })
CODE
	$re .= <<CODE if @charset;
		# look for end-charset regex, but only after initial ^] possibility
		# bash charsets allow '!' to mean negation as well as '^'
		| $charset[0] ( [^!]? (?: $charset[1] )? .*? ) $charset[1] (?{ push \@parts, [ charset => \$1 ] })
		# If start matched but remainder didn't, forcibly die
		| $charset[0] (?{ die 'Expected $charset[1] somewhere after "'.substr(\$_, pos).'"'; })
CODE
	$re .= <<CODE if length $any;
		| $any (?{ push \@parts, [ charset => \\"*" ] })
CODE
	$re .= <<CODE if length $pwild;
		| $pwild (?{ push \@parts, ['**'] })
CODE
	$re .= <<CODE if length $wild;
		| $wild (?{ push \@parts, ['*'] })
CODE
	$re .= <<CODE if @exp;
		| $exp[0] (?{ \$do_exp= 1; push \@parts, [ exp => '{' ] })
		| $exp[1] (?{ \$do_exp= 1; push \@parts, [ exp => ',' ] })
		| $exp[2] (?{ \$do_exp= 1; push \@parts, [ exp => '}' ] })
CODE
	$re .= <<CODE;
		| (.) (?{ \@parts && !ref \$parts[-1]? (\$parts[-1] .= \$1) : (push \@parts, \$1) })
CODE
	$re =~ s/\|/ /; # remove the first '|'
	return ( eval <<CODE or die "$@" );
	sub {
		local \$_= shift if \@_;
		my \@parts;
		my \$do_exp;
		pos= 0;
		()= /\\G (?| $re )/gcx; # list context, to run all iterations
		length == pos or die "Parse error at '".substr(\$_,pos)."'";
		return map _translate_pwild(\$_), \$do_exp? _perform_expansion(\\\@parts) : \\\@parts;
	}
CODE
}

sub _parsed_glob_to_regex {
	my ($self, $spec)= @_;
	my ($sep, $esc, $any, $wild, $pwild)=
		map { !defined $_? '' : ref eq 'Regexp'? $_ : quotemeta }
		@{$self->syntax}{qw( separator escape anychar part_wild path_wild )};
	my $single_char_sep= $sep =~ /^(\\.|.)\z/;
	my $re= @$spec && ref $spec->[0] && $spec->[0][0] eq '**/'? '' :  "(?:$sep)?";
	for (@$spec) {
		if (!ref) { $re .= quotemeta($_) }
		elsif ($_->[0] eq 'charset') { $re .= '[' . $_->[1] . ']' } # TODO handle '!' and escapes
		elsif ($_->[0] eq 'any') { $re .= '.' }
		elsif ($_->[0] eq '*') { $re .= $single_char_sep? "([^$sep]+)" : "( (?: (?! $sep ) . )* )" }
		elsif ($_->[0] eq '/') { $re .= $sep }
		elsif ($_->[0] eq '**') { $re .= "(.*?)" }
		elsif ($_->[0] eq '/**') { $re .= "(?| $sep (.*?) | () \\z )" }
		elsif ($_->[0] eq '**/') { $re .= "(?| (.*?) $sep | \\A () )" }
		elsif ($_->[0] eq '/**/') { $re .= "(?| $sep () | $sep (.+?) $sep )" }
		else { die "Unhandled token ".join(',', @$_) }
	}
	$re .= "(?:$sep)?" unless @$spec && ref $spec->[-1] && $spec->[-1][0] eq '/**';
	#use DDP; &p([$spec, $re]);
	qr/$re/x;
}

=head1 METHODS

=head2 add_pattern

  $path_match->add_pattern($pattern);

Add one element to L</patterns>.

=cut

sub add_pattern {
	my ($self, $pattern)= @_;
	push @{$self->{patterns}}, $pattern;
	$self->_add_patterns_to_trie($#{$self->{patterns}});
	$self;
}

=head2 matches

  my $bool= $path_match->matches( $path )

Returns true if any pattern matches the path.

=cut

sub matches {
	my ($self, $path)= @_;
	for ($path) {
		return $self->_search(($self->{_compiled_nodes} || $self->_compile)->[0], sub { 1 });
	}
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

#our $_callback;
#our @_captures;
#our @_reverse_cap;
#our $_reverse;
#sub search {
#	my ($self, $path, $callback)= @_;
#	local $_callback= $callback;
#	local @_captures= ();
#	local @_reverse_cap;
#	local $_reverse= !1;
#	for ($path) {
#		pos= 0;
#		return $self->_search(($self->{_compiled_nodes} || $self->_compile)->[0]);
#	}
#}

# Nodes test the remainder of the path vs. a list of regexes
# [
#    \@actions_ending_here,
#    $backtrack_path_key,
#    \%patterns_set, (temporary)
#    @patterns,
# ]
# pattern: [ $regex, \%subnodes, \@actions ]

sub _pattern_from_object {
	my ($self, $thing)= @_;
	return $thing unless ref $thing;
	return $thing->[0] if ref $thing eq 'ARRAY';
	return $thing->{pattern} if ref $thing eq 'HASH';
	return $thing->pattern
		if Scalar::Util::blessed($thing) && $thing->can('pattern');
	Carp::croak("Don't know how to get a pattern from '$thing'");
}

#sub _add_patterns_to_trie {
#	my ($self, $from_idx)= @_;
#	$from_idx ||= 0;
#	my $patterns= $self->patterns;
#	my $trie= $self->{_trie} ||= {};
#	delete $self->{_compiled_nodes};
#	for my $idx ($from_idx .. $#$patterns) {
#		my $pattern_text= $self->_pattern_from_object($patterns->[$idx]);
#		pos $pattern_text = 0;
#		$self->_add_pattern_to_trie($trie, $idx) for $pattern_text;
#	}
#}
#
#sub _add_pattern_to_trie {
#	my ($self, $node, $pat_idx)= @_;
#	$node ||= $self->{_trie} ||= {};
#	while (length > pos) {
#		# Descend into trie for each character of constant portion
#		# Slashes adjacent to stars are handled specially.
#		$node= ($node->{$1}//={})
#			while m,\G( [^*/] | / (?! \* \* ) ),gcx;
#		# Path wildcard is lower priority than others, so separate from '*' wilds.
#		# Capture slashes on either side of the '**'.
#		if (m,\G( /? \*\* /? ),gcx) {
#			my $globstar= $1;
#			# When starts with wildcard but ends with literal, change strategy
#			# to match from end of string backward.  Also allow a single-star
#			# component, because that's less guesswork than a globstar.
#			if (substr($_, -3) !~ / \*\* \/? \Z/x) {
#				my $remain= reverse $globstar.substr($_, pos);
#				pos $remain = 0;
#				return $self->_add_pattern_to_trie($node->{reverse}//={}, $pat_idx) for $remain;
#			}
#			else {
#				my $sub_trie= {};
#				push @{ $node->{wild_path} }, [ $globstar, $sub_trie ];
#				$node= $sub_trie;
#			}
#		}
#		# Regular wildcards only continue until the next '/', or '**'
#		elsif(m,\G( \* (?: [^*/] | [*] (?! [*] ) )* ),gcx) {
#			$node= ($node->{wild}{$1}//={});
#		}
#		else { pos == length or die "bug" }
#	}
#	# At end of string, mark this as a terminal node
#	push @{ $node->{terminals} }, $pat_idx;
#}
#
#sub _compile {
#	my ($self)= @_;
#	$self->{_compiled_nodes}= [];
#	$self->_compile_node($self->{_trie}, '', matchers => 1);
#	return $self->{_compiled_nodes};
#}
#
#sub _compile_node {
#	my ($self, $node, $path, %options)= @_;
#	my @trie_paths= grep length == 1, keys %$node;
#	my ($terminals, $wild, $reverse, $wild_path)= @{$node}{qw( terminals wild reverse wild_path )};
#	# Initialize the new compiled node and add it to the list.
#	# This list index is used to refer to nodes from the regex and from other nodes.
#	my $idx= @{ $self->{_compiled_nodes} };
#	my $cnode= {
#		idx => $idx,     # the index of this compiled node in the list
#		path => $path,   # the pattern matched so far, for debugging aid
#		($options{backtrack}? (backtrack => $options{backtrack}) : ()),
#		$options{is_reverse}? (is_reverse => 1) : (),
#	};
#	$cnode->{is_leaf}= 1
#		if $terminals && !@trie_paths && !$wild && !$reverse && !$wild_path;
#	$cnode->{is_backtrack_dest}= 1
#		if length($path) && ($wild || $reverse || $wild_path);
#	$cnode->{terminals}= [ @$terminals ]
#		if $terminals;
#	push @{ $self->{_compiled_nodes} }, $cnode;
#
#	push @{$cnode->{matchers}}, $self->_compile_trie_matcher($node, $path, cnode => $cnode)
#		if $options{matchers} && @trie_paths;
#
#	if ($wild) {
#		# Make a regex out of each of the wild portions, longest pattern first
#		for (sort { length $b <=> length $a } keys %$wild) {
#			# Replace '*' with regex notation for not matching the path separator
#			# But, a lone '*' must match at least one character
#			my $re_text= $_ eq '*'? '( [^\/]+ )'
#				: join '( [^\/]* )', map quotemeta, split /\*/, $_, -1;
#			# Follow this regex with the rest of a trie-construction from this node
#			push @{$cnode->{matchers}},
#				$self->_compile_trie_matcher($wild->{$_}, $path . $_, prefix => $re_text, origin => $cnode->{idx});
#		}
#	}
#	if ($reverse) {
#		$cnode->{reverse}= $self->_compile_node($reverse, '',
#			%options,
#			matchers => 1,
#			backtrack => undef,
#			is_reverse => !$cnode->{is_reverse},
#			origin => $cnode->{idx}
#		);
#	}
#	if ($wild_path) {
#		for (@$wild_path) {
#			my ($globstar, $nextnode)= @$_;
#			# globstar **/ at the start of the pattern may match an empty string
#			# globstar /** at the end of the pattern may match an empty string
#			# globstar /**/ may match a single slash, two slashes, or two slashes with arbitrary chars between
#			# In all cases, the capture should not include the leading or trailing slash
#			my $prefix= ($globstar eq '**/')? '(?| ^() | (.*?) \/ )'
#				: ($globstar eq '/**')? '(?| \/ (.*?) | ()\Z )'
#				: ($globstar eq '/**/')? '(?| ^() | \/()\/? | \/(.+?)\/ | ()\Z )'
#				: '(.*?)';
#			push @{$cnode->{unanchored}}, $self->_compile_trie_matcher($nextnode, $path.$globstar, prefix => $prefix, origin => $cnode->{idx});
#		}
#	}
#	return $cnode;
#}
#
#our $_next_node;
#sub _compile_trie_matcher {
#	my ($self, $node, $path, %options)= @_;
#	my $re= $self->_compile_trie_regex_text($node, $path, %options);
#	$re= "\\G $re" unless $options{unanchored};
#	my $matcher= eval "qr/$re/x" or die "$@";
#	#return bless set_subname("_Matcher /$re/x", $matcher), 'Path::Match::_Matcher';
#}
#
#sub _compile_trie_regex_text {
#	my ($self, $node, $path_so_far, %options)= @_;
#	# On trie nodes that refer to a single character, descend to the next node
#	# and add the character to the start of the regex.
#	my $re= delete $options{prefix};
#	my $cnode= delete $options{cnode};
#	my @keys= keys %$node;
#	while (@keys == 1 && 1 == length $keys[0]) {
#		$re .= quotemeta $keys[0];
#		$path_so_far .= $keys[0];
#		$node= $node->{$keys[0]};
#		$cnode= undef;
#		@keys= keys %$node;
#	}
#	# Create a compild node for any trie node that ends a pattern, or has
#	# wildcards branching from it.
#	if (grep length > 1, @keys) {
#		$cnode ||= $self->_compile_node($node, $path_so_far, %options);
#		# Deeper nodes will need to backtrack to this one if it has any wildcards branching off of it
#		$options{backtrack}= $cnode->{idx}
#			if $cnode->{is_backtrack_dest};
#	}
#	$re .= " \\Z" if $cnode && $cnode->{is_leaf};
#	# Convert each trie sub-node into a regex fragment
#	my @alt= map $self->_compile_trie_regex_text($node->{$_}, $path_so_far.$_, %options, prefix => quotemeta($_)),
#		grep length == 1, @keys;
#	# If further processing needed here, add a stopping point
#	push @alt, '(?{ $_next_node='.$cnode->{idx}.' })' if $cnode && length $re;
#	# prevent backtracking
#	$re .= @alt > 1? ' (?> ' . join(' | ', @alt) . ' )' : ' '.$alt[0];
#	return $re;
#}
#
#sub _search {
#	my ($self, $node, $callback, $reverse)= @_;
#	while (1) {
#		$DEBUG->("node $node->{path}: used '".substr($_, 0, pos)."', remaining '".substr($_, pos)."'") if $DEBUG;
#		if (length == pos) {
#			for (@{$node->{terminals}}) {
#				$DEBUG->("Matched pattern $_: $self->{patterns}[$_]") if $DEBUG;
#				my $ret= $_callback->($self->{patterns}[$_], [@_captures, @_reverse_cap]);
#				return $ret if $ret;
#			}
#		}
#		my $pos= pos;
#		if (defined $node->{matchers}) {
#			for my $m (@{$node->{matchers}}) {
#				$DEBUG->("node $node->{path} check matcher $m") if $DEBUG;
#				if (/$m/gc) {
#					local @_captures= ( @_captures, @{^CAPTURE} )
#						if !$_reverse && @{^CAPTURE};
#					local @_reverse_cap= ( (map scalar reverse($_), reverse @{^CAPTURE} ), @_reverse_cap )
#						if $_reverse && @{^CAPTURE};
#					my $ret= $self->_search($self->{_compiled_nodes}[$_next_node]);
#					return $ret if $ret;
#					pos= $pos; # reset pos to backtrack and look for other wild matches
#				}
#			}
#		}
#		if (defined $node->{reverse}) {
#			$DEBUG->("node $node->{path}: trying reverse match") if $DEBUG;
#			local $_reverse= !$_reverse;
#			my $rev_subject= reverse substr($_, pos);
#			pos($rev_subject)= 0;
#			my $ret= $self->_search($node->{reverse}) for $rev_subject;
#			return $ret if $ret;
#		}
#		if (defined $node->{unanchored}) {
#			for my $m (@{$node->{unanchored}}) {
#				$DEBUG->("node $node->{path} check unanchored matcher $m") if $DEBUG;
#				if (/$m/gc) {
#					local @_captures= ( @_captures, @{^CAPTURE} )
#						if !$_reverse && @{^CAPTURE};
#					local @_reverse_cap= ( (map scalar reverse($_), reverse @{^CAPTURE} ), @_reverse_cap )
#						if $_reverse && @{^CAPTURE};
#					my $ret= $self->_search($self->{_compiled_nodes}[$_next_node]);
#					return $ret if $ret;
#					pos= $pos; # reset pos to backtrack and look for other wild matches
#				}
#			}
#		}
#		last unless $node->{backtrack};
#		$DEBUG->("Backtrack to $node->{backtrack}") if $DEBUG;
#		my $btnode= $self->{_compiled_nodes}[$node->{backtrack}];
#		# Backtracking only happens between non-wildcard alternatives, so can
#		# just compare the length of the nodes' patterns to know how far back
#		# on the input to seek.
#		pos($_) -= length($node->{path}) - length($btnode->{path});
#		$node= $btnode;
#	}
#	return !1;
#}

sub match_best_first { 1 }

sub _sort_rules {
	my ($self, $rules)= @_;
	my %orig_order= map +( 0+$rules->[$_] => $_ ), 0..$#$rules;
	my $rank= sub {
		my $n_literal= grep !ref, @{$_[0][1]};
		my $n_wild= grep +( ref && (index($_->[0],'*') >= 0) ), @{$_[0][1]};
		my $n_pwild= grep +( ref && (index($_->[0],'**') >= 0) ), @{$_[0][1]};
		return !$n_wild? 1 / ($n_literal+1)
			: !$n_pwild? 2 / ($n_literal+1)
			: 3 / ($n_literal+1);
	};
	@$rules= sort { $rank->($a) <=> $rank->($b) or $orig_order{0+$a} <=> $orig_order{0+$b} } @$rules;
	$rules;
}

sub compile { shift->_compile }

sub _compile {
	my $self= shift;
	my @rules;
	for my $p (@{ $self->patterns }) {
		my $glob_text= $self->_pattern_from_object($p);
		push @rules, map [ $glob_text, $_, $p ], $self->_glob_parser->($glob_text);
	}
	$self->_sort_rules(\@rules) if $self->match_best_first;
	my @matchers= map {
		my $regex_text= $self->_parsed_glob_to_regex($_->[1]);
		$self->_compile_simple_matcher($regex_text, $_->[2], 0)
	} @rules;
	#use DDP; &p(\@matchers);
	$self->{_matchers}= \@matchers;
}

sub search {
	my ($self, $subject, $callback)= @_;
	return $self->_search($callback, [], [], $self->{_matchers} // $self->compile)
		for $subject;
}

use constant {
	_SELF     => 0,
	_CALLBACK => 1,
	_CAPTURE  => 2,
	_RCAPTURE => 3,
	_MATCHERS => 4,
};

sub _search {
	#my ($self, $callback, $capture, $rev_cap, $matchers)= @_;
	my $start= pos;
	my $max_cap= $#{$_[_CAPTURE]};
	for my $matcher (@{$_[_MATCHERS]}) {
		return 1 if &$matcher;
		pos= $start;
		$#{$_[_CAPTURE]}= $max_cap;
	}
	return 0;
}

sub _rsearch {
	my $start= pos;
	my $max_rcap= $#{$_[_RCAPTURE]};
	for my $matcher (@{$_[_MATCHERS]}) {
		return 1 if &$matcher;
		pos= $start;
		$#{$_[_RCAPTURE]}= $max_rcap;
	}
	return 0;
}

sub _compile_simple_matcher {
	my ($self, $regex, $pattern, $is_reverse)= @_;
	my $cap= !$is_reverse? '[ @{ $_[_CAPTURE] }, @{^CAPTURE} ]'
		: '[ @{ $_[_CAPTURE] }, reverse @{ $_[_RCAPTURE] }, @{^CAPTURE} ]';
	my $matcher= eval "sub { /\\G $regex \\z/gcx && \$_[_CALLBACK]->(\$pattern, $cap) }" or die "$@";
	set_subname("sub { /\\G $regex \\z/gcx && \$_[_CALLBACK]->(\$pattern, $cap) }", $matcher);
	bless $matcher, 'Path::Match::_Matcher';
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

