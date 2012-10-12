package SPARQL::Parser;

use 5.010;
use strict;
use warnings;

use SPARQL::AST;
use Data::Dumper;
use RDF::Trine::Error;
use SPARQL::Constants;
use SPARQL::Lexer;
use SPARQL::Token;
use URI::NamespaceMap;
use URI::Namespace;
use RDF::Trine::Error;
use RDF::Trine;

sub new {
	my $class	= shift;
	my %args	= @_;
	return bless({ %args, stack => [] }, $class);
}

=item C<< parse ( $base_uri, $rdf, \&handler ) >>

Parses the C<< $data >>, using the given C<< $base_uri >>. Calls the
C<< triple >> method for each RDF triple parsed. This method does nothing by
default, but can be set by using one of the default C<< parse_* >> methods.

=cut

sub parse {
	my $self	= shift;
	my $string	= shift;
	open(my $fh, '<:encoding(UTF-8)', \$string);
	my $l	= SPARQL::Lexer->new($fh);
	$self->_parse($l);
}

=item C<< parse_file ( $base_uri, $fh, $handler ) >>

Parses all data read from the filehandle or file C<< $fh >>, using the given
C<< $base_uri >>. If C<< $fh >> is a filename, this method can guess the
associated parse. For each RDF statement parses C<< $handler >> is called.

=cut

sub parse_file {
	my $self	= shift;
	my $fh		= shift;

	unless (ref($fh)) {
		my $filename	= $fh;
		undef $fh;
		unless ($self->can('parse')) {
			my $pclass = $self->guess_parser_by_filename( $filename );
			$self = $pclass->new() if ($pclass and $pclass->can('new'));
		}
		open( $fh, '<:encoding(UTF-8)', $filename ) or throw RDF::Trine::Error::ParserError -text => $!;
	}
	
	my $l	= SPARQL::Lexer->new($fh);
	$self->_parse($l);
}

sub _parse {
	my $self	= shift;
	my $l		= shift;
	$l->check_for_bom;
	my $t = $self->_next_token($l);
	return unless $t;
	my $q	= $self->_Query($l, $t);
	$t	= $self->_next_token($l);
	if ($t) {
		$self->throw_error($t, $l, sprintf("Expecting end of query but got %s", decrypt_constant($t->type)));
	}
	return $q;
}

################################################################################

sub _unget_token {
	my $self	= shift;
	my $t		= shift;
	push(@{ $self->{ stack } }, $t);
}

sub _next_token {
	my $self	= shift;
	my $l		= shift;
	if (scalar(@{ $self->{ stack } })) {
		my $t	= pop(@{ $self->{ stack } });
# 		Carp::cluck 'again>>> ' . decrypt_constant($t->type) . "\n";
		return $t;
	}
	Carp::cluck "no lexer defined" unless $l;
	my $t	= $l->get_token;
	return unless ($t);
# 	Carp::cluck '>>> ' . decrypt_constant($t->type) . "\n";
	return $t;
}

sub _peek_token {
	my $self	= shift;
	my $l		= shift;
	my $t		= $self->_next_token($l);
	$self->_unget_token($t);
	return $t;
}

sub _get_token_type {
	my $self	= shift;
	my $l		= shift;
	my $type	= shift;
	my $t		= $self->_next_token($l);
	return unless ($t);
	unless ($t->type eq $type) {
		$self->throw_error($t, $l, sprintf("Expecting %s but got %s", decrypt_constant($type), decrypt_constant($t->type)));
	}
	return $t;
}

################################################################################


# [1]  	QueryUnit	  ::=  	Query
# [2]  	Query	  ::=  	Prologue ( SelectQuery | ConstructQuery | DescribeQuery | AskQuery ) ValuesClause
sub _Query {
	my $self	= shift;
	my $l		= shift;
	my $t		= shift;
	my $type	= $t->type;
	
# [4]  	Prologue	  ::=  	( BaseDecl | PrefixDecl )*
	my $base;
	my $namespaces	= URI::NamespaceMap->new({});
	while ($t->type == KEYWORD and ($t->value =~ /^(?:BASE|PREFIX)$/)) {
		if ($t->value eq 'BASE') {
			$t			= $self->_get_token_type($l, IRI);
			$base	= $t->value;
# 			warn "BASE: $base\n";
		} else {
			$t			= $self->_get_token_type($l, PREFIXNAME);
			my $ns	= $t->value;
			$t			= $self->_get_token_type($l, IRI);
			my $uri	= $t->value;
			$namespaces->add_mapping( $ns => $uri );
# 			warn "PREFIX $ns: <$uri>\n";
		}
		$t		= $self->_next_token($l);
	}
	
	unless ($t->type == KEYWORD) {
		$self->throw_error($t, $l, "Expecting query form (SELECT, CONSTRUCT, DESCRIBE or ASK)");
	}
	my $form	= $t->value;
	if ($form eq 'SELECT') {
		return $self->_SelectQuery($l, base => $base, namespaces => $namespaces);
	} elsif ($form eq 'CONSTRUCT') {
	} elsif ($form eq 'DESCRIBE') {
	} elsif ($form eq 'ASK') {
		return $self->_AskQuery($l, base => $base, namespaces => $namespaces);
	}
	
	$self->throw_error($t, $l, "Expecting query form but got $form");
	
	return;
}



# [3]  	UpdateUnit	  ::=  	Update
# [4]  	Prologue	  ::=  	( BaseDecl | PrefixDecl )*
# [5]  	BaseDecl	  ::=  	'BASE' IRIREF
# [6]  	PrefixDecl	  ::=  	'PREFIX' PNAME_NS IRIREF
# [7]  	SelectQuery	  ::=  	SelectClause DatasetClause* WhereClause SolutionModifier
sub _SelectQuery {
	my $self	= shift;
	my $l		= shift;
	my %args	= @_;
	
	my $select	= $self->_SelectClause($l);
	# XXX DatasetClause
	my $algebra	= $self->_WhereClause($l, %args);
	my %mods	= $self->_SolutionModifier($l, %args);

	if (scalar(@{ $select->{extend} || [] })) {
		...
# 		$algebra	= SPARQL::AST->new( type => 'Extend', children => [$algebra], value => [@vars] );
	}
	
	if (scalar(@{ $select->{project} })) {
		my @vars	= @{ $select->{project} };
		$algebra	= SPARQL::AST->new( type => 'Project', children => [$algebra], value => [@vars] );
	}
	
	if ($select->{options}{distinct}) {
		$algebra	= SPARQL::AST->new( type => 'Distinct', children => [$algebra] );
	} elsif ($select->{options}{reduced}) {
		$algebra	= SPARQL::AST->new( type => 'Reduced', children => [$algebra] );
	}
	
	if (exists $mods{offset}) {
		$algebra	= SPARQL::AST->new( type => 'Offset', children => [$algebra], value => $mods{offset} );
	}
	if (exists $mods{limit}) {
		$algebra	= SPARQL::AST->new( type => 'Limit', children => [$algebra], value => $mods{limit} );
	}
	
	return $algebra;
}


# [8]  	SubSelect	  ::=  	SelectClause WhereClause SolutionModifier ValuesClause
# [9]  	SelectClause	  ::=  	'SELECT' ( 'DISTINCT' | 'REDUCED' )? ( ( Var | ( '(' Expression 'AS' Var ')' ) )+ | '*' )
sub _SelectClause {
	my $self	= shift;
	my $l		= shift;
	my $t		= $self->_next_token($l);
	
	my %options;
	my @project;
	if ($t->type == KEYWORD) {
		if ($t->value eq 'DISTINCT') {
			$t		= $self->_next_token($l);
			$options{distinct}++;
		} elsif ($t->value eq 'REDUCED') {
			$t		= $self->_next_token($l);
			$options{reduced}++;
		}
	}
	
	while ($t->type == VAR) {
		push(@project, $t->value);
		$t		= $self->_next_token($l);
	}
	
	$self->_unget_token($t);
	
	
	return { options => \%options, project => \@project };
}

# [10]  	ConstructQuery	  ::=  	'CONSTRUCT' ( ConstructTemplate DatasetClause* WhereClause SolutionModifier | DatasetClause* 'WHERE' '{' TriplesTemplate? '}' SolutionModifier )
# [11]  	DescribeQuery	  ::=  	'DESCRIBE' ( VarOrIri+ | '*' ) DatasetClause* WhereClause? SolutionModifier

# [12]  	AskQuery	  ::=  	'ASK' DatasetClause* WhereClause SolutionModifier
sub _AskQuery {
	my $self	= shift;
	my $l		= shift;
	my %args	= @_;
	
	# XXX DatasetClause
	my $algebra	= $self->_WhereClause($l, %args);
	my %mods	= $self->_SolutionModifier($l, %args);
	return SPARQL::AST->new( type => 'Ask', children => [$algebra] );
}



# [13]  	DatasetClause	  ::=  	'FROM' ( DefaultGraphClause | NamedGraphClause )
# [14]  	DefaultGraphClause	  ::=  	SourceSelector
# [15]  	NamedGraphClause	  ::=  	'NAMED' SourceSelector
# [16]  	SourceSelector	  ::=  	iri
# [17]  	WhereClause	  ::=  	'WHERE'? GroupGraphPattern
sub _WhereClause {
	my $self	= shift;
	my $l		= shift;
	my %args	= @_;
	my $t		= $self->_next_token($l);
	if ($t->type == KEYWORD and $t->value eq 'WHERE') {
		$t		= $self->_next_token($l);
	}
	$self->_unget_token($t);
	
	return $self->_GroupGraphPattern($l, %args);
}

# [18]  	SolutionModifier	  ::=  	GroupClause? HavingClause? OrderClause? LimitOffsetClauses?
sub _SolutionModifier {
	my $self	= shift;
	my $l		= shift;
	my %args	= @_;
	
	my %mods;
	# XXX
	my $t		= $self->_peek_token($l);
	return unless ($t);
	if ($t->type == KEYWORD and $t->value =~ /^(LIMIT|OFFSET)$/) {
		my %lo	= $self->_LimitOffsetClauses($l, %args);
		@mods{ keys %lo }	= values %lo;
	}
	return %mods;
}

# [19]  	GroupClause	  ::=  	'GROUP' 'BY' GroupCondition+
# [20]  	GroupCondition	  ::=  	BuiltInCall | FunctionCall | '(' Expression ( 'AS' Var )? ')' | Var
# [21]  	HavingClause	  ::=  	'HAVING' HavingCondition+
# [22]  	HavingCondition	  ::=  	Constraint
# [23]  	OrderClause	  ::=  	'ORDER' 'BY' OrderCondition+
# [24]  	OrderCondition	  ::=  	 ( ( 'ASC' | 'DESC' ) BrackettedExpression ) | ( Constraint | Var )
# [25]  	LimitOffsetClauses	  ::=  	LimitClause OffsetClause? | OffsetClause LimitClause?
sub _LimitOffsetClauses {
	my $self	= shift;
	my $l		= shift;
	my %args	= @_;
	
	my %mods;
	my $t		= $self->_get_token_type($l, KEYWORD);
	if ($t->value eq 'LIMIT') {
		my $limit	= $self->_next_token($l);
		unless ($limit) {
			$l->throw_error("Expecting LIMIT value");
		}
		$mods{limit}	= $limit->value;
		$t				= $self->_peek_token($l);
		unless ($t) {
			$l->throw_error("Expecting OFFSET value");
		}
		if ($t->type == KEYWORD and $t->value eq 'OFFSET') {
			$self->_get_token_type($l, KEYWORD);
			my $offset	= $self->_next_token($l);
			$mods{offset}	= $offset->value;
		}
	} elsif ($t->value eq 'OFFSET') {
		my $offset	= $self->_next_token($l);
		unless ($offset) {
			$l->throw_error("Expecting OFFSET value");
		}
		$mods{offset}	= $offset->value;
		$t				= $self->_peek_token($l);
		unless ($t) {
			$l->throw_error("Expecting LIMIT value");
		}
		if ($t->type == KEYWORD and $t->value eq 'LIMIT') {
			$self->_get_token_type($l, KEYWORD);
			my $limit	= $self->_next_token($l);
			$mods{limit}	= $limit->value;
		}
	} else {
		$self->throw_error($t, $l, sprintf("Expecting LIMIT or OPTIONAL but got %s", $t->value));
	}
	
	return %mods;
}

# [26]  	LimitClause	  ::=  	'LIMIT' INTEGER
# [27]  	OffsetClause	  ::=  	'OFFSET' INTEGER
# [28]  	ValuesClause	  ::=  	( 'VALUES' DataBlock )?
# [29]  	Update	  ::=  	Prologue ( Update1 ( ';' Update )? )?
# [30]  	Update1	  ::=  	Load | Clear | Drop | Add | Move | Copy | Create | InsertData | DeleteData | DeleteWhere | Modify
# [31]  	Load	  ::=  	'LOAD' 'SILENT'? iri ( 'INTO' GraphRef )?
# [32]  	Clear	  ::=  	'CLEAR' 'SILENT'? GraphRefAll
# [33]  	Drop	  ::=  	'DROP' 'SILENT'? GraphRefAll
# [34]  	Create	  ::=  	'CREATE' 'SILENT'? GraphRef
# [35]  	Add	  ::=  	'ADD' 'SILENT'? GraphOrDefault 'TO' GraphOrDefault
# [36]  	Move	  ::=  	'MOVE' 'SILENT'? GraphOrDefault 'TO' GraphOrDefault
# [37]  	Copy	  ::=  	'COPY' 'SILENT'? GraphOrDefault 'TO' GraphOrDefault
# [38]  	InsertData	  ::=  	'INSERT DATA' QuadData
# [39]  	DeleteData	  ::=  	'DELETE DATA' QuadData
# [40]  	DeleteWhere	  ::=  	'DELETE WHERE' QuadPattern
# [41]  	Modify	  ::=  	( 'WITH' iri )? ( DeleteClause InsertClause? | InsertClause ) UsingClause* 'WHERE' GroupGraphPattern
# [42]  	DeleteClause	  ::=  	'DELETE' QuadPattern
# [43]  	InsertClause	  ::=  	'INSERT' QuadPattern
# [44]  	UsingClause	  ::=  	'USING' ( iri | 'NAMED' iri )
# [45]  	GraphOrDefault	  ::=  	'DEFAULT' | 'GRAPH'? iri
# [46]  	GraphRef	  ::=  	'GRAPH' iri
# [47]  	GraphRefAll	  ::=  	GraphRef | 'DEFAULT' | 'NAMED' | 'ALL'
# [48]  	QuadPattern	  ::=  	'{' Quads '}'
# [49]  	QuadData	  ::=  	'{' Quads '}'
# [50]  	Quads	  ::=  	TriplesTemplate? ( QuadsNotTriples '.'? TriplesTemplate? )*
# [51]  	QuadsNotTriples	  ::=  	'GRAPH' VarOrIri '{' TriplesTemplate? '}'
# [52]  	TriplesTemplate	  ::=  	TriplesSameSubject ( '.' TriplesTemplate? )?

# [53]  	GroupGraphPattern	  ::=  	'{' ( SubSelect | GroupGraphPatternSub ) '}'
sub _GroupGraphPattern {
	my $self	= shift;
	my $l		= shift;
	my %args	= @_;
	$self->_get_token_type($l, LBRACE);
	
	my $t		= $self->_next_token($l);
	my @items;
	if ($t->type == KEYWORD and $t->value eq 'SELECT') {
		@items	= $self->_SubSelect($l, %args);
	} else {
		$self->_unget_token($t);
		@items	= $self->_GroupGraphPatternSub($l, %args);
	}
	
	$self->_get_token_type($l, RBRACE);
	return SPARQL::AST->new( type => 'Group', children => \@items );
}

# [54]  	GroupGraphPatternSub	  ::=  	TriplesBlock? ( GraphPatternNotTriples '.'? TriplesBlock? )*
sub _GroupGraphPatternSub {
	my $self	= shift;
	my $l		= shift;
	my %args	= @_;
	my $t		= $self->_peek_token($l);
	my @items;
	
	if ($t->type == VAR or $t->type == ANON or $t->type == LBRACKET or $t->type == LPAREN or $t->is_term) {
		push(@items, $self->_TriplesBlock($l, %args));
	}
	
	$t		= $self->_peek_token($l);
	while ($self->_token_GraphPatternNotTriples_ok($t)) {
		push(@items, $self->_GraphPatternNotTriples($l, %args));
		$t	= $self->_peek_token($l);
		if ($t->type == DOT) {
			$t	= $self->_peek_token($l);
		}
		if ($t->type == VAR or $t->type == ANON or $t->type == LBRACKET or $t->type == LPAREN or $t->is_term) {
			push(@items, $self->_TriplesBlock($l, %args));
		}
		$t	= $self->_peek_token($l);
	}
	
	
	return @items;
}

# [55]  	TriplesBlock	  ::=  	TriplesSameSubjectPath ( '.' TriplesBlock? )?
sub _TriplesBlock {
	my $self	= shift;
	my $l		= shift;
	my %args	= @_;
	
	my @items	= $self->_TriplesSameSubjectPath($l, %args);
	
	my @triples;
	my $t		= $self->_next_token($l);
	if ($t->type == DOT) {
		$t		= $self->_peek_token($l);
		if ($t->type == VAR or $t->type == ANON or $t->type == LBRACKET or $t->type == LPAREN or $t->is_term) {
			my $bgp	= $self->_TriplesBlock($l, %args);
			push(@triples, @{ $bgp->children });
		}
	} else {
		$self->_unget_token($t);
	}
	
	return SPARQL::AST->new( type => 'BGP', children => [ @items, @triples ] );
}

sub _token_Verb_ok {
	my $self	= shift;
	my $t		= shift;
	my $type	= $t->type;
	return 1 if ($type == VAR);
	return 1 if ($type == HAT);
	return 1 if ($type == LPAREN);
	return 1 if ($type == IRI or $type == PREFIXNAME);
	return 1 if ($type == KEYWORD and ($t->value eq 'DISTINCT' or $t->value eq 'A'));
	return 0;
}

sub _token_GraphPatternNotTriples_ok {
	my $self	= shift;
	my $t		= shift;
	my $type	= $t->type;
	return 1 if ($type == LBRACE);
	return 1 if ($type == KEYWORD and $t->value =~ /^(?:OPTIONAL|GRAPH|MINUS|SERVICE|FILTER|BIND|VALUES)$/);
	return 0;
}

# [56]  	GraphPatternNotTriples	  ::=  	GroupOrUnionGraphPattern | OptionalGraphPattern | MinusGraphPattern | GraphGraphPattern | ServiceGraphPattern | Filter | Bind | InlineData
sub _GraphPatternNotTriples {
	my $self	= shift;
	my $l		= shift;
	my %args	= @_;
	my $t		= $self->_peek_token($l);
	if ($t->type == KEYWORD) {
		my $v	= $t->value;
		given ($v) {
			when ('MINUS') {
				return $self->_MinusGraphPattern($l, %args);
			}
			when ('OPTIONAL') {
				return $self->_OptionalGraphPattern($l, %args);
			}
			when ('GRAPH') {
				return $self->_GraphGraphPattern($l, %args);
			}
			when ('SERVICE') {
				return $self->_ServiceGraphPattern($l, %args);
			}
			when ('FILTER') {
				return $self->_Filter($l, %args);
			}
			when ('BIND') {
				return $self->_Bind($l, %args);
			}
			when ('VALUES') {
				return $self->_InlineData($l, %args);
			}
		}
	} else {
		return $self->_GroupOrUnionGraphPattern($l, %args);
	}
	...
}

# [57]  	OptionalGraphPattern	  ::=  	'OPTIONAL' GroupGraphPattern
sub _OptionalGraphPattern {
	my $self	= shift;
	my $l		= shift;
	my %args	= @_;
	my $t		= $self->_get_token_type($l, KEYWORD);
	unless ($t->value eq 'OPTIONAL') {
		$self->throw_error($t, $l, sprintf("Expecting OPTIONAL but got %s", $t->value));
	}
	
	my $ggp	= $self->_GroupGraphPattern($l, %args);
	return SPARQL::AST->new( type => 'Minus', children => [$ggp] );
}

# [58]  	GraphGraphPattern	  ::=  	'GRAPH' VarOrIri GroupGraphPattern
sub _GraphGraphPattern {
	my $self	= shift;
	my $l		= shift;
	my %args	= @_;
	my $t		= $self->_get_token_type($l, KEYWORD);
	unless ($t->value eq 'GRAPH') {
		$self->throw_error($t, $l, sprintf("Expecting GRAPH but got %s", $t->value));
	}
	
	my $name	= $self->_VarOrIri($l, %args);
	my $ggp	= $self->_GroupGraphPattern($l, %args);
	return SPARQL::AST->new( type => 'Graph', value => $name, children => [$ggp] );
}

# [59]  	ServiceGraphPattern	  ::=  	'SERVICE' 'SILENT'? VarOrIri GroupGraphPattern
sub _ServiceGraphPattern {
	my $self	= shift;
	my $l		= shift;
	my %args	= @_;
	my $t		= $self->_get_token_type($l, KEYWORD);
	unless ($t->value eq 'SERVICE') {
		$self->throw_error($t, $l, sprintf("Expecting GRAPH but got %s", $t->value));
	}
	
	$t			= $self->_peek_token($l);
	my @silent;
	if ($t->type == KEYWORD and $t->value eq 'SILENT') {
		$self->_get_token_type($l, KEYWORD);
		@silent	= 'silent';
	}
	
	my $name	= $self->_VarOrIri($l, %args);
	my $ggp	= $self->_GroupGraphPattern($l, %args);
	return SPARQL::AST->new( type => 'Service', value => [$name, @silent], children => [$ggp] );
}

# [60]  	Bind	  ::=  	'BIND' '(' Expression 'AS' Var ')'
# [61]  	InlineData	  ::=  	'VALUES' DataBlock
# [62]  	DataBlock	  ::=  	InlineDataOneVar | InlineDataFull
# [63]  	InlineDataOneVar	  ::=  	Var '{' DataBlockValue* '}'
# [64]  	InlineDataFull	  ::=  	( NIL | '(' Var* ')' ) '{' ( '(' DataBlockValue* ')' | NIL )* '}'
# [65]  	DataBlockValue	  ::=  	iri |	RDFLiteral |	NumericLiteral |	BooleanLiteral |	'UNDEF'

# [66]  	MinusGraphPattern	  ::=  	'MINUS' GroupGraphPattern
sub _MinusGraphPattern {
	my $self	= shift;
	my $l		= shift;
	my %args	= @_;
	my $t		= $self->_get_token_type($l, KEYWORD);
	unless ($t->value eq 'MINUS') {
		$self->throw_error($t, $l, sprintf("Expecting MINUS but got %s", $t->value));
	}
	
	my $ggp	= $self->_GroupGraphPattern($l, %args);
	return SPARQL::AST->new( type => 'Minus', children => [$ggp] );
}

# [67]  	GroupOrUnionGraphPattern	  ::=  	GroupGraphPattern ( 'UNION' GroupGraphPattern )*
sub _GroupOrUnionGraphPattern {
	my $self	= shift;
	my $l		= shift;
	my %args	= @_;
	
	my @p;
	my $p		= $self->_GroupGraphPattern($l, %args);
	my $t		= $self->_peek_token($l);
	while ($t->type == KEYWORD and $t->value eq 'UNION') {
		$self->_get_token_type($l, KEYWORD);
		my $p2		= $self->_GroupGraphPattern($l, %args);
		push(@p, $p2);
		$t		= $self->_peek_token($l);
	}
	
	unshift(@p, $p);
	while (scalar(@p) > 1) {
		my $q	= pop(@p);
		my $p	= pop(@p);
		push(@p, SPARQL::AST->new( type => 'Union', children => [$p, $q] ));
	}
	return $p[0];
}

# [68]  	Filter	  ::=  	'FILTER' Constraint
sub _Filter {
	my $self	= shift;
	my $l		= shift;
	my %args	= @_;
	
	my $t		= $self->_get_token_type($l, KEYWORD);
	unless ($t->value eq 'FILTER') {
		$self->throw_error($t, $l, sprintf("Expecting FILTER but got %s", $t->value));
	}
	my $expr	= $self->_Constraint($l, %args);
	return SPARQL::AST->new( type => 'Filter', children => [$expr] );
}

# [69]  	Constraint	  ::=  	BrackettedExpression | BuiltInCall | FunctionCall
sub _Constraint {
	my $self	= shift;
	my $l		= shift;
	my %args	= @_;
	
	my $t		= $self->_peek_token($l);
	if ($t->type == LPAREN) {
		# BrackettedExpression
		return $self->_BrackettedExpression($l, %args);
	} elsif ($t->type == KEYWORD) {
		# BuiltInCall
		...
	} elsif ($t->type == IRI or $t->type == PREFIXNAME) {
		# FunctionCall
		...
	}
}

# [70]  	FunctionCall	  ::=  	iri ArgList
# [71]  	ArgList	  ::=  	NIL | '(' 'DISTINCT'? Expression ( ',' Expression )* ')'
# [72]  	ExpressionList	  ::=  	NIL | '(' Expression ( ',' Expression )* ')'
# [73]  	ConstructTemplate	  ::=  	'{' ConstructTriples? '}'
# [74]  	ConstructTriples	  ::=  	TriplesSameSubject ( '.' ConstructTriples? )?
# [75]  	TriplesSameSubject	  ::=  	VarOrTerm PropertyListNotEmpty |	TriplesNode PropertyList
# [76]  	PropertyList	  ::=  	PropertyListNotEmpty?
# [77]  	PropertyListNotEmpty	  ::=  	Verb ObjectList ( ';' ( Verb ObjectList )? )*
# [78]  	Verb	  ::=  	VarOrIri | 'a'
# [79]  	ObjectList	  ::=  	Object ( ',' Object )*
# [80]  	Object	  ::=  	GraphNode

# [81]  	TriplesSameSubjectPath	  ::=  	VarOrTerm PropertyListPathNotEmpty |	TriplesNodePath PropertyListPath
sub _TriplesSameSubjectPath {
	my $self	= shift;
	my $l		= shift;
	my %args	= @_;
	
	my $t		= $self->_next_token($l);
	my $type	= $t->type;
	$self->_unget_token($t);
	
	if ($type == LBRACKET or $type == ANON or $type == LPAREN) {
		# TriplesNodePath PropertyListPath
		my ($subj, @tst)	= $self->_TriplesNodePath($l, %args);
		my @st				= $self->_PropertyListPath($l, %args);
		foreach my $st (@st) {
			$st->{subject}	||= $subj;
		}
		push(@st, @tst);
		
		my @triples	= map {
			SPARQL::AST->new( type => 'Triple', value => [ @{ $_ }{ qw(subject predicate object) } ] );
		} @st;
		
		return @triples;
	} else {
		my $subj	= $self->_VarOrTerm($l, %args);
		my @st		= $self->_PropertyListPathNotEmpty($l, %args);
		foreach my $st (@st) {
			$st->{subject}	||= $subj;
		}
		
		my @triples	= map {
			SPARQL::AST->new( type => 'Triple', value => [ @{ $_ }{ qw(subject predicate object) } ] );
		} @st;
		
		return @triples;
	}
}

# [82]  	PropertyListPath	  ::=  	PropertyListPathNotEmpty?
sub _PropertyListPath {
	my $self	= shift;
	my $l		= shift;
	my %args	= @_;
	my $t		= $self->_peek_token($l);
	if ($self->_token_Verb_ok($t)) {
		return $self->_PropertyListPathNotEmpty($l, %args);
	}
	return;
}

# [83]  	PropertyListPathNotEmpty	  ::=  	( VerbPath | VerbSimple ) ObjectListPath ( ';' ( ( VerbPath | VerbSimple ) ObjectList )? )*
sub _PropertyListPathNotEmpty {
	my $self	= shift;
	my $l		= shift;
	my %args	= @_;
	
	my @plist;
	
	my $t		= $self->_next_token($l);
	while (1) {
		my $path;
		if ($t->type == VAR) {
			# VerbSimple
			$path	= $t->as_node(%args);
		} else {
			# VerbPath
			$self->_unget_token($t);
			$path	= $self->_VerbPath($l, %args);
		}
		
		# ObjectListPath ( ';' ( ( VerbPath | VerbSimple ) ObjectList )? )*
		
		my @st	= $self->_ObjectListPath($l, %args);
		foreach my $s (@st) {
			$s->{predicate}	||= $path;
		}
		
		push(@plist, @st);
		
		$t		= $self->_next_token($l);
		if ($t->type != SEMICOLON) {
			$self->_unget_token($t);
			last;
		} else {
			$t		= $self->_next_token($l);
		}
	}
	
	return @plist;
}

# [84]  	VerbPath	  ::=  	Path
sub _VerbPath {
	my $self	= shift;
	my $l		= shift;
	my %args	= @_;
	return $self->_Path($l, %args);
}

# [85]  	VerbSimple	  ::=  	Var
# [86]  	ObjectListPath	  ::=  	ObjectPath ( ',' ObjectPath )*
sub _ObjectListPath {
	my $self	= shift;
	my $l		= shift;
	my %args	= @_;
	
	my @st;
	while (1) {
		push(@st, $self->_ObjectPath($l, %args));
		my $t		= $self->_next_token($l);
		if ($t->type != COMMA) {
			$self->_unget_token($t);
			last;
		}
	}
	return @st;
}

# [87]  	ObjectPath	  ::=  	GraphNodePath
sub _ObjectPath {
	my $self	= shift;
	my $l		= shift;
	my %args	= @_;
	
	# _GraphNodePath needs to be able to return both triples (i.e. for [ :p :o ]) and just a single node (i.e. for [])
	my ($o, @st)	= $self->_GraphNodePath($l, %args);
	push(@st, { object => $o });
	return @st;
}

# [88]  	Path	  ::=  	PathAlternative
sub _Path {
	my $self	= shift;
	my $l		= shift;
	my %args	= @_;
	return $self->_PathAlternative($l, %args);
}

# [89]  	PathAlternative	  ::=  	PathSequence ( '|' PathSequence )*
sub _PathAlternative {
	my $self	= shift;
	my $l		= shift;
	my %args	= @_;
	
	my @path;
	while (1) {
		push(@path, $self->_PathSequence($l, %args));
		my $t		= $self->_next_token($l);
		if ($t->type != OR) {
			$self->_unget_token($t);
			last;
		}
	}
	return $path[0] if (scalar(@path) == 1);
	return SPARQL::AST->new( type => 'PathAlternative', children => \@path );
}

# [90]  	PathSequence	  ::=  	PathEltOrInverse ( '/' PathEltOrInverse )*
sub _PathSequence {
	my $self	= shift;
	my $l		= shift;
	my %args	= @_;
	
	my @path;
	while (1) {
		push(@path, $self->_PathEltOrInverse($l, %args));
		my $t		= $self->_next_token($l);
		if ($t->type != SLASH) {
			$self->_unget_token($t);
			last;
		}
	}
	return $path[0] if (scalar(@path) == 1);
	return SPARQL::AST->new( type => 'PathSequence', children => \@path );
}

# [91]  	PathElt	  ::=  	PathPrimary PathMod?
# [93]  	PathMod	  ::=  	'?' | '*' | '+'
sub _PathElt {
	my $self	= shift;
	my $l		= shift;
	my %args	= @_;
	my $path	= $self->_PathPrimary($l, %args);
	my $t		= $self->_next_token($l);
	my $type	= $t->type;
	if ($type == QUESTION) {
		return SPARQL::AST->new( type => 'PathZeroOrOne', children => [$path] );
	} elsif ($type == STAR) {
		return SPARQL::AST->new( type => 'PathZeroOrMore', children => [$path] );
	} elsif ($type == PLUS) {
		return SPARQL::AST->new( type => 'PathOneOrMore', children => [$path] );
	} else {
		$self->_unget_token($t);
	}
	return $path;
}

# [92]  	PathEltOrInverse	  ::=  	PathElt | '^' PathElt
sub _PathEltOrInverse {
	my $self	= shift;
	my $l		= shift;
	my %args	= @_;
	my $t		= $self->_next_token($l);
	if ($t->type == HAT) {
		my $path	= $self->_PathElt($l, %args);
		return SPARQL::AST->new( type => 'PathInverse', children => $path );
	} else {
		$self->_unget_token($t);
		return $self->_PathElt($l, %args);
	}
}

# [94]  	PathPrimary	  ::=  	iri | 'a' | '!' PathNegatedPropertySet | '(' Path ')' | 'DISTINCT' '(' Path ')'
sub _PathPrimary {
	my $self	= shift;
	my $l		= shift;
	my %args	= @_;
	my $t		= $self->_next_token($l);
	my $type	= $t->type;
	if ($type == IRI or $type == PREFIXNAME or ($type == KEYWORD and $t->value eq 'A')) {
		return $t->as_node(%args);
	} elsif ($type == BANG) {
		my $path	= $self->_PathNegatedPropertySet($l, %args);
		return SPARQL::AST->new( type => 'PathNot', children => $path );
	} elsif ($type == LPAREN) {
		...
	} elsif ($type == KEYWORD and $t->value eq 'DISTINCT') {
		$self->_get_token_type($l, LPAREN);
		my $path	= $self->_Path($l, %args);
		$self->_get_token_type($l, RPAREN);
		return SPARQL::AST->new( type => 'PathDistinct', children => $path );
	} else {
		$self->throw_error($t, $l, sprintf("Expecting PrimaryPath but got %s", decrypt_constant($t->type)));
	}
}

# [95]  	PathNegatedPropertySet	  ::=  	PathOneInPropertySet | '(' ( PathOneInPropertySet ( '|' PathOneInPropertySet )* )? ')'
# [96]  	PathOneInPropertySet	  ::=  	iri | 'a' | '^' ( iri | 'a' )
# [97]  	Integer	  ::=  	INTEGER
# [98]  	TriplesNode	  ::=  	Collection |	BlankNodePropertyList
# [99]  	BlankNodePropertyList	  ::=  	'[' PropertyListNotEmpty ']'

# [100]  	TriplesNodePath	  ::=  	CollectionPath |	BlankNodePropertyListPath
sub _TriplesNodePath {
	my $self	= shift;
	my $l		= shift;
	my %args	= @_;

	my $t		= $self->_peek_token($l);
# 	warn 'triplesnodepath>>> ' . decrypt_constant($t->type);
	if ($t->type == ANON) {
		$self->_next_token($l);
		my $b	= RDF::Trine::Node::Blank->new();
		return ($b);
	} elsif ($t->type == LBRACKET) {
		return $self->_BlankNodePropertyListPath($l, %args);
	} elsif ($t->type == LPAREN) {
		return $self->_CollectionPath($l, %args);
	} else {
		$self->throw_error($t, $l, sprintf("Expecting Collection or blank node property list but got %s", decrypt_constant($t->type)));
	}
}

# [101]  	BlankNodePropertyListPath	  ::=  	'[' PropertyListPathNotEmpty ']'
sub _BlankNodePropertyListPath {
	my $self	= shift;
	my $l		= shift;
	my %args	= @_;
	
	my $node	= RDF::Trine::Node::Blank->new();
	$self->_get_token_type($l, LBRACKET);
	my @st		= $self->_PropertyListPathNotEmpty($l, %args);
	$self->_get_token_type($l, RBRACKET);
	foreach my $s (@st) {
		$s->{subject}	||= $node;
	}
	return ($node, @st);
}

# [102]  	Collection	  ::=  	'(' GraphNode+ ')'
# [103]  	CollectionPath	  ::=  	'(' GraphNodePath+ ')'
# [104]  	GraphNode	  ::=  	VarOrTerm |	TriplesNode

# [105]  	GraphNodePath	  ::=  	VarOrTerm |	TriplesNodePath
sub _GraphNodePath {
	my $self	= shift;
	my $l		= shift;
	my %args	= @_;
	
	my $t		= $self->_next_token($l);
	my $type	= $t->type;
	$self->_unget_token($t);
	
	if ($type == LBRACKET or $type == ANON or $type == LPAREN) {
		return $self->_TriplesNodePath($l, %args);
	} else {
		my $node	= $self->_VarOrTerm($l, %args);
		return ($node);
	}
}

# [106]  	VarOrTerm	  ::=  	Var | GraphTerm
sub _VarOrTerm {
	my $self	= shift;
	my $l		= shift;
	my %args	= @_;
	my $t		= $self->_next_token($l);
	if ($t->type == STRING1D) {
		my $next	= $self->_peek_token($l);
		my ($lang, $dt);
		if ($next->type == LANG) {
			my $lt	= $self->_get_token_type($l, LANG);
			$lang	= $lt->value;
		} elsif ($next->type == HATHAT) {
			$self->_get_token_type($l, HATHAT);
			my $dtt	= $self->_next_token($l);
			$dt		= $dtt->value;
		}
		return RDF::Trine::Node::Literal->new($t->value, $lang, $dt);
	} else {
		return $t->as_node(%args);
	}
}

# [107]  	VarOrIri	  ::=  	Var | iri
sub _VarOrIri {
	my $self	= shift;
	my $l		= shift;
	my %args	= @_;
	my $t		= $self->_next_token($l);
	my $type	= $t->type;
	unless ($type == VAR or $type == IRI or $type == PREFIXNAME) {
		$self->throw_error($t, $l, sprintf("Expecting VAR or IRIref but got %s", decrypt_constant($t->type)));
	}
	return $t->as_node( %args );

}

# [108]  	Var	  ::=  	VAR1 | VAR2
# [109]  	GraphTerm	  ::=  	iri |	RDFLiteral |	NumericLiteral |	BooleanLiteral |	BlankNode |	NIL
# [110]  	Expression	  ::=  	ConditionalOrExpression
sub _Expression {
	my $self	= shift;
	my $l		= shift;
	my %args	= @_;
	return $self->_ConditionalOrExpression($l, %args);
}

# [111]  	ConditionalOrExpression	  ::=  	ConditionalAndExpression ( '||' ConditionalAndExpression )*
sub _ConditionalOrExpression {
	my $self	= shift;
	my $l		= shift;
	my %args	= @_;

	my @p;
	my $p		= $self->_ConditionalAndExpression($l, %args);
	my $t		= $self->_peek_token($l);
	while ($t->type == OROR) {
		$self->_get_token_type($l, OROR);
		my $p2		= $self->_ConditionalAndExpression($l, %args);
		push(@p, $p2);
		$t		= $self->_peek_token($l);
	}
	
	unshift(@p, $p);
	while (scalar(@p) > 1) {
		my $q	= pop(@p);
		my $p	= pop(@p);
		push(@p, SPARQL::AST->new( type => 'LogicalOr', children => [$p, $q] ));
	}
	return $p[0];
}

# [112]  	ConditionalAndExpression	  ::=  	ValueLogical ( '&&' ValueLogical )*
sub _ConditionalAndExpression {
	my $self	= shift;
	my $l		= shift;
	my %args	= @_;

	my @p;
	my $p		= $self->_ValueLogical($l, %args);
	my $t		= $self->_peek_token($l);
	while ($t->type == ANDAND) {
		$self->_get_token_type($l, ANDAND);
		my $p2		= $self->_ValueLogical($l, %args);
		push(@p, $p2);
		$t		= $self->_peek_token($l);
	}
	
	unshift(@p, $p);
	while (scalar(@p) > 1) {
		my $q	= pop(@p);
		my $p	= pop(@p);
		push(@p, SPARQL::AST->new( type => 'LogicalAnd', children => [$p, $q] ));
	}
	return $p[0];
}

# [113]  	ValueLogical	  ::=  	RelationalExpression
sub _ValueLogical {
	my $self	= shift;
	my $l		= shift;
	my %args	= @_;
	return $self->_RelationalExpression($l, %args);
}

# [114]  	RelationalExpression	  ::=  	NumericExpression ( '=' NumericExpression | '!=' NumericExpression | '<' NumericExpression | '>' NumericExpression | '<=' NumericExpression | '>=' NumericExpression | 'IN' ExpressionList | 'NOT' 'IN' ExpressionList )?
sub _RelationalExpression {
	my $self	= shift;
	my $l		= shift;
	my %args	= @_;
	my $expr	= $self->_NumericExpression($l, %args);
	my $t		= $self->_peek_token($l);
	# XXX
	if ($t->type == EQUALS) {
		$self->_get_token_type($l, EQUALS);
		my $e2	= $self->_NumericExpression($l, %args);
		$expr	= SPARQL::AST->new( type => 'RelationalExpression', children => [$expr, $e2], value => '=' );
	} elsif ($t->type == NOTEQUALS) {
		$self->_get_token_type($l, NOTEQUALS);
		my $e2	= $self->_NumericExpression($l, %args);
		$expr	= SPARQL::AST->new( type => 'RelationalExpression', children => [$expr, $e2], value => '!=' );
	} elsif ($t->type == LT) {
		$self->_get_token_type($l, LT);
		my $e2	= $self->_NumericExpression($l, %args);
		$expr	= SPARQL::AST->new( type => 'RelationalExpression', children => [$expr, $e2], value => '<' );
	} elsif ($t->type == GT) {
		$self->_get_token_type($l, GT);
		my $e2	= $self->_NumericExpression($l, %args);
		$expr	= SPARQL::AST->new( type => 'RelationalExpression', children => [$expr, $e2], value => '>' );
	} elsif ($t->type == LE) {
		$self->_get_token_type($l, LE);
		my $e2	= $self->_NumericExpression($l, %args);
		$expr	= SPARQL::AST->new( type => 'RelationalExpression', children => [$expr, $e2], value => '<=' );
	} elsif ($t->type == GE) {
		$self->_get_token_type($l, GE);
		my $e2	= $self->_NumericExpression($l, %args);
		$expr	= SPARQL::AST->new( type => 'RelationalExpression', children => [$expr, $e2], value => '>=' );
	} elsif ($t->type == KEYWORD and $t->value eq 'IN') {
		...
	}
	return $expr;
}

# [115]  	NumericExpression	  ::=  	AdditiveExpression
sub _NumericExpression {
	my $self	= shift;
	my $l		= shift;
	my %args	= @_;
	return $self->_AdditiveExpression($l, %args);
}

# [116]  	AdditiveExpression	  ::=  	MultiplicativeExpression ( '+' MultiplicativeExpression | '-' MultiplicativeExpression | ( NumericLiteralPositive | NumericLiteralNegative ) ( ( '*' UnaryExpression ) | ( '/' UnaryExpression ) )* )*
sub _AdditiveExpression {
	my $self	= shift;
	my $l		= shift;
	my %args	= @_;
	my $expr	= $self->_MultiplicativeExpression($l, %args);
	my $t		= $self->_peek_token($l);
	# XXX
	if ($t->type == PLUS) {
		$self->_get_token_type($l, PLUS);
		my $e2	= $self->_MultiplicativeExpression($l, %args);
		$expr	= SPARQL::AST->new( type => 'AdditiveExpression', children => [$expr, $e2], value => '+' );
	} elsif ($t->type == MINUS) {
		$self->_get_token_type($l, MINUS);
		my $e2	= $self->_MultiplicativeExpression($l, %args);
		$expr	= SPARQL::AST->new( type => 'AdditiveExpression', children => [$expr, $e2], value => '-' );
	}
	return $expr;
}

# [117]  	MultiplicativeExpression	  ::=  	UnaryExpression ( '*' UnaryExpression | '/' UnaryExpression )*
sub _MultiplicativeExpression {
	my $self	= shift;
	my $l		= shift;
	my %args	= @_;
	my $expr	= $self->_UnaryExpression($l, %args);
	# XXX
	return $expr;
}

# [118]  	UnaryExpression	  ::=  	  '!' PrimaryExpression |	'+' PrimaryExpression |	'-' PrimaryExpression |	PrimaryExpression
sub _UnaryExpression {
	my $self	= shift;
	my $l		= shift;
	my %args	= @_;
	
	my $t		= $self->_peek_token($l);
	if ($t->type == BANG) {
		...
	} elsif ($t->type == PLUS) {
		...
	} elsif ($t->type == MINUS) {
		...
	} else {
		return $self->_PrimaryExpression($l, %args);
	}
}

# [119]  	PrimaryExpression	  ::=  	BrackettedExpression | BuiltInCall | iriOrFunction | RDFLiteral | NumericLiteral | BooleanLiteral | Var
sub _PrimaryExpression {
	my $self	= shift;
	my $l		= shift;
	my %args	= @_;
	
	# XXX
	return $self->_NumericLiteral($l, %args);
}

# [120]  	BrackettedExpression	  ::=  	'(' Expression ')'
sub _BrackettedExpression {
	my $self	= shift;
	my $l		= shift;
	my %args	= @_;
	
	$self->_get_token_type($l, LPAREN);
	my $expr	= $self->_Expression($l, %args);
	$self->_get_token_type($l, RPAREN);
	return $expr;
}

# [121]  	BuiltInCall	  ::=  	  Aggregate 
# |	'STR' '(' Expression ')' 
# |	'LANG' '(' Expression ')' 
# |	'LANGMATCHES' '(' Expression ',' Expression ')' 
# |	'DATATYPE' '(' Expression ')' 
# |	'BOUND' '(' Var ')' 
# |	'IRI' '(' Expression ')' 
# |	'URI' '(' Expression ')' 
# |	'BNODE' ( '(' Expression ')' | NIL ) 
# |	'RAND' NIL 
# |	'ABS' '(' Expression ')' 
# |	'CEIL' '(' Expression ')' 
# |	'FLOOR' '(' Expression ')' 
# |	'ROUND' '(' Expression ')' 
# |	'CONCAT' ExpressionList 
# |	SubstringExpression 
# |	'STRLEN' '(' Expression ')' 
# |	StrReplaceExpression 
# |	'UCASE' '(' Expression ')' 
# |	'LCASE' '(' Expression ')' 
# |	'ENCODE_FOR_URI' '(' Expression ')' 
# |	'CONTAINS' '(' Expression ',' Expression ')' 
# |	'STRSTARTS' '(' Expression ',' Expression ')' 
# |	'STRENDS' '(' Expression ',' Expression ')' 
# |	'STRBEFORE' '(' Expression ',' Expression ')' 
# |	'STRAFTER' '(' Expression ',' Expression ')' 
# |	'YEAR' '(' Expression ')' 
# |	'MONTH' '(' Expression ')' 
# |	'DAY' '(' Expression ')' 
# |	'HOURS' '(' Expression ')' 
# |	'MINUTES' '(' Expression ')' 
# |	'SECONDS' '(' Expression ')' 
# |	'TIMEZONE' '(' Expression ')' 
# |	'TZ' '(' Expression ')' 
# |	'NOW' NIL 
# |	'UUID' NIL 
# |	'STRUUID' NIL 
# |	'MD5' '(' Expression ')' 
# |	'SHA1' '(' Expression ')' 
# |	'SHA256' '(' Expression ')' 
# |	'SHA384' '(' Expression ')' 
# |	'SHA512' '(' Expression ')' 
# |	'COALESCE' ExpressionList 
# |	'IF' '(' Expression ',' Expression ',' Expression ')' 
# |	'STRLANG' '(' Expression ',' Expression ')' 
# |	'STRDT' '(' Expression ',' Expression ')' 
# |	'sameTerm' '(' Expression ',' Expression ')' 
# |	'isIRI' '(' Expression ')' 
# |	'isURI' '(' Expression ')' 
# |	'isBLANK' '(' Expression ')' 
# |	'isLITERAL' '(' Expression ')' 
# |	'isNUMERIC' '(' Expression ')' 
# |	RegexExpression 
# |	ExistsFunc 
# |	NotExistsFunc
# [122]  	RegexExpression	  ::=  	'REGEX' '(' Expression ',' Expression ( ',' Expression )? ')'
# [123]  	SubstringExpression	  ::=  	'SUBSTR' '(' Expression ',' Expression ( ',' Expression )? ')'
# [124]  	StrReplaceExpression	  ::=  	'REPLACE' '(' Expression ',' Expression ',' Expression ( ',' Expression )? ')'
# [125]  	ExistsFunc	  ::=  	'EXISTS' GroupGraphPattern
# [126]  	NotExistsFunc	  ::=  	'NOT' 'EXISTS' GroupGraphPattern
# [127]  	Aggregate	  ::=  	  'COUNT' '(' 'DISTINCT'? ( '*' | Expression ) ')' 
# | 'SUM' '(' 'DISTINCT'? Expression ')' 
# | 'MIN' '(' 'DISTINCT'? Expression ')' 
# | 'MAX' '(' 'DISTINCT'? Expression ')' 
# | 'AVG' '(' 'DISTINCT'? Expression ')' 
# | 'SAMPLE' '(' 'DISTINCT'? Expression ')' 
# | 'GROUP_CONCAT' '(' 'DISTINCT'? Expression ( ';' 'SEPARATOR' '=' String )? ')'
# [128]  	iriOrFunction	  ::=  	iri ArgList?
# [129]  	RDFLiteral	  ::=  	String ( LANGTAG | ( '^^' iri ) )?

# [130]  	NumericLiteral	  ::=  	NumericLiteralUnsigned | NumericLiteralPositive | NumericLiteralNegative
# [131]  	NumericLiteralUnsigned	  ::=  	INTEGER |	DECIMAL |	DOUBLE
# [132]  	NumericLiteralPositive	  ::=  	INTEGER_POSITIVE |	DECIMAL_POSITIVE |	DOUBLE_POSITIVE
# [133]  	NumericLiteralNegative	  ::=  	INTEGER_NEGATIVE |	DECIMAL_NEGATIVE |	DOUBLE_NEGATIVE
sub _NumericLiteral {
	my $self	= shift;
	my $l		= shift;
	my %args	= @_;
	
	my $t		= $self->_next_token($l);
	my $node	= $t->as_node(%args);
	return SPARQL::AST->new( type => 'Node', value => $node );
}

# [134]  	BooleanLiteral	  ::=  	'true' |	'false'
# [135]  	String	  ::=  	STRING_LITERAL1 | STRING_LITERAL2 | STRING_LITERAL_LONG1 | STRING_LITERAL_LONG2
# [136]  	iri	  ::=  	IRIREF |	PrefixedName
# [137]  	PrefixedName	  ::=  	PNAME_LN | PNAME_NS
# [138]  	BlankNode	  ::=  	BLANK_NODE_LABEL |	ANON










































sub throw_error {
	my $self	= shift;
	my $t		= shift;
	my $l		= shift;
	my $message	= shift;
	my $line	= $t->start_line;
	my $col		= $t->start_column;
	Carp::cluck "$message at $line:$col";
	my $text	= "$message at $line:$col";
	if (defined($t->value)) {
		$text	.= " (near '" . $t->value . "')";
	}
	RDF::Trine::Error::ParserError::Tokenized->throw(
		-text => $text,
		-object => $t,
	);
}

1;
