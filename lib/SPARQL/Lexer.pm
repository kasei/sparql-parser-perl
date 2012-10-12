package SPARQL::Lexer;

use SPARQL::Constants;
use SPARQL::Token;
use 5.014;
use Moose;
use Data::Dumper;
use RDF::Trine::Error;

my $r_nameChar_extra		= qr'[-0-9\x{B7}\x{0300}-\x{036F}\x{203F}-\x{2040}]'o;
my $r_nameStartChar_minus_underscore	= qr'[A-Za-z\x{00C0}-\x{00D6}\x{00D8}-\x{00F6}\x{00F8}-\x{02FF}\x{0370}-\x{037D}\x{037F}-\x{1FFF}\x{200C}-\x{200D}\x{2070}-\x{218F}\x{2C00}-\x{2FEF}\x{3001}-\x{D7FF}\x{F900}-\x{FDCF}\x{FDF0}-\x{FFFD}\x{00010000}-\x{000EFFFF}]'o;
my $r_nameStartChar			= qr/[A-Za-z_\x{00C0}-\x{00D6}\x{00D8}-\x{00F6}\x{00F8}-\x{02FF}\x{0370}-\x{037D}\x{037F}-\x{1FFF}\x{200C}-\x{200D}\x{2070}-\x{218F}\x{2C00}-\x{2FEF}\x{3001}-\x{D7FF}\x{F900}-\x{FDCF}\x{FDF0}-\x{FFFD}\x{10000}-\x{EFFFF}]/;
my $r_nameChar				= qr/${r_nameStartChar}|[-0-9\x{b7}\x{0300}-\x{036f}\x{203F}-\x{2040}]/;
my $r_prefixName			= qr/(?:(?!_)${r_nameStartChar})(?:$r_nameChar)*/;
my $r_nameChar_test			= qr"(?:$r_nameStartChar|$r_nameChar_extra)";
my $r_double				= qr'[+-]?([0-9]+\.[0-9]*[eE][+-]?[0-9]+|\.[0-9]+[eE][+-]?[0-9]+|[0-9]+[eE][+-]?[0-9]+)';
my $r_decimal				= qr'[+-]?([0-9]+\.[0-9]*|\.([0-9])+)';
my $r_integer				= qr'[+-]?[0-9]+';
our $r_IRI_REF				= qr/<([^<>"{}|^`\\\x{00}-\x{20}])*>/;
our $r_PN_CHARS_BASE		= qr/([A-Z]|[a-z]|[\x{00C0}-\x{00D6}]|[\x{00D8}-\x{00F6}]|[\x{00F8}-\x{02FF}]|[\x{0370}-\x{037D}]|[\x{037F}-\x{1FFF}]|[\x{200C}-\x{200D}]|[\x{2070}-\x{218F}]|[\x{2C00}-\x{2FEF}]|[\x{3001}-\x{D7FF}]|[\x{F900}-\x{FDCF}]|[\x{FDF0}-\x{FFFD}]|[\x{10000}-\x{EFFFF}])/;
our $r_PN_CHARS_U			= qr/([_]|${r_PN_CHARS_BASE})/;
our $r_PN_LOCAL_ESCAPED		= qr{(?:\\([-~.!&'()*+,;=/?#@%_\$]))|%[0-9A-Fa-f]{2}};
our $r_PN_CHARS				= qr/${r_PN_CHARS_U}|-|[0-9]|\x{00B7}|[\x{0300}-\x{036F}]|[\x{203F}-\x{2040}]/;
our $r_PN_LOCAL				= qr/((${r_PN_CHARS_U}|[:0-9]|${r_PN_LOCAL_ESCAPED})((${r_PN_CHARS}|${r_PN_LOCAL_ESCAPED}|[:.])*(${r_PN_CHARS}|[:]|${r_PN_LOCAL_ESCAPED}))?)/;
our $r_PN_PREFIX			= qr/(${r_PN_CHARS_BASE}((${r_PN_CHARS}|[.])*${r_PN_CHARS})?)/;
our $r_PNAME_NS				= qr/((${r_PN_PREFIX})?:)/;
our $r_PNAME_LN				= qr/(${r_PNAME_NS}${r_PN_LOCAL})/;
our $r_VARNAME				= qr/((${r_PN_CHARS_U}|[0-9])(${r_PN_CHARS_U}|[0-9]|\x{00B7}|[\x{0300}-\x{036F}]|[\x{203F}-\x{2040}])*)/;

has file => (
	is => 'ro',
	isa => 'FileHandle',
	required => 1,
);

has linebuffer => (
	is => 'rw',
	isa => 'Str',
	default => '',
);

has line => (
	is => 'rw',
	isa => 'Int',
	default => 1,
);

has column => (
	is => 'rw',
	isa => 'Int',
	default => 1,
);

has buffer => (
	is => 'rw',
	isa => 'Str',
	default => '',
);

has start_column => (
	is => 'rw',
	isa => 'Int',
	default => -1,
);

has start_line => (
	is => 'rw',
	isa => 'Int',
	default => -1,
);

# has 'verbose' => (
# 	is => 'rw',
# 	isa => 'Int',
# 	default => 0,
# );

sub BUILDARGS {
	my $class	= shift;
	if (scalar(@_) == 1) {
		return { file => shift };
	} else {
		return $class->SUPER::BUILDARGS(@_);
	}
}

sub new_token {
	my $self		= shift;
	my $type		= shift;
	my $start_line	= $self->start_line;
	my $start_col	= $self->start_column;
	my $line		= $self->line;
	my $col			= $self->column;
	return SPARQL::Token->fast_constructor(
			$type,
			$start_line,
			$start_col,
			$line,
			$col,
			\@_,
		);
}

sub lex_file {
	my $self	= shift;
	$self->get_token();
}

sub fill_buffer {
	my $self	= shift;
	unless (length($self->buffer)) {
		my $line	= $self->file->getline;
		if (defined($line)) {
			$line		=~ s/\\u([0-9A-Fa-f]{4})/chr(hex($1))/ge;
			$line		=~ s/\\U([0-9A-Fa-f]{8})/chr(hex($1))/ge;
			$self->{buffer}	.= $line;
		}
	}
}

sub check_for_bom {
	my $self	= shift;
	my $c	= $self->_peek_char();
	if ($c eq "\x{FEFF}") {
		$self->_get_char;
	}
}


my %CHAR_TOKEN	= (
	','	=> COMMA,
	'.'	=> DOT,
	'='	=> EQUALS,
	'{'	=> LBRACE,
	'['	=> LBRACKET,
	'('	=> LPAREN,
	'-'	=> MINUS,
	'+'	=> PLUS,
	'}'	=> RBRACE,
	']'	=> RBRACKET,
	')'	=> RPAREN,
	';'	=> SEMICOLON,
	'/'	=> SLASH,
	'*'	=> STAR,
);

my %METHOD_TOKEN	= (
	q[@]	=> 'get_language',
	q[<]	=> 'get_iriref_or_relational',
	q[?]	=> 'get_variable',
	q[$]	=> 'get_variable',
	q[!]	=> 'get_bang',
	q[>]	=> 'get_relational',
	q[|]	=> 'get_or',
	q[']	=> 'get_single_literal',
	q["]	=> 'get_double_literal',
	q[_]	=> 'get_bnode',
	q[:]	=> 'get_pname',
);

sub get_token {
	my $self	= shift;
	while (1) {
		unless (length($self->{buffer})) {
			$self->fill_buffer;
		}
		if (length($self->{buffer}) == 0) {
			return;
		}
# 		warn "getting token with buffer: " . Dumper($self->{buffer});
		my $c	= $self->_peek_char();
		
		$self->start_column( $self->column );
		$self->start_line( $self->line );
		
# WS
		if ($c =~ /[ \r\n\t]/) {
			while (length($c) and $c =~ /[\t\r\n ]/) {
				$self->_get_char;
				$c		= $self->_peek_char;
			}
			
			# we're ignoring whitespace tokens, but we could return them here instead of falling through to the 'next':
# 			return $self->new_token(WS);
			next;
		}
# COMMENT
		elsif ($c eq '#') {
			# we're ignoring comment tokens, but we could return them here instead of falling through to the 'next':
			$self->get_comment();
			next;
		}
# NIL
		elsif ($c eq '(' and $self->{buffer} =~ /^[(][ \r\n\t]*[)]/) {
			$self->_read_length($+[0]);
			return $self->new_token(NIL);
		}
# ANON
		elsif ($c eq '[' and $self->{buffer} =~ /^\[[ \r\n\t]*\]/) {
			$self->_read_length($+[0]);
			return $self->new_token(ANON);
		}
		
# DOUBLE
		elsif ($self->{buffer} =~ /^${r_double}/) {
			return $self->get_number;
		}
		
# DECIMAL
		elsif ($self->{buffer} =~ /^${r_decimal}/) {
			return $self->get_number;
		}
		
# INTEGER
		elsif ($self->{buffer} =~ /^${r_integer}/) {
			return $self->get_number;
		}
		
# HAT / HATHAT
		elsif ($c eq '^') {
			if ($self->{buffer} =~ /^\^{2}/) {
				$self->_read_word('^^'); return $self->new_token(HATHAT);
			} else {
				$self->_read_word('^'); return $self->new_token(HAT);
			}
		}
# Single char dispatch:
# - LANG
# - IRIREF
# - VAR / QUESTION
# - NOTEQUALS / BANG
# - LE / LT
# - GE / GT
# - OROR / OR
# - STRING3D / STRING1D
# - STRING3S / STRING1S
# - BNODE
		elsif (defined(my $method = $METHOD_TOKEN{$c})) { return $self->$method() }


# Direct mapping:
# - COMMA
# - DOT
# - EQUALS
# - LBRACE
# - LBRACKET
# - LPAREN
# - MINUS
# - PLUS
# - RBRACE
# - RBRACKET
# - RPAREN
# - SEMICOLON
# - SLASH
# - STAR
		elsif (defined(my $name = $CHAR_TOKEN{$c})) { $self->_get_char; return $self->new_token($name, $c); }
# - ANDAND
		elsif ($c eq '&') {
			$self->_read_word('&&');
			return $self->new_token(ANDAND);
		}
# PREFIXNAME
		elsif ($c =~ /[A-Za-z\x{00C0}-\x{00D6}\x{00D8}-\x{00F6}\x{00F8}-\x{02FF}\x{0370}-\x{037D}\x{037F}-\x{1FFF}\x{200C}-\x{200D}\x{2070}-\x{218F}\x{2C00}-\x{2FEF}\x{3001}-\x{D7FF}\x{F900}-\x{FDCF}\x{FDF0}-\x{FFFD}\x{10000}-\x{EFFFF}]/) {
# 			} elsif ($self->{buffer} =~ /^(?:true|false)\b/) {
# 				my $bool	= $self->_read_length($+[0]);
# 				return $self->new_token(BOOLEAN, $bool);
# 			} else {
				my $t	= $self->get_pname;
				return $t if $t;
# 			}
		}

# KEYWORD / BOOLEAN
		return $self->get_keyword;
	}
}

sub _get_char_safe {
	my $self	= shift;
	my $char	= shift;
	my $c		= $self->_get_char;
	if ($c ne $char) {
		$self->throw_error("Expected '$char' but got '$c'");
	}
	return $c;
}

sub _get_char_fill_buffer {
	my $self	= shift;
	if (length($self->{buffer}) == 0) {
		$self->fill_buffer;
		if (length($self->{buffer}) == 0) {
			return;
		}
	}
	my $c		= substr($self->{buffer}, 0, 1, '');
	if ($c eq "\n") {
# 		$self->{linebuffer}	= '';
		$self->{line}	= 1+$self->{line};
		$self->{column}	= 1;
	} else {
# 		$self->{linebuffer}	.= $c;
		$self->{column}	= 1+$self->{column};
	}
	return $c;
}

sub _get_char {
	my $self	= shift;
	my $c		= substr($self->{buffer}, 0, 1, '');
	if ($c eq "\n") {
# 		$self->{linebuffer}	= '';
		$self->{line}	= 1+$self->{line};
		$self->{column}	= 1;
	} else {
# 		$self->{linebuffer}	.= $c;
		$self->{column}	= 1+$self->{column};
	}
	return $c;
}

sub _peek_char {
	my $self	= shift;
	if (length($self->{buffer}) == 0) {
		$self->fill_buffer;
		if (length($self->{buffer}) == 0) {
			return;
		}
	}
	my $c		= substr($self->{buffer}, 0, 1);
	return $c;
}

sub _read_word {
	my $self	= shift;
	my $word	= shift;
	while (length($self->{buffer}) < length($word)) {
		$self->fill_buffer;
	}
	
	if ((length($self->{buffer}) < length($word)) or substr($self->{buffer}, 0, length($word)) ne $word) {
		$self->throw_error("Expected '$word'");
	}
	
	my $lines	= ($word =~ tr/\n//);
	my $lastnl	= rindex($word, "\n");
	my $cols	= length($word) - $lastnl - 1;
	$self->{lines}	+= $lines;
	if ($lines) {
		$self->{column}	= $cols;
	} else {
		$self->{column}	+= $cols;
	}
	substr($self->{buffer}, 0, length($word), '');
}

sub _read_length {
	my $self	= shift;
	my $len		= shift;
	while (length($self->{buffer}) < $len) {
		$self->fill_buffer;
	}
	
	if (length($self->{buffer}) < $len) {
		my $remaining	= length($self->{buffer});
		$self->throw_error("Expecting $len bytes but only $remaining remaining");
	}
	
	my $word	= substr($self->{buffer}, 0, $len, '');
	my $lines	= ($word =~ tr/\n//);
	my $lastnl	= rindex($word, "\n");
	my $cols	= length($word) - $lastnl - 1;
	$self->{lines}	+= $lines;
	if ($lines) {
		$self->{column}	= $cols;
	} else {
		$self->{column}	+= $cols;
	}
	return $word;
}

sub _new_pname {
	my $self	= shift;
	my @pname	= @_;
	if (scalar(@pname) == 2) {
		my $local	= $pname[1];
		if ($local =~ /[%\\]/) {
			my $new	= '';
			my $len		= length($local);
			my $index	= 0;
			while ($index < $len) {
				my $c	= substr($local, $index, 1);
				if ($c eq '\\') {
					$new	.= substr($local, ++$index, 1);
				} elsif ($c eq '%') {
					my $ord	= substr($local, $index+1, 2);
					$new	.= chr(hex($ord));
					$index	+= 2;
				} else {
					$new	.= $c;
				}
				$index++;
			}
			$pname[1]	= $new;
		}
	}
	return $self->new_token(PREFIXNAME, @pname);
}

sub get_pname {
	my $self	= shift;
	
	if ($self->{buffer} =~ /^$r_PNAME_LN/) {
		my $pname	= $self->_read_length($+[0]);
		my @values	= split(/:/, $pname, 2);
# 		warn "full prefixedname: '$pname'";
		return $self->_new_pname( @values );
	} elsif ($self->{buffer} =~ /^$r_PNAME_NS/) {
		my $pname	= $self->_read_length($+[0]);
		my @values	= split(/:/, $pname, 2);
# 		warn "prefix: '$pname'";
		return $self->_new_pname( @values );
	} else {
		return;
	}
}

sub get_iriref_or_relational {
	my $self	= shift;
	if ($self->{buffer} =~ /^${r_IRI_REF}/) {
		$self->_get_char_safe('<');
		my $iri	= '';
		while (1) {
			my $c	= $self->_peek_char;
			last unless defined($c);
			if (substr($self->{buffer}, 0, 1) eq '\\') {
				$self->_get_char_safe('\\');
				my $esc	= $self->_get_char;
				given ($esc) {
					when('\\'){ $iri .= "\\" }
					when('"'){ $iri .= '"' }
					when('r'){ $iri .= "\r" }
					when('t'){ $iri .= "\t" }
					when('n'){ $iri .= "\n" }
					when('>'){ $iri .= ">" }
					when('U'){
						my $codepoint	= $self->_read_length(8);
						unless ($codepoint =~ /^[0-9A-Fa-f]+$/) {
							$self->throw_error("Bad unicode escape codepoint '$codepoint'");
						}
						$iri .= chr(hex($codepoint));
					}
					when('u'){
						my $codepoint	= $self->_read_length(4);
						unless ($codepoint =~ /^[0-9A-Fa-f]+$/) {
							$self->throw_error("Bad unicode escape codepoint '$codepoint'");
						}
						$iri .= chr(hex($codepoint));
					}
					default {
						$self->throw_error("Unrecognized iri escape '$esc'");
					}
				}
			} elsif ($self->{buffer} =~ /^[^>\\]+/) {
				$iri	.= $self->_read_length($+[0]);
			} elsif (substr($self->{buffer}, 0, 1) eq '>') {
				last;
			} else {
				$self->throw_error("Got '$c' while expecting IRI character");
			}
		}
		$self->_get_char_safe(q[>]);
		return $self->new_token(IRI, $iri);
	} else {
		$self->_get_char_safe('<');
		my $c	= $self->_peek_char;
		if ($c eq '=') {
			$self->_get_char_safe('=');
			return $self->new_token(LE);
		} else {
			return $self->new_token(LT);
		}
	}
}

sub get_bang {
	my $self	= shift;
	if ($self->{buffer} =~ /^!=/) {
		$self->_read_word('!=');
		return $self->new_token(NOTEQUALS);
	} else {
		$self->_get_char_safe('!');
		return $self->new_token(BANG);
	}
}

sub get_or {
	my $self	= shift;
	if ($self->{buffer} =~ /^[|][|]/) {
		$self->_read_word('||');
		return $self->new_token(OROR);
	} else {
		$self->_get_char_safe('|');
		return $self->new_token(OR);
	}
}

sub get_bnode {
	my $self	= shift;
	$self->_read_word('_:');
	unless ($self->{buffer} =~ /^(?:${r_PN_CHARS_U}|[0-9])(?:(?:${r_PN_CHARS}|[.])*${r_PN_CHARS})?/o) {
		$self->throw_error("Expected: name");
	}
	my $name	= $self->_read_length($+[0]);
	return $self->new_token(BNODE, $name);
}

sub get_variable {
	my $self	= shift;
	my $c		= $self->_get_char;
	unless ($self->{buffer} =~ /^${r_VARNAME}/o) {
		if ($c eq '?') {
			return $self->new_token(QUESTION);
		} else {
			$self->_error("Expected: variable name");
		}
	}
	my $name	= $self->_read_length($+[0]);
	return $self->new_token(VAR, $name);
}

sub get_number {
	my $self	= shift;
	if ($self->{buffer} =~ /^${r_double}\b/) {
		return $self->new_token(DOUBLE, $self->_read_length($+[0]));
	} elsif ($self->{buffer} =~ /^${r_decimal}\b/) {
		return $self->new_token(DECIMAL, $self->_read_length($+[0]));
	} elsif ($self->{buffer} =~ /^${r_integer}\b/) {
		return $self->new_token(INTEGER, $self->_read_length($+[0]));
	} else {
		my $c		= $self->_peek_char;
		if ($c eq '-') {
			$self->_get_char;
			return $self->new_token(MINUS);
		} elsif ($c eq '+') {
			$self->_get_char;
			return $self->new_token(PLUS);
		} else {
			$self->throw_error("Expected number");
		}
	}
}

sub get_comment {
	my $self	= shift;
	$self->_get_char_safe('#');
	my $comment	= '';
	my $c		= $self->_peek_char;
	while (length($c) and $c !~ /[\r\n]/) {
		$comment	.= $self->_get_char;
		$c			= $self->_peek_char;
	}
	if (length($c) and $c =~ /[\r\n]/) {
		$self->_get_char;
	}
	return $self->new_token(COMMENT, $comment);
}

sub get_double_literal {
	my $self	= shift;
	my $c		= $self->_peek_char();
	$self->_get_char_safe(q["]);
	if (substr($self->{buffer}, 0, 2) eq q[""]) {
		# #x22 #x22 #x22 lcharacter* #x22 #x22 #x22
		$self->_read_word(q[""]);
		
		my $quote_count	= 0;
		my $string	= '';
		while (1) {
			if (length($self->{buffer}) == 0) {
				$self->fill_buffer;
				if (length($self->{buffer}) == 0) {
					$self->throw_error("Found EOF in string literal");
				}
			}
			if (substr($self->{buffer}, 0, 1) eq '"') {
				my $c	= $self->_get_char;
				$quote_count++;
				if ($quote_count == 3) {
					last;
				}
			} else {
				if ($quote_count) {
					$string	.= '"' foreach (1..$quote_count);
					$quote_count	= 0;
				}
				if (substr($self->{buffer}, 0, 1) eq '\\') {
					my $c	= $self->_get_char;
# 					$self->_get_char_safe('\\');
					my $esc	= $self->_get_char_fill_buffer;
					given ($esc) {
						when('\\'){ $string .= "\\" }
						when('"'){ $string .= '"' }
						when("'"){ $string .= "'" }
						when('r'){ $string .= "\r" }
						when('t'){ $string .= "\t" }
						when('n'){ $string .= "\n" }
						when('>'){ $string .= ">" }
						when('U'){
							my $codepoint	= $self->_read_length(8);
							unless ($codepoint =~ /^[0-9A-Fa-f]+$/) {
								$self->throw_error("Bad unicode escape codepoint '$codepoint'");
							}
							$string .= chr(hex($codepoint));
						}
						when('u'){
							my $codepoint	= $self->_read_length(4);
							unless ($codepoint =~ /^[0-9A-Fa-f]+$/) {
								$self->throw_error("Bad unicode escape codepoint '$codepoint'");
							}
							$string .= chr(hex($codepoint));
						}
						default {
							$self->throw_error("Unrecognized string escape '$esc'");
						}
					}
				} else {
					$self->{buffer}	=~ /^[^"\\]+/;
					$string	.= $self->_read_length($+[0]);
				}
			}
		}
		return $self->new_token(STRING3D, $string);
	} else {
		### #x22 scharacter* #x22
		my $string	= '';
		while (1) {
			if (substr($self->{buffer}, 0, 1) eq '\\') {
				my $c	= $self->_peek_char;
				$self->_get_char_safe('\\');
				my $esc	= $self->_get_char;
				given ($esc) {
					when('\\'){ $string .= "\\" }
					when('"'){ $string .= '"' }
					when("'"){ $string .= "'" }
					when('r'){ $string .= "\r" }
					when('t'){ $string .= "\t" }
					when('n'){ $string .= "\n" }
					when('>'){ $string .= ">" }
					when('U'){
						my $codepoint	= $self->_read_length(8);
						unless ($codepoint =~ /^[0-9A-Fa-f]+$/) {
							$self->throw_error("Bad unicode escape codepoint '$codepoint'");
						}
						$string .= chr(hex($codepoint));
					}
					when('u'){
						my $codepoint	= $self->_read_length(4);
						unless ($codepoint =~ /^[0-9A-Fa-f]+$/) {
							$self->throw_error("Bad unicode escape codepoint '$codepoint'");
						}
						$string .= chr(hex($codepoint));
					}
					default {
						$self->throw_error("Unrecognized string escape '$esc'");
					}
				}
			} elsif ($self->{buffer} =~ /^[^"\\]+/) {
				$string	.= $self->_read_length($+[0]);
			} elsif (substr($self->{buffer}, 0, 1) eq '"') {
				last;
			} else {
				$self->throw_error("Got '$c' while expecting string character");
			}
		}
		$self->_get_char_safe(q["]);
		return $self->new_token(STRING1D, $string);
	}
}

sub get_single_literal {
	my $self	= shift;
	my $c		= $self->_peek_char();
	$self->_get_char_safe("'");
	if (substr($self->{buffer}, 0, 2) eq q['']) {
		# #x22 #x22 #x22 lcharacter* #x22 #x22 #x22
		$self->_read_word(q['']);
		
		my $quote_count	= 0;
		my $string	= '';
		while (1) {
			if (length($self->{buffer}) == 0) {
				$self->fill_buffer;
				if (length($self->{buffer}) == 0) {
					$self->throw_error("Found EOF in string literal");
				}
			}
			if (substr($self->{buffer}, 0, 1) eq "'") {
				my $c	= $self->_get_char;
				$quote_count++;
				if ($quote_count == 3) {
					last;
				}
			} else {
				if ($quote_count) {
					$string	.= "'" foreach (1..$quote_count);
					$quote_count	= 0;
				}
				if (substr($self->{buffer}, 0, 1) eq '\\') {
					my $c	= $self->_get_char;
# 					$self->_get_char_safe('\\');
					my $esc	= $self->_get_char_fill_buffer;
					given ($esc) {
						when('\\'){ $string .= "\\" }
						when('"'){ $string .= '"' }
						when("'"){ $string .= "'" }
						when('r'){ $string .= "\r" }
						when('t'){ $string .= "\t" }
						when('n'){ $string .= "\n" }
						when('>'){ $string .= ">" }
						when('U'){
							my $codepoint	= $self->_read_length(8);
							unless ($codepoint =~ /^[0-9A-Fa-f]+$/) {
								$self->throw_error("Bad unicode escape codepoint '$codepoint'");
							}
							$string .= chr(hex($codepoint));
						}
						when('u'){
							my $codepoint	= $self->_read_length(4);
							unless ($codepoint =~ /^[0-9A-Fa-f]+$/) {
								$self->throw_error("Bad unicode escape codepoint '$codepoint'");
							}
							$string .= chr(hex($codepoint));
						}
						default {
							$self->throw_error("Unrecognized string escape '$esc'");
						}
					}
				} else {
					$self->{buffer}	=~ /^[^'\\]+/;
					$string	.= $self->_read_length($+[0]);
				}
			}
		}
		return $self->new_token(STRING3S, $string);
	} else {
		### #x22 scharacter* #x22
		my $string	= '';
		while (1) {
			if (substr($self->{buffer}, 0, 1) eq '\\') {
				my $c	= $self->_peek_char;
				$self->_get_char_safe('\\');
				my $esc	= $self->_get_char;
				given ($esc) {
					when('\\'){ $string .= "\\" }
					when('"'){ $string .= '"' }
					when("'"){ $string .= "'" }
					when('r'){ $string .= "\r" }
					when('t'){ $string .= "\t" }
					when('n'){ $string .= "\n" }
					when('>'){ $string .= ">" }
					when('U'){
						my $codepoint	= $self->_read_length(8);
						unless ($codepoint =~ /^[0-9A-Fa-f]+$/) {
							$self->throw_error("Bad unicode escape codepoint '$codepoint'");
						}
						$string .= chr(hex($codepoint));
					}
					when('u'){
						my $codepoint	= $self->_read_length(4);
						unless ($codepoint =~ /^[0-9A-Fa-f]+$/) {
							$self->throw_error("Bad unicode escape codepoint '$codepoint'");
						}
						$string .= chr(hex($codepoint));
					}
					default {
						$self->throw_error("Unrecognized string escape '$esc'");
					}
				}
			} elsif ($self->{buffer} =~ /^[^'\\]+/) {
				$string	.= $self->_read_length($+[0]);
			} elsif (substr($self->{buffer}, 0, 1) eq "'") {
				last;
			} else {
				$self->throw_error("Got '$c' while expecting string character");
			}
		}
		$self->_get_char_safe(q[']);
		return $self->new_token(STRING1S, $string);
	}
}

{
my %KEYWORDS	= map { $_ => 1 } qw(
	ABS
	ADD
	ALL
	AS
	ASC
	ASK
	AVG
	BASE
	BIND
	BNODE
	BOUND
	BY
	CEIL
	CLEAR
	COALESCE
	CONCAT
	CONSTRUCT
	CONTAINS
	COPY
	COUNT
	CREATE
	DATATYPE
	DAY
	DEFAULT
	DELETE
	DELETE DATA
	DELETE WHERE
	DESC
	DESCRIBE
	DISTINCT
	DISTINCT
	DROP
	ENCODE_FOR_URI
	EXISTS
	FILTER
	FLOOR
	FROM
	GRAPH
	GROUP
	GROUP_CONCAT
	HAVING
	HOURS
	IF
	IN
	INSERT
	INSERT DATA
	INTO
	IRI
	ISBLANK
	ISIRI
	ISLITERAL
	ISNUMERIC
	ISURI
	LANG
	LANGMATCHES
	LCASE
	LIMIT
	LOAD
	MAX
	MD5
	MIN
	MINUS
	MINUTES
	MONTH
	MOVE
	NAMED
	NOT
	NOW
	OFFSET
	OPTIONAL
	ORDER
	PREFIX
	RAND
	REDUCED
	REGEX
	REPLACE
	ROUND
	SAMETERM
	SAMPLE
	SECONDS
	SELECT
	SEPARATOR
	SERVICE
	SHA1
	SHA256
	SHA384
	SHA512
	SILENT
	STR
	STRAFTER
	STRBEFORE
	STRDT
	STRENDS
	STRLANG
	STRLEN
	STRSTARTS
	STRUUID
	SUBSTR
	SUM
	TIMEZONE
	TO
	TZ
	UCASE
	UNDEF
	UNION
	URI
	USING
	UUID
	VALUES
	WHERE
	WITH
	YEAR
);
my $pattern		= join('|', keys %KEYWORDS);
my $keywords_re	= qr/^(?:a|(?i:$pattern))\b/;
sub get_keyword {
	my $self	= shift;
	if ($self->{buffer} =~ $keywords_re) {
		my $keyword	= $self->_read_length($+[0]);
		return $self->new_token(KEYWORD, uc($keyword));
	} elsif ($self->{buffer} =~ /^(?:true|false)\b/) {
		my $bool	= $self->_read_length($+[0]);
		return $self->new_token(BOOLEAN, $bool);
	} else {
		$self->throw_error("Expected keyword");
	}
}
}

sub get_language {
	my $self	= shift;
	$self->_get_char_safe('@');
	if ($self->{buffer} =~ /^[a-z]+(-[a-z0-9]+)*\b/) {
		my $lang	= $self->_read_length($+[0]);
		return $self->new_token(LANG, $lang);
	} else {
		$self->throw_error("Expected language tag");
	}
}

sub get_relational {
	my $self	= shift;
	$self->_get_char_safe('>');
	my $c	= $self->_peek_char;
	if ($c eq '=') {
		$self->_get_char_safe('=');
		return $self->new_token(GE);
	} else {
		return $self->new_token(GT);
	}
}

sub throw_error {
	my $self	= shift;
	my $error	= shift;
	my $line	= $self->start_line;
	my $col		= $self->start_column;
# 	Carp::cluck "$line:$col: $error: " . Dumper($self->{buffer});
	RDF::Trine::Error::ParserError::Positioned->throw(
		-text => "$error at $line:$col",
		-value => [$line, $col],
	);
}

__PACKAGE__->meta->make_immutable;

