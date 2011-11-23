package Morpheem::Renderer::SVG;
use strict;
use warnings;

use charnames ':full';

use SVG;

# XXX
our $square_size = 25;
our $margin      = 1;

my $board_size  = 15 * $square_size + 16 * $margin;
my $rack_width  = 7 * $square_size + 8 * $margin;
my $rack_height = $square_size + 2 * $margin;
my $font_size   = 0.39 * $square_size;

sub new { bless { }, shift }

# colours lifted from Google Images pictures of real scrabble boards (tile
# colour lightened)
my %colours = (
        board  => '#E3F6FF',
        border => "#20111C",
        centre => "#E32745",
        tile   => "#DFBC95",
        rack   => "#CCAA77",
    );

# TODO other languages
# NOTE these are WordFeud point values, not Scrabble
my %values = qw(
    a 1   b  4   c 4   d 2   e  1
    f 4   g  3   h 4   i 1   j 10
    k 5   l  1   m 3   n 1   o  1
    p 4   q 10   r 1   s 1   t  1
    u 2   v  4   w 4   x 8   y  4   z 10
);

sub makesquare
{
    my ($self, $board, %args) = @_;
    my ($x, $y, $squarestyle, $textstyle, $colour, $text, $prefix) =
        map { defined($_) ? $_ : "" } @args{qw(x y squarestyle textstyle colour text prefix)};

    my $xc = $x * ($square_size + $margin) + $margin;
    my $yc = $y * ($square_size + $margin) + $margin;

    my $group = $board->group(id => "${prefix}tile_${x}_${y}");
    my $square = $group->rect(
            width  => $square_size,
            height => $square_size,
            x      => $xc,
            y      => $yc,
            style  => "fill:$colour;$squarestyle",
        );

    $group->text(
            x     => 0.20 * $square_size + $xc,
            y     => 0.67 * $square_size + $yc,
            style => "font-size:${font_size}pt;fill:white;font-family:monospace;$textstyle",
        )->tspan->cdata($text);

    return $group;
}

sub makeletter
{
    my ($self, $board, %args) = @_;
    my ($x, $y, $letter, $isblank) = @args{qw(x y letter isblank)};
    my $group = $self->makesquare($board, %args, colour => $colours{tile}, prefix =>
            "letter_", text => uc $letter, squarestyle => "fill-opacity:85%",
            textstyle => "fill:black");

    my $xc = $x * ($square_size + $margin) + $margin;
    my $yc = $y * ($square_size + $margin) + $margin;
    my $font_size = $font_size * 0.5;

    unless ($isblank) {
        $group->text(
                x     => 0.55 * $square_size + $xc,
                y     => 0.77 * $square_size + $yc,
                style => "font-size:${font_size}pt;fill:black;font-family:monospace",
            )->tspan->cdata($values{lc $letter});
    }

    return $group;
}

sub drawmargins
{
    my ($self, $board, %args) = @_;
    my $g = $board->group(id => 'margins');
    for my $y (0 .. $args{y}) {
        $g->rect(
                id     => "margin_row_$y",
                y      => $y * ($square_size + $margin),
                height => $margin,
                width  => $args{board_width},
                style  => "fill:$colours{border}",
            );
    }
    for my $x (0 .. $args{x}) {
        $g->rect(
                id     => "margin_col_$x",
                x      => $x * ($square_size + $margin),
                height => $args{board_height},
                width  => $margin,
                style  => "fill:$colours{border}",
            );
    }

    return $g;
}

# four reflection symmetries implicit
# NOTE these are WordFeud coordinates, not Scrabble
my %specials = (
        TW => [ [0, 4], ],
        DL => [ [0, 7], [1, 1], [2, 6], [4, 6], ] ,
        DW => [ [2, 2], [3, 7], [4, 4], ], 
        TL => [ [0, 0], [1, 5], [3, 3], [5, 5], ],
    );

sub _makeboard
{
    my ($self, $g, $desc) = @_;

    my @specials = (
        "" => '#E3F6FF',
        DL => '#2E89CD',
        TL => '#579B0B',
        DW => '#E32745',
        TW => '#FB4C08',
    );

    my $y = 0;
    for my $row (@{ $desc->{board} }) {
        my $x = 0;
        for my $elt (@$row) {
            my @args = (colour => $specials[$elt * 2 + 1],
                        text   => $specials[$elt * 2 + 0],
                        style  => "fill-opacity:100%");
            $self->makesquare($g, x => $x, y => $y, @args);
            $x++;
        }
        $y++;
    }

    $self->makesquare($g, x => 7, y => 7, colour => $colours{centre}, prefix =>
            "centre_", text => "\N{BLACK STAR}", textstyle =>
            "font-size:@{[1.1*$font_size]}pt");
}

sub makeboard
{
	my ($self, $game, $desc) = @_;

	my $priv = $game->{_m};
    my $boardsvg = $priv->{svg}{board} = SVG->new(width => $board_size, height => $board_size);
    my $board = $priv->{boardgroup} = $boardsvg->group(id => 'board');
    $board->rect(
            id     => "rect_board",
            style  => "fill:$colours{board}",
            x      => 0,
            y      => 0,
            width  => $board_size,
            height => $board_size,
        );
    $self->drawmargins($board, x => 15, y => 15, board_height => $board_size, board_width => $board_size);

    $self->_makeboard($board, $desc);
}

sub makerack
{
	my ($self, $game, $letters) = @_;
    # TODO don't reconstruct from scratch all the time
    my $racksvg = $game->{_m}{svg}{rack} = SVG->new(width => $rack_width, height => $rack_height);
    my $rack = $self->{rack} = $racksvg->group(id => 'rack');
    $rack->rect(
            id     => "rect_rack",
            style  => "fill:$colours{rack}",
            x      => 0,
            y      => 0,
            width  => $rack_width,
            height => $rack_height,
        );

    my $x = 0;
	# XXX {_m} needs removing
    $game->{_m}{rack} = $letters;
    for my $tile (@$letters) {
        $self->makeletter($rack, x => $x, y => 0, letter => uc $tile);
        $x++;
    }
}

1;

