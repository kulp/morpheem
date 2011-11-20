#!/usr/bin/env perl

package Furries;
use base qw(Gtk2::GladeXML::Simple);

use PAR;
#use lib "furries";

use strict;
use warnings;

use charnames ':full';
use utf8;

use Gtk2 '-init';
use Gnome2::Rsvg;
use JSON;
use List::Util qw(min);
use SVG;
use YAML qw(LoadFile);

my $glade_file = "board.glade";

my $square_size = 25;
my $margin      = 1;
my $board_size  = 15 * $square_size + 16 * $margin;
my $rack_width  = 7 * $square_size + 8 * $margin;
my $rack_height = $square_size + 2 * $margin;
my $font_size   = 0.39 * $square_size;

# colours lifted from Google Images pictures of real scrabble boards (tile
# colour lightened)
my %colours = (
        TW => "#FB4C08",
        DL => "#2E89CD",
        DW => "#E32745",
        TL => "#579B0B",

        board  => "#E3F6FF",
        border => "#20111C",
        center => "#E32745",
        tile   => "#DFBC95",
        rack   => "#CCAA77",
    );

# TODO other languages
# XXX these are WordFeud point values, not Scrabble !
my %values = (
    a =>  1,
    b =>  4,
    c =>  4,
    d =>  2,
    e =>  1,
    f =>  4,
    g =>  3,
    h =>  4,
    i =>  1,
    j => 10,
    k =>  5,
    l =>  1,
    m =>  3,
    n =>  1,
    o =>  1,
    p =>  4,
    q => 10,
    r =>  1,
    s =>  1,
    t =>  1,
    u =>  2,
    v =>  4,
    w =>  4,
    x => 10, # TODO check
    y =>  4,
    z => 10,
);

my %private;

sub makesquare
{
    my ($board, %args) = @_;
    my ($x, $y, $squarestyle, $textstyle, $colour, $text) = @args{qw(x y squarestyle textstyle colour text)};
    $squarestyle ||= "";
    $textstyle ||= "";
    $text ||= "";

    my $xc = $x * ($square_size + $margin) + $margin;
    my $yc = $y * ($square_size + $margin) + $margin;

    my $square = $board->rect(
            # TODO ids ?
            #id     => "rect_${x}_${y}",
            width  => $square_size,
            height => $square_size,
            x      => $xc,
            y      => $yc,
            style  => "fill:$colour;$squarestyle",
        );

    $board->text(
            x     => 0.20 * $square_size + $xc,
            y     => 0.67 * $square_size + $yc,
            style => "font-size:${font_size}pt;fill:white;font-family:monospace;$textstyle",
        )->tspan->cdata($text);
}

sub makeletter
{
    my ($board, %args) = @_;
    my ($x, $y, $letter, $isblank) = @args{qw(x y letter isblank)};
    makesquare($board, %args, colour => $colours{tile}, text => uc $letter,
            squarestyle => "fill-opacity:85%", textstyle => "fill:black");

    my $xc = $x * ($square_size + $margin) + $margin;
    my $yc = $y * ($square_size + $margin) + $margin;
    my $font_size = $font_size * 0.5;

    unless ($isblank) {
        $board->text(
                x     => 0.55 * $square_size + $xc,
                y     => 0.77 * $square_size + $yc,
                style => "font-size:${font_size}pt;fill:black;font-family:monospace",
            )->tspan->cdata($values{lc $letter});
    }
}

sub drawmargins
{
    my ($board, %args) = @_;

    for my $y (0 .. $args{y}) {
        $board->rect(
                id     => "margin_row_$y",
                y      => $y * ($square_size + $margin),
                height => $margin,
                width  => $args{board_width},
                style  => "fill:$colours{border}",
            );
    }
    for my $x (0 .. $args{x}) {
        $board->rect(
                id     => "margin_col_$x",
                x      => $x * ($square_size + $margin),
                height => $args{board_height},
                width  => $margin,
                style  => "fill:$colours{border}",
            );
    }
}

my %specials = (
        TW => [ [ 0, 0], [ 7, 0], [14, 0],
                [ 0, 7],          [14, 7],
                [ 0,14], [ 7,14], [14,14] ],
        DL => [ [ 3, 0],          [11, 0],
                [ 6, 2],          [ 8, 2],
                [ 0, 3], [ 7, 3], [14, 3],
                [ 2, 6], [ 6, 6], [ 8, 6], [12, 6],
                [ 3, 7],          [11, 7],
                [ 2, 8], [ 6, 8], [ 8, 8], [12, 8],
                [ 0,11], [ 7,11], [14,11],
                [ 6,12],          [ 8,12],
                [ 3,14],          [11,14], ],
        DW => [ [ 1, 1],          [13, 1],
                [ 2, 2],          [12, 2],
                [ 3, 3],          [11, 3],
                [ 4, 4],          [10, 4],
                [ 4,10],          [10,10],
                [ 3,11],          [11,11],
                [ 2,12],          [12,12],
                [ 1,13],          [13,13], ],
        TL => [ [ 5, 1],          [ 9, 1],
                [ 1, 5], [ 5, 5], [ 9, 5], [13, 5],
                [ 1, 9], [ 5, 9], [ 9, 9], [13, 9],
                [ 5,13],          [ 9,13], ],

    );

sub makedefaultboard
{
    my ($board) = @_;

    makesquare($board, x => 7, y => 7, colour => $colours{center}, text =>
            "\N{BLACK STAR}", textstyle => "font-size:@{[1.1*$font_size]}pt");
    for my $type (keys %specials) {
        for my $c (@{ $specials{$type} }) {
            makesquare($board, x => $c->[0], y => $c->[1], colour => $colours{$type}, text => $type);
        }
    }
}

sub loadboard
{
    my ($self, $board, $game) = @_;
    for my $tile (@{ $game->{game}{tiles} }) {
        # TODO handle blanks
        my $x = $tile->[0];
        my $y = $tile->[1];
        my $letter = $tile->[2];
        my $isblank = $tile->[3];
        makeletter($board, x => $x, y => $y, letter => $letter, isblank => $isblank);
        $self->{_board}[$y][$x] = $letter;
    }
}

sub loadrack
{
    my ($self, $rack, $game) = @_;
    my $players = $game->{game}{players};
    # XXX finding myself this way is a hack
    my ($me) = grep { $_->{rack} } @$players;
    my $x = 0;
    for my $tile (@{ $me->{rack} }) {
        makeletter($rack, x => $x, y => 0, letter => uc $tile);
        $self->{_rack}[$x] = $tile;
        $x++;
    }
}

sub new
{
    my $class = shift;
    my $self;
    if (-f $glade_file) {
        $self = $class->SUPER::new($glade_file);
    } else {
        # Maybe we were packed with PAR
        my $tmp = File::Temp->new;
        print $tmp PAR::read_file(basename $glade_file);
        close $tmp;
        $self = $class->SUPER::new($tmp->filename);
    }

    my $boardsvg = $private{$self}{boardsvg} = SVG->new(width => $board_size, height => $board_size);
    my $boardlayer = $boardsvg->group(id => 'layer');
    my $board = $private{$self}{board} = $boardlayer->group(id => 'board');
    $board->rect(
            id     => "rect_board",
            style  => "fill:$colours{board}",
            x      => 0,
            y      => 0,
            width  => $board_size,
            height => $board_size,
        );
    drawmargins($board, x => 15, y => 15, board_height => $board_size, board_width => $board_size);

    my $racksvg = $private{$self}{racksvg} = SVG->new(width => $rack_width, height => $rack_height);
    my $racklayer = $racksvg->group(id => 'layer');
    my $rack = $private{$self}{rack} = $racklayer->group(id => 'rack');
    $rack->rect(
            id     => "rect_rack",
            style  => "fill:$colours{rack}",
            x      => 0,
            y      => 0,
            width  => $rack_width,
            height => $rack_height,
        );
    #drawmargins($rack, x => 7, y => 1, board_height => $rack_height, board_width => $rack_width);

    makedefaultboard($board);
    my $dump = LoadFile("board.yaml") or die "Failed to load board";
    $self->loadboard($board, $dump->{content} || die "Bad board");
    $self->loadrack($rack  , $dump->{content} || die "Bad rack");

    return $self;
}

# takes a GtkWidget
sub _boardsize
{
    my $self = shift;
    my $b = shift || $self->get_widget('boardarea');
    my $size = min($b->allocation->width, $b->allocation->height);
}

sub _cellsize
{
    return shift->_boardsize / 15;
}

sub boardarea_draw_cb
{
    my ($self, $b, $event) = @_;
    warn "drawing $b";

    my $r = Gnome2::Rsvg::Handle->new;

    my $boardsvg = $private{$self}{boardsvg};
    my $size = $self->_boardsize($b);
    $r->set_size_callback( sub { ($size, $size) });
    $r->write($boardsvg->xmlify) or die "Failed to write SVG to RSVG handle";
    $r->close or die "Failed to parse SVG";
    # XXX deprecated, use Cairo
    #warn $boardsvg->xmlify;
    $r->get_pixbuf->render_to_drawable($b->window, $b->style->fg_gc($b->state),
            0, 0, 0, 0, $size, $size, "GDK_RGB_DITHER_NONE", 0, 0);

    return 0; # propagate
}

sub rackarea_draw_cb
{
    my ($self, $b, $event) = @_;
    warn "drawing $b";

    my $r = Gnome2::Rsvg::Handle->new;

    my $racksvg = $private{$self}{racksvg};
    my $height = $b->allocation->height;
    my $width  = $height * 7 + $margin * 8;
    $r->set_size_callback( sub { ($height, $width) });
    $r->write($racksvg->xmlify) or die "Failed to write SVG to RSVG handle";
    $r->close or die "Failed to parse SVG";
    # XXX deprecated, use Cairo
    $r->get_pixbuf->render_to_drawable($b->window, $b->style->fg_gc($b->state),
            0, 0, 0, 0, $width, $height, "GDK_RGB_DITHER_NONE", 0, 0);

    return 0; # propagate
}

sub boardclick_cb
{
    my ($self, $ebox, $event) = @_;
    my $size = $self->_cellsize;
    my ($x, $y) = map { int $_ / $size } ($event->x, $event->y);
    # x and y are in cell coordinates
    warn "$x, $y";
    return 0;
}

sub gtk_main_quit  { Gtk2->main_quit }
sub show_about_box { shift->get_widget('aboutdialog1')->show }
sub hide_about_box { $_[1]->hide }

package main;
use strict;
use warnings;

Furries->new->run;

