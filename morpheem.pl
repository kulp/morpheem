#!/usr/bin/env perl

package Morpheem;
use base qw(Gtk2::GladeXML::Simple);

use PAR;
#use lib "morpheem";

use strict;
use warnings;

use charnames ':full';
use utf8;

use Gtk2 '-init';
use Gtk2::Ex::Simple::List;

use Gnome2::Rsvg;
use JSON;
use List::Util qw(min sum);
use List::MoreUtils qw(uniq);
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
    my ($board, %args) = @_;
    my ($x, $y, $squarestyle, $textstyle, $colour, $text, $prefix) =
        map { defined() ? $_ : "" } @args{qw(x y squarestyle textstyle colour text prefix)};

    my $xc = $x * ($square_size + $margin) + $margin;
    my $yc = $y * ($square_size + $margin) + $margin;

    my $group = $board->group(id => "${prefix}tile_${x}_${y}");
    my $square = $group->rect(
            # TODO ids ?
            #id     => "rect_${x}_${y}",
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
    my ($board, %args) = @_;
    my ($x, $y, $letter, $isblank) = @args{qw(x y letter isblank)};
    my $group = makesquare($board, %args, colour => $colours{tile}, prefix =>
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

# four reflection symmetries implicit
# NOTE these are WordFeud coordinates, not Scrabble
my %specials = (
        TW => [ [0, 4], ],
        DL => [ [0, 7], [1, 1], [2, 6], [4, 6], ] ,
        DW => [ [2, 2], [3, 7], [4, 4], ], 
        TL => [ [0, 0], [1, 5], [3, 3], [5, 5], ],
    );

sub makedefaultboard
{
    my ($board) = @_;

    makesquare($board, x => 7, y => 7, colour => $colours{centre}, prefix =>
            "centre_", text => "\N{BLACK STAR}", textstyle =>
            "font-size:@{[1.1*$font_size]}pt");
    for my $type (keys %specials) {
        for my $c (@{ $specials{$type} }) {
            my ($x, $y) = @$c;
            my @args = (colour => $colours{$type},
                        text   => $type,
                        style  => "fill-opacity:100%");
            # Construct reflection symmetries. We end up writing over the
            # diagonals twice, but at 100% opacity we don't care.
            makesquare($board, x =>      $x, y =>      $y, @args, prefix => "octant_a_");
            makesquare($board, x =>      $x, y => 14 - $y, @args, prefix => "octant_b_");
            makesquare($board, x => 14 - $x, y =>      $y, @args, prefix => "octant_c_");
            makesquare($board, x => 14 - $x, y => 14 - $y, @args, prefix => "octant_d_");
            makesquare($board, x =>      $y, y =>      $x, @args, prefix => "octant_e_");
            makesquare($board, x =>      $y, y => 14 - $x, @args, prefix => "octant_f_");
            makesquare($board, x => 14 - $y, y =>      $x, @args, prefix => "octant_g_");
            makesquare($board, x => 14 - $y, y => 14 - $x, @args, prefix => "octant_h_");
        }
    }
}

sub updatescore
{
    my ($self, $game) = @_;

    my $players = $game->{players};
    my @users = sort { $a->{position} <=> $b->{position} } @$players;

    $self->get_widget('labelUser0points')->set_text($users[0]->{score});
    $self->get_widget('labelUser0'      )->set_text($users[0]->{username});

    $self->get_widget('labelUser1points')->set_text($users[1]->{score});
    $self->get_widget('labelUser1'      )->set_text($users[1]->{username});

    $self->get_widget('labeltilesleft'  )->set_text($game->{bag_count});
}

sub loadboard
{
    my ($self, $board, $game) = @_;
    for my $tile (@{ $game->{tiles} }) {
        # TODO handle blanks
        my ($x, $y, $letter, $isblank) = @$tile;
        makeletter($board, x => $x, y => $y, letter => $letter, isblank => $isblank);
        $self->{_board}[$y][$x] = $letter;
    }

    $self->updatescore($game);
}

sub setrack
{
    my ($self, $game, $letters) = @_;
    $letters ||= $game->{_rack};

    my $racksvg = $self->{racksvg} = SVG->new(width => $rack_width, height => $rack_height);
    my $racklayer = $racksvg->group(id => 'layer');
    my $rack = $self->{rack} = $racklayer->group(id => 'rack');
    $rack->rect(
            id     => "rect_rack",
            style  => "fill:$colours{rack}",
            x      => 0,
            y      => 0,
            width  => $rack_width,
            height => $rack_height,
        );

    my $x = 0;
    $game->{_rack} = $letters;
    for my $tile (@$letters) {
        makeletter($rack, x => $x, y => 0, letter => uc $tile);
        $x++;
    }

}

sub loadrack
{
    my ($self, $game) = @_;
    my $players = $game->{players};
    # XXX finding myself this way is a hack
    my ($me) = grep { $_->{rack} } @$players;
    $self->setrack($game, $me->{rack});

    $self->{_rackbak} = [ @{ $game->{_rack} } ];
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

    my $boardsvg = $self->{boardsvg} = SVG->new(width => $board_size, height => $board_size);
    my $boardlayer = $boardsvg->group(id => 'layer');
    my $board = $self->{board} = $boardlayer->group(id => 'board');
    $board->rect(
            id     => "rect_board",
            style  => "fill:$colours{board}",
            x      => 0,
            y      => 0,
            width  => $board_size,
            height => $board_size,
        );
    drawmargins($board, x => 15, y => 15, board_height => $board_size, board_width => $board_size);

    makedefaultboard($board);
    my $dump = LoadFile("board.yaml") or die "Failed to load board";
    $self->loadboard($board, my $game = $self->{_currentgame} = $dump->{content}{game} || die "Bad board");
    $self->loadrack($game, $dump->{content} || die "Bad rack");

    my $tv = $self->get_widget('treeviewgames');
    my $sl = Gtk2::Ex::Simple::List->new_from_treeview(
                    $tv, qw(
                    Ready       pixbuf
                    Icon        pixbuf
                    With        text
                    ));

    push @{ $sl->{data} },
         [ $tv->render_icon("gtk-yes", "small-toolbar"), undef, "Anon2345" ],
         [ $tv->render_icon("gtk-no" , "small-toolbar"), undef, "Anon0123" ];

    return $self;
}

# takes a GtkWidget
sub _boardsize
{
    my $self = shift;
    my $b = shift || $self->get_widget('boardarea');
    return min($b->allocation->width, $b->allocation->height);
}

sub _cellsize
{
    return shift->_boardsize / 15;
}

sub boardarea_draw_cb
{
    my ($self, $b, $event) = @_;

    my $r = Gnome2::Rsvg::Handle->new;

    my $boardsvg = $self->{boardsvg};
    my $size = $self->_boardsize($b);

    $r->set_size_callback( sub { ($size, $size) });
    $r->write($boardsvg->xmlify) or die "Failed to write SVG to RSVG handle";
    $r->close or die "Failed to parse SVG";
    # XXX deprecated, use Cairo
    $r->get_pixbuf->render_to_drawable($b->window, $b->style->fg_gc($b->state),
            0, 0, 0, 0, $size, $size, "GDK_RGB_DITHER_NONE", 0, 0);

    return 0; # propagate
}

sub rackarea_draw_cb
{
    my ($self, $b, $event) = @_;

    my $r = Gnome2::Rsvg::Handle->new;

    my $racksvg = $self->{racksvg};
    my $height = $b->allocation->height;
    my $width  = $height * 7 + $margin * 8;
    $b->set_size_request($width, $height);
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

    my $game = $self->{_currentgame};
    if (defined $self->{_hotletter}) {
        # XXX implement isblank
        my $isblank = 0;
        # XXX abstract naming convention
        my $already = $self->{boardsvg}->getElementByID("letter_tile_${x}_${y}");
        # TODO support swapping with existing tile ?
        return if $already;

        my $letter = $self->{_hotletter};
        my $group = makeletter($self->{boardsvg}, x => $x, y => $y, letter => $letter, isblank => $isblank);
        $self->get_widget('buttonclear')->sensitive(1);
        $self->get_widget('buttonplay')->sensitive(1);

        my $id = $group->getElementID;
        $self->{_temptiles}{$id} = {
            letter  => $letter,
            group   => $group,
            x       => $x,
            y       => $y,
            id      => $id,
            isblank => $isblank ? JSON::true : JSON::false,
        };
        $self->_back_out(delete $self->{_temprack});

        splice @{ $game->{_rack} }, $self->{_hotindex}, 1;
        $self->setrack($game);

        $self->{_hotindex } = undef;
        $self->{_hotletter} = undef;
    } else {
        # take a letter back
        my $id = "letter_tile_${x}_${y}";
        if (my $hash = delete $self->{_temptiles}{$id}) {
            my $letter = $hash->{letter};
            $self->_back_out([ $hash->{group} ]);
            $self->setrack($game, [ @{ $game->{_rack} }, $letter ]);
        }
    }

    $self->get_widget('rackarea')->queue_draw;
    $self->get_widget('boardarea')->queue_draw;
    $self->get_widget('buttonclear')->queue_draw;
    $self->get_widget('buttonplay')->queue_draw;

    return 0;
}

sub rackclick_cb
{
    my ($self, $rack, $event) = @_;
    my ($x) = int $event->x / $rack->allocation->height;

    my $game = $rack->{_currentgame};
    # TODO allow multiple selection for exchange
    $self->_back_out(delete $self->{_temprack});
    return if $x >= @{ $game->{_rack} };

    if (defined $self->{_hotletter}) {
        my $r = $game->{_rack};
        ($r->[$x], $r->[$self->{_hotindex}]) = ($r->[$self->{_hotindex}], $r->[$x]);
        $self->setrack($game);

        $self->{_hotindex } = undef;
        $self->{_hotletter} = undef;
    } else {
        my $group = makesquare($self->{racksvg}, x => $x, y => 0, squarestyle
                => "fill-opacity:25%", colour => "red");
        push @{ $self->{_temprack} }, $group;

        $self->{_hotindex } = $x;
        $self->{_hotletter} = $game->{_rack}[$x];
    }

    $self->get_widget('rackarea')->queue_draw;

    return 0;
}

sub shuffle_rack
{
    my ($self, $button) = @_;

    my $game = $self->{_currentgame};

    $self->{_hotindex } = undef;
    $self->{_hotletter} = undef;

    my @new;
    my @old = @{ $game->{_rack} };
    return if @old <= 1;
    MIX: {
        @new = ();
        my @temp = @old;
        push @new, splice @temp, rand +@temp, 1 while @temp;

        # ensure we don't get the same permutation (feels like a bug to the user)
        for my $i (0 .. $#new) {
            # NOTE we could exhaust entropy here in theory
            last MIX if $new[$i] ne $old[$i];
        }
        redo MIX;
    }

    $self->setrack($game, \@new);
    $self->get_widget('rackarea')->queue_draw;
}

sub _back_out
{
    my ($self, $list) = @_;
    if ($list and my @tiles = @$list) {
        while (my $tile = pop @tiles) {
            $tile->getParentElement->removeChild($tile) if $tile->getParentElement;
        }
    }
}

sub _find_words
{
    sub num { $a <=> $b }

    my ($self, $game, $tiles) = @_;
    $tiles ||= [ values %{ $self->{_temptiles} } ];
    return () unless @$tiles;

    my @words;

    # First, check that the move is legal
    my @xs = sort { $b->{x} - $a->{x} } @$tiles;
    my @ys = sort { $b->{y} - $a->{y} } @$tiles;
    my $dx = $xs[0]->{x} - $xs[-1]->{x};
    my $dy = $ys[0]->{y} - $ys[-1]->{y};

    return () if $dx and $dy;   # all in same axis

    # TODO handle first move specially

    my @map;
    for my $tile (@$tiles) {
        $map[$tile->{y}][$tile->{x}] = $tile->{letter};
    }

    # TODO fix warnings

    my $board = $self->{_board};

    if (!$dx) {
        my $x = $xs[0]->{x};
        for my $y ($xs[0]->{y} .. $xs[-1]->{y}) {
            return () if not $board->[$y][$x] and not $map[$y][$x];
        }
    }

    if (!$dy) {
        my $y = $ys[0]->{y};
        for my $x ($ys[0]->{x} .. $ys[-1]->{x}) {
            return () if not $board->[$y][$x] and not $map[$y][$x];
        }
    }

    push @words, join "", map $_->{letter}, @xs if !$dx && $dx == @xs - 1;
    push @words, join "", map $_->{letter}, @ys if !$dy && $dy == @ys - 1;

    for my $tile (@xs) {
        my $y = $tile->{y};
        my $xSTART = my $xEND = my $x = $tile->{x};
        1 while $xSTART >  0 && ($board->[$y][--$xSTART] || $map[$y][$xSTART]);
        1 while $xEND   < 14 && ($board->[$y][++$xEND  ] || $map[$y][$xEND  ]);

        # TODO why is `grep defined' needed
        my @stuff = grep defined, map { $board->[$y][$_] || $map[$y][$_] } $xSTART .. $xEND;
        push @words, join "", @stuff;
    }

    for my $tile (@ys) {
        my $x = $tile->{x};
        my $ySTART = my $yEND = my $y = $tile->{y};
        1 while $ySTART >  0 && ($board->[--$ySTART][$x] || $map[$ySTART][$x]);
        1 while $yEND   < 14 && ($board->[++$yEND  ][$x] || $map[$yEND  ][$x]);

        # TODO why is `grep defined' needed
        my @stuff = grep defined, map { $board->[$_][$x] || $map[$_][$x] } $ySTART .. $yEND;
        push @words, join "", @stuff;
    }

    my @ok = uniq grep { length > 1 } @words;
    if (!$game->{move_count}) {
        my $centred = grep { $_->{x} == 7 and $_->{y} == 7 } @$tiles;
        return $centred ? @ok : ();
    } else {
        # iff the tiles are adjacent to something, the total length of the words
        # created will be greater than the number of tiles placed
        my $len = sum map length, @ok;
        return $len > @$tiles ? @ok : ();
    }
}

sub _play_move
{
    my ($self, $game) = @_;

    my @tiles = map {
        [ $_->{x}, $_->{y}, $_->{letter}, $_->{isblank} ? JSON::true : JSON::false ]
    } values %{ $self->{_temptiles} };
    my @words = $self->_find_words($game);
    # TODO complain loudly to the user when there are no words
    return unless @words;

    my %data = (
            ruleset => 0,
            words   => \@words,
            move    => \@tiles,
        );

    warn encode_json(\%data);
}

sub buttonplay_clicked_cb
{
    my ($self, $button) = @_;
    $self->_play_move($self->{_currentgame});
}

sub buttonclear_clicked_cb
{
    my ($self, $button) = @_;
    my $game = $self->{_currentgame};

    # TODO when does _rackbak get updated ?
    $self->setrack($game, [ @{ $self->{_rackbak} } ]);

    $self->_back_out([ map $_->{group}, values %{ delete $self->{_temptiles} } ]);
    $self->_back_out(delete $self->{_temprack});

    $self->get_widget('buttonplay')->sensitive(0);
    $self->get_widget('buttonclear')->sensitive(0);

    # TODO buttons don't update their look until I mouse over them ?
    $self->get_widget('rackarea')->queue_draw;
    $self->get_widget('boardarea')->queue_draw;
    $self->get_widget('buttonplay')->queue_draw;
    $self->get_widget('buttonclear')->queue_draw;

    return 0; # propagate ?
}

sub gtk_main_quit  { Gtk2->main_quit }
sub show_about_box { shift->get_widget('aboutdialog1')->show }
sub hide_about_box { $_[1]->hide }

package main;
use strict;
use warnings;

Morpheem->new->run;

