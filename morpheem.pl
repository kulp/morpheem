#!/usr/bin/env perl

package Morpheem;
use base qw(Gtk2::GladeXML::Simple);

use PAR;
use lib ".";
use lib "Morpheem";

use strict;
use warnings;

use utf8;

use Morpheem::Renderer::SVG;

use Gtk2 -init;
use Gtk2::SimpleList;
use Glib::Event;
use Event;
use AnyEvent;
use Coro;

use File::Temp;
use JSON;
use List::MoreUtils qw(uniq);
use List::Util qw(min sum);
use WWW::Mechanize::Gzip;
use YAML qw(LoadFile);

use XXX;

my $glade_file = "board.glade";

my $serverid = sprintf "%02d", int rand 7;
my $urlbase = qq(http://game$serverid.wordfeud.com/wf);

# XXX how naughty
my $square_size = $Morpheem::Renderer::SVG::square_size;
my $margin = $Morpheem::Renderer::SVG::margin;

# XXX
my $pixels = 60;

sub _updatescore
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

sub _loadboard
{
    my ($self, $game) = @_;
    my $board = $game->{_m}{boardgroup};
    for my $tile (@{ $game->{tiles} }) {
        # TODO handle blanks
        my ($x, $y, $letter, $isblank) = @$tile;
        $game->{_m}{renderer}->makeletter(parent => $board, x => $x, y => $y, letter => $letter, isblank => $isblank);
        $game->{_m}{board}[$y][$x] = $letter;
    }

    $self->_updatescore($game);
}

sub _setrack
{
    my ($self, $game, $letters) = @_;
    $game->{_m}{rack} = [ @$letters ] if $letters;

    $game->{_m}{renderer}->makerack($game->{_m}{rack});
    $self->get_widget('rackarea')->queue_draw;
}

sub _loadrack
{
    my ($self, $game, $rack) = @_;
    $self->_setrack($game, $rack);

    # TODO why do I need || [] here
    $game->{_m}{rackbak} = [ @{ $game->{_m}{rack} || [] } ];
}

sub _myturn
{
    my ($self, $game) = @_;
    my $running     = $game->{is_running};
    my $currplayer  = $game->{current_player};
    my $myturn      = !!($running && $currplayer == $self->_me($game)->{position});
}

sub _me
{
    my ($self, $game) = @_;
    my $players = $game->{players};
    my ($me) = grep { $_->{id} eq $self->{_me}{id} } @$players;
    return $me;
}

sub loadgame
{
    my ($self, $game) = @_;

    my $me = $game->{_m}{me} = $self->_me($game);

    my $desc = decode_json($self->{_www}->get($urlbase . "/board/$game->{board}/")->content)->{content};
    $game->{_m}{renderer}->makeboard($desc);

    $self->_loadboard($game);
    $self->_loadrack($game, $me->{rack});

    $self->{_games}{ $game->{id} } = $game;

    my $sl          = $self->{_list};
    my $yes         = $sl->render_icon("gtk-yes", "small-toolbar");
    my $no          = $sl->render_icon("gtk-no" , "small-toolbar");
    my @icons       = ($no, $yes);
    my $myturn      = $self->_myturn($game);
    my $players     = $game->{players};
    my $otherplayer = $players->[1 - $me->{position}]; # TODO support more than two players ?
    my $rows        = $sl->{data};

    my $small_pixbuf;
    # TODO don't fetch the same image more than once
    async {
        eval {
            my $file = File::Temp->new;
            my $w = $self->{_www}->clone;
            $w->get("http://avatars.wordfeud.com/$pixels/$otherplayer->{id}");
            $w->save_content($file);
            # TODO avoid hitting filesystem ?
            my $large_pixbuf = Gtk2::Gdk::Pixbuf->new_from_file($file);
            $small_pixbuf = $large_pixbuf->scale_simple(48, 48, "GDK_INTERP_HYPER");
        };

        push @$rows,
            [ $game->{id}, $icons[$myturn], $small_pixbuf, $otherplayer->{username} ];

        $sl->select($#$rows);
        $self->{_currentgame} = $game; # XXX
    };
}

sub _row_activated_cb
{
    my ($self, $sl, $path, $column) = @_;
    my $row_ref = $sl->get_row_data_from_path($path);
    my $gameid  = $row_ref->[0]; # id field
    my $game    = $self->{_games}{$gameid};

    $self->{_currentgame} = $game;

    $self->_updatescore($game);

    $self->get_widget('buttonplay')->sensitive($self->_tilesout($game));
    $self->get_widget('buttonclear')->sensitive($self->_tilesout($game));

    $self->get_widget($_)->queue_draw for qw(
            rackarea
            boardarea
            buttonplay
            buttonclear
            labelUser0points
            labelUser1points
            labelUser0points
            labelUser1points
            labeltilesleft
        );
}

sub new
{
    my ($class, %args) = @_;
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

    my $w = $self->{_www} = WWW::Mechanize::GZip->new(
            # XXX lies
            agent           => 'WebFeudClient/1.2.11 (iOS)',
            default_headers => HTTP::Headers->new(
                    # XXX Content_Type here doesn't get respected ?
                    Content_Type    => 'application/json',
                    Accept_Encoding => 'gzip',
                    Connection      => 'keep-alive',
                ),
        );

    my $sl = $self->{_list} = Gtk2::SimpleList->new_from_treeview(
            $self->get_widget('treeviewgames'),
            id     => 'text',
            State  => 'pixbuf',
            Avatar => 'pixbuf',
            User   => 'text',
            Move   => 'text',
        );

    $sl->get_selection->set_mode("browse"); # always have one selected
    $sl->get_column(0)->set(visible => 0);
    $sl->signal_connect(row_activated => sub { $self->_row_activated_cb(@_) });

    $self->{_www}->post($urlbase . '/user/login/',
            Content_Type => 'application/json',
            Content      => encode_json({
                                username => $args{username},
                                password => $args{password},
                            }),
        );

    $self->{_me} = decode_json($w->content)->{content};

    $w->post($urlbase . '/user/games/');
    my $games = decode_json($w->content)->{content}{games};

    for my $gr (@$games) {
        $w->post($urlbase . "/game/$gr->{id}/");
        my $game = decode_json($w->content)->{content}{game};
        $game->{_m}{renderer} = Morpheem::Renderer::SVG->new;
        $self->loadgame($game);
    }

    # hold open the AE engine
    $self->{_run} = AnyEvent->condvar;

    $self->get_widget('mainwindow')->set_title("Morpheem ($self->{_me}{username})");

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

    my $size = $self->_boardsize($b);
    my $game = $self->{_currentgame};
    my $pixbuf = $game->{_m}{renderer}->renderboard(
            game   => $self->{_currentgame},
            height => $size,
            width  => $size,
        );
    $pixbuf->render_to_drawable($b->window, $b->style->fg_gc($b->state),
            0, 0, 0, 0, $size, $size, "GDK_RGB_DITHER_NONE", 0, 0);

    return 0; # propagate
}

sub rackarea_draw_cb
{
    my ($self, $b, $event) = @_;

    my $height = $b->allocation->height;
    my $width  = $height * 7 + $margin * 8;
    $b->set_size_request($width, $height);

    my $game = $self->{_currentgame};
    my $pixbuf = $game->{_m}{renderer}->renderrack(
            height => $height,
            width  => $width,
        );
    $pixbuf->render_to_drawable($b->window, $b->style->fg_gc($b->state),
            0, 0, 0, 0, $width, $height, "GDK_RGB_DITHER_NONE", 0, 0);

    return 0; # propagate
}

sub blanksarea_draw_cb
{
    my ($self, $b, $event) = @_;

    my $height = $b->allocation->height;
    my $width  = int($height / 5 * 6);
    #$b->set_size_request($width, $height);
    my $game = $self->{_currentgame};
    my $pixbuf = $game->{_m}{renderer}->renderblanks(
            height => $height,
            width  => $width,
        );
    $pixbuf->render_to_drawable($b->window, $b->style->fg_gc($b->state),
            0, 0, 0, 0, $width, $height, "GDK_RGB_DITHER_NONE", 0, 0);

    return 0; # propagate
}

sub blanksclick_cb
{
    my ($self, $b, $event) = @_;
    my $size = $b->allocation->width / 6;
    my ($x, $y) = map { int $_ / $size } ($event->x, $event->y);
    # x and y are in cell coordinates

    my $char = ord('A') + $y * 6 + $x;
    if ($char >= ord('A') and $char <= ord('Z')) {
        $self->get_widget('blanksdialog')->response($char);
    }

    return 0;
}

sub blanksdialog_cancel_cb
{
    my ($self, $b) = @_;
    $self->get_widget('blanksdialog')->hide;
}

sub boardclick_cb
{
    my ($self, $ebox, $event) = @_;
    my $size = $self->_cellsize;
    my ($x, $y) = map { int $_ / $size } ($event->x, $event->y);
    # x and y are in cell coordinates

    my $game = $self->{_currentgame};
    if (defined $self->{_hotletter}) {
        my $letter = $self->{_hotletter};
        my $isblank = $letter eq "";
        my $already = $game->{_m}{renderer}->get_laid_tile(x => $x, y => $y);
        # TODO support swapping with existing tile ?
        return if $already;

        if ($isblank) {
            my $blankval = $self->get_widget('blanksdialog')->run;
            return unless $blankval;
            $letter = chr($blankval);
        }

        my $group = $game->{_m}{renderer}->makeletter(x => $x, y => $y, letter => $letter, isblank => $isblank);
        $self->get_widget('buttonclear')->sensitive(1);
        $self->get_widget('buttonplay')->sensitive(1);

        my $id = $group->getElementID;
        $game->{_m}{temptiles}{$id} = {
            letter  => $letter,
            group   => $group,
            x       => $x,
            y       => $y,
            id      => $id,
            isblank => $isblank,
        };
        $self->_back_out(delete $game->{_m}{temprack});

        splice @{ $game->{_m}{rack} }, $self->{_hotindex}, 1;
        $self->_setrack($game);

        $self->{_hotindex } = undef;
        $self->{_hotletter} = undef;
    } else {
        # take a letter back
        my $id = "letter_tile_${x}_${y}";
        if (my $hash = delete $game->{_m}{temptiles}{$id}) {
            my $letter = $hash->{isblank} ? "" : $hash->{letter};
            $self->_back_out([ $hash->{group} ]);
            $self->_setrack($game, [ @{ $game->{_m}{rack} }, $letter ]);
        }
    }

    $self->get_widget('boardarea')->queue_draw;
    $self->get_widget('buttonclear')->queue_draw;
    $self->get_widget('buttonplay')->queue_draw;

    return 0;
}

sub rackclick_cb
{
    my ($self, $rack, $event) = @_;
    my ($x) = int $event->x / $rack->allocation->height;

    my $game = $self->{_currentgame};
    # TODO allow multiple selection for exchange
    $self->_back_out(delete $game->{_m}{temprack});
    return if $x >= @{ $game->{_m}{rack} };

    if (defined $self->{_hotletter}) {
        my $r = $game->{_m}{rack};
        ($r->[$x], $r->[$self->{_hotindex}]) = ($r->[$self->{_hotindex}], $r->[$x]);
        $self->_setrack($game);

        $self->{_hotindex } = undef;
        $self->{_hotletter} = undef;
    } else {
        $game->{_m}{renderer}->highlight(x => $x, y => 0);

        $self->{_hotindex } = $x;
        $self->{_hotletter} = $game->{_m}{rack}[$x];
    }

    $self->get_widget('rackarea')->queue_draw;

    return 0;
}

sub shuffle_rack
{
    my ($self, $button) = @_;

    my $game = $self->{_currentgame} or return;

    $self->{_hotindex } = undef;
    $self->{_hotletter} = undef;

    my @new;
    my @old = @{ $game->{_m}{rack} };
    return if @old <= 1;
    my $tries = 0;
    MIX: {
        @new = ();
        my @temp = @old;
        push @new, splice @temp, rand +@temp, 1 while @temp;

        # try to not get the same permutation (feels like a bug to the user)
        # we don't run forever because if we have all the same tile the loop
        # will never complete, and we don't want to check for this explicitly
        for my $i (0 .. $#new) {
            # NOTE we could exhaust entropy here in theory
            last MIX if $new[$i] ne $old[$i] or $tries++ > 10;
        }
        redo MIX;
    }

    $self->_setrack($game, \@new);
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
    $tiles ||= [ values %{ $game->{_m}{temptiles} } ];
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

    my $board = $game->{_m}{board};

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
        return (defined($len) and $len > @$tiles) ? @ok : ();
    }
}

sub _play_move
{
    my ($self, $game) = @_;

    my @tiles = map {
        [ 0+$_->{x}, 0+$_->{y}, $_->{letter}, $_->{isblank} ? JSON::true : JSON::false ]
    } values %{ $game->{_m}{temptiles} };
    my @words = $self->_find_words($game);
    # TODO complain loudly to the user when there are no words
    return unless @words;

    my %data = (
            ruleset => 0,
            words   => \@words,
            move    => \@tiles,
        );

    my $w = $self->{_www};
    $w->post($urlbase . "/game/$game->{id}/move/",
            Content      => encode_json(\%data),
            Content_Type => 'application/json',
        );
    my $response = decode_json($w->content);
    if ($response->{status} ne "success") {
        WWW $response;
    } else {
        $self->_setrack($game, [ @{ $game->{_m}{rack} }, @{ $response->{content}{new_tiles} } ]);
        # TODO we shouldn't have to reload the game all the time
    }
}

sub buttonplay_clicked_cb
{
    my ($self, $button) = @_;
    $self->_play_move($self->{_currentgame});
}

sub _tilesout
{
    my ($self, $game) = @_;
    return +@{ $game->{_m}{rackbak} } != +@{ $game->{_m}{rack} };
}

sub buttonclear_clicked_cb
{
    my ($self, $button) = @_;
    my $game = $self->{_currentgame};

    # TODO when does _rackbak get updated ?
    $self->_setrack($game, [ @{ $game->{_m}{rackbak} } ]);

    $self->_back_out([ map $_->{group}, values %{ delete $game->{_m}{temptiles} } ]);
    $self->_back_out(delete $game->{_m}{temprack});

    $self->get_widget('buttonplay')->sensitive($self->_tilesout($game));
    $self->get_widget('buttonclear')->sensitive($self->_tilesout($game));

    # TODO buttons don't update their look until I mouse over them ?
    $self->get_widget($_)->queue_draw for qw(
            rackarea boardarea buttonplay buttonclear
        );

    return 0; # propagate ?
}

sub gtk_main_quit  { Gtk2->main_quit }
sub show_about_box { shift->get_widget('aboutdialog1')->show }
sub hide_dialog    { $_[1]->hide }

package main;
use strict;
use warnings;

use Digest::SHA1 qw(sha1_hex);

my $username = shift or die "supply username on command-line";
my $password = 'qqqqqq';
my $salt = "JarJarBinks9";
my $pwhash = sha1_hex($password . $salt);
Morpheem->new(username => $username, password => $pwhash)->run;

