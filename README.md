Nanowrimo mode
==============
[![MELPA](http://melpa.org/packages/nanowrimo-badge.svg)](http://melpa.org/#/nanowrimo)

It the month of November countless people write 50,000 words as a part
of [Nanowrimo](http://www.nanowrimo.org).  It's no surprise that
consistency and motivation are key ingredients to success.  This mode
is meant to help provide feedback on progress and, by showing current
speed, perhaps even a little motivation.

This mode adds to the modeline a wordcount, words per minute, and an
estimate of how long you will have to continue writing to meet the
day's goal.

Feedback and feature requests are welcome, as are pull requests.

Installation
============

Installation is easiest performed via
[MELPA](http://melpa.milkbox.net).  You can also download it, add it's
path to `load-path` and `(require 'nanowrimo)`.

Usage
=====

After installation simply type, `M-x nanowrimo-mode RET` and the
current word count will appear in the modeline.  Moreover, the words
per minute of the current session will also appear as well as an
estimate of when you will meet today's goal, as set by
`nanowrimo-today-goal`.  When you are done writing simply turn it off
with by typing `M-x nanowrimo-mode RET` again.

If you wish to not show the current WPM or the estimate their display
can be turned off, and a few other settings changed via customize:

    (customize-group 'nanowrimo)

If you wish to change the way words are counted please you can
customize `nanowrimo-count-words-function`.  By default all words (as
defined by `forward-word`) are counted unless you are in org mode
*and* have [org-wc](https://github.com/dato/org-wc) installed.  In
this case, only the current subtree is counted with the thought that
you will create a subtree for each day.  In addition, org-mode markup
like drawers are ignored when counting words.

With org-mode
-------------

If you use org-mode, you can run `M-x nanowrimo-insert-org-table RET`
to insert a table which will track progress throughout nanowrimo and
give you a score.  The table is from Bob Newell, and can be used to
set `nanowrimo-today-goal` automatically.  Simply customize
`nanowrimo-today-goal-calculation-function`.

There is also a redacted export which can be used to create an
anonymous version of your novel for submitting to the word count
fairies.