# ShrimpLang 2.0 (There is now a [Shrimp3](https://github.com/T-O-R-U-S/shrimp3), for your enjoyment!)

A rewrite of the original shrimp, but hopefully done right this time.

Mild syntactic differences, if you want to check those out, you might want to take a look at the [main](main.imp) file for the basic Shrimp program (Shrimp this program will sometimes error out!)

Currently, the main concern is making ShrimpLang 2 turning complete (That's already been done, actually!) -- after that, comes bringing it up to spec with ShrimpLang 1 (Which has also already been done)

## Update, it's turing complete -- up next, array manipulation (but first I need to optimize this spaghetti)!

It's also ~10x slower than Python. I ran a benchmark to check. This is a sad day for the Shrimp people (but also unexpected. Why'd you think I ran a benchmark!? The code is spaghetti! :()

For now, I plan to focus on getting rid of all of the deep cloning that is happening, because it is a massive detriment to performance. There are also probably other places to optimize.
